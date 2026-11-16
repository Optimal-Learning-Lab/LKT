"""Finalize one complete frozen PFA study and its paired shared-control comparison."""
import argparse
import csv
import hashlib
import json
from pathlib import Path

def sha(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1048576), b""):
            h.update(block)
    return h.hexdigest()

def read_json(path):
    return json.loads(path.read_text(encoding="utf-8-sig"))

def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))

def keyed(values):
    if len(values) != 15:
        raise ValueError("Expected exactly fifteen fold/seed records.")
    result = {(r["partition"], int(r["seed"])): r for r in values}
    if len(result) != 15 or set(result) != {(f"cv{f}", s) for f in range(1, 6) for s in (1, 2, 3)}:
        raise ValueError("Incomplete or duplicate fold/seed records.")
    return result

def mean(values):
    return sum(values) / len(values)

def atomic_json(path, value):
    temp = path.with_suffix(path.suffix + ".tmp")
    temp.write_bytes((json.dumps(value, indent=2, ensure_ascii=False) + "\n").encode("utf-8"))
    temp.replace(path)

def verify(directory):
    protocol = read_json(directory / "protocol.json")
    runtime_path = Path(protocol["runtime_provenance_file"])
    if sha(runtime_path) != protocol["runtime_provenance_sha256"] or protocol["runtime_provenance_sha256"] != protocol["readiness"]["runtime_provenance_sha256"]:
        raise ValueError("Frozen runtime provenance changed.")
    script_root = Path(protocol["source_root"])
    for name, expected in protocol["script_hashes"].items():
        if sha(script_root / name) != expected:
            raise ValueError("Frozen script changed: " + name)
    if Path(__file__).resolve() != (script_root / "scripts/finalize-pfa-gru-study.py").resolve():
        raise ValueError("Use the maintained frozen finalization script.")
    reports = directory / "reports"
    audit = read_json(reports / "independent-python-auc.json")
    if protocol["total_fits"] != 15 or not audit["complete"] or audit["phase_auc_comparisons"] != 45:
        raise ValueError("Missing complete independent metric verification.")
    if sha(directory / "study.rds") != protocol["protocol_sha256"]:
        raise ValueError("Frozen protocol changed.")
    if sha(reports / "fits.csv") != audit["reference_fits_sha256"]:
        raise ValueError("Fit metrics changed after independent verification.")
    fits = keyed(rows(reports / "fits.csv"))
    independent = keyed(audit["per_fit"])
    for key, fit in fits.items():
        predictions = reports / (fit["run_id"] + "-predictions.csv")
        check = read_json(reports / (fit["run_id"] + "-verification.json"))
        if not check["complete"] or check["fit_sha256"] != sha(directory / "models" / (fit["run_id"] + ".rds")):
            raise ValueError("Saved weights or replay record changed.")
        if independent[key]["file_sha256"] != sha(predictions):
            raise ValueError("Prediction export changed after independent metric verification.")
        if float(check["maximum_replay_probability_difference"]) >= 3e-6:
            raise ValueError("Saved-model replay exceeds the fixed tolerance.")
    pooled = {int(r["seed"]): r for r in rows(reports / "pooled-by-seed.csv")}
    if set(pooled) != {1, 2, 3}:
        raise ValueError("Incomplete pooled held-out seed metrics.")
    for result in audit["per_seed"]:
        if abs(float(pooled[result["seed"]]["AUC"]) - result["heldout_AUC"]) > 1e-12:
            raise ValueError("R and independent Python pooled AUC disagree.")
    return protocol, fits, audit, pooled

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--condition-dir", required=True, type=Path)
    parser.add_argument("--control-dir", type=Path)
    args = parser.parse_args()
    directory = args.condition_dir.resolve()
    protocol, fits, audit, pooled = verify(directory)
    reports = directory / "reports"
    control = args.control_dir.resolve() if args.control_dir else None
    if protocol["kind"] == "mechanism" and control is None:
        raise ValueError("A mechanism requires the verified shared control.")
    if protocol["kind"] == "mechanism" and control != Path(protocol["spec"]["control_directory"]).resolve():
        raise ValueError("Control directory differs from the frozen mechanism specification.")
    comparison = None
    if control:
        base_protocol, base_fits, base_audit, base_pooled = verify(control)
        base_verification = read_json(control / "reports" / "verification.json")
        if not base_verification.get("complete") or base_protocol["kind"] != "shared_control":
            raise ValueError("Shared control must already have final verification.")
        for field in ("original_baseline_protocol_sha256", "data_sha256", "partition_hashes",
                      "candidate_source_sha256", "frozen_settings", "script_hashes", "runtime_provenance_sha256"):
            if protocol[field] != base_protocol[field]:
                raise ValueError("Incompatible shared-control protocol: " + field)
        runtime = protocol["readiness"].get("runtime_provenance_sha256")
        if not runtime or runtime != base_protocol["readiness"].get("runtime_provenance_sha256"):
            raise ValueError("Runtime/resource provenance differs from shared controls.")
        paired = []
        for key in sorted(fits):
            fit = fits[key]; base = base_fits[key]
            candidate_rows = rows(reports / (fit["run_id"] + "-predictions.csv"))
            baseline_rows = rows(control / "reports" / (base["run_id"] + "-predictions.csv"))
            identifiers = ("partition", "seed", "source_row", "participant", "role", "observed")
            if len(candidate_rows) != len(baseline_rows) or any(
                any(a[k] != b[k] for k in identifiers)
                for a, b in zip(candidate_rows, baseline_rows)
            ):
                raise ValueError("Paired predictions have different labels, rows, participants or masks.")
            entry = {"partition": key[0], "seed": key[1]}
            for phase in ("training", "validation", "heldout"):
                name = phase + "_AUC"
                entry[name] = float(fit[name])
                entry["baseline_" + name] = float(base[name])
                entry["delta_" + name] = float(fit[name]) - float(base[name])
            paired.append(entry)
        per_seed = []
        for seed in (1, 2, 3):
            local = [r for r in paired if r["seed"] == seed]
            result = {"seed": seed, "heldout_AUC": float(pooled[seed]["AUC"]),
                      "baseline_heldout_AUC": float(base_pooled[seed]["AUC"])}
            result["delta_heldout_AUC"] = result["heldout_AUC"] - result["baseline_heldout_AUC"]
            for phase in ("training", "validation"):
                for prefix in ("", "baseline_", "delta_"):
                    field = prefix + phase + "_AUC"
                    result[field] = mean([r[field] for r in local])
            per_seed.append(result)
        comparison = {
            "control_directory": str(control), "control_protocol_sha256": base_protocol["protocol_sha256"],
            "per_fit": paired, "per_seed": per_seed,
            "mean_delta_training_AUC": mean([r["delta_training_AUC"] for r in paired]),
            "mean_delta_validation_AUC": mean([r["delta_validation_AUC"] for r in paired]),
            "mean_delta_seed_pooled_heldout_AUC": mean([r["delta_heldout_AUC"] for r in per_seed]),
            "heldout_aggregation": "Pool participant-fold held-out rows within each seed, then average the three seed AUCs.",
            "interpretation": "Exploratory repeatedly used Cloze folds; no independent-fold significance claim, stacking or default promotion."
        }
        atomic_json(reports / "paired-baseline-comparison.json", comparison)
    parameter_counts = {int(r["parameter_count"]) for r in fits.values()}
    if len(parameter_counts) != 1:
        raise ValueError("Parameter count differs among fits.")
    result = {
        "complete": True, "condition": protocol["condition"], "kind": protocol["kind"],
        "fits": 15, "seeds": [1, 2, 3], "folds": 5,
        "protocol_sha256": protocol["protocol_sha256"],
        "independent_metrics_sha256": sha(reports / "independent-python-auc.json"),
        "finalization_script_sha256": sha(Path(__file__)),
        "mean_training_AUC": audit["mean_training_AUC"],
        "mean_validation_AUC": audit["mean_validation_AUC"],
        "mean_seed_pooled_heldout_AUC": audit["mean_seed_pooled_heldout_AUC"],
        "per_seed": audit["per_seed"], "parameter_count": parameter_counts.pop(),
        "sum_fit_seconds": sum(float(r["elapsed_seconds"]) for r in fits.values()),
        "maximum_replay_probability_difference": max(float(r["maximum_replay_probability_difference"]) for r in fits.values()),
        "paired_comparison": comparison,
        "all_source_rows_labels_and_masks_verified": True,
        "architecture_selection": "Frozen prior mean validation AUC; not reselected.",
        "checkpoint_selection": "Minimum validation log loss in each fit.",
        "status": "Complete exploratory batch; no default promotion."
    }
    atomic_json(reports / "verification.json", result)
    print(json.dumps({k: result[k] for k in ("complete", "condition", "mean_training_AUC", "mean_validation_AUC", "mean_seed_pooled_heldout_AUC", "parameter_count")}, indent=2))

if __name__ == "__main__":
    main()
