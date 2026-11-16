"""Independently recompute phase and seed-pooled AUC from explicit prediction exports."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

def auc(y, p):
    n1 = sum(y)
    n0 = len(y) - n1
    if not n1 or not n0:
        raise ValueError("AUC requires both observed classes.")
    ordered = sorted(zip(p, y), key=lambda pair: pair[0])
    positive_rank_sum = 0.0
    start = 0
    while start < len(ordered):
        end = start + 1
        while end < len(ordered) and ordered[end][0] == ordered[start][0]:
            end += 1
        average_rank = (start + 1 + end) / 2.0
        positive_rank_sum += average_rank * sum(label for _, label in ordered[start:end])
        start = end
    return (positive_rank_sum - n1 * (n1 + 1) / 2.0) / (n1 * n0)

def file_hash(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prediction-dir", type=Path, required=True)
    parser.add_argument("--pattern", required=True)
    parser.add_argument("--reference-fits", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    files = sorted(args.prediction_dir.glob(args.pattern))
    if len(files) != 15:
        raise ValueError("Exactly fifteen prediction exports are required.")
    with args.reference_fits.open(encoding="utf-8-sig", newline="") as stream:
        references = {(r["partition"], int(r["seed"])): r for r in csv.DictReader(stream)}
    expected_keys = {(f"cv{fold}", seed) for fold in range(1, 6) for seed in (1, 2, 3)}
    if set(references) != expected_keys:
        raise ValueError("Reference fit records do not form the complete five-fold/three-seed design.")
    pooled = {seed: {} for seed in (1, 2, 3)}
    seen = set()
    per_fit = []
    maximum_delta = 0.0
    for path in files:
        with path.open(encoding="utf-8-sig", newline="") as stream:
            rows = list(csv.DictReader(stream))
        keys = {(r["partition"], int(r["seed"])) for r in rows}
        if len(keys) != 1:
            raise ValueError("Each export must contain one fold and seed.")
        key = next(iter(keys))
        if key in seen or key not in expected_keys:
            raise ValueError("Duplicate or unexpected fit export.")
        seen.add(key)
        row_ids = [int(r["source_row"]) for r in rows]
        if len(set(row_ids)) != len(row_ids):
            raise ValueError("An export duplicates a source row.")
        by_role = {role: ([], []) for role in (1, 2, 3)}
        for row in rows:
            role = int(row["role"])
            observed = float(row["observed"])
            prediction = float(row["prediction"])
            if role not in by_role or observed not in (0, 1) or not math.isfinite(prediction) or not 0 < prediction < 1:
                raise ValueError("Invalid role, label or probability.")
            y, p = by_role[role]
            y.append(int(observed));p.append(prediction)
            if role == 3:
                source = int(row["source_row"])
                if source in pooled[key[1]]:
                    raise ValueError("Held-out source row appears in more than one fold for a seed.")
                pooled[key[1]][source] = (int(observed), prediction)
        metrics = {"partition": key[0], "seed": key[1], "file_sha256": file_hash(path)}
        for role, phase in ((1, "training"), (2, "validation"), (3, "heldout")):
            y, p = by_role[role]
            value = auc(y, p)
            reference = float(references[key][phase + "_AUC"])
            delta = abs(value - reference)
            maximum_delta = max(maximum_delta, delta)
            if delta > 1e-12:
                raise ValueError(f"Independent AUC differs for {key}, {phase}: {delta}")
            metrics[phase + "_AUC"] = value
            metrics[phase + "_rows"] = len(y)
        per_fit.append(metrics)
    if seen != expected_keys or any(len(rows) != 55122 for rows in pooled.values()):
        raise ValueError("Incomplete fit or pooled held-out coverage.")
    if any(set(pooled[seed]) != set(pooled[1]) for seed in (2, 3)):
        raise ValueError("Held-out source coverage differs across seeds.")
    seed_metrics = []
    for seed in (1, 2, 3):
        values = list(pooled[seed].values())
        y = [r[0] for r in values];p = [r[1] for r in values]
        seed_metrics.append({"seed": seed, "heldout_AUC": auc(y, p),
                             "heldout_log_loss": -sum(math.log(v) if label else math.log1p(-v) for label, v in values) / len(values)})
    result = {"complete": True, "independent_implementation": "Python standard-library Mann-Whitney average tied ranks",
              "fits": 15, "phase_auc_comparisons": 45, "maximum_auc_difference": maximum_delta,
              "per_fit": per_fit, "per_seed": seed_metrics,
              "mean_seed_pooled_heldout_AUC": sum(r["heldout_AUC"] for r in seed_metrics) / 3,
              "mean_training_AUC": sum(r["training_AUC"] for r in per_fit) / 15,
              "mean_validation_AUC": sum(r["validation_AUC"] for r in per_fit) / 15,
              "reference_fits_sha256": file_hash(args.reference_fits),
              "verification_script_sha256": file_hash(Path(__file__))}
    args.output.write_bytes((json.dumps(result, indent=2) + "\n").encode("utf-8"))
    print("PASS independent Python AUC: 45 phase comparisons, fifteen fits, three pooled seeds.")

if __name__ == "__main__":
    main()
