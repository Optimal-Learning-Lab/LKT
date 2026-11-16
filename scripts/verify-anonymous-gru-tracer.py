"""Independently audit the initial tracer's three phase AUCs, without fitting."""
import csv
import importlib.util
import json
import math
from pathlib import Path
import sys

root, out = map(Path, sys.argv[1:])
spec = importlib.util.spec_from_file_location("rank_audit", str(root / "scripts/verify-pfa-gru-metrics.py"))
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)
rows = list(csv.DictReader((out / "reports/predictions.csv").open()))
if len(rows) != 55122 or len({r["source_row"] for r in rows}) != len(rows):
    raise ValueError("Incomplete response coverage.")
students = {}
for r in rows:
    students.setdefault(r["student"], set()).add(r["role"])
if any(len(roles) != 1 for roles in students.values()):
    raise ValueError("Student roles overlap.")
references = {r["phase"]: r for r in csv.DictReader((out / "reports/metrics.csv").open())}
checks = []
for role, phase in (("1", "training"), ("2", "validation"), ("3", "heldout")):
    subset = [r for r in rows if r["role"] == role]
    y, p = [int(r["observed"]) for r in subset], [float(r["prediction"]) for r in subset]
    if not all(v in (0, 1) for v in y) or not all(math.isfinite(v) and 0 < v < 1 for v in p):
        raise ValueError("Invalid observations or probabilities.")
    value = audit.auc(y, p)
    loss = -sum(math.log(v) if label else math.log1p(-v) for label, v in zip(y, p)) / len(y)
    if abs(value - float(references[phase]["AUC"])) > 1e-12 or abs(loss - float(references[phase]["log_loss"])) > 1e-12:
        raise ValueError("Independent metrics differ.")
    checks.append({"phase": phase, "rows": len(y), "AUC": value, "log_loss": loss})
result = {"complete": True, "fits": 1, "phase_auc_comparisons": 3, "metrics": checks,
          "protocol_sha256": audit.file_hash(out / "study.rds"),
          "prediction_sha256": audit.file_hash(out / "reports/predictions.csv"),
          "model_sha256": audit.file_hash(out / "models/cv1__anonymous_constant__seed1.rds")}
(out / "reports/independent-audit.json").write_text(json.dumps(result, indent=2))
print("PASS independent initial-tracer AUC/log-loss audit, disjoint students and all response rows")
