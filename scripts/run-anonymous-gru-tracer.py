"""Run one frozen baseline tracer test on Big Blue, without simulator integration."""
import datetime
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

cpath = Path(sys.argv[1]).resolve()
c = json.loads(cpath.read_text(encoding="utf-8-sig"))
if not os.environ.get("SLURM_JOB_ID") or os.name != "posix":
    raise ValueError("Scientific execution requires Big Blue Slurm.")
if any(os.environ.get(k) != "1" for k in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS")):
    raise ValueError("Use the declared single-threaded runtime.")
root, out, reference = map(Path, (c["root"], c["output"], c["reference"]))
env = os.environ.copy()
env["LKT_RUNTIME_CONFIG"] = str(cpath)
env["LKT_RELEASE_FILE"] = c["release_file"]

def sha(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()

def quota():
    total = sum(int(subprocess.check_output(["du", "-sb", p], text=True).split()[0])
                for p in c["storage_roots"])
    if total + 100000000 > int(c["storage_bytes_limit"]):
        raise ValueError("Insufficient space within the combined 10 GB storage cap.")
    print("STORAGE_BYTES", total, flush=True)
    return total

def run(action):
    log = out / "logs" / (action + ".log") if action != "plan" else out.parent / "results-plan.log"
    print("START", action, datetime.datetime.now(datetime.timezone.utc).isoformat(), flush=True)
    with log.open("a") as stream:
        subprocess.run([c["rscript"], "--vanilla", str(root / "scripts/anonymous-gru-tracer-study.R"),
                        str(root), str(out), str(reference), action], env=env,
                       stdout=stream, stderr=subprocess.STDOUT, check=True)

quota()
if not (out / "study.rds").exists():
    if out.exists():
        raise ValueError("Incomplete plan requires explicit review.")
    run("plan")
if not (out / "smoke.rds").exists():
    run("smoke")
quota()
run("fit")
subprocess.run([c["python"], "-B", str(root / "scripts/verify-anonymous-gru-tracer.py"),
                str(root), str(out)], env=env, check=True)
total = quota()
receipt = {"complete": True, "fits": 1, "fold": "cv1", "seed": 1,
           "protocol_sha256": sha(out / "study.rds"), "storage_bytes": total,
           "metrics_sha256": sha(out / "reports/metrics.csv"),
           "model_sha256": sha(out / "models/cv1__anonymous_constant__seed1.rds"),
           "audit_sha256": sha(out / "reports/independent-audit.json")}
(out / "completed.json").write_text(json.dumps(receipt, indent=2))
print("COMPLETE initial six-input anonymous tracer test", flush=True)
