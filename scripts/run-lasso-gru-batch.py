"""Run the frozen Cloze Lasso/GRU condition on Big Blue, with durable receipts."""
import argparse
import csv
import datetime
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time


def sha(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as f:
        for block in iter(lambda: f.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def write(path, value):
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(value, indent=2), encoding="utf-8")
    tmp.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config", type=Path)
    a = parser.parse_args()
    c = json.loads(a.config.read_text(encoding="utf-8-sig"))
    if os.name != "posix" or not os.environ.get("SLURM_JOB_ID"):
        raise ValueError("Scientific runs require the declared Big Blue Slurm runtime.")
    workers = int(c["workers"])
    if not 1 <= workers <= 3 or int(os.environ["SLURM_CPUS_PER_TASK"]) < workers:
        raise ValueError("Allocate at least one CPU per single-threaded fit, maximum three fits.")
    for key in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS"):
        if os.environ.get(key) != "1":
            raise ValueError("Single-thread runtime variable differs: " + key)
    root, out, reference = map(Path, (c["root"], c["output"], c["reference"]))
    script = root / "scripts/lasso-gru-study.R"
    env = os.environ.copy()
    env["LKT_RELEASE_FILE"] = c["release_file"]
    env["LKT_RUNTIME_CONFIG"] = str(a.config.resolve())
    limit = int(c["storage_bytes_limit"])

    def space():
        total = 0
        for path in c["storage_roots"]:
            total += int(subprocess.check_output(["du", "-sb", path], text=True).split()[0])
        return total

    def run(action):
        with (out / "logs" / (action + ".log")).open("a", encoding="utf-8") as f:
            subprocess.run([c["rscript"], "--vanilla", str(script), str(root), str(out),
                            str(reference), action], env=env, stdout=f, stderr=subprocess.STDOUT, check=True)

    if space() + 250000000 > limit:
        raise ValueError("Insufficient space under the combined home/project storage cap.")
    # Plan creates this tree; never overwrite an existing frozen plan.
    if not (out / "study.rds").exists():
        if out.exists():
            raise ValueError("Incomplete existing study directory needs explicit review.")
        log = out.parent / (out.name + "-plan.log")
        with log.open("w", encoding="utf-8") as f:
            subprocess.run([c["rscript"], "--vanilla", str(script), str(root), str(out),
                            str(reference), "plan"], env=env, stdout=f, stderr=subprocess.STDOUT, check=True)
    protocol = json.loads((out / "protocol.json").read_text())
    fingerprint = sha(out / "study.rds")
    if protocol["protocol_sha256"] != fingerprint or protocol["candidate_count"] != 318:
        raise ValueError("Protocol identity or candidate count differs.")
    for path, expected in protocol["script_hashes"].items():
        if sha(root / path) != expected:
            raise ValueError("Frozen runner changed: " + path)
    run("select")
    with (out / "tasks.csv").open(newline="") as f:
        tasks = list(csv.DictReader(f))
    if len(tasks) != 15:
        raise ValueError("Expected five folds and three seeds, exactly fifteen tasks.")
    pending, done, active, failed = [], [], {}, []
    for index, task in enumerate(tasks, start=1):
        receipt = out / "reports" / (task["run_id"] + "-complete.json")
        if receipt.exists():
            saved = json.loads(receipt.read_text())
            if saved["protocol_sha256"] != fingerprint:
                raise ValueError("Completed task belongs to another protocol.")
            for path, expected in saved["files"].items():
                if sha(out / path) != expected:
                    raise ValueError("Completed artifact changed: " + path)
            done.append(task["run_id"])
        else:
            pending.append((index, task))
    status_path = out / "batch-status.json"
    last_check, total = 0, 0
    while pending or active:
        if time.monotonic() - last_check > 60:
            total, last_check = space(), time.monotonic()
        while pending and len(active) < workers and not failed and total + 100000000 < limit:
            index, task = pending.pop(0)
            attempt = len(list((out / "logs").glob(task["run_id"] + "-attempt*.log"))) + 1
            log = out / "logs" / (task["run_id"] + f"-attempt{attempt}.log")
            handle = log.open("w", encoding="utf-8")
            process = subprocess.Popen([c["rscript"], "--vanilla", str(script), str(root), str(out),
                                        str(reference), "fit", str(index)],
                                       env=env, stdout=handle, stderr=subprocess.STDOUT)
            active[task["run_id"]] = (process, handle)
        for name, (process, handle) in list(active.items()):
            code = process.poll()
            if code is None:
                continue
            handle.close()
            del active[name]
            if code:
                failed.append({"task": name, "exit_code": code})
                continue
            files = [f"models/{name}.rds", f"training/{name}.csv",
                     f"reports/{name}-metrics.csv", f"reports/{name}-predictions.csv"]
            write(out / "reports" / (name + "-complete.json"),
                  {"protocol_sha256": fingerprint, "files": {p: sha(out / p) for p in files}})
            done.append(name)
        write(status_path, {"utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
                            "job_id": os.environ["SLURM_JOB_ID"], "completed": done,
                            "active": list(active), "pending": [t[1]["run_id"] for t in pending],
                            "failed": failed, "storage_bytes": total, "storage_limit": limit})
        if not active and (failed or total + 100000000 >= limit):
            raise RuntimeError("Batch held after a fit failure or insufficient storage; no running jobs were terminated.")
        if active:
            time.sleep(10)
    run("report")
    subprocess.run([c["python"], "-B", str(root / "scripts/verify-pfa-gru-metrics.py"),
                    "--prediction-dir", str(out / "reports"), "--pattern", "*-predictions.csv",
                    "--reference-fits", str(out / "reports/fold-seed-results.csv"),
                    "--output", str(out / "reports/independent-metric-audit.json")], check=True, env=env)
    write(out / "completed.json", {"protocol_sha256": fingerprint, "fits": len(done),
                                   "summary_sha256": sha(out / "reports/summary.csv")})


if __name__ == "__main__":
    main()
