"""Run exactly the frozen fifteen tasks, retaining each attempt and failure."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

def sha(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--root", type=Path, required=True)
    p.add_argument("--study", type=Path, required=True)
    p.add_argument("--condition-dir", type=Path, required=True)
    p.add_argument("--spec", type=Path, required=True)
    p.add_argument("--rscript", type=Path, required=True)
    p.add_argument("--candidate-json", type=Path, required=True)
    p.add_argument("--library", type=Path, required=True)
    p.add_argument("--baseline", type=Path, required=True)
    a = p.parse_args()
    out = a.condition_dir.resolve()
    if not (out / "study.rds").is_file():
        raise ValueError("Freeze the plan before launching fits.")
    protocol = json.loads((out / "protocol.json").read_text(encoding="utf-8-sig"))
    if sha(out / "study.rds") != protocol["protocol_sha256"]:
        raise ValueError("Frozen study protocol changed.")
    if protocol["total_fits"] != 15:
        raise ValueError("Expected exactly fifteen tasks.")
    if protocol.get("schema") != 2:
        raise ValueError("This runner requires a newly frozen schema-two protocol.")
    spec = json.loads(a.spec.read_text(encoding="utf-8-sig"))
    runtime_path = Path(protocol["runtime_provenance_file"]).resolve()
    if runtime_path != Path(spec["runtime_provenance_file"]).resolve():
        raise ValueError("Runtime provenance path differs from the frozen specification.")
    runtime_sha = sha(runtime_path)
    if runtime_sha != protocol["runtime_provenance_sha256"] or runtime_sha != protocol["readiness"]["runtime_provenance_sha256"]:
        raise ValueError("Runtime provenance changed after readiness/plan freeze.")
    runtime = json.loads(runtime_path.read_text(encoding="utf-8-sig"))
    if a.root.resolve() != Path(protocol["source_root"]).resolve():
        raise ValueError("Source root differs from the frozen protocol.")
    for name, expected in protocol["script_hashes"].items():
        if sha(a.root / name) != expected:
            raise ValueError("Frozen script changed: " + name)
    if Path(__file__).resolve() != (a.root / "scripts/run-pfa-gru-batch.py").resolve():
        raise ValueError("Launch the maintained, frozen batch runner.")
    if a.rscript.resolve() != Path(runtime["rscript_executable_path"]).resolve() or sha(a.rscript) != runtime["rscript_executable_sha256"]:
        raise ValueError("Rscript executable differs from the frozen runtime.")
    if a.library.resolve() != Path(runtime["candidate"]["library"]).resolve():
        raise ValueError("Installed candidate library differs from the frozen runtime.")
    workers = runtime["runtime_policy"]["maximum_concurrent_research_fits"]
    policy = runtime["runtime_policy"]
    allocated_cpus = policy["allocated_cpus"]
    if (type(workers) is not int or not 1 <= workers <= 15 or
            type(allocated_cpus) is not int or allocated_cpus < workers or
            policy["torch_threads_per_fit"] != 1):
        raise ValueError("Invalid frozen resource policy; declare 1..15 single-thread fits within allocated CPUs.")
    if (runtime.get("schema") != 2 or runtime.get("execution_host") != "Big Blue" or
            runtime.get("scheduler") != "slurm"):
        raise ValueError("Explicit portable environment requires runtime schema two.")
    declared_environment = policy["environment"]
    required_environment = {"LC_ALL", "LANG", "LANGUAGE", "R_TESTS",
                            "OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS"}
    if (not isinstance(declared_environment, dict) or
            set(declared_environment) != required_environment or
            any(not isinstance(value, str) for value in declared_environment.values()) or
            any(not declared_environment[name] for name in ("LC_ALL", "LANG", "LANGUAGE")) or
            declared_environment["R_TESTS"] != "" or
            any(declared_environment[name] != "1" for name in
                ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS"))):
        raise ValueError("Runtime must declare exact locale/startup/single-thread environment.")
    slurm_cpus = os.environ.get("SLURM_CPUS_PER_TASK", "")
    if not slurm_cpus.isdecimal() or int(slurm_cpus) < allocated_cpus:
        raise ValueError("Run inside Slurm on Big Blue with the declared CPU allocation.")
    lock = out / "batch.lock"
    logs = out / "logs"
    logs.mkdir(exist_ok=True)
    status_path = out / "batch-status.json"
    if status_path.exists():
        state = json.loads(status_path.read_text(encoding="utf-8-sig"))
        if state["condition"] != protocol["condition"] or state["research_tasks"] != 15 or state["protocol_sha256"] != protocol["protocol_sha256"]:
            raise ValueError("Existing batch status belongs to another protocol.")
        if state["complete"]:
            raise ValueError("Batch is already complete; report/finalize its existing fits.")
        if not isinstance(state["attempts"], list) or not isinstance(state["launches"], list):
            raise ValueError("Existing batch status lacks retained attempt/launch history.")
    else:
        state = {"complete": False, "condition": protocol["condition"], "research_tasks": 15,
                 "maximum_workers": workers, "protocol_sha256": protocol["protocol_sha256"],
                 "started_unix": time.time(), "attempts": [], "launches": []}
    state["launches"].append({"pid": os.getpid(), "started_unix": time.time(),
                              "runtime_provenance_sha256": runtime_sha})
    state["failure"] = False
    descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    os.write(descriptor, str(os.getpid()).encode("ascii"))
    os.close(descriptor)
    def save():
        temp = status_path.with_suffix(".json.tmp")
        temp.write_bytes((json.dumps(state, indent=2) + "\n").encode("utf-8"))
        temp.replace(status_path)
    active = {}
    next_task = 0
    failure = False
    environment = os.environ.copy()
    environment.update(declared_environment)
    environment["R_LIBS"] = os.pathsep.join(runtime["library_paths"])
    candidate = a.candidate_json.resolve()
    library = a.library.resolve()
    baseline = a.baseline.resolve()
    try:
        save()
        while next_task < 15 or active:
            while not failure and next_task < 15 and len(active) < workers:
                task = next_task
                prefix = logs / ("task-" + str(task).zfill(2))
                attempt_no = 1
                while Path(str(prefix) + f"-attempt-{attempt_no}.stdout.log").exists():
                    attempt_no += 1
                stdout_path = Path(str(prefix) + f"-attempt-{attempt_no}.stdout.log")
                stderr_path = Path(str(prefix) + f"-attempt-{attempt_no}.stderr.log")
                stdout = stdout_path.open("wb")
                stderr = stderr_path.open("wb")
                command = [str(a.rscript), "--vanilla", str(a.root / "scripts/pfa-gru-study.R"),
                           str(a.root), str(out), "fit", str(task), str(candidate),
                           str(library), str(baseline), str(a.spec)]
                process = subprocess.Popen(command, stdout=stdout, stderr=stderr, env=environment,
                                           creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
                attempt = {"task_id": task, "attempt": attempt_no, "pid": process.pid,
                           "started_unix": time.time(), "command": command, "stdout": str(stdout_path),
                           "stderr": str(stderr_path), "exit_code": None}
                state["attempts"].append(attempt)
                active[task] = (process, stdout, stderr, attempt)
                next_task += 1
                save()
                print(f"START task {task:02d}, process {process.pid}", flush=True)
            finished = []
            for task, (process, stdout, stderr, attempt) in active.items():
                code = process.poll()
                if code is not None:
                    stdout.close(); stderr.close()
                    attempt.update({"finished_unix": time.time(), "exit_code": code})
                    finished.append(task)
                    if code != 0:
                        failure = True
                    print(f"END task {task:02d}, exit {code}", flush=True)
            for task in finished:
                del active[task]
            if finished:
                save()
            if failure and not active:
                break
            time.sleep(1)
        state.update({"complete": not failure and next_task == 15,
                      "finished_unix": time.time(), "failure": failure})
        save()
        if failure:
            raise RuntimeError("A frozen task failed; unlaunched tasks remain pending. No automatic retraining or approximation.")
        print("COMPLETE all fifteen frozen tasks.", flush=True)
    finally:
        # Existing jobs are never targeted. Active owned fits may finish if the launcher is interrupted.
        if not active:
            lock.unlink()

if __name__ == "__main__":
    main()
