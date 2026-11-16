"""Host-Python-3.6 controller; all Python runtime work executes in the pinned SIF."""
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tarfile
import tempfile
import urllib.request

PYTHON_URL = "https://github.com/astral-sh/python-build-standalone/releases/download/20260901/cpython-3.12.14+20260901-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz"
PYTHON_SHA = "72748da13197c1fb161e3afeef20a6a385ff24f2165e6e2758e47008e7faba4c"
REQUIRED = {"schema", "provider", "workspace", "container", "container_sha256",
 "r_dependency_receipt", "r_dependency_receipt_sha256", "controller", "controller_sha256",
 "worker", "worker_sha256", "runtime_dir", "runtime_limit_bytes", "model_dir",
 "manifest_sha256", "package_lock", "package_lock_sha256", "helper", "helper_sha256",
 "original_package_lock", "original_package_lock_sha256",
 "linux_dependency_contract", "linux_dependency_contract_sha256",
 "probe_request", "probe_request_sha256", "reports_dir", "python_url", "python_sha256",
 "storage_roots", "persistent_limit_bytes", "required_free_bytes", "locale"}


def sha(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def inside(path, parent):
    return path == parent or parent in path.parents


def usage(roots):
    return sum(int(subprocess.check_output(["du", "-sx", "-B1", "--", str(root)],
                   universal_newlines=True).split()[0]) for root in roots)


def save(path, record):
    temp = path.with_suffix(".json.tmp")
    temp.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    temp.replace(path)


def main():
    if len(sys.argv) != 2:
        raise ValueError("Usage: setup-pfa-semantic-runtime.py CONFIG.json")
    config = Path(sys.argv[1]).resolve(strict=True)
    cfg = json.loads(config.read_text(encoding="utf-8-sig"))
    if (set(cfg) != REQUIRED or cfg["schema"] != 2 or
            cfg["provider"] != "python-build-standalone" or
            cfg["python_url"] != PYTHON_URL or cfg["python_sha256"] != PYTHON_SHA):
        raise ValueError("Select the documented exact managed CPython3.12.14 provider/archive contract.")
    job = os.environ.get("SLURM_JOB_ID", "")
    cpus = os.environ.get("SLURM_CPUS_PER_TASK", "")
    if not job.isdecimal() or not cpus.isdecimal() or int(cpus) < 1:
        raise ValueError("Semantic setup requires its Big Blue Slurm allocation.")
    workspace = Path(cfg["workspace"]).resolve(strict=True)
    if not inside(config, workspace):
        raise ValueError("Stage the configuration inside the explicitly selected workspace.")
    for name in ("container", "r_dependency_receipt", "controller", "worker", "package_lock", "original_package_lock", "linux_dependency_contract", "helper", "probe_request"):
        path = Path(cfg[name]).resolve(strict=True)
        if not Path(cfg[name]).is_absolute() or not inside(path, workspace) or sha(path) != cfg[name + "_sha256"]:
            raise ValueError("Selected artifact path/hash mismatch: " + name)
    if cfg["original_package_lock_sha256"] != "bbb795b9c1f42aae978a4c637bbf42f96ecfbbf93d7448bbb94d9c7a58558f9c":
        raise ValueError("The immutable original 34-distribution version record changed.")
    if Path(__file__).resolve() != Path(cfg["controller"]).resolve():
        raise ValueError("Invoke the selected maintained controller.")
    r_receipt = json.loads(Path(cfg["r_dependency_receipt"]).read_text())
    if (not r_receipt["complete"] or r_receipt["phase"] != "dependencies" or
            r_receipt["container_sha256"] != cfg["container_sha256"] or
            not r_receipt["r_version"].startswith("R version 4.5.3 ")):
        raise ValueError("The exact SIF requires its verified R4.5.3 dependency receipt.")
    runtime = Path(cfg["runtime_dir"]).resolve()
    reports = Path(cfg["reports_dir"]).resolve()
    model = Path(cfg["model_dir"]).resolve(strict=True)
    for path in (runtime, reports, model):
        if path == workspace or not inside(path, workspace) or any(c in str(path) for c in ",:\n\r"):
            raise ValueError("Runtime, reports and staged model must be explicit workspace descendants.")
    if runtime.exists() and any(runtime.iterdir()):
        raise ValueError("A fresh empty semantic runtime is required; failed attempts are never resumed.")
    if any(inside(a, b) for a, b in ((model, runtime), (runtime, model), (reports, runtime))):
        raise ValueError("The one installed runtime, existing model and provenance must be separate directories.")
    manifest = model / "lkt-semantic-manifest.json"
    if sha(manifest) != cfg["manifest_sha256"]:
        raise ValueError("The staged existing semantic manifest changed.")
    meta = json.loads(manifest.read_text())
    if meta["revision"] != "e8c3b32edf5434bc2275fc9bab85f82640a19130":
        raise ValueError("Only the already selected frozen MPNet revision is permitted.")
    for record in meta["files"]:
        path = (model / record["path"]).resolve(strict=True)
        if not inside(path, model) or path.stat().st_size != record["bytes"] or sha(path) != record["sha256"]:
            raise ValueError("Staged model file differs: " + record["path"])
    roots = [Path(path).resolve(strict=True) for path in cfg["storage_roots"]]
    if (len(roots) != len(set(roots)) or any(inside(a, b) for a in roots for b in roots if a != b) or
            not any(inside(workspace, root) for root in roots)):
        raise ValueError("Declare distinct nonoverlapping total home/project storage roots including workspace.")
    for name, ceiling in (("persistent_limit_bytes", 10000000000), ("runtime_limit_bytes", 2000000000)):
        if type(cfg[name]) is not int or not 0 < cfg[name] <= ceiling:
            raise ValueError("Invalid explicit storage ceiling: " + name)
    before = usage(roots)
    if type(cfg["required_free_bytes"]) is not int or cfg["required_free_bytes"] <= 0 or before + cfg["required_free_bytes"] > cfg["persistent_limit_bytes"]:
        raise ValueError("Insufficient declared persistent-storage headroom.")
    runtime.mkdir(parents=True, exist_ok=True)
    reports.mkdir(parents=True, exist_ok=True)
    record_path = reports / ("semantic-runtime-" + job + ".launcher.json")
    if record_path.exists():
        raise ValueError("This Slurm attempt already has a retained launcher record.")
    record = {"schema": 2, "complete": False, "job_id": job, "configuration_sha256": sha(config),
              "persistent_bytes_before": before, "persistent_limit_bytes": cfg["persistent_limit_bytes"]}
    save(record_path, record)
    with tempfile.TemporaryDirectory(prefix="pfa-semantic-" + job + "-", dir="/tmp") as temporary:
        scratch = Path(temporary)
        for name in ("home", "tmp", "cache"):
            (scratch / name).mkdir()
        archive = scratch / "cpython.tar.gz"
        urllib.request.urlretrieve(cfg["python_url"], archive)
        if sha(archive) != cfg["python_sha256"]:
            raise ValueError("Published CPython archive SHA256 mismatch.")
        with tarfile.open(archive) as bundle:
            for member in bundle.getmembers():
                destination = (runtime / member.name).resolve()
                if not inside(destination, runtime) or not (member.name.startswith("python/") or member.name == "python" and member.isdir()):
                    raise ValueError("Managed interpreter archive path escapes its declared installation.")
                if member.issym() or member.islnk():
                    target = ((destination.parent if member.issym() else runtime) / member.linkname).resolve()
                    if not inside(target, runtime):
                        raise ValueError("Managed interpreter archive link escapes its declared installation.")
            bundle.extractall(runtime)
        interpreter = runtime / "python/bin/python3.12"
        if not interpreter.is_file():
            raise ValueError("The verified managed archive lacks its declared Python3.12 executable.")
        environment = {
            "HOME": str(scratch / "home"), "TMPDIR": str(scratch / "tmp"),
            "XDG_CACHE_HOME": str(scratch / "cache"), "HF_HOME": str(scratch / "cache/hf"),
            "TORCH_HOME": str(scratch / "cache/torch"), "PIP_CONFIG_FILE": "/dev/null",
            "PIP_NO_CACHE_DIR": "1", "PYTHONDONTWRITEBYTECODE": "1", "PYTHONNOUSERSITE": "1",
            "HF_HUB_OFFLINE": "1", "TRANSFORMERS_OFFLINE": "1", "TOKENIZERS_PARALLELISM": "false",
            "OMP_NUM_THREADS": "1", "MKL_NUM_THREADS": "1", "OPENBLAS_NUM_THREADS": "1",
            "LC_ALL": cfg["locale"], "LANG": cfg["locale"], "LANGUAGE": "C",
            "SLURM_JOB_ID": job, "SLURM_CPUS_PER_TASK": cpus}
        command = ["singularity", "exec", "--userns", "--cleanenv", "--no-home",
                   "--bind", str(workspace) + ":" + str(workspace),
                   "--bind", str(model) + ":" + str(model) + ":ro",
                   "--bind", str(scratch) + ":" + str(scratch), "--pwd", str(workspace),
                   "--env", ",".join(k + "=" + v for k, v in environment.items()),
                   cfg["container"], str(interpreter), "-B", cfg["worker"], str(config), str(scratch), "install"]
        host_env = os.environ.copy()
        host_env.update({"TMPDIR": str(scratch / "tmp"), "SINGULARITY_TMPDIR": str(scratch / "tmp"),
                         "SINGULARITY_CACHEDIR": str(scratch / "cache")})
        record.update({"command": command, "scratch_directory": str(scratch), "python_archive_sha256": cfg["python_sha256"]})
        save(record_path, record)
        process = subprocess.Popen(command, env=host_env, start_new_session=True)
        try:
            while process.poll() is None:
                try:
                    process.wait(timeout=15)
                except subprocess.TimeoutExpired:
                    if usage(roots) > cfg["persistent_limit_bytes"] or usage([runtime]) > cfg["runtime_limit_bytes"]:
                        raise RuntimeError("Owned semantic installer exceeded its explicit storage ceiling.")
            if process.returncode:
                raise RuntimeError("In-SIF semantic setup exited " + str(process.returncode))
        except BaseException:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
            raise
        finally:
            record.update({"exit_code": process.returncode, "persistent_bytes_after": usage(roots),
                           "runtime_bytes": usage([runtime])})
            record["complete"] = (process.returncode == 0 and record["persistent_bytes_after"] <= cfg["persistent_limit_bytes"]
                                  and record["runtime_bytes"] <= cfg["runtime_limit_bytes"])
            save(record_path, record)
        if not record["complete"]:
            raise RuntimeError("Semantic runtime did not satisfy its completion/storage contract.")
    record["scratch_removed"] = not Path(record["scratch_directory"]).exists()
    save(record_path, record)
    print(json.dumps(record, indent=2))


if __name__ == "__main__":
    main()
