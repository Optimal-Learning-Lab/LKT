"""Install an explicitly selected private PFA R environment inside Slurm only."""
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tarfile
import tempfile
import time
import urllib.request

CMAKE_URL = "https://github.com/Kitware/CMake/releases/download/v3.31.8/cmake-3.31.8-linux-x86_64.tar.gz"
CMAKE_SHA = "630615d8e98ac33eba7fbe472626dff5c899c85af3c024585ae109166a6909d0"
COMMON = {"Schema", "Workspace", "Private-Library", "Container", "Container-SHA256",
          "Description", "Description-SHA256", "Setup-R-Script", "Setup-R-SHA256",
          "Setup-Python-Script", "Setup-Python-SHA256", "Rscript-Executable",
          "Reports-Directory", "CRAN-Repository", "Install-CPUs", "Storage-Roots",
          "Storage-Bytes-Limit", "Required-Free-Bytes", "Locale"}
PACKAGE = {"Dependency-Receipt", "Dependency-Receipt-SHA256", "Candidate-JSON", "Candidate-JSON-SHA256"}


def sha(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1048576), b""):
            digest.update(block)
    return digest.hexdigest()


def inside(path, parent):
    return path == parent or parent in path.parents


def read_config(path, phase):
    result = {}
    for line in path.read_text(encoding="utf-8-sig").splitlines():
        if not line.strip():
            raise ValueError("DCF setup configuration must contain one unfolded record, without blank lines.")
        key, separator, value = line.partition(":")
        if not separator or key in result or key != key.strip() or not value.strip():
            raise ValueError("Invalid, empty or duplicated DCF field: " + key)
        result[key] = value.strip()
    expected = COMMON | (PACKAGE if phase == "package" else set())
    if set(result) != expected or result["Schema"] != "1":
        raise ValueError("Configuration must declare exactly the documented fields for this phase.")
    for key in expected:
        if key.endswith("-SHA256") and not re.fullmatch("[a-f0-9]{64}", result[key]):
            raise ValueError("An actual lowercase SHA256 is required: " + key)
    for key in ("Workspace", "Private-Library", "Container", "Description", "Setup-R-Script",
                "Setup-Python-Script", "Rscript-Executable", "Reports-Directory"):
        if not Path(result[key]).is_absolute() or any(c in result[key] for c in "\n\r,:|"):
            raise ValueError("An explicit safe absolute path is required: " + key)
    for key in ("Install-CPUs", "Storage-Bytes-Limit", "Required-Free-Bytes"):
        if not result[key].isdecimal() or int(result[key]) <= 0:
            raise ValueError("An explicit positive integer is required: " + key)
    if int(result["Storage-Bytes-Limit"]) > 10000000000:
        raise ValueError("The total persistent-storage ceiling cannot exceed ten billion bytes.")
    if not result["CRAN-Repository"].startswith("https://") or any(c in result["Locale"] for c in ",=\n\r"):
        raise ValueError("Declare an HTTPS CRAN repository and an explicit locale.")
    return result


def storage_bytes(roots):
    total = 0
    for root in roots:
        value = subprocess.check_output(["du", "-sx", "-B1", "--", str(root)], universal_newlines=True)
        total += int(value.split()[0])
    return total


def save(path, record):
    # Each Slurm attempt gets its own immutable launcher record.
    temporary = path.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)


def main():
    if len(sys.argv) != 3 or sys.argv[2] not in ("dependencies", "package"):
        raise ValueError("Usage: setup-pfa-environment.py CONFIG.dcf dependencies|package")
    config_path = Path(sys.argv[1]).resolve(strict=True)
    phase = sys.argv[2]
    cfg = read_config(config_path, phase)
    job = os.environ.get("SLURM_JOB_ID", "")
    allocation = os.environ.get("SLURM_CPUS_PER_TASK", "")
    if not job.isdecimal() or not allocation.isdecimal() or int(allocation) < int(cfg["Install-CPUs"]):
        raise ValueError("Environment setup requires its declared Slurm CPU allocation on Big Blue.")
    workspace = Path(cfg["Workspace"]).resolve(strict=True)
    if not inside(config_path, workspace):
        raise ValueError("The selected configuration must be staged inside its workspace.")
    private = Path(cfg["Private-Library"]).resolve()
    reports = Path(cfg["Reports-Directory"]).resolve()
    if private == workspace or reports == workspace or not inside(private, workspace) or not inside(reports, workspace):
        raise ValueError("Private library and reports must be explicit descendants of the workspace.")
    for name in ("Container", "Description", "Setup-R-Script", "Setup-Python-Script"):
        path = Path(cfg[name]).resolve(strict=True)
        if not inside(path, workspace) or sha(path) != cfg[name.replace("-Script", "") + "-SHA256"]:
            raise ValueError("Selected artifact path/hash mismatch: " + name)
    if Path(__file__).resolve() != Path(cfg["Setup-Python-Script"]).resolve():
        raise ValueError("Invoke the explicitly selected maintained setup controller.")
    if phase == "package":
        for name in ("Dependency-Receipt", "Candidate-JSON"):
            path = Path(cfg[name]).resolve(strict=True)
            if not inside(path, workspace) or sha(path) != cfg[name + "-SHA256"]:
                raise ValueError("Selected package-phase artifact mismatch: " + name)
    root_values = cfg["Storage-Roots"].split("|")
    if any(not Path(value).is_absolute() for value in root_values):
        raise ValueError("Storage accounting roots must be explicit absolute directories.")
    roots = [Path(value).resolve(strict=True) for value in root_values]
    if len(roots) != len(set(roots)) or any(inside(a, b) for a in roots for b in roots if a != b):
        raise ValueError("Storage accounting roots must be distinct and nonoverlapping.")
    if not any(inside(workspace, root) for root in roots):
        raise ValueError("The workspace must be included in total persistent storage accounting.")
    ceiling = int(cfg["Storage-Bytes-Limit"])
    before = storage_bytes(roots)
    if before + int(cfg["Required-Free-Bytes"]) > ceiling:
        raise ValueError("Insufficient declared persistent storage headroom; no installation started.")
    if phase == "dependencies" and private.exists() and any(private.iterdir()):
        raise ValueError("Dependency installation requires a fresh empty private library.")
    if phase == "package" and not private.is_dir():
        raise ValueError("Package phase requires the explicitly verified dependency library.")
    private.mkdir(parents=True, exist_ok=True)
    reports.mkdir(parents=True, exist_ok=True)
    record_path = reports / (phase + "-" + job + ".launcher.json")
    if record_path.exists():
        raise ValueError("This Slurm attempt already has a launcher record.")
    record = {"complete": False, "phase": phase, "job_id": job,
              "configuration_sha256": sha(config_path), "controller_sha256": sha(Path(__file__)),
              "persistent_bytes_before": before, "persistent_bytes_limit": ceiling}
    save(record_path, record)
    # All downloads, compiler products, CMake, caches and R temp files are local
    # to this job's /tmp directory; the context removes it even after a failure.
    with tempfile.TemporaryDirectory(prefix="pfa-setup-" + job + "-", dir="/tmp") as temporary:
        scratch = Path(temporary)
        record["scratch_directory"] = str(scratch)
        for name in ("tmp", "cache", "home"):
            (scratch / name).mkdir()
        archive = scratch / "cmake.tar.gz"
        urllib.request.urlretrieve(CMAKE_URL, archive)
        if sha(archive) != CMAKE_SHA:
            raise ValueError("Pinned CMake archive SHA256 mismatch.")
        with tarfile.open(archive) as bundle:
            for member in bundle.getmembers():
                destination = (scratch / member.name).resolve()
                if not inside(destination, scratch):
                    raise ValueError("CMake archive path escapes job scratch.")
                if member.issym() or member.islnk():
                    target = ((destination.parent if member.issym() else scratch) / member.linkname).resolve()
                    if not inside(target, scratch):
                        raise ValueError("CMake archive link escapes job scratch.")
            bundle.extractall(scratch)
        cmake = scratch / "cmake-3.31.8-linux-x86_64/bin/cmake"
        if not cmake.is_file():
            raise ValueError("Pinned CMake distribution has no declared executable.")
        environment = {
            "HOME": str(scratch / "home"), "TMPDIR": str(scratch / "tmp"),
            "XDG_CACHE_HOME": str(scratch / "cache"), "R_LIBS_USER": str(private),
            "R_LIBS": str(private), "LC_ALL": cfg["Locale"], "LANG": cfg["Locale"],
            "LANGUAGE": "C", "R_TESTS": "", "OMP_NUM_THREADS": "1",
            "MKL_NUM_THREADS": "1", "OPENBLAS_NUM_THREADS": "1", "CUDA": "cpu",
            "TORCH_CUDATOOLKIT": "FALSE", "TORCH_INSTALL": "0",
            "TORCH_HOME": str(private / "torch"), "TORCH_VERIFY_LOAD": "TRUE",
            "LKT_SETUP_CMAKE": str(cmake), "LKT_SETUP_SCRATCH": str(scratch),
            "SLURM_JOB_ID": job, "SLURM_CPUS_PER_TASK": allocation,
            "MAKEFLAGS": "-j1", "CMAKE_BUILD_PARALLEL_LEVEL": "1"}
        command = ["singularity", "exec", "--userns", "--cleanenv", "--no-home",
                   "--bind", str(workspace) + ":" + str(workspace),
                   "--bind", str(scratch) + ":" + str(scratch), "--pwd", str(workspace),
                   "--env", ",".join(key + "=" + value for key, value in environment.items()),
                   cfg["Container"], cfg["Rscript-Executable"], "--vanilla",
                   cfg["Setup-R-Script"], str(config_path), phase]
        host_environment = os.environ.copy()
        host_environment.update({"SINGULARITY_CACHEDIR": str(scratch / "cache"),
                                 "SINGULARITY_TMPDIR": str(scratch / "tmp"), "TMPDIR": str(scratch / "tmp")})
        record["command"] = command
        save(record_path, record)
        process = subprocess.Popen(command, env=host_environment, start_new_session=True)
        try:
            while process.poll() is None:
                try:
                    process.wait(timeout=15)
                except subprocess.TimeoutExpired:
                    used = storage_bytes(roots)
                    if used > ceiling:
                        raise RuntimeError("Persistent storage exceeded the declared ceiling during this owned installer.")
            if process.returncode != 0:
                raise RuntimeError("Selected container installer exited " + str(process.returncode))
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
            record["exit_code"] = process.returncode
            record["persistent_bytes_after"] = storage_bytes(roots)
            record["complete"] = process.returncode == 0 and record["persistent_bytes_after"] <= ceiling
            save(record_path, record)
        if not record["complete"]:
            raise RuntimeError("Installer did not satisfy the completion/storage contract.")
    record["scratch_removed"] = not Path(record["scratch_directory"]).exists()
    save(record_path, record)
    print(json.dumps(record, indent=2))


if __name__ == "__main__":
    main()
