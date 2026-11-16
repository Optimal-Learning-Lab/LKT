"""Exact Linux semantic environment installer/verifier; run only inside the pinned SIF."""
import hashlib
import importlib.metadata
import json
import math
import os
from pathlib import Path
import platform
import re
import subprocess
import sys
import urllib.parse
import urllib.request

PYPI = "https://pypi.org/simple"
TORCH_INDEX = "https://download.pytorch.org/whl/cpu"
EXPECTED_CORE = {"torch": "2.6.0+cpu", "sentence-transformers": "3.4.1",
                 "transformers": "4.48.3", "numpy": "2.5.4"}


def sha(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1048576), b""):
            h.update(block)
    return h.hexdigest()


def canonical(name):
    return re.sub(r"[-_.]+", "-", name).lower()


def run(command):
    print("EXEC", json.dumps([str(value) for value in command]), flush=True)
    subprocess.run([str(value) for value in command], check=True)


def download(url, expected, destination):
    if not re.fullmatch("[a-f0-9]{64}", expected):
        raise ValueError("A published SHA256 is required for every wheel.")
    parsed = urllib.parse.urlsplit(url)
    if parsed.scheme != "https" or parsed.hostname not in ("files.pythonhosted.org", "download.pytorch.org", "download-r2.pytorch.org"):
        raise ValueError("Wheel URL is outside the explicitly approved official sources.")
    urllib.request.urlretrieve(url, destination)
    if sha(destination) != expected:
        raise ValueError("Published wheel SHA256 mismatch: " + destination.name)


def main():
    if len(sys.argv) != 4 or sys.argv[3] not in ("install", "verify"):
        raise ValueError("Usage: setup-pfa-semantic-worker.py CONFIG.json JOB_SCRATCH install|verify")
    config = Path(sys.argv[1]).resolve(strict=True)
    cfg = json.loads(config.read_text())
    scratch = Path(sys.argv[2]).resolve(strict=True)
    runtime = Path(cfg["runtime_dir"]).resolve(strict=True)
    venv = runtime / "venv"
    python = venv / "bin/python"
    mode = sys.argv[3]
    job = os.environ.get("SLURM_JOB_ID", "")
    expected_prefix = runtime / ("python" if mode == "install" else "venv")
    if (platform.python_version() != "3.12.14" or platform.system() != "Linux" or
            platform.machine() != "x86_64" or not job.isdecimal() or
            Path(sys.prefix).resolve() != expected_prefix.resolve() or
            sha(Path(__file__)) != cfg["worker_sha256"] or
            sha(Path(cfg["package_lock"])) != cfg["package_lock_sha256"] or
            sha(Path(cfg["original_package_lock"])) != cfg["original_package_lock_sha256"] or
            sha(Path(cfg["linux_dependency_contract"])) != cfg["linux_dependency_contract_sha256"] or
            sha(Path(cfg["helper"])) != cfg["helper_sha256"] or
            os.environ.get("PIP_CONFIG_FILE") != "/dev/null" or
            os.environ.get("PYTHONDONTWRITEBYTECODE") != "1" or
            os.environ.get("PYTHONNOUSERSITE") != "1"):
        raise ValueError("Execution differs from the explicit in-SIF managed Python/worker/source contract.")
    if scratch.parent != Path("/tmp") or not scratch.name.startswith("pfa-semantic-" + job + "-"):
        raise ValueError("All setup artifacts must remain in the selected job's node-local scratch.")
    lock = json.loads(Path(cfg["package_lock"]).read_text())
    normalized = {canonical(name): version for name, version in lock.items()}
    original = json.loads(Path(cfg["original_package_lock"]).read_text(encoding="utf-8-sig"))
    original_normalized = {canonical(name): version for name, version in original.items()}
    contract = json.loads(Path(cfg["linux_dependency_contract"]).read_text())
    if (cfg["schema"] != 2 or
            cfg["original_package_lock_sha256"] != "bbb795b9c1f42aae978a4c637bbf42f96ecfbbf93d7448bbb94d9c7a58558f9c" or
            len(original_normalized) != 34 or len(normalized) != 35 or len(normalized) != len(lock) or
            any(normalized.get(k) != v for k, v in original_normalized.items()) or
            set(normalized) - set(original_normalized) != {"hf-xet"} or
            normalized["hf-xet"] != "1.8.0" or any(normalized.get(k) != v for k, v in EXPECTED_CORE.items()) or
            contract["schema"] != 1 or contract["platform_system"] != "Linux" or
            contract["platform_machine"] != "x86_64" or contract["python_version"] != "3.12.14" or
            contract["original_package_lock_sha256"] != cfg["original_package_lock_sha256"] or
            contract["linux_package_lock_sha256"] != cfg["package_lock_sha256"] or
            contract["additions"] != {"hf-xet": "1.8.0"}):
        raise ValueError("Linux requires the unchanged original34 plus the explicit approved hf-xet1.8.0 closure.")
    pip = [python, "-B", "-m", "pip", "--disable-pip-version-check", "--no-cache-dir"]
    if mode == "install":
        if venv.exists():
            raise ValueError("Create the one current environment fresh; no partial-install resume is supported.")
        run([sys.executable, "-B", "-m", "venv", venv])
        wheels = scratch / "wheels"
        wheels.mkdir()
        # Bootstrap the exact pinned packaging tool from one official universal
        # wheel before resolving target-specific artifacts. The bundled ensurepip
        # tool is used only for this ordinary initial package installation.
        with urllib.request.urlopen("https://pypi.org/pypi/pip/" + lock["pip"] + "/json", timeout=120) as stream:
            metadata = json.load(stream)
        candidates = [entry for entry in metadata["urls"] if entry["filename"] == "pip-" + lock["pip"] + "-py3-none-any.whl"]
        if len(candidates) != 1 or candidates[0].get("yanked"):
            raise ValueError("The exact official pip bootstrap wheel is unavailable or ambiguous.")
        entry = candidates[0]
        bootstrap = wheels / entry["filename"]
        download(entry["url"], entry["digests"]["sha256"], bootstrap)
        run(pip + ["install", "--no-index", "--no-deps", "--no-compile", bootstrap])
        reports = []
        # Each explicit repository resolves its own locked packages. There is no
        # extra index, transitive dependency resolution or source compilation.
        groups = [(PYPI, {name: version for name, version in lock.items() if canonical(name) != "torch"}, "pypi"),
                  (TORCH_INDEX, {"torch": normalized["torch"]}, "torch-cpu")]
        for index, versions, label in groups:
            report = scratch / (label + "-resolution.json")
            run(pip + ["install", "--dry-run", "--ignore-installed", "--no-deps",
                       "--only-binary=:all:", "--index-url", index, "--report", report] +
                       [name + "==" + version for name, version in versions.items()])
            reports.append(json.loads(report.read_text()))
        selected = {}
        requirements = []
        for report in reports:
            for item in report["install"]:
                name = canonical(item["metadata"]["name"])
                version = item["metadata"]["version"]
                if name in selected or normalized.get(name) != version:
                    raise ValueError("Wheel resolution changed or duplicated a pinned distribution.")
                info = item["download_info"]
                url = info["url"]
                digest = info["archive_info"]["hashes"]["sha256"]
                if name in contract["approved_wheels"]:
                    approved = contract["approved_wheels"][name]
                    if version != approved["version"] or url != approved["url"] or digest != approved["sha256"]:
                        raise ValueError("Linux dependency/source wheel differs from its explicit approved contract.")
                host = urllib.parse.urlsplit(url).hostname
                if (name == "torch" and host not in ("download.pytorch.org", "download-r2.pytorch.org")) or (name != "torch" and host != "files.pythonhosted.org"):
                    raise ValueError("A resolved wheel came from the wrong explicit repository.")
                filename = urllib.parse.unquote(Path(urllib.parse.urlsplit(url).path).name)
                if not filename.endswith(".whl") or "/" in filename or " " in filename:
                    raise ValueError("Only explicitly selected binary wheel artifacts are allowed.")
                destination = wheels / filename
                download(url, digest, destination)
                selected[name] = {"name": item["metadata"]["name"], "version": version,
                                  "url": url, "sha256": digest, "bytes": destination.stat().st_size}
                requirements.append(str(destination) + " --hash=sha256:" + digest)
        if set(selected) != set(normalized):
            raise ValueError("Resolution did not cover the exact complete package lock.")
        wheel_lock = scratch / "wheels.requirements.txt"
        wheel_lock.write_text("\n".join(requirements) + "\n")
        (scratch / "wheel-provenance.json").write_text(json.dumps(selected, indent=2) + "\n")
        run(pip + ["install", "--no-index", "--no-deps", "--no-compile", "--force-reinstall",
                   "--require-hashes", "--only-binary=:all:", "-r", wheel_lock])
        run(pip + ["check"])
        run([python, "-B", Path(__file__), config, scratch, "verify"])
        return
    # Verification executes under the newly installed venv, inside the same SIF.
    actual = {canonical(dist.metadata["Name"]): dist.version for dist in importlib.metadata.distributions()}
    if actual != normalized:
        raise ValueError("Installed distribution set/versions differ from the complete frozen lock.")
    from packaging.requirements import Requirement
    hub_requirements = importlib.metadata.metadata("huggingface-hub").get_all("Requires-Dist") or []
    if contract["requirement"] not in hub_requirements:
        raise ValueError("Installed Hub wheel metadata differs from the reviewed mandatory Linux requirement.")
    mandatory = Requirement(contract["requirement"])
    if not mandatory.marker.evaluate() or not mandatory.specifier.contains(actual["hf-xet"]):
        raise ValueError("The exact Linux hf-xet requirement is not active and satisfied.")
    for declaration in importlib.metadata.metadata("hf-xet").get_all("Requires-Dist") or []:
        dependency = Requirement(declaration)
        if dependency.marker is None or dependency.marker.evaluate({"extra": ""}):
            raise ValueError("The approved hf-xet wheel unexpectedly adds another mandatory dependency.")
    import torch
    import numpy as np
    if torch.__version__ != "2.6.0+cpu" or torch.version.cuda is not None or np.__version__ != normalized["numpy"]:
        raise ValueError("The explicit CPU Torch/numpy runtime differs from the frozen versions.")
    request = Path(cfg["probe_request"])
    if sha(request) != cfg["probe_request_sha256"]:
        raise ValueError("The original encoding probe request changed.")
    texts = json.loads(request.read_text(encoding="utf-8-sig"))["texts"]
    if not isinstance(texts, list) or "" not in texts or not any(isinstance(x, str) and x for x in texts):
        raise ValueError("The frozen real-encoder request must cover authentic text and an empty string.")
    output_paths = [scratch / "encoding-first.json", scratch / "encoding-repeat.json"]
    for output in output_paths:
        run([python, "-B", cfg["helper"], "--model-dir", cfg["model_dir"],
             "--manifest-sha256", cfg["manifest_sha256"], "--request", request, "--output", output])
    first, repeated = [json.loads(path.read_text()) for path in output_paths]
    if output_paths[0].read_bytes() != output_paths[1].read_bytes():
        raise ValueError("Repeated exact CPU offline encoding is not deterministic.")
    if (first["embedding_dim"] != 768 or len(first["embeddings"]) != len(texts) or
            first["revision"] != "e8c3b32edf5434bc2275fc9bab85f82640a19130" or
            first["manifest_sha256"] != cfg["manifest_sha256"]):
        raise ValueError("The real frozen encoder returned an incompatible identity/shape.")
    norms = []
    for row in first["embeddings"]:
        if len(row) != 768 or not all(math.isfinite(value) for value in row):
            raise ValueError("Real encoder output has invalid width or nonfinite coordinates.")
        norm = math.sqrt(sum(value * value for value in row))
        if abs(norm - 1) > 1e-5:
            raise ValueError("Real encoder output violates its normalization contract.")
        norms.append(norm)
    reports_dir = Path(cfg["reports_dir"])
    report = reports_dir / ("semantic-runtime-" + job + ".json")
    test_config_path = reports_dir / ("semantic-test-config-" + job + ".json")
    if report.exists() or test_config_path.exists():
        raise ValueError("Existing semantic verification/config artifacts are immutable.")
    test_config = {"question_col": "Problem.Name", "response_col": "Input", "kc_col": "KC..Cluster.",
                   "kc_strip_number": True, "python": str(python), "helper": cfg["helper"],
                   "helper_sha256": cfg["helper_sha256"], "model_dir": cfg["model_dir"],
                   "model_id": "sentence-transformers/all-mpnet-base-v2",
                   "revision": first["revision"], "manifest_sha256": cfg["manifest_sha256"]}
    test_config_path.write_text(json.dumps(test_config, indent=2) + "\n")
    record = {"schema": 2, "complete": True, "job_id": job, "configuration_sha256": sha(config),
              "container_sha256": cfg["container_sha256"], "r_dependency_receipt_sha256": cfg["r_dependency_receipt_sha256"],
              "python_version": platform.python_version(), "python_executable": str(python),
              "python_executable_sha256": sha(python), "python_prefix": sys.prefix,
              "provider": cfg["provider"], "python_archive_url": cfg["python_url"], "python_archive_sha256": cfg["python_sha256"],
              "package_lock_sha256": cfg["package_lock_sha256"], "installed_distributions": actual,
              "original_package_lock_sha256": cfg["original_package_lock_sha256"],
              "linux_dependency_contract_sha256": cfg["linux_dependency_contract_sha256"],
              "linux_dependency_additions": contract["additions"], "mandatory_linux_marker_verified": True,
              "wheel_provenance": json.loads((scratch / "wheel-provenance.json").read_text()),
              "model_id": test_config["model_id"], "revision": first["revision"], "manifest_sha256": cfg["manifest_sha256"],
              "helper_sha256": cfg["helper_sha256"], "probe_request_sha256": cfg["probe_request_sha256"],
              "probe_output_sha256": sha(output_paths[0]), "probe_rows": len(texts), "embedding_dim": 768,
              "row_norms": norms, "empty_string_finite": True, "exact_repeat_output": True,
              "offline_model_only": True, "pip_check_passed": True, "research_fits": 0,
              "semantic_test_config": str(test_config_path), "semantic_test_config_sha256": sha(test_config_path),
              "environment": {name: os.environ[name] for name in ("HF_HUB_OFFLINE", "TRANSFORMERS_OFFLINE", "TOKENIZERS_PARALLELISM", "PYTHONDONTWRITEBYTECODE", "PYTHONNOUSERSITE", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS")}}
    report.write_text(json.dumps(record, indent=2) + "\n")
    print("VERIFIED", report, "SHA256", sha(report), flush=True)


if __name__ == "__main__":
    main()
