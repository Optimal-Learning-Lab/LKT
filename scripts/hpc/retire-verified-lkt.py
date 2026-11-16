"""Retire CONFIG's old dependency LKT or an explicitly declared old release.

Full archived file hashes and unchanged-source checks precede removal. Shared
dependencies, original sources, protocols, and scientific results are excluded.
"""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile


def sha(data):
    return hashlib.sha256(data).hexdigest()


def no_jobs():
    jobs = subprocess.check_output(["squeue", "-h", "-u", "ppavlik"], universal_newlines=True)
    if jobs.strip():
        raise ValueError("Wait for all account jobs before retiring an installation.")


def main():
    c = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig"))
    root = Path(c["root"]).resolve()
    release = dict(line.split("=", 1) for line in Path(c["release_file"]).read_text().splitlines()
                   if line and not line.startswith("#"))
    active = (root / release["lkt_library"] / "LKT").resolve()
    if len(sys.argv) == 3:
        old_file = Path(sys.argv[2]).resolve()
        old_release = dict(line.split("=", 1) for line in old_file.read_text().splitlines()
                           if line and not line.startswith("#"))
        old_root = old_file.parents[3]
        relative = ".r-libs/LKT-" + old_release["lkt_version"] + "-" + old_release["lkt_source_sha256"][:12]
        test_roots = [Path(p).resolve() for p in c.get("retirement_verification_roots", [])]
        if any(root not in p.parents for p in test_roots):
            raise ValueError("Temporary verification roots must be inside this deployment.")
        if (old_root.parent != root.parent and old_root not in test_roots) or old_release["lkt_library"] != relative:
            raise ValueError("Old descriptor must name its canonical sibling deployment installation.")
        old = (old_root / relative / "LKT").resolve()
        if sha((old / "research-source-manifest.json").read_bytes()) != old_release["lkt_manifest_sha256"]:
            raise ValueError("Superseded installation does not match its declared release.")
    elif len(sys.argv) == 2:
        old = (Path(c["dependencies"]) / "LKT").resolve()
    else:
        raise ValueError("Usage: CONFIG [EXPLICIT_SUPERSEDED_RELEASE_FILE]")
    if old.name != "LKT" or old == active:
        raise ValueError("Target is not the exact superseded LKT installation.")
    if not active.is_dir() or sha((active / "research-source-manifest.json").read_bytes()) != release["lkt_manifest_sha256"]:
        raise ValueError("Require the declared, verified active package.")
    if "verification_receipts" in c:
        # A scoring-only release has no scientific fitting run. Its explicitly
        # declared, successful native-check and benchmark receipts authorize
        # retirement instead; never invent a results/completed.json placeholder.
        receipts = c["verification_receipts"]
        if not isinstance(receipts, list) or not receipts:
            raise ValueError("Declare nonempty scoring verification receipts.")
        for name in receipts:
            receipt = Path(name).resolve()
            if root not in receipt.parents or not json.loads(receipt.read_text())["passed"]:
                raise ValueError("Require successful verification receipts within this deployment.")
        native_check = Path(c["verified_check_directory"]).resolve()
        if native_check.parent != root or not (native_check / "VERIFICATION_COMPLETE").is_file():
            raise ValueError("Require the declared completed native package check.")
    elif not (root / "results/completed.json").is_file():
        raise ValueError("Require the completed scientific run for this deployment.")
    if not (old / "DESCRIPTION").read_text().startswith("Package: LKT\n"):
        raise ValueError("Retirement target is not LKT.")
    no_jobs()
    files = sorted(p.relative_to(old) for p in old.rglob("*") if p.is_file())
    if any(p.is_symlink() for p in old.rglob("*")) or old.is_symlink():
        raise ValueError("Linked installation paths are not permitted.")
    snapshot = {str(p): sha((old / p).read_bytes()) for p in files}
    fingerprint = sha(json.dumps(snapshot, sort_keys=True).encode())
    history_root = root / "retired-installations"
    history = Path(c.get("retirement_record_directory", str(history_root))).resolve()
    if history != history_root and history_root not in history.parents:
        raise ValueError("Retirement records must stay in this deployment's historical storage.")
    history.mkdir(parents=True, exist_ok=True)
    if (history / "plan.json").exists() or (history / "completed.json").exists():
        raise ValueError("Preserve existing retirement records; declare a fresh record directory.")
    archive = history / ("LKT-" + fingerprint[:16] + ".tar.gz")
    if archive.exists():
        raise ValueError("Use the existing retirement record; do not overwrite it.")
    with tarfile.open(str(archive), "w:gz") as tar:
        tar.add(str(old), arcname="LKT")
    with tarfile.open(str(archive)) as tar:
        members = [p for p in tar.getmembers() if p.isfile()]
        if len(members) != len(snapshot):
            raise ValueError("Archived file count differs.")
        for member in members:
            relative = str(Path(member.name).relative_to("LKT"))
            if sha(tar.extractfile(member).read()) != snapshot[relative]:
                raise ValueError("Archived file hash differs.")
    if snapshot != {str(p.relative_to(old)): sha(p.read_bytes()) for p in old.rglob("*") if p.is_file()}:
        raise ValueError("Installation changed during archival.")
    no_jobs()
    record = {"target": str(old), "active": str(active), "archive": str(archive),
              "archive_sha256": sha(archive.read_bytes()), "files": snapshot}
    (history / "plan.json").write_text(json.dumps(record, indent=2))
    shutil.rmtree(str(old))
    if old.exists():
        raise ValueError("Superseded installation is still present.")
    (history / "completed.json").write_text(json.dumps(record, indent=2))
    print("PASS retired exactly the superseded LKT; shared dependencies/results preserved")


if __name__ == "__main__":
    main()
