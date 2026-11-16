"""Deploy an R CMD check installation through the shared release descriptor.

CONFIG CHECK_DIRECTORY. This copies an immutable, successful check artifact;
it is not an additional maintained source or an alternate-library search.
"""
import hashlib
import json
from pathlib import Path
import shutil
import sys
import tarfile


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    c = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8-sig"))
    check = Path(sys.argv[2]).resolve()
    root = Path(c["root"]).resolve()
    if check.parent != root or not (check / "VERIFICATION_COMPLETE").is_file():
        raise ValueError("Require a completed check directly within this deployment.")
    log = (check / "LKT.Rcheck/00check.log").read_text()
    if "* checking tests ... OK" not in log or "ERROR" in log:
        raise ValueError("Native package verification did not pass.")
    package = check / "LKT.Rcheck/LKT"
    archive = Path(c["archive"])
    if sha(archive) != c["archive_sha256"]:
        raise ValueError("Source archive identity changed.")
    with tarfile.open(str(archive)) as tar:
        manifest = tar.extractfile("LKT/inst/research-source-manifest.json").read()
        description = tar.extractfile("LKT/DESCRIPTION").read().decode()
    if (package / "research-source-manifest.json").read_bytes() != manifest:
        raise ValueError("Checked installation differs from declared source manifest.")
    version = next(line.split(":", 1)[1].strip() for line in description.splitlines()
                   if line.startswith("Version:"))
    relative = ".r-libs/LKT-" + version + "-" + c["archive_sha256"][:12]
    target = root / relative / "LKT"
    target.parent.mkdir(parents=True, exist_ok=True)
    if not target.exists():
        shutil.copytree(str(package), str(target))
    files = sorted(p.relative_to(package) for p in package.rglob("*") if p.is_file())
    if files != sorted(p.relative_to(target) for p in target.rglob("*") if p.is_file()):
        raise ValueError("Deployed file inventory differs.")
    if any(sha(package / p) != sha(target / p) for p in files):
        raise ValueError("Deployed package file hashes differ.")
    release = Path(c["release_file"])
    release.parent.mkdir(parents=True, exist_ok=True)
    text = "\n".join(["lkt_version=" + version, "lkt_source_archive=" + archive.name,
                       "lkt_source_sha256=" + c["archive_sha256"],
                       "lkt_manifest_sha256=" + hashlib.sha256(manifest).hexdigest(),
                       "lkt_library=" + relative]) + "\n"
    if release.exists() and release.read_text() != text:
        raise ValueError("Refusing to replace an incompatible deployment release.")
    release.write_text(text)
    print("PASS checked package deployment:", version, len(files), "verified files")


if __name__ == "__main__":
    main()
