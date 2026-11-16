"""Complete a generated build input using data already present on Big Blue.

The uploaded bundle contains source text and hashes only. No learner rows,
partitions, predictions, checkpoints or packaged sample data are uploaded.
"""
from pathlib import Path
import hashlib
import json
import tarfile

base = Path.cwd()
spec = json.loads((base / 'remote-inputs.json').read_text())
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
reference = Path(spec['reference_directory'])
for relative, expected in spec['reference_hashes'].items():
    assert sha(reference / relative) == expected, relative
old_archive = Path(spec['existing_source_archive'])
assert sha(old_archive) == spec['existing_source_archive_sha256']
source = base / 'package-source' / 'LKT'
with tarfile.open(old_archive, 'r:gz') as archive:
    for name in ('samplelkt.rda', 'largerawsample.rda'):
        member = archive.getmember('LKT/data/' + name)
        assert member.isfile()
        destination = source / 'data' / name
        assert not destination.exists()
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(archive.extractfile(member).read())
for relative, expected in spec['package_source_hashes'].items():
    assert sha(source / relative) == expected, relative
print('PASS: existing remote scientific inputs and every reconstructed build-input file match frozen hashes.')
