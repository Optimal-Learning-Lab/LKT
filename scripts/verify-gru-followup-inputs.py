"""Verify existing remote study inputs before any follow-up fitting."""
import hashlib,json
from pathlib import Path
spec=json.loads(Path('remote-inputs.json').read_text())
base=Path(spec['reference_directory'])
for relative,expected in spec['reference_hashes'].items():
    assert hashlib.sha256((base/relative).read_bytes()).hexdigest()==expected,relative
print('PASS existing reference dataset, partitions, candidate, source archive, controls and canonical predictions match frozen local hashes.')
