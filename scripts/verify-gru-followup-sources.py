"""Verify every declared existing study/runtime input before fitting."""
import hashlib,json
from pathlib import Path
spec=json.loads(Path('remote-inputs.json').read_text())
assert len(spec['sources'])==2
for source in spec['sources']:
    base=Path(source['directory'])
    for relative,expected in source['hashes'].items():
        assert hashlib.sha256((base/relative).read_bytes()).hexdigest()==expected, str(base/relative)
print('PASS all existing reference and runtime input hashes; scientific data and fitted controls stay on Big Blue.')
