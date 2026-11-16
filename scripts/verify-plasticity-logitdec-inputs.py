"""Verify the explicitly selected existing candidate and full-input reference."""
import hashlib
import json
from pathlib import Path

spec=json.loads(Path('inputs.json').read_text())
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
for relative,expected in spec['reference_hashes'].items():
    assert sha(Path(spec['remote_reference'])/relative)==expected,relative
for relative,expected in spec['candidate_hashes'].items():
    assert sha(Path(spec['remote_candidate_root'])/relative)==expected,relative
assert sha(Path(spec['dependencies'])/'rocker-r-4.5.3.sif')==spec['container_sha256']
print('PASS exact existing candidate, reference protocol, reports, data, folds and container.')
