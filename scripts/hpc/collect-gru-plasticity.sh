#!/bin/bash
# Run inside the completed, frozen Big Blue study directory.
set -euo pipefail
test -f COMPARISON_COMPLETE
test -f results/reports/verification.json
for task in {0..44}; do test -f "FIT_${task}_COMPLETE"; done
python3 - <<'PY'
import json,subprocess
from pathlib import Path
m=json.loads(Path('original-submission.json').read_text())
jobs=','.join(m[k] for k in ('setup','fits','report'))
r=subprocess.run(['sacct','-j',jobs,'--format=JobID,State,ExitCode','--parsable2'],stdout=subprocess.PIPE,stderr=subprocess.PIPE,universal_newlines=True,check=True)
Path('final-sacct.psv').write_text(r.stdout)
assert not any(x in r.stdout for x in ('FAILED','TIMEOUT','OUT_OF_MEMORY','CANCELLED'))
assert len(list(Path('results/models').glob('*.rds')))==45
PY
test ! -e completed-results.tar.gz
tar -czf completed-results.tar.gz candidate results logs SETUP_COMPLETE COMPARISON_COMPLETE \
  original-submission.json submission-events.tsv bundle.sha256 remote-inputs.json final-sacct.psv FIT_*_COMPLETE
sha256sum completed-results.tar.gz > completed-results.sha256
