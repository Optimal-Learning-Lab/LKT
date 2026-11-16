#!/bin/bash
# RUN_DIRECTORY. Collect only a complete, verified experiment; never move inputs.
set -euo pipefail
cd "$1"
test -f COMPARISON_COMPLETE
for task in {0..29}; do test -f "FIT_${task}_COMPLETE"; done
test ! -e completed-results.tar.gz
sha256sum -c bundle.sha256 > final-source-verification.txt
jobs=$(python3 -c 'import json;p=json.load(open("original-submission.json"));print(",".join(p[k] for k in ("setup","fits","report")))')
sacct -j "$jobs" --format=JobID%40,JobName%30,State,ExitCode,Elapsed,NodeList --parsable2 > final-sacct.psv
python3 - <<'PY'
import csv,json
from pathlib import Path
p=json.loads(Path('original-submission.json').read_text())
v=json.loads(Path('results/reports/verification.json').read_text())
assert v['complete'] and v['fits']==30 and v['learned_parameter_triplets']==15
rows=list(csv.DictReader(Path('final-sacct.psv').open(),delimiter='|'))
expected=[p['setup'],p['report']]+[f"{p['fits']}_{i}" for i in range(30)]
for job in expected:
    match=[r for r in rows if r['JobID']==job]
    assert len(match)==1 and match[0]['State']=='COMPLETED' and match[0]['ExitCode']=='0:0', (job,match)
Path('remote-completion.json').write_text(json.dumps(dict(status='complete',successful_fits=30,
    independently_checked_phase_metric_pairs=90,learned_parameter_triplets=15,
    scheduler_jobs_verified=32,active_release_promoted=False,verification=v),indent=2)+'\n')
PY
extra=()
for file in scheduler-resource-update.tsv scheduler-resource-update.json; do
  if [[ -f "$file" ]]; then extra+=("$file"); fi
done
tar -czf completed-results.tar.gz results candidate scripts logs smoke \
  original-submission.json submission-events.tsv bundle.sha256 remote-inputs.json \
  upload-verification.txt final-source-verification.txt final-sacct.psv remote-completion.json \
  "${extra[@]}" SETUP_COMPLETE COMPARISON_COMPLETE \
  FIT_*_COMPLETE
sha256sum completed-results.tar.gz > completed-results.sha256
cat remote-completion.json
cat completed-results.sha256
