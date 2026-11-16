"""Read-only progress watch for the frozen 60-fit experiment. RUN_DIRECTORY."""
from pathlib import Path
import datetime
import json
import subprocess
import sys
import time

base = Path(sys.argv[1]).resolve()
meta = json.loads((base/'original-submission.json').read_text())
conditions = ('stock_full','decay_full','stock_no_ppes','decay_no_ppes')
while True:
    receipts = list(base.glob('FIT_*_COMPLETE'))
    reports = list((base/'results/reports').glob('*-verification.txt'))
    counts = {name:sum('__'+name+'__' in p.name for p in reports) for name in conditions}
    print(datetime.datetime.utcnow().isoformat()+'Z', 'completed',len(receipts),'/60',counts,flush=True)
    jobs = ','.join(meta[key] for key in ('setup','fits','report'))
    result = subprocess.run(['sacct','-j',jobs,'--format=JobID,State,ExitCode','--parsable2'],
      stdout=subprocess.PIPE,stderr=subprocess.PIPE,universal_newlines=True)
    if result.returncode:
        raise RuntimeError(result.stderr)
    failed = [row for row in result.stdout.splitlines()
      if any(state in row for state in ('FAILED','TIMEOUT','OUT_OF_MEMORY','CANCELLED','NODE_FAIL'))]
    if failed:
        raise RuntimeError(failed)
    if (base/'COMPARISON_COMPLETE').exists():
        print('COMPARISON_COMPLETE',flush=True)
        break
    time.sleep(45)
