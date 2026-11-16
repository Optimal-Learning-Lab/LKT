"""Submit the fit/report dependency chain after a successful separate setup.

Run on the cluster: SCRIPT RUN_DIRECTORY SETUP_JOB_ID NODE.
Every accepted submission is recorded immediately; an error stops submission.
"""
import csv
import datetime
import json
from pathlib import Path
import subprocess
import sys

run, setup, node = sys.argv[1:]
base = Path(run).resolve()
assert setup.isdigit() and node.startswith('ac') and node[2:].isdigit()
assert (base/'SETUP_COMPLETE').is_file()
assert not (base/'original-submission.json').exists()
assert not (base/'submission-events.tsv').exists()


def command(args):
    result = subprocess.run(args, cwd=str(base), stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, universal_newlines=True)
    if result.returncode:
        raise RuntimeError((args, result.returncode, result.stdout, result.stderr))
    return result.stdout.strip()


rows = list(csv.DictReader(command(['sacct', '-j', setup, '--format=JobID,State,ExitCode',
                                  '--parsable2']).splitlines(), delimiter='|'))
assert any(r['JobID']==setup and r['State']=='COMPLETED' and r['ExitCode']=='0:0' for r in rows), rows
events = base/'submission-events.tsv'


def record(kind, job):
    with events.open('a') as stream:
        stream.write('{}\t{}\t{}\t{}\n'.format(datetime.datetime.utcnow().isoformat()+'Z', kind, job, node))


record('setup_verified', setup)
fits = command(['sbatch', '--parsable', '--nodelist='+node,
                'scripts/hpc/run-gru-plasticity-logitdec.slurm'])
assert fits.isdigit(), fits
record('fits_submitted', fits)
report = command(['sbatch', '--parsable', '--nodelist='+node, '--dependency=afterok:'+fits,
                  'scripts/hpc/report-gru-plasticity-logitdec.slurm'])
assert report.isdigit(), report
record('report_submitted', report)
metadata = dict(setup=setup, fits=fits, report=report, node=node, fits_count=45,
                design='remove only student logitdec: stock/constant/fatigue x 5 folds x 3 seeds; compare prior full-input fits',
                active_release_promoted=False)
(base/'original-submission.json').write_text(json.dumps(metadata, indent=2)+'\n')
print(json.dumps(metadata, indent=2))
