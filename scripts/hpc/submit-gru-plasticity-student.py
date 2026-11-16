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
assert (base/'STUDENT_SETUP_COMPLETE').is_file()
assert not (base/'student-submission.json').exists()
assert not (base/'student-submission-events.tsv').exists()


def command(args):
    result = subprocess.run(args, cwd=str(base), stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, universal_newlines=True)
    if result.returncode:
        raise RuntimeError((args, result.returncode, result.stdout, result.stderr))
    return result.stdout.strip()


rows = list(csv.DictReader(command(['sacct', '-j', setup, '--format=JobID,State,ExitCode',
                                  '--parsable2']).splitlines(), delimiter='|'))
assert any(r['JobID']==setup and r['State']=='COMPLETED' and r['ExitCode']=='0:0' for r in rows), rows
events = base/'student-submission-events.tsv'


def record(kind, job):
    with events.open('a') as stream:
        stream.write('{}\t{}\t{}\t{}\n'.format(datetime.datetime.utcnow().isoformat()+'Z', kind, job, node))


record('setup_verified', setup)
fits = command(['sbatch', '--parsable', '--nodelist='+node,
                'scripts/hpc/run-gru-plasticity-student.slurm'])
assert fits.isdigit(), fits
record('fits_submitted', fits)
report = command(['sbatch', '--parsable', '--nodelist='+node, '--dependency=afterok:'+fits,
                  'scripts/hpc/report-gru-plasticity-student.slurm'])
assert report.isdigit(), report
record('report_submitted', report)
metadata = dict(setup=setup, fits=fits, report=report, node=node, fits_count=15,
                design='15 constant-rate fits without student-level logitdec; compare existing 15 full-input fits',
                active_release_promoted=False)
(base/'student-submission.json').write_text(json.dumps(metadata, indent=2)+'\n')
print(json.dumps(metadata, indent=2))
