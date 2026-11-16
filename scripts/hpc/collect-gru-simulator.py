"""Collect one explicitly configured simulator job; never submit or restart jobs.

CONFIG_JSON. Polls scheduler/checkpoint metadata, verifies downloaded file hashes
and embedded SVGs, then runs only explicitly declared guarded retirements.
"""
from pathlib import Path
import base64
import datetime
import hashlib
import json
import re
import shlex
import subprocess
import sys
import time
import xml.etree.ElementTree as ET


def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1048576), b''):
            h.update(block)
    return h.hexdigest()


def main():
    assert len(sys.argv) in (2, 3) and (len(sys.argv) == 2 or sys.argv[2] == '--once')
    c = json.loads(Path(sys.argv[1]).read_text(encoding='utf-8-sig'))
    out = Path(c['local_output']).resolve()
    assert out.parent == Path(c['source_root']).resolve().parent / 'LKT-outputs'
    remote = c['remote_workspace']; run = c['remote_run']; weekly = c['weekly_output']
    assert remote.startswith('/project/ppavlik/') and re.fullmatch(r'run-[0-9]+', run)
    assert weekly.startswith(run + '/pilot/') and '..' not in weekly
    job = str(c['job_id']); assert job.isdecimal()
    opts = ['-o', 'BatchMode=yes', '-o', 'StrictHostKeyChecking=yes', '-o', 'ConnectTimeout=15',
            '-o', 'ServerAliveInterval=15', '-o', 'ServerAliveCountMax=2']

    def transfer_call(function, args, retry=True, **kwargs):
        attempts = c.get('transport_attempts',8) if retry else 1
        for attempt in range(attempts):
            try:
                return function(args,**kwargs)
            except subprocess.CalledProcessError as error:
                if error.returncode!=255 or attempt+1==attempts:
                    raise
                print('Transport interrupted; retry',attempt+2,'of',attempts,flush=True)
                time.sleep(c.get('transport_retry_seconds',15))

    def ssh(command, retry=True):
        return transfer_call(subprocess.check_output,['ssh.exe'] + opts + [c['host'], command],
                             retry=retry,universal_newlines=True)

    def remote_python(code, *args):
        return ssh('python3 -c ' + shlex.quote(code) + ' ' + ' '.join(shlex.quote(str(v)) for v in args))

    def write(name, value):
        target = out / name; temporary = target.with_suffix(target.suffix + '.tmp')
        temporary.write_text(json.dumps(value, indent=2) + '\n', encoding='utf-8')
        temporary.replace(target)

    status_code = """import json,subprocess,sys
from pathlib import Path
w=Path(sys.argv[1]);r=w/sys.argv[2];weekly=w/sys.argv[3];job=sys.argv[4]
queued=subprocess.check_output(['squeue','-h','-u','ppavlik','-o','%A'],universal_newlines=True).splitlines()
active=job in [value.strip() for value in queued]
state=subprocess.check_output(['sacct','-X','-j',job,'--format=State','-n','-P'],universal_newlines=True).strip()
done=json.loads((r/'completed.json').read_text()) if (r/'completed.json').is_file() else None
print(json.dumps(dict(active=active,state=state,completed=done,students_complete=len(list((weekly/'batched').glob('student_*.rds'))))))
"""
    deadline = time.monotonic() + c.get('maximum_watch_hours', 12) * 3600
    while True:
        status = json.loads(remote_python(status_code, remote, run, weekly, job))
        status['updated_utc'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
        status['job_id'] = int(job);write('live-status.json', status)
        print(json.dumps(status), flush=True)
        if len(sys.argv) == 3:
            return
        if not status['active']:
            break
        if time.monotonic() >= deadline:
            raise RuntimeError('Bounded watcher duration reached; running job was not altered.')
        time.sleep(c.get('interval_seconds', 45))
    if status['state'].splitlines() != ['COMPLETED'] or not status['completed'] or not status['completed']['passed']:
        raise RuntimeError('Simulator job did not complete successfully; checkpoints are preserved.')

    manifest_code = """import hashlib,json,sys
from pathlib import Path
w=Path(sys.argv[1]);roots=sys.argv[2:];files=[]
for root in roots:
 p=w/root
 candidates=[p] if p.is_file() else sorted(p.rglob('*'))
 for f in candidates:
  if f.is_file():
   h=hashlib.sha256()
   with f.open('rb') as stream:
    for block in iter(lambda:stream.read(1048576),b''):h.update(block)
   files.append(dict(path=str(f.relative_to(w)),sha256=h.hexdigest(),bytes=f.stat().st_size))
print(json.dumps(files))
"""

    def collect(roots):
        manifest = json.loads(remote_python(manifest_code, remote, *roots))
        for f in manifest:
            relative = Path(f['path']);assert not relative.is_absolute() and '..' not in relative.parts
            target = (out / relative).resolve();assert out in target.parents
            if target.exists():
                if sha(target) != f['sha256']:
                    raise RuntimeError('Existing local artifact differs: ' + str(target))
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            temporary = target.with_suffix(target.suffix + '.download')
            transfer_call(subprocess.check_call,['scp.exe', '-q'] + opts +
                          [c['host'] + ':' + remote + '/' + f['path'], str(temporary)])
            assert temporary.stat().st_size == f['bytes'] and sha(temporary) == f['sha256']
            temporary.replace(target)
        return manifest

    roots = [run, c['source_snapshot'],
             'logs/gru_simulator_pilot_' + job + '.out', 'logs/gru_simulator_pilot_' + job + '.err']
    manifest = collect(roots);write('downloaded-study-manifest.json', manifest)
    done = json.loads((out / weekly / 'completed.json').read_text())
    assert done['passed'] and done['students'] == 5 and done['learning_weeks'] == 20 and done['items'] == 100
    assert done['checkpoint_recovery'] and done['canonical_student_assignments'] and done['disjoint_posttests']
    html_path = out / weekly / 'simulation_report.html';html = html_path.read_text(encoding='utf-8')
    svg_checks = []
    for i in range(1, 6):
        start = html.index('id="main-figure-accuracy-' + str(i) + '"')
        match = re.search(r'data:image/svg\+xml;base64,([^"\s]+)', html[start:]);assert match
        svg = base64.b64decode(match.group(1));tree = ET.fromstring(svg)
        assert tree.tag.endswith('svg') and ('SimStudent' + str(i)).encode() in svg and b'Probability' in svg
        svg_checks.append(dict(student=i,bytes=len(svg)))
    write('weekly-svg-verification.json', dict(passed=True,offline_xml_verification=True,students=svg_checks))
    if c.get('offline_svg_auditor'):
        subprocess.check_call([c['node_executable'], c['offline_svg_auditor'], str(html_path), str(out / 'verification/weekly')])

    retirements = c.get('retirements', [])
    if retirements:
        while ssh('squeue -h -u ppavlik').strip():
            if time.monotonic() >= deadline:
                raise RuntimeError('Results collected; retirement deferred because other account jobs remain active.')
            time.sleep(c.get('interval_seconds', 45))
        for retirement in retirements:
            args = ['python3', remote + '/scripts/hpc/retire-verified-lkt.py', retirement['config'], retirement['release']]
            # Retirement is a mutation: do not repeat it after an uncertain
            # transport outcome. Its own durable receipt must be inspected.
            print(ssh(' '.join(shlex.quote(v) for v in args),retry=False), flush=True)
        collect(['retired-installations'])
    write('collection-completed.json', dict(passed=True,job_id=int(job),students=5,learning_weeks=20,
        html_report=str(html_path),trial_count=done['trials'],simulation_seconds=done['simulation_seconds'],
        source_manifest_verified=True,embedded_svg_traces=5,retirements_completed=len(retirements)))
    print('PASS final study collected and verified', flush=True)


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print('COLLECTION FAILED:', repr(error), file=sys.stderr, flush=True)
        raise
