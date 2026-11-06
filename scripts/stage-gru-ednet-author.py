"""Freeze an isolated Big Blue payload from the central released LKT archive."""
import argparse
import csv
import datetime
import hashlib
import json
from pathlib import Path
import re
import shutil
import tarfile

def submission_script(remote,archive_name,fits):
    return f'''#!/bin/bash -l
set -euo pipefail
cd {remote}
test ! -e original-submission.json
sha256sum -c bundle-upload.sha256
tar -xzf {archive_name}
mkdir -p logs
for file in scripts/hpc/*-gru-ednet-author.slurm; do bash -n "$file"; done
python3 - <<'PY'
import datetime,hashlib,json,pathlib,subprocess
path=pathlib.Path('original-submission.json')
assert not path.exists()
record=dict(fresh_fits={fits},node='ac04',submitted_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
            protocol_sha256=hashlib.sha256(pathlib.Path('study/study.rds').read_bytes()).hexdigest(),status='submitting')
def save():
    temp=path.with_suffix('.tmp');temp.write_text(json.dumps(record,indent=2)+'\\n');temp.replace(path)
save()
try:
    for phase in ['setup','fits','report']:
        extra=[]
        # Dependencies are set after their owning job has been accepted.
        if phase=='fits':extra=['--array=1-{fits}%3','--dependency=afterok:'+record['setup']]
        if phase=='report':extra=['--dependency=afterok:'+record['fits']]
        script='run' if phase=='fits' else phase
        job=subprocess.check_output(['sbatch','--parsable','--nodelist=ac04',*extra,'scripts/hpc/'+script+'-gru-ednet-author.slurm'],universal_newlines=True).strip().split(';')[0]
        assert job.isdigit();record[phase]=job;save()
    record['status']='submitted';save()
except Exception as error:
    record['status']='submission_failed';record['error']=str(error);save();raise
print(json.dumps(record,indent=2))
PY
'''


def main():
    p=argparse.ArgumentParser();p.add_argument('root',type=Path);p.add_argument('study',type=Path)
    p.add_argument('label');a=p.parse_args();root=a.root.resolve();study=a.study.resolve()
    assert re.fullmatch(r'ednet-[a-z0-9-]+',a.label)
    assert (study/'study.rds').is_file() and not list((study/'models').glob('*.rds'))
    sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
    central=root.parent/'PolicyFactory2/workflows/simulation/shared/lkt-release.env'
    release=dict(line.strip().split('=',1) for line in central.read_text().splitlines() if line.strip() and not line.startswith('#'))
    archive=root.parent/'PolicyFactory2'/release['lkt_source_archive']
    assert sha(archive)==release['lkt_source_sha256']
    with (study/'tasks.csv').open(newline='') as f:tasks=list(csv.DictReader(f))
    assert len(tasks) in (3,5)
    stage=study.parent/(study.name+'-bundle');assert not stage.exists();stage.mkdir()
    bundle=stage/'payload';bundle.mkdir()
    files=['scripts/run-gru-ednet-author-protocol.R','scripts/ednet-author-protocol.R',
           'scripts/gru-encoder-study.R','scripts/load-current-lkt.R','scripts/prepare-ednet-unvamp.py',
           'scripts/test-ednet-unvamp.py','scripts/test-ednet-author-protocol.R','scripts/README-gru-ednet-author-protocols.md']
    files += [f'scripts/hpc/{phase}-gru-ednet-author.slurm' for phase in ['setup','run','report']]
    for name in files:
        target=bundle/name;target.parent.mkdir(parents=True,exist_ok=True)
        data=(root/name).read_bytes()
        target.write_bytes(data.replace(b'\r\n',b'\n') if name.endswith('.slurm') else data)
    for name in ['lkt-release.env','lkt_release.R']:
        target=bundle/'workflows/simulation/shared'/name;target.parent.mkdir(parents=True,exist_ok=True)
        shutil.copyfile(central.parent/name,target)
    shutil.copyfile(archive,bundle/archive.name)
    (bundle/'deployment.env').write_text(''.join(f'{k}={v}\n' for k,v in release.items()),newline='\n')
    shutil.copytree(study,bundle/'study')
    payload=sorted(path for path in bundle.rglob('*') if path.is_file())
    (bundle/'bundle.sha256').write_text(''.join(f'{sha(path)}  {path.relative_to(bundle).as_posix()}\n' for path in payload),newline='\n')
    tar=stage/(a.label+'.tar.gz')
    with tarfile.open(tar,'w:gz') as stream:
        for path in sorted(bundle.iterdir()):stream.add(path,arcname=path.name)
    (stage/'bundle-upload.sha256').write_text(f'{sha(tar)}  {tar.name}\n',newline='\n')
    remote='/home/ppavlik/LKT-'+a.label
    (stage/'submit-study.sh').write_text(submission_script(remote,tar.name,len(tasks)),newline='\n')
    metadata=dict(release=release,protocol_sha256=sha(study/'study.rds'),archive_sha256=sha(tar),
                  archive_bytes=tar.stat().st_size,remote=remote,fresh_fits=len(tasks),node='ac04',
                  staging_script_sha256=sha(Path(__file__)),created_utc=datetime.datetime.now(datetime.timezone.utc).isoformat())
    (stage/'stage-provenance.json').write_text(json.dumps(metadata,indent=2)+'\n')
    print(json.dumps(metadata,indent=2))


if __name__=='__main__':main()
