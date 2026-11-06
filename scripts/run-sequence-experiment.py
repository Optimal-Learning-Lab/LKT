"""Bounded local orchestration; all model mathematics remain in LKT."""
import argparse
import csv
import json
import os
from pathlib import Path
import subprocess
import time

def write_status(output, status, *, replace=os.replace, pause=time.sleep):
    """Atomically publish status, allowing brief Windows reader sharing locks.

    This is advisory progress only; model files remain the acceptance boundary.
    Persistent access failures still stop the launcher after active fits finish.
    """
    temp, target = output/'status.tmp', output/'status.json'
    temp.write_text(json.dumps(status,indent=2)+'\n',encoding='utf-8')
    for attempt in range(20):
        try:
            replace(temp,target)
            return
        except PermissionError:
            if attempt == 19:
                raise
            if attempt == 0:
                print('Progress replacement temporarily denied; retrying for up to 2 seconds',flush=True)
            pause(.1)


def run():
    p = argparse.ArgumentParser()
    p.add_argument('--root', required=True); p.add_argument('--output', required=True)
    p.add_argument('--library', required=True); p.add_argument('--rscript', required=True)
    p.add_argument('--workers', type=int, default=2)
    a = p.parse_args()
    root, output = Path(a.root).resolve(), Path(a.output).resolve()
    if not 1 <= a.workers <= 4:
        raise ValueError('Use 1 to 4 single-threaded local workers')
    env = os.environ.copy(); env['R_LIBS'] = os.pathsep.join([a.library,str(root/'.codex-r-lib')])
    if os.name == 'nt':
        # Windows R accepts C; the shell's C.UTF-8 locale is not a Windows locale.
        for name in ['LANG','LC_ALL','LC_CTYPE','LC_COLLATE','LC_MONETARY','LC_TIME']:
            env[name] = 'C'
    def command(action, *tail):
        return [a.rscript, '--vanilla', str(root/'scripts/compare-sequence-units.R'), action, str(root), str(output), a.library, *tail]
    if not (output/'protocol.rds').exists():
        subprocess.run(command('plan'), cwd=root, env=env, check=True)
    with (output/'logs/smoke.log').open('a',encoding='utf-8') as log:
        subprocess.run(command('smoke'), cwd=root, env=env, stdout=log, stderr=subprocess.STDOUT, check=True)
    with (output/'tasks.csv').open(newline='',encoding='utf-8') as f:
        tasks = list(csv.DictReader(f))
    pending = [t for t in tasks if not (output/'models'/f"{t['id']}.rds").exists()]
    running, failed = [], []
    try:
        while pending or running:
            while pending and len(running) < a.workers and not failed:
                task = pending.pop(0); log = (output/'logs'/f"{task['id']}.log").open('w',encoding='utf-8')
                process = subprocess.Popen(command('fit',task['id']),cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT)
                running.append((task,process,log))
            for item in running[:]:
                task, process, log = item
                if process.poll() is not None:
                    log.close(); running.remove(item)
                    if process.returncode:
                        failed.append(task['id'])
            completed = sum((output/'models'/f"{t['id']}.rds").exists() for t in tasks)
            status = dict(completed=completed,total=len(tasks),running=[t['id'] for t,_,_ in running],failed=failed)
            write_status(output,status)
            if failed and not running:
                raise RuntimeError(f'Fit failures: {failed}; see individual logs')
            time.sleep(2)
    finally:
        # A launcher failure must not leave orphaned fit processes or closed logs.
        for task,process,log in running:
            process.wait()
            log.close()
    for action in ['verify','report']:
        with (output/'logs'/f'{action}.log').open('w',encoding='utf-8') as log:
            subprocess.run(command(action),cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
    print('Completed 72 fits, replay verification, and report')

if __name__ == '__main__':
    run()
