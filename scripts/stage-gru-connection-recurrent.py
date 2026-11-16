"""Freeze a source-only Big Blue bundle for the matched Cloze gate-fatigue comparison."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil
import tarfile


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run():
    p = argparse.ArgumentParser()
    p.add_argument('--root', required=True)
    p.add_argument('--output', required=True)
    p.add_argument('--build', required=True)
    p.add_argument('--reference', required=True)
    p.add_argument('--remote-reference', required=True)
    p.add_argument('--existing-input-spec', required=True)
    p.add_argument('--dependencies', required=True)
    a = p.parse_args()
    root, output, build, reference = map(Path, (a.root, a.output, a.build, a.reference))
    assert not output.exists(), output
    checks = list(csv.DictReader((build/'source-checksums.csv').open(newline='', encoding='utf-8-sig')))
    for row in checks:
        assert sha(root/row['path']) == row['sha256'] == sha(build/'LKT'/row['path']), row['path']
    output.mkdir(parents=True)
    bundle = output/'code-only-bundle'
    bundle.mkdir()
    package_hashes = {row['path']: row['sha256'] for row in checks}
    for relative in package_hashes:
        if relative.startswith('data/'):
            continue
        assert Path(relative).suffix.lower() not in ('.rds', '.rda', '.rdata', '.csv', '.tsv', '.gz', '.zip', '.parquet')
        target = bundle/'package-source'/'LKT'/relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(build/'LKT'/relative, target)
    sources = ['compare-gru-connection-recurrent.R', 'gru-connection-fatigue-study.R', 'gru-connection-recurrent-study.R', 'gru-plasticity-study.R', 'gru-encoder-study.R', 'hpc/submit-gru-connection-recurrent.py', 
               'prepare-gru-simplified-remote.py']
    sources += [f'hpc/{x}-gru-connection-recurrent.slurm' for x in ('setup', 'run', 'report')]
    for relative in sources:
        target = bundle/'scripts'/relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((root/'scripts'/relative).read_bytes().replace(b'\r\n', b'\n') if relative.endswith('.slurm')
                           else (root/'scripts'/relative).read_bytes())
    required = ['study.rds', 'data/prepared_data.rds'] + [f'partitions/cv{k}.rds' for k in range(1, 6)]
    existing = json.loads(Path(a.existing_input_spec).read_text(encoding='utf-8-sig'))
    spec = dict(reference_directory=a.remote_reference,
                reference_hashes={f: sha(reference/f) for f in required},
                package_source_hashes=package_hashes,
                existing_source_archive=existing['existing_source_archive'],
                existing_source_archive_sha256=existing['existing_source_archive_sha256'],
                dependencies_directory=a.dependencies)
    (bundle/'remote-inputs.json').write_text(json.dumps(spec, indent=2)+'\n', encoding='utf-8', newline='\n')
    version = re.search(r'^Version:\s*(\S+)', (build/'LKT'/'DESCRIPTION').read_text(), re.M).group(1)
    archive = f'LKT_{version}.tar.gz'
    candidate = dict(version=version, source_archive=archive,
                     manifest_sha256=sha(build/'LKT'/'inst'/'research-source-manifest.json'),
                     validation_candidate=True, active_release_promoted=False,
                     windows_verified_source_archive_sha256=sha(build/archive),
                     verified_build_input_sha256=sha(build/'source-checksums.csv'))
    local = output/'local-candidate'
    local.mkdir()
    shutil.copyfile(build/archive, local/archive)
    local_meta = dict(candidate, source_sha256=sha(local/archive))
    (local/'candidate.json').write_text(json.dumps(local_meta, indent=2)+'\n', encoding='utf-8', newline='\n')
    (bundle/'candidate').mkdir()
    (bundle/'candidate'/'candidate-template.json').write_text(json.dumps(candidate, indent=2)+'\n', encoding='utf-8', newline='\n')
    for target in bundle.rglob('*'):
        if target.is_file():
            assert target.suffix.lower() not in ('.rds', '.rda', '.rdata', '.csv', '.tsv', '.parquet'), target
    files = sorted(x for x in bundle.rglob('*') if x.is_file())
    (bundle/'bundle.sha256').write_text(''.join(f'{sha(x)}  {x.relative_to(bundle).as_posix()}\n' for x in files), encoding='utf-8', newline='\n')
    archive_path = output/'gru-connection-recurrent-code-only.tar.gz'
    with tarfile.open(archive_path, 'w:gz') as tar:
        for target in sorted(x for x in bundle.rglob('*') if x.is_file()):
            tar.add(target, arcname=target.relative_to(bundle).as_posix(), recursive=False)
    (output/'code-only-upload.sha256').write_text(f'{sha(archive_path)}  {archive_path.name}\n', encoding='utf-8', newline='\n')
    print(f'PASS source-only bundle: {len(files)} files; SHA256 {sha(archive_path)}')


if __name__ == '__main__':
    run()
