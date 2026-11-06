import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock

spec=importlib.util.spec_from_file_location('stage',Path(__file__).with_name('stage-gru-ednet-author.py'))
stage=importlib.util.module_from_spec(spec);spec.loader.exec_module(stage)


class SubmissionTests(unittest.TestCase):
    def exercise(self,fits,fail=False):
        shell=stage.submission_script('/home/ppavlik/test','test.tar.gz',fits)
        code=shell.split("python3 - <<'PY'\n",1)[1].rsplit('\nPY',1)[0]
        with tempfile.TemporaryDirectory() as folder:
            path=Path(folder).resolve();(path/'study').mkdir();(path/'study/study.rds').write_bytes(b'fixture')
            previous=Path.cwd();os.chdir(path);calls=[]
            def submit(args,universal_newlines):
                self.assertTrue(universal_newlines)
                record=json.loads((path/'original-submission.json').read_text())
                if calls:self.assertIn('setup',record)
                calls.append(args)
                if fail and len(calls)==2:raise subprocess.CalledProcessError(1,args)
                return str(100+len(calls))+'\n'
            try:
                with mock.patch('subprocess.check_output',side_effect=submit),contextlib.redirect_stdout(io.StringIO()):
                    if fail:
                        with self.assertRaises(subprocess.CalledProcessError):exec(compile(code,'submission','exec'),{})
                    else:
                        exec(compile(code,'submission','exec'),{})
                        with self.assertRaises(AssertionError):exec(compile(code,'submission','exec'),{})
                record=json.loads((path/'original-submission.json').read_text())
                self.assertEqual(record['fresh_fits'],fits);self.assertEqual(record['setup'],'101')
                if fail:self.assertEqual(record['status'],'submission_failed')
                else:
                    self.assertEqual(record['status'],'submitted')
                    self.assertIn('--dependency=afterok:101',calls[1])
                    self.assertIn(f'--array=1-{fits}%3',calls[1])
                    self.assertIn('--dependency=afterok:102',calls[2])
            finally:os.chdir(previous)

    def test_five_task_submission_receipts_and_duplicate_prevention(self):self.exercise(5)
    def test_three_task_submission_receipts(self):self.exercise(3)
    def test_partial_submission_remains_observable(self):self.exercise(5,fail=True)


if __name__=='__main__':unittest.main()
