"""Regression checks for launcher progress publication and worker ownership."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import sys
import tempfile
import threading
import unittest
from unittest import mock

spec = importlib.util.spec_from_file_location('launcher',Path(__file__).with_name('run-sequence-experiment.py'))
launcher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(launcher)


class LauncherTests(unittest.TestCase):
    def test_transient_access_failure_retains_atomic_publication(self):
        with tempfile.TemporaryDirectory() as folder:
            output=Path(folder);target=output/'status.json'
            target.write_text('{"completed": 1}')
            attempts=[];pauses=[]
            def replace(source,destination):
                attempts.append(1)
                if len(attempts)<3:
                    self.assertEqual(json.loads(target.read_text()),{'completed':1})
                    raise PermissionError('reader holds destination')
                os.replace(source,destination)
            with contextlib.redirect_stdout(io.StringIO()) as messages:
                launcher.write_status(output,{'completed':2},replace=replace,pause=pauses.append)
            self.assertEqual(json.loads(target.read_text()),{'completed':2})
            self.assertEqual(pauses,[.1,.1])
            self.assertIn('temporarily denied',messages.getvalue())

    def test_persistent_failure_is_bounded_and_preserves_old_status(self):
        with tempfile.TemporaryDirectory() as folder:
            output=Path(folder);target=output/'status.json';target.write_text('{"completed": 1}')
            replace=mock.Mock(side_effect=PermissionError('not writable'));pauses=[]
            with contextlib.redirect_stdout(io.StringIO()),self.assertRaises(PermissionError):
                launcher.write_status(output,{'completed':2},replace=replace,pause=pauses.append)
            self.assertEqual(replace.call_count,20)
            self.assertEqual(len(pauses),19)
            self.assertEqual(json.loads(target.read_text()),{'completed':1})

    @unittest.skipUnless(os.name=='nt','Windows sharing semantics')
    def test_real_windows_reader_lock(self):
        with tempfile.TemporaryDirectory() as folder:
            output=Path(folder);target=output/'status.json';target.write_text('{}')
            reader=target.open('rb');timer=threading.Timer(.25,reader.close);timer.start()
            calls=[]
            def replace(source,destination):
                calls.append(1);os.replace(source,destination)
            try:
                with contextlib.redirect_stdout(io.StringIO()):
                    launcher.write_status(output,{'completed':2},replace=replace)
            finally:
                timer.join();reader.close()
            self.assertGreater(len(calls),1)
            self.assertEqual(json.loads(target.read_text()),{'completed':2})

    def test_launcher_error_waits_for_every_started_worker(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);(root/'logs').mkdir();(root/'models').mkdir()
            (root/'protocol.rds').touch()
            (root/'tasks.csv').write_text('id\none\ntwo\n')
            processes=[]
            def start(*args,**kwargs):
                process=mock.Mock();process.poll.return_value=None
                processes.append(process);return process
            args=['launcher','--root',folder,'--output',folder,'--library',folder,'--rscript','Rscript']
            with mock.patch.object(sys,'argv',args),mock.patch.object(launcher.subprocess,'run'),\
                 mock.patch.object(launcher.subprocess,'Popen',side_effect=start),\
                 mock.patch.object(launcher,'write_status',side_effect=OSError('status device failed')):
                with self.assertRaisesRegex(OSError,'status device failed'):
                    launcher.run()
            self.assertEqual(len(processes),2)
            for process in processes:process.wait.assert_called_once_with()


if __name__=='__main__':
    unittest.main()
