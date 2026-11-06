import importlib.util
from pathlib import Path
import unittest
import pandas as pd

spec=importlib.util.spec_from_file_location('prepare',Path(__file__).with_name('prepare-ednet-unvamp.py'))
prepare=importlib.util.module_from_spec(spec);spec.loader.exec_module(prepare)


class PreparationTests(unittest.TestCase):
    def test_filter_then_cap_does_not_reapply_thresholds_to_truncated_histories(self):
        d=pd.DataFrame({'participant':[1,1,2,2,3,3],
            'question_id':['a','b','b','a','a','b'],
            'timestamp':[1,2,1,2,1,2],'original_position':[1,2,1,2,1,2]})
        result,trace,eligibility=prepare.filter_then_cap(d,cap=1,min_items=2,min_students=2)
        self.assertEqual(len(result),3)
        self.assertEqual(result.participant.nunique(),3)
        self.assertEqual(result.question_id.tolist(),['a','b','a'])
        self.assertTrue((eligibility.distinct_students==3).all())
        self.assertEqual(trace[-1]['responses'],6)
        # Item b legitimately has only one student after the cap.
        self.assertEqual(result.groupby('question_id').participant.nunique()['b'],1)
        cap_first=d.groupby('participant').head(1)
        before,_=prepare.threshold_filter(cap_first,min_items=2,min_students=2)
        self.assertTrue(before.empty)

    def test_global_graph_matches_distinct_pair_fixed_point(self):
        import numpy as np
        edges=[(1,0),(1,1),(2,0),(2,1),(3,0),(3,2),(4,2)]
        d=pd.DataFrame(edges*3,columns=['participant','question_id'])
        expected,_=prepare.threshold_filter(d,min_items=2,min_students=2)
        neighbors=[np.array([q for u,q in edges if u==uid],dtype=np.int32) for uid in [1,2,3,4]]
        uids,items,counts,item_counts,trace=prepare.global_threshold_core([1,2,3,4],neighbors,3,2,2)
        self.assertEqual(set(uids),set(expected.participant))
        self.assertEqual(set(items),set(expected.question_id))
        self.assertTrue((counts>=2).all())
        self.assertTrue((item_counts[items]>=2).all())
        self.assertGreater(len(trace),1)

    def test_qualification_counts_answered_distinct_items_in_full_history(self):
        d=pd.DataFrame({'question_id':['q'+str(i) for i in range(10)]+['q0','missing'],
                        'user_answer':['a']*11+[None]})
        self.assertEqual(prepare.answered_item_count(d),10)
        self.assertEqual(prepare.answered_item_count(d.iloc[:9]),9)
        with self.assertRaisesRegex(ValueError,'answer code'):
            prepare.answered_item_count(d.assign(user_answer='invalid'))

    def test_qualified_sample_keeps_drawing_until_requested_count(self):
        def read(uid):
            n={1:7,2:10,3:9,4:12}[uid]
            return pd.DataFrame({'question_id':range(n),'user_answer':['a']*n})
        audit=[]
        result=list(prepare.sample_students([1,2,3,4],read,2,'qualified-students',audit))
        self.assertEqual([uid for uid,_ in result],[2,4])
        self.assertEqual([r['selected'] for r in audit],[False,True,False,True])
        self.assertEqual([r['candidate_rank'] for r in audit],[1,2,3,4])
        raw=list(prepare.sample_students([1,2,3,4],read,2,'all-students',[]))
        self.assertEqual([uid for uid,_ in raw],[1,2])
        with self.assertRaisesRegex(ValueError,'too few eligible'):
            list(prepare.sample_students([1,3],read,1,'qualified-students',[]))

    def test_session_duplicates_missing_answers_and_chronological_cap(self):
        d=pd.DataFrame(dict(timestamp=[4,1,2,3,5,6],question_id=['a','a','a','a','b','c'],
            user_answer=['b',None,'a','d','b','c'],elapsed_time=[1]*6,solving_id=[2,1,1,1,3,4]))
        result=prepare.clean_student(d,cap=3)
        self.assertEqual(result.original_position.tolist(),[3,1,5])
        self.assertEqual(result.question_id.tolist(),['a','a','b'])

    def test_thresholds_count_unique_pairs_and_iterate_to_fixed_point(self):
        edges=[(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'c'),(4,'c')]
        d=pd.DataFrame(edges*3,columns=['participant','question_id'])
        result,trace=prepare.threshold_filter(d,min_items=2,min_students=2)
        self.assertEqual(set(zip(result.participant,result.question_id)),{(1,'a'),(1,'b'),(2,'a'),(2,'b')})
        self.assertGreater(len(trace),1)

    def test_split_is_deterministic_whole_student_and_60_20_20(self):
        a=prepare.split_students(range(101));b=prepare.split_students(reversed(range(101)))
        pd.testing.assert_frame_equal(a,b)
        self.assertTrue(a.participant.is_unique)
        self.assertEqual(a.author_phase.value_counts().to_dict(),{1:60,3:21,2:20})


if __name__=='__main__':unittest.main()
