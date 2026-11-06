"""Paper-aligned UNVaMP EdNet preparation, with explicit unreleased details.

Uses official KT1/content archives. No UNVaMP implementation is substituted for
LKT. Choices absent from the paper are frozen here before model outcomes exist.
"""
import argparse
import csv
import hashlib
import io
import json
from pathlib import Path
import random
import re
import sys
import zipfile

import numpy as np
import pandas as pd


def sha(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as f:
        for block in iter(lambda:f.read(8*1024*1024),b''):h.update(block)
    return h.hexdigest()


def clean_student(frame,cap=500):
    required={'timestamp','question_id','user_answer','elapsed_time','solving_id'}
    if not required.issubset(frame):raise ValueError('KT1 schema is incomplete')
    d=frame.copy();d['original_position']=np.arange(len(d))+1
    # Drop missing answers before choosing the first valid session response.
    valid=d.user_answer.notna() & d.user_answer.astype(str).str.strip().ne('')
    d=d.loc[valid].copy()
    if not d.user_answer.isin(list('abcd')).all():raise ValueError('Unexpected KT1 answer code')
    if d[list(required-{'user_answer'})].isna().any().any():raise ValueError('Missing required KT1 fields')
    d=d.sort_values(['timestamp','original_position'],kind='stable')
    d=d.drop_duplicates(['solving_id','question_id'],keep='first')
    if (d.elapsed_time<0).any():raise ValueError('Negative elapsed duration')
    return d.head(cap).copy()


def answered_item_count(frame):
    """Qualification uses full raw histories, before truncation or item filtering."""
    if not {'question_id','user_answer'}.issubset(frame):
        raise ValueError('Qualification requires question and answer columns')
    valid=frame.user_answer.notna() & frame.user_answer.astype(str).str.strip().ne('')
    answered=frame.loc[valid]
    if not answered.user_answer.isin(list('abcd')).all():
        raise ValueError('Unexpected KT1 answer code')
    if answered.question_id.isna().any():raise ValueError('Answered question identity is missing')
    return int(answered.question_id.nunique())


def sample_students(ordered_ids,read_student,size,sampling_frame,audit):
    """Rejection sampling from a random permutation is uniform among qualifiers."""
    if sampling_frame not in ('all-students','qualified-students') or size<1:
        raise ValueError('Invalid sampling frame or sample size')
    selected=0
    for rank,uid in enumerate(ordered_ids,1):
        frame=read_student(uid);count=answered_item_count(frame)
        accept=sampling_frame=='all-students' or count>=10
        audit.append(dict(candidate_rank=rank,participant=uid,raw_responses=len(frame),
                          answered_distinct_items=count,selected=accept))
        if not accept:continue
        selected+=1
        yield uid,frame
        if selected==size:return
    raise ValueError('Sampling population contains too few eligible students')


def threshold_filter(data,min_items=10,min_students=100):
    d=data.copy();trace=[]
    while True:
        before=len(d)
        students=d.groupby('participant').question_id.nunique()
        d=d.loc[d.participant.isin(students.index[students>=min_items])]
        items=d.groupby('question_id').participant.nunique()
        d=d.loc[d.question_id.isin(items.index[items>=min_students])]
        trace.append(dict(pass_number=len(trace)+1,responses=len(d),
                          students=d.participant.nunique(),items=d.question_id.nunique()))
        if len(d)==before:break
    if len(d):
        assert d.groupby('participant').question_id.nunique().min()>=min_items
        assert d.groupby('question_id').participant.nunique().min()>=min_students
    return d.copy(),trace


def filter_then_cap(data,cap=500,min_items=10,min_students=100):
    """Establish sample eligibility on full histories; do not refilter the cap."""
    filtered,trace=threshold_filter(data,min_items,min_students)
    eligibility=filtered.groupby('question_id').participant.nunique().rename('distinct_students').reset_index()
    chronological=filtered.sort_values(['participant','timestamp','original_position'],kind='stable')
    capped=chronological.groupby('participant',sort=False).head(cap).copy()
    return capped,trace,eligibility


def global_threshold_core(uids,neighbors,n_items,min_items=10,min_students=100):
    """Bipartite fixed point; each student/item edge occurs exactly once."""
    if not len(uids):raise ValueError('No initially qualifying students')
    lengths=np.array([len(v) for v in neighbors],dtype=np.int64)
    if np.any(lengths==0):raise ValueError('Global graph contains empty histories')
    offsets=np.r_[0,np.cumsum(lengths)[:-1]]
    edges=np.concatenate(neighbors)
    students=np.ones(len(uids),dtype=bool);items=np.ones(n_items,dtype=bool);trace=[]
    while True:
        old_students=students.copy();old_items=items.copy()
        student_counts=np.add.reduceat(items[edges],offsets,dtype=np.int32)
        students &= student_counts>=min_items
        item_counts=np.bincount(edges[np.repeat(students,lengths)],minlength=n_items)
        items &= item_counts>=min_students
        trace.append(dict(pass_number=len(trace)+1,students=int(students.sum()),items=int(items.sum())))
        if np.array_equal(students,old_students) and np.array_equal(items,old_items):break
    assert np.all(student_counts[students]>=min_items) and np.all(item_counts[items]>=min_students)
    return np.asarray(uids)[students],np.flatnonzero(items),student_counts[students],item_counts,trace


def kt1_global_eligibility(archive,members,question_ids):
    """Stream full KT1; answer validity and item metadata define graph edges."""
    lookup={q:i for i,q in enumerate(question_ids)}
    uids=[];neighbors=[];raw_rows=0;answered_rows=0
    for k,(uid,name) in enumerate(members.items(),1):
        seen=set()
        with io.TextIOWrapper(archive.open(name),encoding='utf-8',newline='') as stream:
            reader=csv.DictReader(stream)
            if set(reader.fieldnames)!={'timestamp','solving_id','question_id','user_answer','elapsed_time'}:
                raise ValueError('KT1 schema is incomplete')
            for row in reader:
                raw_rows+=1;answer=row['user_answer']
                if not answer or not answer.strip():continue
                if answer not in 'abcd' or len(answer)!=1:raise ValueError('Unexpected KT1 answer code')
                answered_rows+=1
                if row['question_id'] in lookup:seen.add(lookup[row['question_id']])
        if len(seen)>=10:
            uids.append(uid);neighbors.append(np.fromiter(seen,dtype=np.int32))
        if k%100000==0:print('Full KT1 students examined:',k,flush=True)
    result=global_threshold_core(uids,neighbors,len(question_ids))
    eligible_uids,eligible_indices,student_counts,item_counts,trace=result
    summary=dict(population='All available KT1 student files, before sampling and truncation',
        students_examined=len(members),raw_responses=raw_rows,answered_responses=answered_rows,
        initially_qualified_students=len(uids),retained_students=len(eligible_uids),
        retained_items=len(eligible_indices),trace=trace)
    return set(eligible_uids.tolist()),set(question_ids[i] for i in eligible_indices),student_counts,item_counts,summary


def split_students(ids,seed=2):
    values=np.array(sorted(set(ids)),dtype=np.int64)
    values=np.random.default_rng(seed).permutation(values)
    a=int(.6*len(values));b=a+int(.2*len(values))
    phases=np.r_[np.ones(a,dtype=int),np.full(b-a,2,dtype=int),np.full(len(values)-b,3,dtype=int)]
    return pd.DataFrame({'participant':values,'author_phase':phases})


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('archives',type=Path);parser.add_argument('output',type=Path)
    parser.add_argument('--sampling-frame',choices=['all-students','qualified-students'],
                        default='all-students')
    parser.add_argument('--threshold-scope',choices=['sample','kt1'],default='sample')
    parser.add_argument('--cap-order',choices=['before-filtering','after-filtering'],default=None,
                        help='Sample-scope filtering only; default preserves the original cap-first procedure')
    args=parser.parse_args();out=args.output.resolve()
    if args.threshold_scope=='kt1' and args.sampling_frame!='qualified-students':
        raise ValueError('Full-KT1 thresholds require sampling qualified students')
    if args.threshold_scope=='kt1' and args.cap_order is not None:
        raise ValueError('Full-KT1 thresholds already precede sampling and capping')
    cap_order=args.cap_order if args.cap_order is not None else ('after-filtering' if args.threshold_scope=='kt1' else 'before-filtering')
    if out.exists():raise FileExistsError('Preserve existing preparation; choose a new output')
    out.mkdir(parents=True)
    archives=args.archives.resolve()
    with zipfile.ZipFile(archives/'EdNet-Contents.zip') as z:
        questions=pd.read_csv(z.open('contents/questions.csv'),dtype={'question_id':str,'correct_answer':str,'tags':str})
    assert questions.question_id.is_unique
    if questions[['question_id','correct_answer','tags']].isna().any().any():raise ValueError('Missing item metadata')
    # Untagged metadata cannot provide the model's required skill identifier.
    questions=questions.loc[questions.tags.ne('-1'),['question_id','correct_answer','tags']].copy()
    questions['tags']=questions.tags.str.replace(';','_',regex=False)
    frames=[];raw_rows=0;selected=[];sampling_audit=[];global_summary=None
    with zipfile.ZipFile(archives/'EdNet-KT1.zip') as z:
        members={int(m.group(1)):name for name in z.namelist() if (m:=re.fullmatch(r'KT1/u(\d+)\.csv',name))}
        if args.threshold_scope=='kt1':
            eligible_uids,eligible_items,_,item_counts,global_summary=kt1_global_eligibility(z,members,questions.question_id.to_list())
            pd.DataFrame({'participant':sorted(eligible_uids)}).to_csv(out/'global-eligible-students.csv',index=False)
            pd.DataFrame({'question_id':questions.question_id,'distinct_qualified_students':item_counts,
                'eligible':questions.question_id.isin(eligible_items)}).to_csv(out/'global-item-counts.csv',index=False)
            print('Full KT1 fixed point:',json.dumps(global_summary),flush=True)
        # Uniform random ordering over available students, with a fixed seed.
        candidates=list(range(max(members)+1));random.Random(2).shuffle(candidates)
        ordered_ids=(u for u in candidates if u in members and
                     (args.threshold_scope=='sample' or u in eligible_uids))
        def read_student(uid):
            return pd.read_csv(z.open(members[uid]),dtype={'question_id':str,'user_answer':str})
        sample=sample_students(ordered_ids,read_student,20000,args.sampling_frame,sampling_audit)
        for k,(uid,frame) in enumerate(sample):
            selected.append(uid)
            raw_rows+=len(frame)
            if args.threshold_scope=='kt1':
                frame=clean_student(frame,cap=len(frame))
                frame=frame.loc[frame.question_id.isin(eligible_items)].head(500).copy()
            elif cap_order=='after-filtering':frame=clean_student(frame,cap=len(frame))
            else:frame=clean_student(frame)
            frame['participant']=uid
            frames.append(frame)
            if (k+1)%2000==0:print('Prepared sampled students:',k+1,flush=True)
    data=pd.concat(frames,ignore_index=True)
    cleaned_rows=len(data);capped_rows=len(data)
    data=data.merge(questions,on='question_id',how='inner',validate='many_to_one')
    metadata_rows=len(data)
    if args.threshold_scope=='sample' and cap_order=='after-filtering':
        data,trace,eligibility=filter_then_cap(data)
        eligibility.to_csv(out/'item-eligibility-before-cap.csv',index=False)
        capped_rows=len(data)
    elif args.threshold_scope=='sample':data,trace=threshold_filter(data)
    else:
        if not set(data.participant).issubset(eligible_uids) or not set(data.question_id).issubset(eligible_items):
            raise ValueError('Sample contains globally ineligible identities')
        trace=[dict(pass_number=0,responses=len(data),students=data.participant.nunique(),items=data.question_id.nunique())]
    if data.empty:raise ValueError('Threshold filtering removed the sample')
    data=data.sort_values(['participant','timestamp','original_position'],kind='stable')
    assignments=split_students(data.participant)
    data=data.merge(assignments,on='participant',validate='many_to_one',sort=False)
    position=data.groupby('participant',sort=False).cumcount()+1
    export=pd.DataFrame({'Anon.Student.Id':data.participant.astype(str),
        'KC..Default.':data.question_id,'KC..Cluster.':data.tags,
        'CF..ansbin.':(data.user_answer==data.correct_answer).astype(int),
        'CF..Time.':data.timestamp/1000,'Duration..sec.':data.elapsed_time/1000,
        'comparison_source_row':data.participant.astype(str)+':'+data.original_position.astype(str),
        'benchmark_position':position,'author_phase':data.author_phase})
    assert export.comparison_source_row.is_unique and not export.isna().any().any()
    assert export.groupby('Anon.Student.Id').size().max()<=500
    export.to_csv(out/'ednet-lkt.csv',index=False)
    assignments.to_csv(out/'participant-assignments.csv',index=False)
    pd.DataFrame({'participant':selected}).to_csv(out/'sampled-students.csv',index=False)
    pd.DataFrame(sampling_audit).to_csv(out/'sampling-audit.csv',index=False)
    record=dict(protocol='UNVaMP paper-aligned procedural comparison',
        paper='https://educationaldatamining.org/edm2026/proceedings/2026.EDM.full-papers.72/index.html',
        exact_author_sample=False,sampling_seed=2,split_seed=2,initial_students=20000,
        sampling_frame=args.sampling_frame,examined_students=len(sampling_audit),
        threshold_scope=args.threshold_scope,global_filtering=global_summary,
        cap_order=cap_order,cleaned_sample_responses=cleaned_rows,
        precap_filtered_responses=(trace[-1]['responses'] if args.threshold_scope=='sample' and cap_order=='after-filtering' else None),
        qualification_rejections=sum(not row['selected'] for row in sampling_audit),
        raw_responses=raw_rows,capped_responses=capped_rows,metadata_responses=metadata_rows,
        responses=len(export),retained_students=export['Anon.Student.Id'].nunique(),
        items=export['KC..Default.'].nunique(),skill_combinations=export['KC..Cluster.'].nunique(),
        filtering_trace=trace,choices=dict(
            sampling=('Uniform random available KT1 students' if args.sampling_frame=='all-students' else
                      'Uniform random KT1 students with >=10 distinct validly answered items in full raw histories; first 20000 qualifiers in seed-2 random ordering'),
            qualification=('Full KT1 iterative 10-distinct-items/student and 100-distinct-students/item fixed point, with valid answers and tagged metadata, before sampling/cap' if args.threshold_scope=='kt1' else
                'Before cap, metadata exclusion and iterative item/student filtering; nonmissing a-d answers; distinct question IDs'),
            session='Official KT1 solving_id, within student',
            duplicates='First valid response per student/solving_id/question_id; later sessions retained',
            chronology='timestamp, with original file position breaking ties',
            cap=('Earliest 500 cleaned globally eligible responses, after full-KT1 threshold filtering; no sample-level rethresholding' if args.threshold_scope=='kt1' else
                 ('Earliest 500 retained responses after iterative sample-level threshold filtering; no rethresholding after cap' if cap_order=='after-filtering' else
                  'Earliest 500 cleaned responses, before iterative threshold filtering')),
            thresholds='Fixed point: >=10 distinct items/student and >=100 distinct students/item; population='+args.threshold_scope,
            missing_metadata='Exclude questions with absent metadata or missing/untagged skills',
            split='Whole students; floor(60%) train, floor(20%) validation, remainder test; PCG64 seed 2',
            primary_targets='Exclude each retained student first response; also report all-response scores',
            skill_representation='Original ordered item-tag combination, semicolons represented by underscores'),
        archive_sha256={name:sha(archives/name) for name in ['EdNet-KT1.zip','EdNet-Contents.zip']},
        exported_data_sha256=sha(out/'ednet-lkt.csv'),
        participant_assignments_sha256=sha(out/'participant-assignments.csv'),
        sampling_audit_sha256=sha(out/'sampling-audit.csv'),
        item_eligibility_before_cap_sha256=(sha(out/'item-eligibility-before-cap.csv') if args.threshold_scope=='sample' and cap_order=='after-filtering' else None),
        global_eligibility_sha256=({name:sha(out/name) for name in ['global-eligible-students.csv','global-item-counts.csv']} if args.threshold_scope=='kt1' else None),
        preprocessing_sha256=sha(__file__),python=sys.version,numpy=np.__version__,pandas=pd.__version__)
    (out/'provenance.json').write_text(json.dumps(record,indent=2)+'\n')
    print(json.dumps({k:record[k] for k in ['responses','retained_students','items','skill_combinations','filtering_trace']},indent=2))


if __name__=='__main__':main()
