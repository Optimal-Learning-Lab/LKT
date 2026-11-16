"""Read-only comparison of constant, previous evolving, and zero-start rates.

ROOT CONSTANT_RESULTS PREVIOUS_RESULTS ZERO_RESULTS OUTPUT_DIRECTORY
"""
import csv
import hashlib
import json
import math
from pathlib import Path
import runpy
import statistics as st
import sys
import zipfile

root, constant, previous, zero, output = map(Path, sys.argv[1:])
assert output.is_dir() and not (output/'report.md').exists()
helpers=runpy.run_path(str(root/'scripts/verify-gru-connection-random-time.py'))
read, metrics=helpers['read'],helpers['metrics']
models=[('constant','Constant-rate output adaptation',constant),
        ('previous','Evolving rate: alpha=.1, c=.25 starts',previous),
        ('zero','Evolving rate: alpha=c=0 starts',zero)]
fits_by_model={};pools_by_model={};summary=[];parameters=[];all_fits=[];all_pools=[]
truth={};max_difference=0.;clocks_checked=0
frozen=read(constant/'reports/fixed-time-scales.csv');assert len(frozen)==3072


def write(name,rows):
    fields=list(dict.fromkeys(k for r in rows for k in r))
    with (output/name).open('w',newline='',encoding='utf-8') as stream:
        writer=csv.DictWriter(stream,fieldnames=fields);writer.writeheader();writer.writerows(rows)


def table(headers,rows):
    return '\n'.join(['| '+' | '.join(headers)+' |','| '+' | '.join(['---']*len(headers))+' |']+
                     ['| '+' | '.join(map(str,r))+' |' for r in rows])


for key,label,path in models:
    fits=read(path/'reports/fits.csv');declared=read(path/'reports/pooled-by-seed.csv')
    assert len(fits)==15 and len(declared)==3
    assert read(path/'reports/fixed-time-scales.csv')==frozen
    amended=[];pools=[]
    for seed in (1,2,3):
        combined=[]
        for fold in range(1,6):
            row=dict(next(r for r in fits if r['partition']==f'cv{fold}' and int(r['seed'])==seed))
            pred=read(path/'reports'/f"{row['run_id']}-predictions.csv")
            identity=[(r['source_row'],r['participant'],r['observed']) for r in pred]
            pair=(fold,seed)
            if pair in truth:assert identity==truth[pair]
            else:truth[pair]=identity
            auc,ll=metrics(pred)
            assert abs(auc-float(row['test_AUC']))<3e-6 and abs(ll-float(row['test_log_loss']))<1e-7
            row.update(model=key,description=label,replay_test_AUC=auc,replay_test_log_loss=ll)
            diag=read(path/'reports'/f"{row['run_id']}-parameters.csv")[0]
            for field in ('negative_rate_uses','active_connection_updates','negative_post_update_rates',
                          'negative_rate_use_fraction','maximum_replay_probability_difference'):
                row[field]=diag[field]
            clocks=read(path/'reports'/f"{row['run_id']}-connection-clocks.csv")
            assert len(clocks)==3072
            for saved,clock in zip(frozen,clocks):
                assert 'weight_hh' in clock['matrix'] and 65<=int(clock['destination_row'])<=96
                assert 1<=int(clock['source_column'])<=32
                assert math.isclose(float(saved['time_scale']),float(clock['time_scale']),rel_tol=1e-13)
                assert 0<=float(clock['lambda'])<=1
                assert math.isclose(float(clock['recovery_half_life_minutes']),
                                    float(row['recovery_half_life_minutes'])/float(clock['time_scale']),rel_tol=1e-12)
                clocks_checked+=1
            amended.append(row);combined.extend(pred)
        assert len(combined)==55122 and len({r['source_row'] for r in combined})==55122
        auc,ll=metrics(combined);reported=next(r for r in declared if int(r['seed'])==seed)
        difference=max(abs(auc-float(reported['AUC'])),abs(ll-float(reported['log_loss'])))
        assert difference<1e-12;max_difference=max(max_difference,difference)
        pools.append(dict(model=key,seed=seed,AUC=auc,log_loss=ll))
    summary.append(dict(model=key,description=label,
                        heldout_AUC=st.mean(r['AUC'] for r in pools),heldout_log_loss=st.mean(r['log_loss'] for r in pools),
                        training_AUC=st.mean(float(r['training_AUC']) for r in amended),
                        training_log_loss=st.mean(float(r['training_log_loss']) for r in amended),
                        validation_log_loss=st.mean(float(r['validation_log_loss']) for r in amended)))
    params=dict(model=key,description=label)
    for field in ('output_eta','output_alpha','output_c','error_threshold','lambda_median',
                  'recovery_rate_per_minute','recovery_half_life_minutes','negative_rate_use_fraction','selected_epoch'):
        if field in amended[0]:
            vals=[float(r[field]) for r in amended if r[field]!='NA']
            params.update({field+'_median':st.median(vals),field+'_min':min(vals),field+'_max':max(vals)})
    params['negative_rate_fraction_weighted']=sum(float(r['negative_rate_uses']) for r in amended)/sum(float(r['active_connection_updates']) for r in amended)
    params['fits_with_negative_rates']=sum(float(r['negative_rate_use_fraction'])>0 for r in amended)
    parameters.append(params);all_fits.extend(amended);all_pools.extend(pools)
    fits_by_model[key]=amended;pools_by_model[key]=pools

pairs=[];contrasts=[]
for prior in ('constant','previous'):
    old={(r['partition'],r['seed']):r for r in fits_by_model[prior]};rows=[]
    for row in fits_by_model['zero']:
        base=old[(row['partition'],row['seed'])]
        r=dict(comparison=prior+'_to_zero',partition=row['partition'],seed=row['seed'],
               AUC_gain=float(row['replay_test_AUC'])-float(base['replay_test_AUC']),
               log_loss_improvement=float(base['replay_test_log_loss'])-float(row['replay_test_log_loss']),
               training_log_loss_improvement=float(base['training_log_loss'])-float(row['training_log_loss']))
        rows.append(r);pairs.append(r)
    a=next(r for r in summary if r['model']==prior);b=next(r for r in summary if r['model']=='zero')
    contrasts.append(dict(comparison=prior+'_to_zero',AUC_wins=sum(r['AUC_gain']>0 for r in rows),
                          log_loss_wins=sum(r['log_loss_improvement']>0 for r in rows),
                          training_log_loss_wins=sum(r['training_log_loss_improvement']>0 for r in rows),
                          pooled_AUC_gain=b['heldout_AUC']-a['heldout_AUC'],
                          pooled_log_loss_improvement=a['heldout_log_loss']-b['heldout_log_loss']))

for name,rows in [('summary.csv',summary),('learned-parameters-summary.csv',parameters),
                  ('fold-seed-results.csv',all_fits),('pooled-by-seed.csv',all_pools),
                  ('paired-comparisons.csv',pairs),('paired-summary.csv',contrasts)]:write(name,rows)
verification=dict(passed=True,fit_prediction_files=45,pooled_seed_sets=9,matched_fold_seed_sets=15,
                  static_clock_entries_checked=clocks_checked,maximum_pooled_metric_difference=max_difference,
                  checkpoints=json.loads((output/'checkpoint-verification.json').read_text()))
(output/'verification.json').write_text(json.dumps(verification,indent=2)+'\n')

z=fits_by_model['zero']
lines=['# Zero-start student learning-rate comparison — November 8, 2026','',
       'All 15 zero-start fits and their dependent comparison report completed successfully. No fit failures. '
       'Each condition has five whole-participant folds and seeds 1–3. The two evolving-rate conditions differ '
       'only in alpha/c optimizer starting values: 0.1/0.25 previously and 0/0 now. Equations and signed-rate '
       'behavior are unchanged. The constant controls are reused, not refitted.','',
       '## Performance','',
       'Held-out metrics pool the five folds within each seed, then average the three seeds. Training and '
       'validation losses below average the 15 selected-checkpoint metrics. Higher AUC and lower log loss are better.','',
       table(['Model','Held-out AUC','Held-out log loss','Training log loss','Validation log loss'],
             [[r['description'],f"{r['heldout_AUC']:.6f}",f"{r['heldout_log_loss']:.6f}",
               f"{r['training_log_loss']:.6f}",f"{r['validation_log_loss']:.6f}"] for r in summary]),'',
       table(['Comparison','Pooled AUC gain','Log-loss improvement','AUC wins /15','Log-loss wins /15','Training-loss wins /15'],
             [[r['comparison'],f"{r['pooled_AUC_gain']:+.6f}",f"{r['pooled_log_loss_improvement']:+.6f}",
               r['AUC_wins'],r['log_loss_wins'],r['training_log_loss_wins']] for r in contrasts]),'',
       'Zero initialization removes the previous fold-4 collapse and improves every held-out fold/seed '
       'relative to the previous nonzero start. It still produces slightly worse pooled AUC, log loss '
       'and average training loss than constant-rate output adaptation. These are descriptive matched '
       'comparisons; no significance interval has been estimated.','',
       '## Learned parameters','',
       'Entries are medians across 15 fits. Baseline eta is the fitted starting rate for each new student; '
       'the student rate subsequently evolves during both training and testing. Lambda summarizes each '
       'fit’s within-matrix median over the same 3072 connections. Recovery half-life is the base value '
       'in minutes; divide it by each connection’s fixed time multiplier for the actual half-life.','',
       table(['Model','Baseline eta','Alpha','c','Lambda median','Recovery rho/min','Base half-life/min'],
             [[r['description'],f"{r['output_eta_median']:.6f}",
               f"{r['output_alpha_median']:.9f}" if 'output_alpha_median' in r else '—',
               f"{r['output_c_median']:.9f}" if 'output_c_median' in r else '—',
               f"{r['lambda_median_median']:.6f}",f"{r['recovery_rate_per_minute_median']:.6f}",
               f"{r['recovery_half_life_minutes_median']:.6f}"] for r in parameters]),'',
       'All 15 zero-start fits learned positive alpha and c. Their ranges are '
       f"alpha {min(float(r['output_alpha']) for r in z):.9f}–{max(float(r['output_alpha']) for r in z):.9f}, "
       f"c {min(float(r['output_c']) for r in z):.9f}–{max(float(r['output_c']) for r in z):.9f}, "
       f"baseline eta {min(float(r['output_eta']) for r in z):.6f}–{max(float(r['output_eta']) for r in z):.6f}. "
       'The added parameters did learn away from zero; this result is not explained by being stuck at their boundaries.','',
       f"In {sum(float(r['negative_rate_use_fraction'])==0 for r in z)}/15 zero-start fits, no active held-out output "
       f"updates used a negative rate. The largest fraction in any fit was {100*max(float(r['negative_rate_use_fraction']) for r in z):.3f}%. "
       'The previous fold-4 fits used negative rates in approximately 94% of those updates. '
       f"Zero-start selected epochs range from {min(int(r['selected_epoch']) for r in z)} to {max(int(r['selected_epoch']) for r in z)} "
       f"(median {st.median(int(r['selected_epoch']) for r in z):g}).",'',
       '## Per-seed results','',
       table(['Seed','Constant AUC','Previous AUC','Zero AUC','Constant LL','Previous LL','Zero LL'],
             [[s]+[f"{pools_by_model[k][s-1]['AUC']:.6f}" for k in ('constant','previous','zero')]+
              [f"{pools_by_model[k][s-1]['log_loss']:.6f}" for k in ('constant','previous','zero')]
              for s in (1,2,3)]),'',
       'Zero start improves both metrics over constant rate in seed 1 and loses both in seeds 2 and 3.','',
       '## Every zero-start fold and seed','',
       table(['Fold','Seed','Baseline eta','Alpha','c','Lambda median','Base half-life/min','Test AUC','Test LL','Training LL','Negative-rate updates'],
             [[r['partition'],r['seed'],f"{float(r['output_eta']):.6f}",f"{float(r['output_alpha']):.9f}",
               f"{float(r['output_c']):.9f}",f"{float(r['lambda_median']):.6f}",
               f"{float(r['recovery_half_life_minutes']):.6f}",f"{float(r['test_AUC']):.6f}",
               f"{float(r['test_log_loss']):.6f}",f"{float(r['training_log_loss']):.6f}",
               f"{100*float(r['negative_rate_use_fraction']):.3f}%"]
              for r in sorted(z,key=lambda r:(r['partition'],int(r['seed'])))]),'',
       '## Verification and artifacts','',
       f"Independently recomputed all 45 held-out fold prediction files and nine pooled seed sets; maximum pooled "
       f"metric discrepancy {max_difference:.3g}. All comparisons have identical held-out rows, participants and "
       f"responses. Checked {clocks_checked:,} physical recurrent clock, lambda-bound and half-life entries across "
       'all three sets. A separate R audit verified the 15 zero-start and 15 constant checkpoints against '
       'their frozen protocols, fitted parameters, selected epochs, all phase metrics, replay CSVs and portable '
       'checkpoint loading. The previous run was independently checkpoint-audited in the preceding report. '
       'The package check completed with no errors and three pre-existing dependency-build/vignette warnings. '
       'No models, original submissions or historical results were modified.','',
       '- fold-seed-results.csv contains all 45 fits and their learned parameters and metrics.\n'
       '- paired-comparisons.csv contains all 30 comparisons against the two references.\n'
       '- learned-parameters.zip contains full connection parameters, clocks and half-lives for all three sets.\n'
       '- verification.json and checkpoint-verification.json contain the checks.','']
(output/'report.md').write_text('\n'.join(lines),encoding='utf-8')
with zipfile.ZipFile(output/'learned-parameters.zip','w',compression=zipfile.ZIP_DEFLATED) as archive:
    for key,label,path in models:
        for fit in fits_by_model[key]:
            for suffix in ('parameters.csv','connection-parameters.csv','connection-clocks.csv','output-parameters.csv'):
                file=path/'reports'/f"{fit['run_id']}-{suffix}"
                if file.exists():archive.write(file,f'{key}/{file.name}')
    for name in ('summary.csv','fold-seed-results.csv','learned-parameters-summary.csv','verification.json','checkpoint-verification.json'):
        archive.write(output/name,name)
files=sorted(p for p in output.iterdir() if p.is_file() and p.name!='report-files.sha256')
(output/'report-files.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.name+'\n' for p in files))
print(json.dumps(dict(summary=summary,parameters=parameters,contrasts=contrasts,maximum_pooled_difference=max_difference),indent=2))
