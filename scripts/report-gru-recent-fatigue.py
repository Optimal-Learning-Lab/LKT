"""Audit saved held-out predictions and report a manifest of matched GRU studies.

ROOT MODEL_MANIFEST_JSON OUTPUT_DIRECTORY
The manifest supplies explicit result directories, model keys and labels. No fits.
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

root, manifest, output = map(Path, sys.argv[1:])
assert output.is_dir() and not (output/'report.md').exists()
helpers = runpy.run_path(str(root/'scripts/verify-gru-connection-random-time.py'))
read, metrics = helpers['read'], helpers['metrics']
spec = json.loads(manifest.read_text())
models = spec['models']
by_key = {}
all_fits, all_pool, summary, parameter_summary = [], [], [], []
truth = {}
largest_difference = 0.0


def write(name, rows):
    keys = list(dict.fromkeys(k for r in rows for k in r))
    with (output/name).open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=keys)
        writer.writeheader()
        writer.writerows(rows)


def mean(rows, key):
    return st.mean(float(x[key]) for x in rows)


def table(headers, rows):
    return '\n'.join(['| '+' | '.join(headers)+' |',
                      '| '+' | '.join(['---']*len(headers))+' |']+
                     ['| '+' | '.join(map(str, row))+' |' for row in rows])


for item in models:
    path = Path(item['results'])
    fits = read(path/'reports/fits.csv')
    declared = read(path/'reports/pooled-by-seed.csv')
    if 'condition' in item:
        fits = [r for r in fits if r['candidate']==item['condition']]
        declared = [r for r in declared if r.get('condition', r.get('candidate'))==item['condition']]
    assert len(fits)==15 and len(declared)==3
    pools, amended = [], []
    for seed in (1, 2, 3):
        combined = []
        for fold in range(1, 6):
            matches = [r for r in fits if r['partition']==f'cv{fold}' and int(r['seed'])==seed]
            assert len(matches)==1
            row = dict(matches[0])
            pred = read(path/'reports'/f"{row['run_id']}-predictions.csv")
            identity = [(x['source_row'], x['participant'], x['observed']) for x in pred]
            key = (fold, seed)
            if key in truth:
                assert identity==truth[key]
            else:
                truth[key]=identity
            auc, ll = metrics(pred)
            # Fit-time and replay probabilities differ by float32 batching roundoff.
            assert abs(auc-float(row['test_AUC']))<3e-6
            assert abs(ll-float(row['test_log_loss']))<1e-7
            row.update(model=item['key'], description=item['label'], replay_test_AUC=auc, replay_test_log_loss=ll)
            if item.get('rate_diagnostics', False):
                diagnostics = read(path/'reports'/f"{row['run_id']}-parameters.csv")[0]
                for k in ('negative_rate_uses', 'active_connection_updates', 'negative_post_update_rates',
                          'negative_rate_use_fraction', 'maximum_replay_probability_difference'):
                    row[k]=diagnostics[k]
            amended.append(row)
            combined.extend(pred)
        assert len(combined)==55122 and len({r['source_row'] for r in combined})==55122
        auc, ll=metrics(combined)
        reported=next(r for r in declared if int(r['seed'])==seed)
        difference=max(abs(auc-float(reported['AUC'])),abs(ll-float(reported['log_loss'])))
        assert difference<1e-12
        largest_difference=max(largest_difference,difference)
        pools.append(dict(model=item['key'], seed=seed, AUC=auc, log_loss=ll))
    record=dict(model=item['key'],description=item['label'],fits=15,
                fatigued_connections=int(amended[0]['connection_count']),
                test_AUC=mean(pools,'AUC'),test_log_loss=mean(pools,'log_loss'),
                training_AUC=mean(amended,'training_AUC'),training_log_loss=mean(amended,'training_log_loss'),
                validation_AUC=mean(amended,'validation_AUC'),validation_log_loss=mean(amended,'validation_log_loss'))
    parameters=dict(model=item['key'],description=item['label'])
    for k in ('output_eta','output_alpha','output_c','error_threshold','lambda_median',
              'recovery_rate_per_minute','recovery_half_life_minutes','negative_rate_use_fraction'):
        if k in amended[0]:
            values=[float(r[k]) for r in amended]
            parameters.update({k+'_median':st.median(values),k+'_min':min(values),k+'_max':max(values)})
    by_key[item['key']]=dict(spec=item,fits=amended,pool=pools,summary=record,parameters=parameters)
    all_fits.extend(amended);all_pool.extend(pools);summary.append(record);parameter_summary.append(parameters)

constant=by_key['recurrent_constant'];adaptive=by_key['recurrent_evolving'];original=by_key['input_recurrent_constant']
# Both latest studies must retain the precise physical recurrent-connection clocks.
fixed = read(Path(constant['spec']['results'])/'reports/fixed-time-scales.csv')
assert len(fixed)==3072
clocks_checked=0
for model in (constant, adaptive):
    path=Path(model['spec']['results'])
    assert read(path/'reports/fixed-time-scales.csv')==fixed
    for fit in model['fits']:
        clocks=read(path/'reports'/f"{fit['run_id']}-connection-clocks.csv")
        assert len(clocks)==3072
        for frozen, clock in zip(fixed, clocks):
            assert 'weight_hh' in clock['matrix'] and 65<=int(clock['destination_row'])<=96
            assert 1<=int(clock['source_column'])<=32
            assert math.isclose(float(frozen['time_scale']),float(clock['time_scale']),rel_tol=1e-13)
            assert 0<=float(clock['lambda'])<=1
            assert math.isclose(float(clock['recovery_half_life_minutes']),
                                float(fit['recovery_half_life_minutes'])/float(clock['time_scale']),rel_tol=1e-12)
            clocks_checked+=1

pairs, contrasts=[] ,[]
for old,new in ((original,constant),(constant,adaptive)):
    old_rows={(r['partition'],r['seed']):r for r in old['fits']}
    current=[]
    for row in new['fits']:
        prior=old_rows[(row['partition'],row['seed'])]
        r=dict(comparison=old['spec']['key']+'_to_'+new['spec']['key'],partition=row['partition'],seed=row['seed'],
               test_AUC_gain=float(row['replay_test_AUC'])-float(prior['replay_test_AUC']),
               test_log_loss_improvement=float(prior['replay_test_log_loss'])-float(row['replay_test_log_loss']),
               training_log_loss_improvement=float(prior['training_log_loss'])-float(row['training_log_loss']))
        current.append(r);pairs.append(r)
    contrasts.append(dict(comparison=current[0]['comparison'],test_AUC_wins=sum(r['test_AUC_gain']>0 for r in current),
                          test_log_loss_wins=sum(r['test_log_loss_improvement']>0 for r in current),
                          training_log_loss_wins=sum(r['training_log_loss_improvement']>0 for r in current),
                          pooled_AUC_gain=new['summary']['test_AUC']-old['summary']['test_AUC'],
                          pooled_log_loss_improvement=old['summary']['test_log_loss']-new['summary']['test_log_loss']))

for name,rows in [('summary.csv',summary),('learned-parameters-summary.csv',parameter_summary),
                  ('fold-seed-results.csv',all_fits),('pooled-by-seed.csv',all_pool),
                  ('paired-comparisons.csv',pairs),('paired-summary.csv',contrasts)]:
    write(name,rows)

audit=dict(passed=True,models=len(models),fit_prediction_files=len(all_fits),pooled_seed_sets=len(all_pool),
           matched_fold_seed_sets=15,rows_per_pooled_seed=55122,clocks_checked=clocks_checked,
           maximum_pooled_metric_difference=largest_difference,
           checkpoints=json.loads((output/'checkpoint-verification.json').read_text()))
(output/'verification.json').write_text(json.dumps(audit,indent=2)+'\n')

lines=['# Completed GRU fatigue comparisons — November 8, 2026','',
       'All scientific arrays and their dependent reports completed successfully. No jobs remain running. '
       'The failed quota-limited setup started no scientific fits and is excluded.','',
       'Each condition contains five whole-participant folds and seeds 1–3 (15 fits). '
       'Held-out AUC and log loss below are computed by pooling the five held-out folds within each seed, '
       'then averaging the three seed results. Training/validation losses are arithmetic means of the 15 '
       'selected-checkpoint metrics. Lower log loss and higher AUC are better.','',
       'All conditions below use four numeric inputs: PPES and student-level logitdec are removed; '
       'student-item logitdec remains. They retain the same categorical inputs and training budget. '
       'Random-clock conditions share the exact retained subsets of the original fixed 0.1–10 time multipliers.','',
       '## All recent connection-fatigue conditions','',
       table(['Condition','Fits','Fatigued weights','Held-out AUC','Held-out log loss','Training log loss'],
             [[r['description'],15,r['fatigued_connections'],f"{r['test_AUC']:.6f}",f"{r['test_log_loss']:.6f}",f"{r['training_log_loss']:.6f}"] for r in summary]),'',
       '“New unit strength” means the GRU proposal before the update gate blends it with the previous hidden state. '
       'The input/recurrent variants fatigue only connections contributing to that proposal; reset/update gates are ordinary. '
       'The all-connections conditions also fatigue gate connections. All rows retain constant output adaptation '
       'except the evolving-rate row.','',
       '## Newly completed comparisons','',
       table(['Comparison','AUC change','Log-loss improvement','AUC wins /15','Log-loss wins /15','Training-loss wins /15'],
             [[r['comparison'],f"{r['pooled_AUC_gain']:+.6f}",f"{r['pooled_log_loss_improvement']:+.6f}",r['test_AUC_wins'],r['test_log_loss_wins'],r['training_log_loss_wins']] for r in contrasts]),'',
       'The recurrent-only change is very small and does not improve the average result. '
       'The evolving student-rate model loses on both held-out metrics in every matched fold/seed.','',
       '## Learned parameters','',
       'Values below are medians across fits; the lambda entry is the median of each fit’s within-matrix median. '
       'Different scopes contain different physical connections, so lambda medians are descriptive rather than '
       'estimates on one common set of weights. Recovery half-life is the base half-life in minutes; '
       'each connection’s actual half-life is the base value divided by its fixed time multiplier.','',
       table(['Condition','Baseline eta','Alpha','c','Lambda median','Recovery rho/min','Base half-life/min'],
             [[r['description'],f"{r['output_eta_median']:.6f}",
               f"{r['output_alpha_median']:.6f}" if 'output_alpha_median' in r else '—',
               f"{r['output_c_median']:.6f}" if 'output_c_median' in r else '—',
               f"{r['lambda_median_median']:.6f}",f"{r['recovery_rate_per_minute_median']:.6f}",
               f"{r['recovery_half_life_minutes_median']:.6f}"] for r in parameter_summary]),'',
       'The evolving model predicts first, applies v_new = v_old + eta_student*(y-p)*z, then updates '
       'eta_student_new = eta_student + alpha*abs(y-p) - c for the next response. Each student starts '
       'at the fitted baseline eta. Shared baseline eta/alpha/c are fixed during testing; student eta '
       'continues evolving and is allowed to become negative. Alpha/c started at 0.1/0.25 and baseline eta '
       'at zero. At those starts, every response initially reduces eta by at least 0.15.','',
       '## Fold 4 failure in the evolving-rate experiment','',
       table(['Seed','Training AUC','Test AUC','Test log loss','Alpha','c','c/alpha','Negative-rate updates','Selected epoch'],
             [[r['seed'],f"{float(r['training_AUC']):.4f}",f"{float(r['test_AUC']):.4f}",f"{float(r['test_log_loss']):.4f}",
               f"{float(r['output_alpha']):.5f}",f"{float(r['output_c']):.5f}",f"{float(r['error_threshold']):.3f}",
               f"{100*float(r['negative_rate_use_fraction']):.1f}%",r['selected_epoch']]
              for r in adaptive['fits'] if r['partition']=='cv4']),'',
       'Because c/alpha exceeds 1 in these three fits, every binary-response error decreases the student rate. '
       'About 94% of active held-out output-connection updates use a negative rate. Training as well as '
       'testing is near chance, so this is a failure to fit these data, not only a generalization gap. '
       'The other 12 fits also lose both held-out metrics to their controls. The result establishes poor '
       'performance for this initialization and matched training budget; it does not establish that all '
       'student-specific rate mechanisms must fail.','',
       '## Per-seed held-out results for the two new sets','',
       table(['Seed','Constant AUC','Evolving AUC','Constant log loss','Evolving log loss'],
             [[s,f"{constant['pool'][s-1]['AUC']:.6f}",f"{adaptive['pool'][s-1]['AUC']:.6f}",
               f"{constant['pool'][s-1]['log_loss']:.6f}",f"{adaptive['pool'][s-1]['log_loss']:.6f}"] for s in (1,2,3)]),'']
for model in (constant,adaptive):
    lines+=['## '+model['spec']['label']+' — every fold and seed','',
            table(['Fold','Seed','Starting eta','Alpha','c','Lambda median','Base half-life/min','Test AUC','Test log loss','Train log loss'],
                  [[r['partition'],r['seed'],f"{float(r['output_eta']):.6f}",
                    f"{float(r['output_alpha']):.6f}" if 'output_alpha' in r else '—',
                    f"{float(r['output_c']):.6f}" if 'output_c' in r else '—',
                    f"{float(r['lambda_median']):.6f}",f"{float(r['recovery_half_life_minutes']):.6f}",
                    f"{float(r['test_AUC']):.6f}",f"{float(r['test_log_loss']):.6f}",f"{float(r['training_log_loss']):.6f}"]
                   for r in sorted(model['fits'],key=lambda x:(x['partition'],int(x['seed'])))]),'']
lines+=['## Verification and artifacts','',
        f"Independently recomputed held-out metrics from all {len(all_fits)} fit prediction files and {len(all_pool)} pooled seed sets. "
        f"Largest pooled-metric discrepancy: {largest_difference:.3g}. Verified identical held-out rows/responses/students "
        f"throughout and {clocks_checked:,} physical recurrent clock/half-life entries in the latest 30 fits. "
        'The separate R audit verified all 30 checkpoints against exact settings, whole-student splits, '
        'parameter counts, fitted parameter values, selected epochs and all training/validation/test metrics. '
        'Saved-model replay probabilities agree within 1.2e-7. No models or original results were modified.','',
        'The ordinary package check completed with no errors and three warnings: dependency packages built '
        'under a newer R patch release, and the two warnings about intentionally unbuilt vignette outputs.','',
        '- summary.csv: all eight conditions.\n- fold-seed-results.csv: every learned scalar and metric for all 120 fits.\n'
        '- pooled-by-seed.csv: all 24 seed-level summaries.\n- paired-comparisons.csv: both new matched contrasts.\n'
        '- learned-parameters.zip: full connection lambda/clock/half-life tables and scalar diagnostics for the latest 30 fits.\n'
        '- verification.json and checkpoint-verification.json: independent checks.','']
(output/'report.md').write_text('\n'.join(lines),encoding='utf-8')
with zipfile.ZipFile(output/'learned-parameters.zip','w',compression=zipfile.ZIP_DEFLATED) as archive:
    for model in (constant,adaptive):
        path=Path(model['spec']['results'])/'reports'
        for fit in model['fits']:
            for suffix in ('connection-clocks.csv','connection-parameters.csv','parameters.csv','output-parameters.csv'):
                file=path/f"{fit['run_id']}-{suffix}"
                if file.exists():
                    archive.write(file,f"{model['spec']['key']}/{file.name}")
    for name in ('fold-seed-results.csv','learned-parameters-summary.csv','summary.csv','verification.json','checkpoint-verification.json'):
        archive.write(output/name,name)
files=sorted(p for p in output.iterdir() if p.is_file() and p.name!='report-files.sha256')
(output/'report-files.sha256').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+p.name+'\n' for p in files))
print(json.dumps(dict(summary=summary,contrasts=contrasts,maximum_pooled_metric_difference=largest_difference),indent=2))
