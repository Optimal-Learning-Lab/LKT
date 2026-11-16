"""Independent held-out metrics, matched rows and fixed-clock checks; no model fitting."""
import argparse,csv,json,math,statistics
from pathlib import Path

def read(path):
    with Path(path).open(newline='',encoding='utf-8-sig') as stream:return list(csv.DictReader(stream))

def metrics(rows):
    y=[int(x['observed']) for x in rows];p=[float(x['prediction']) for x in rows]
    assert len(y)>0 and all(0<v<1 for v in p)
    order=sorted(range(len(p)),key=p.__getitem__);positive_rank=0.;start=0
    while start<len(order):
        end=start+1
        while end<len(order) and p[order[end]]==p[order[start]]:end+=1
        rank=(start+1+end)/2
        positive_rank+=rank*sum(y[order[k]] for k in range(start,end));start=end
    n1=sum(y);n0=len(y)-n1
    return (positive_rank-n1*(n1+1)/2)/(n1*n0),-sum(math.log(v) if t else math.log1p(-v) for t,v in zip(y,p))/len(y)

def main():
    a=argparse.ArgumentParser();a.add_argument('results');a.add_argument('baseline');a.add_argument('output');args=a.parse_args()
    new,old,out=map(Path,(args.results,args.baseline,args.output));out.mkdir(parents=True,exist_ok=False)
    nfits=read(new/'reports/fits.csv');ofits=read(old/'reports/fits.csv');assert len(nfits)==len(ofits)==15
    nf={(r['partition'],r['seed']):r for r in nfits};of={(r['partition'],r['seed']):r for r in ofits};assert nf.keys()==of.keys()
    scale=[float(r['time_scale']) for r in read(new/'reports/fixed-time-scales.csv')]
    assert len(scale)==7200 and min(scale)>=.1 and max(scale)<=10
    pooled=[];max_difference=0.;pairs=[];parameters=[]
    declared={int(r['seed']):r for r in read(new/'reports/pooled-by-seed.csv')}
    for seed in (1,2,3):
        combined=[]
        for fold in range(1,6):
            key=(f'cv{fold}',str(seed));r=nf[key];base=of[key];name=r['run_id']
            q=read(new/'reports'/f'{name}-predictions.csv');b=read(old/'reports'/f"{base['run_id']}-predictions.csv")
            assert [(x['source_row'],x['observed']) for x in q]==[(x['source_row'],x['observed']) for x in b]
            combined.extend(q);auc,ll=metrics(q)
            assert abs(auc-float(r['test_AUC']))<3e-6 and abs(ll-float(r['test_log_loss']))<1e-7
            clocks=read(new/'reports'/f'{name}-connection-clocks.csv');assert len(clocks)==len(scale)
            old_clocks=read(old/'reports'/f"{base['run_id']}-connection-clocks.csv")
            assert len(old_clocks)==21600
            indices=[int(c['full_connection_index'])-1 for c in clocks]
            assert len(set(indices))==7200 and indices==sorted(indices)
            matrix_rows={}
            for c in old_clocks:matrix_rows[c['matrix']]=max(matrix_rows.get(c['matrix'],0),int(c['destination_row']))
            old_candidate=[]
            for scale_value,c,index in zip(scale,clocks,indices):
                original=old_clocks[index]
                assert all(c[k]==original[k] for k in ('matrix','destination_row','source_column'))
                assert int(c['destination_row'])>2*matrix_rows[c['matrix']]//3
                assert math.isclose(scale_value,float(c['time_scale']),rel_tol=1e-13)
                assert math.isclose(scale_value,float(original['time_scale']),rel_tol=1e-13)
                assert 0<=float(c['lambda'])<=1
                assert math.isclose(float(c['recovery_half_life_minutes']),float(r['recovery_half_life_minutes'])/scale_value,rel_tol=1e-12)
                old_candidate.append(float(original['lambda']))
            record=dict(partition=key[0],seed=seed,
                all_connections_candidate_lambda_median=statistics.median(old_candidate),
                candidate_only_lambda_median=statistics.median(float(c['lambda']) for c in clocks))
            for field in ('output_eta','recovery_rate_per_minute','recovery_half_life_minutes','training_log_loss'):
                record['all_connections_'+field]=float(base[field]);record['candidate_only_'+field]=float(r[field])
            parameters.append(record)
            pairs.append(dict(partition=key[0],seed=seed,AUC_gain=auc-float(base['test_AUC']),
                log_loss_improvement=float(base['test_log_loss'])-ll,
                training_log_loss_improvement=float(base['training_log_loss'])-float(r['training_log_loss'])))
        assert len(combined)==55122 and len({r['source_row'] for r in combined})==55122
        auc,ll=metrics(combined);delta=max(abs(auc-float(declared[seed]['AUC'])),abs(ll-float(declared[seed]['log_loss'])))
        assert delta<1e-12;max_difference=max(max_difference,delta);pooled.append(dict(seed=seed,AUC=auc,log_loss=ll))
    for name,rows in [('paired',pairs),('pooled',pooled),('paired-parameters',parameters)]:
        with (out/f'{name}.csv').open('w',newline='') as stream:
            w=csv.DictWriter(stream,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
    result=dict(passed=True,matched_fold_sets=15,pooled_metric_pairs=3,static_clocks_checked=7200*15,maximum_pooled_difference=max_difference,
        heldout_AUC=statistics.mean(r['AUC'] for r in pooled),heldout_log_loss=statistics.mean(r['log_loss'] for r in pooled),
        AUC_wins=sum(r['AUC_gain']>0 for r in pairs),log_loss_wins=sum(r['log_loss_improvement']>0 for r in pairs))
    result['parameter_medians']={k:statistics.median(r[k] for r in parameters) for k in parameters[0] if k not in ('partition','seed')}
    (out/'verification.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))

if __name__=='__main__':main()
