"""Trace Writer nitrate singleton summaries; diagnostic outputs only."""
from pathlib import Path
import csv
import math
import statistics

base = Path(__file__).resolve().parents[1]
repo = base.parents[1]
raw = list(csv.DictReader((repo / 'inputs/Studies/meta_final/Writer_et_al_2014.csv').open()))
daily = [r for r in csv.DictReader((base / 'data/source/01_daily_time_series_paired.csv').open())
         if r['Study_ID'] == 'Writer et al. 2014']
observed = {}
for r in raw:
    if r['NO3'] not in ('', 'NA', '-9999'):
        observed[r['Site'], r['Sampling_Date'].strip()] = float(r['NO3']) * 0.225905
processed = {}
for r in daily:
    key = (r['Site'], r['Sampling_Date'])
    value = None if r['NO3_Interp_mg_N_L'] in ('', 'NA') else float(r['NO3_Interp_mg_N_L'])
    if key in processed:
        assert processed[key] == value  # Shared-reference copies must agree.
    processed[key] = value
restored = processed | observed  # Keep original observations; do not extend interpolation.
trace = []
for (site, date), value in sorted(observed.items()):
    trace.append(dict(site=site, date=date, observed_mg_N_L=value,
                      processed_mg_N_L=processed.get((site, date)),
                      observed_value_lost=processed.get((site, date)) is None))
summary = []
for label, values in [('current_daily', processed), ('observed_only', observed),
                      ('daily_preserving_observations_diagnostic', restored)]:
    for site in ('PNF', 'PSF'):
        for year in ('2012', '2013'):
            pairs = []
            for (s, date), burn in sorted(values.items()):
                ref = values.get(('PBR', date))
                if s == site and date.startswith(year) and burn is not None and ref is not None and burn > 0 and ref > 0:
                    pairs.append((date, math.log(burn / ref)))
            effects = [x[1] for x in pairs]
            summary.append(dict(scenario=label, site=site, year=year, n=len(effects),
                                lnRR_mean=statistics.mean(effects) if effects else None,
                                lnRR_var=statistics.variance(effects)/len(effects) if len(effects)>1 else None,
                                dates=';'.join(x[0] for x in pairs)))
for name, rows in [('writer_nitrate_observation_trace.csv', trace), ('writer_nitrate_variance_diagnostic.csv', summary)]:
    with (base / 'data/audit' / name).open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0])); writer.writeheader(); writer.writerows(rows)
for row in summary:
    print({k:v for k,v in row.items() if k != 'dates'})
# Reproduce every Writer nitrate summary in the retained model table.
model = [r for r in csv.DictReader((base / 'data/derived/lasso_model_table.csv').open())
         if r['Study_ID']=='Writer et al. 2014' and r['response_var']=='NO3']
for r in model:
    check = next(x for x in summary if x['scenario']=='current_daily' and x['site']==r['Site_Burn'] and x['year']==r['year'])
    assert check['n']==int(r['lnRR_n'])
    assert math.isclose(check['lnRR_mean'],float(r['lnRR_mean']),abs_tol=1e-12)
    if r['lnRR_var']:
        assert math.isclose(check['lnRR_var'],float(r['lnRR_var']),abs_tol=1e-12)
print('All current Writer nitrate annual summaries reproduced from the daily snapshot.')
