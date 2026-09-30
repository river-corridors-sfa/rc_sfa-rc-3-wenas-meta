"""Independently verify annual arithmetic-mean ratios and paired delta variances."""
import csv
import math
import statistics
from collections import defaultdict
from pathlib import Path

root = Path(__file__).resolve().parents[1]
groups = defaultdict(list)
with (root / 'data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv').open() as stream:
    for row in csv.DictReader(stream):
        if row['valid'] == 'TRUE':
            groups[(row['candidate_pair_id'], row['response_var'], row['year'])].append(
                (float(row['burn']), float(row['reference'])))
with (root / 'data/derived/lasso_model_table.csv').open() as stream:
    rows = list(csv.DictReader(stream))
assert len(rows) == len(groups) == 110
changed = 0
singletons = 0
for row in rows:
    values = groups[(row['candidate_pair_id'], row['response_var'], row['year'])]
    burn, ref = zip(*values)
    mb, mr = statistics.mean(burn), statistics.mean(ref)
    expected = math.log(mb / mr)
    assert math.isclose(float(row['lnRR_mean']), expected, abs_tol=1e-12)
    assert math.isclose(float(row['annual_mean_burn']), mb, rel_tol=1e-12)
    assert math.isclose(float(row['annual_mean_reference']), mr, rel_tol=1e-12)
    assert int(row['lnRR_n']) == len(values)
    assert row['effect_size_definition'] == 'log_ratio_annual_arithmetic_means'
    changed += not math.isclose(expected, statistics.mean(math.log(x/y) for x,y in values), abs_tol=1e-10)
    if len(values) == 1:
        assert row['lnRR_var'] == ''
        singletons += 1
    else:
        # Expand the delta formula independently, including covariance.
        covariance = sum((x-mb)*(y-mr) for x,y in values)/(len(values)-1)
        variance = (statistics.variance(burn)/mb**2 + statistics.variance(ref)/mr**2
                    - 2*covariance/(mb*mr))/len(values)
        assert math.isclose(float(row['lnRR_var']), variance, rel_tol=1e-9, abs_tol=1e-12)
assert singletons == 6
assert changed > 0
print(f'PASS: {len(rows)} annual ratios and variances; {singletons} singleton variances missing; {changed} responses differ from daily-log means.')
