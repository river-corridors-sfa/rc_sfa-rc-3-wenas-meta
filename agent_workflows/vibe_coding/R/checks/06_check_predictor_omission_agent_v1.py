"""Audit paired omission fits, bootstrap groups, metrics and primary reproduction."""
import csv
import gzip
import math
from collections import defaultdict
from pathlib import Path

root = Path(__file__).resolve().parents[2]
output = root / 'output/paper/predictor_omission'

def read(path):
    opener = gzip.open if path.suffix == '.gz' else open
    with opener(path, 'rt') as stream:
        return list(csv.DictReader(stream))

settings = read(output / 'run_settings.csv')[0]
assert int(settings['attempts']) == 1000, 'Manuscript check requires 1,000 attempts.'
specs = read(output / 'specifications.csv')
assert len(specs) == 24
all_specs = {r['specification'] for r in specs}
assert len(all_specs) == 12
spec_candidates = {(r['response_var'], r['specification']): set(r['candidates'].split(';')) for r in specs}
for analyte in ('DOC', 'NO3'):
    full = spec_candidates[analyte, 'full']
    individual_omissions = [full - spec_candidates[analyte, spec] for spec in all_specs
        if len(full - spec_candidates[analyte, spec]) == 1]
    assert len(individual_omissions) == 8 and set.union(*individual_omissions) == full
    wildfire = {'burn_percent_fire_year', 'burn_sev_high'}
    assert spec_candidates[analyte, 'omit_all_watershed'] == wildfire | {'post_fire_year'}
    assert spec_candidates[analyte, 'omit_both_burn'] == full - wildfire
members = defaultdict(lambda: {'training': [], 'held_out': []})
for r in read(output / 'split_membership.csv'):
    members[r['response_var'], r['validation'], r['iteration']][r['role']].append(r['Study_ID'])
for split in members.values():
    assert not set(split['training']) & set(split['held_out'])
folds = defaultdict(dict)
for r in read(output / 'inner_fold_assignments.csv'):
    group = (r['response_var'], r['validation'], r['iteration'])
    assert r['Study_ID'] not in folds[group], 'A study has multiple inner folds.'
    folds[group][r['Study_ID']] = r['inner_fold']
for group, assignments in folds.items():
    assert set(assignments) == set(members[group]['training'])
    assert len(set(assignments.values())) == min(5, len(assignments))

primary = read(root / 'data/derived/lasso_model_table.csv')
row_keys = {str(i): (r['candidate_pair_id'], r['response_var'], r['year']) for i, r in enumerate(primary, 1)}
excluded = {(r['candidate_pair_id'], r['response_var'], r['year'])
            for r in read(root / 'config/paper_record_exclusions.csv')}
predictions = read(output / 'heldout_predictions.csv.gz')
rows_by_spec = defaultdict(set)
errors = defaultdict(list)
primary_predictions = {(r['response_var'], r['row_id']): float(r['predicted'])
    for r in read(root / 'output/paper/tables/heldout_predictions.csv') if r['model'] == 'lasso'}
for r in predictions:
    group = (r['response_var'], r['validation'], r['iteration'])
    assert r['Study_ID'] in members[group]['held_out']
    assert row_keys[r['row_id']] == (r['candidate_pair_id'], r['response_var'], r['year'])
    assert row_keys[r['row_id']] not in excluded
    key = group + (r['specification'],)
    assert r['row_id'] not in rows_by_spec[key]
    rows_by_spec[key].add(r['row_id'])
    difference = float(r['observed']) - float(r['predicted'])
    assert math.isfinite(difference)
    errors[key + (r['Study_ID'],)].append(difference * difference)
    if r['validation'] == 'leave_one_study_out' and r['specification'] == 'full':
        assert math.isclose(float(r['predicted']), primary_predictions[r['response_var'], r['row_id']], abs_tol=1e-10)
for group in members:
    available = [rows_by_spec[group + (spec,)] for spec in all_specs if group + (spec,) in rows_by_spec]
    if available:
        assert len(available) == 12 and all(s == available[0] for s in available)

study_mse = {key: sum(values) / len(values) for key, values in errors.items()}
oob_mse = defaultdict(list)
for key, mse in study_mse.items():
    if key[1] == 'bootstrap_oob':
        oob_mse[key[:4]].append(mse)
for r in read(output / 'paired_oob_differences.csv'):
    key = (r['response_var'], 'bootstrap_oob', r['iteration'])
    omitted = math.sqrt(sum(oob_mse[key + (r['specification'],)]) / len(oob_mse[key + (r['specification'],)]))
    full = math.sqrt(sum(oob_mse[key + ('full',)]) / len(oob_mse[key + ('full',)]))
    assert math.isclose(float(r['delta_RMSE']), omitted - full, abs_tol=1e-10)

status = read(output / 'bootstrap_status.csv')
successful = {(r['response_var'], r['iteration']) for r in status if r['successful'] == 'TRUE'}
for r in read(output / 'coefficients.csv.gz'):
    available = r['predictor'] in spec_candidates[r['response_var'], r['specification']]
    assert (r['available'] == 'TRUE') == available
    if not available:
        assert r['coefficient'] in ('NA', '')
reference_stability = {(r['response_var'], r['predictor']): r
    for r in read(root / 'output/paper/tables/selection_stability.csv')}
for r in read(output / 'selection_stability.csv'):
    expected = sum(a == r['response_var'] for a, _ in successful)
    assert int(r['successful_refits']) == expected
    if r['specification'] == 'full':
        ref = reference_stability[r['response_var'], r['predictor']]
        assert math.isclose(float(r['selection_frequency']), float(ref['selection_frequency']), abs_tol=1e-12)
        for field in ('median_coefficient', 'positive_frequency', 'negative_frequency'):
            assert math.isclose(float(r[field]), float(ref[field]), abs_tol=1e-10)
for r in read(output / 'bootstrap_summary.csv'):
    assert int(r['attempted']) == int(r['successful']) + int(r['failed']) == 1000
    assert int(r['paired_oob_refits']) + int(r['successful_without_oob']) == int(r['successful'])
loso = {(r['response_var'], r['specification']): float(r['RMSE'])
    for r in read(output / 'performance_loso.csv')}
for r in read(output / 'group_contribution_summary.csv'):
    a = r['response_var']
    full = loso[a, 'full']
    watershed_change = loso[a, 'omit_all_watershed'] - full
    wildfire_change = loso[a, 'omit_both_burn'] - full
    assert math.isclose(float(r['watershed_minus_wildfire']), watershed_change - wildfire_change, abs_tol=1e-10)
    assert math.isclose(float(r['watershed_relative_RMSE_change']), watershed_change / full, abs_tol=1e-10)
    assert math.isclose(float(r['wildfire_relative_RMSE_change']), wildfire_change / full, abs_tol=1e-10)
    assert math.isclose(sum(float(r[field]) for field in ('proportion_watershed_larger',
        'proportion_groups_tied', 'proportion_wildfire_larger')), 1, abs_tol=1e-10)
print('PASS: twelve paired specifications; disjoint publication groups; shared held-out records; '
      'independently recomputed error differences; explicit omitted cells; primary predictions and stability reproduced.')
