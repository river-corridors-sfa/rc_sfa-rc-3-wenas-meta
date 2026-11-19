"""Independently audit Figure 3 groups, splits, baseline, errors and provenance."""
import csv
import hashlib
import math
from collections import defaultdict
from pathlib import Path

root = Path(__file__).resolve().parents[2]
output = root / 'output/paper/predictor_group_cv'

def read(path):
    with path.open() as stream:
        return list(csv.DictReader(stream))

primary = read(root / 'data/derived/lasso_model_table.csv')
models = {'time_only', 'intercept_only', 'time_watershed', 'time_wildfire', 'time_full'}
expected = {(str(i), r['response_var']): r for i, r in enumerate(primary, 1)}
excluded = {(r['candidate_pair_id'], r['response_var'], r['year'])
            for r in read(root / 'config/paper_record_exclusions.csv')}
reference = {(r['row_id'], r['response_var'], r['model']): float(r['predicted'])
             for r in read(root / 'output/paper/tables/heldout_predictions.csv')
             if r['model'] in {'time_only', 'intercept_only'}}
groups = defaultdict(set)
for r in read(output / 'predictor_groups.csv'):
    groups[r['response_var'], r['model']].add(r['predictor'])
    assert int(r['penalty_factor']) == (0 if r['predictor'] == 'post_fire_year' else 1)
for analyte, soil in [('DOC', 'omws'), ('NO3', 'clayws')]:
    assert groups[analyte, 'time_only'] == {'post_fire_year'}
    assert groups[analyte, 'time_wildfire'] == {'post_fire_year', 'burn_percent_fire_year', 'burn_sev_high'}
    assert groups[analyte, 'time_watershed'] == {'post_fire_year', 'Area_watershed_km', 'runoffws', 'bfiws', 'forest_cover', soil}
    assert not groups[analyte, 'intercept_only']
    assert groups[analyte, 'time_full'] == groups[analyte, 'time_watershed'] | groups[analyte, 'time_wildfire']
predictions = read(output / 'heldout_predictions.csv')
assert len(predictions) == 5 * len(primary)
seen = set()
errors = defaultdict(list)
by_row = defaultdict(dict)
for r in predictions:
    original = expected[r['row_id'], r['response_var']]
    assert r['Study_ID'] == r['held_out_study'] == original['Study_ID']
    assert (r['candidate_pair_id'], r['response_var'], r['year']) not in excluded
    key = (r['row_id'], r['response_var'], r['model'])
    assert key not in seen
    seen.add(key)
    assert math.isclose(float(r['observed']), float(original['lnRR_mean']), abs_tol=1e-12)
    prediction = float(r['predicted'])
    assert math.isfinite(prediction)
    if r['model'] in {'time_only', 'intercept_only'}:
        assert math.isclose(prediction, reference[r['row_id'], r['response_var'], r['model']], abs_tol=1e-10)
    by_row[r['response_var'], r['row_id']][r['model']] = prediction
    errors[r['response_var'], r['model'], r['held_out_study']].append((float(r['observed']) - prediction) ** 2)
assert all(set(values) == models for values in by_row.values())
folds = defaultdict(dict)
for r in read(output / 'inner_fold_assignments.csv'):
    assert r['Study_ID'] != r['held_out_study']
    key = r['response_var'], r['held_out_study']
    assert r['Study_ID'] not in folds[key]
    folds[key][r['Study_ID']] = r['inner_fold']
for (a, held_out), assignment in folds.items():
    assert set(assignment) == {r['Study_ID'] for r in primary if r['response_var'] == a} - {held_out}
    assert len(set(assignment.values())) == min(5, len(assignment))
group_mse = defaultdict(list)
for (a, model, _), values in errors.items():
    group_mse[a, model].append(sum(values) / len(values))
rmse = {key: math.sqrt(sum(values) / len(values)) for key, values in group_mse.items()}
for r in read(output / 'performance.csv'):
    assert math.isclose(float(r['RMSE']), rmse[r['response_var'], r['model']], abs_tol=1e-12)
for r in read(output / 'group_improvement.csv'):
    a, model = r['response_var'], r['model']
    difference = rmse[a, 'intercept_only'] - rmse[a, model]
    assert math.isclose(float(r['RMSE_improvement']), difference, abs_tol=1e-12)
    assert math.isclose(float(r['relative_RMSE_improvement']), difference / rmse[a, 'intercept_only'], abs_tol=1e-12)
# Wherever every group coefficient is zero, predictions must match the common
# unpenalized time-only model, rather than shrink the time effect as well.
nonzero_groups = defaultdict(bool)
for r in read(output / 'outer_coefficients.csv'):
    assert r['predictor'] in groups[r['response_var'], r['model']] | {'(Intercept)'}
    if r['predictor'] not in {'(Intercept)', 'post_fire_year'} and abs(float(r['coefficient'])) > 1e-10:
        nonzero_groups[r['response_var'], r['model'], r['held_out_study']] = True
for r in predictions:
    if r['model'] != 'intercept_only' and not nonzero_groups[r['response_var'], r['model'], r['held_out_study']]:
        assert math.isclose(float(r['predicted']), by_row[r['response_var'], r['row_id']]['time_only'], abs_tol=1e-8)
for r in read(output / 'input_manifest.csv'):
    assert hashlib.md5((root / r['path']).read_bytes()).hexdigest() == r['md5']
assert (root / 'output/paper/figures/figure_4_lasso_results/figure_4_lasso_results.png').exists()
assert not (root / 'output/paper/figures/figure_3_lasso_results').exists()
print('PASS: 515 matched held-out predictions; common time-only and intercept-only benchmarks; intended predictor groups; '
      'unpenalized time behavior; disjoint study folds; independent RMSE/improvement calculations; provenance and renumbering.')
