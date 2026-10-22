"""Verify exclusion propagation through manuscript fits, sensitivities and figures."""
import csv
import math
from collections import Counter
from pathlib import Path

root = Path(__file__).resolve().parents[2]

def read(relative):
    with (root / relative).open() as stream:
        return list(csv.DictReader(stream))

def key(row):
    return row['candidate_pair_id'], row['response_var'], row['year']

excluded = {key(r) for r in read('config/paper_record_exclusions.csv')}
primary = read('data/derived/lasso_model_table.csv')
observed = read('data/derived/paper_observed_only_model_table.csv')
matched = read('data/derived/paper_interpolated_matched_model_table.csv')
assert len(primary) == 103 and len(observed) == len(matched) == 88
assert {key(r) for r in observed} == {key(r) for r in matched}
assert {key(r) for r in read('data/audit/paper_record_exclusions.csv')} == excluded
scenarios = [('primary', primary, 'output/paper/tables'),
             ('equal_row', primary, 'output/paper/sensitivity/equal_row'),
             ('precision_family', primary, 'output/paper/sensitivity/precision_family'),
             ('observed_only', observed, 'output/paper/sensitivity/observed_only'),
             ('interpolated_matched', matched, 'output/paper/sensitivity/interpolated_matched')]
for scenario, rows, folder in scenarios:
    expected = {key(r) for r in rows}
    assert not expected & excluded, scenario
    fitting = read(folder + '/fitting_weights.csv')
    assert len(fitting) == len(expected) and {key(r) for r in fitting} == expected, scenario
    # Row IDs are assigned when each scenario's model table is loaded.
    predictions = read(folder + '/heldout_predictions.csv')
    assert len(predictions) == 4 * len(rows), scenario
    seen = Counter()
    for prediction in predictions:
        original = rows[int(prediction['row_id']) - 1]
        assert prediction['response_var'] == original['response_var']
        assert prediction['held_out_study'] == original['Study_ID']
        assert math.isclose(float(prediction['observed']), float(original['lnRR_mean']), abs_tol=1e-12)
        assert math.isfinite(float(prediction['predicted']))
        seen[prediction['row_id'], prediction['model']] += 1
    assert len(seen) == len(predictions) and set(seen.values()) == {1}, scenario
    summaries = read(folder + '/bootstrap_summary.csv')
    assert len(summaries) == 2
    for r in summaries:
        assert int(r['attempted']) == 1000
        assert int(r['successful']) + int(r['failed']) == 1000
    print(f'PASS: {scenario}: {len(rows)} eligible records, complete unique predictions, 1,000 bootstrap attempts/analyte.')

counts = {'DOC': 35, 'NO3': 68}
for r in read('output/paper/balanced_synthesis/balanced_CR2_results.csv'):
    expected = counts if r['scenario'] == 'primary' else {'DOC': 32, 'NO3': 56}
    assert int(r['rows']) == expected[r['response_var']]
for r in read('output/paper/tables/working_covariance_inference.csv'):
    assert int(r['rows']) == counts[r['response_var']]
assert not read('output/paper/tables/working_covariance_failures.csv')
for r in read('output/paper/balanced_synthesis/balanced_weights.csv'):
    assert key(r) not in excluded
figure_rows = read('output/paper/tables/figure_2_response_points_weights.csv')
assert len(figure_rows) == 103 and {key(r) for r in figure_rows} == {key(r) for r in primary}
for r in read('output/paper/tables/figure_2_response_summary.csv'):
    assert int(r['n']) == counts[r['response_var']]
    assert math.isclose(float(r['balanced_mean']), float(r['estimate']), abs_tol=1e-12)
print('PASS: balanced CR2, conditional REML and Figure 2 all use the excluded-record-free inventories.')
