"""Independently check paired publication errors and all component-bootstrap draws."""
import csv
import gzip
import hashlib
import math
from collections import defaultdict
from pathlib import Path

root = Path(__file__).resolve().parents[2]
output = root / 'output/paper/predictor_group_cv/paired_uncertainty'

def read(path):
    with path.open() as stream:
        return list(csv.DictReader(stream))

def quantile(values, probability):
    ordered = sorted(values)
    index = (len(ordered) - 1) * probability
    lower = int(index)
    upper = min(lower + 1, len(ordered) - 1)
    return ordered[lower] + (ordered[upper] - ordered[lower]) * (index - lower)

settings = read(output / 'run_settings.csv')[0]
draws = int(settings['draws'])
assert draws == 100000
assert settings['model_refitting'] == 'FALSE'
tol = float(settings['tie_tolerance'])
models = ['intercept_only', 'time_only', 'time_wildfire', 'time_watershed', 'time_full']
predictions = read(root / 'output/paper/predictor_group_cv/heldout_predictions.csv')
errors = defaultdict(list)
for row in predictions:
    assert row['Study_ID'] == row['held_out_study']
    errors[row['response_var'], row['model'], row['Study_ID']].append(
        (float(row['observed']) - float(row['predicted'])) ** 2)
mse = {key: sum(values) / len(values) for key, values in errors.items()}
rmse = {key: math.sqrt(value) for key, value in mse.items()}
comparisons = read(output / 'comparisons.csv')
assert len(comparisons) == 10
paired = read(output / 'paired_publication_RMSE_differences.csv')
assert len(paired) == 190
counts = defaultdict(lambda: defaultdict(int))
for row in paired:
    a, study = row['response_var'], row['Study_ID']
    ref, candidate = row['reference_model'], row['candidate_model']
    improvement = rmse[a, ref, study] - rmse[a, candidate, study]
    assert math.isclose(float(row['RMSE_improvement']), improvement, abs_tol=1e-12)
    assert math.isclose(float(row['RMSE_difference']), -improvement, abs_tol=1e-12)
    outcome = 'improved' if improvement > tol else 'worsened' if improvement < -tol else 'tied'
    assert row['outcome'] == outcome
    counts[a, ref, candidate][outcome] += 1
for row in read(output / 'publication_improvement_counts.csv'):
    key = row['response_var'], row['reference_model'], row['candidate_model']
    assert all(int(row[outcome]) == counts[key][outcome] for outcome in ['improved', 'tied', 'worsened'])
    assert sum(counts[key].values()) == int(row['publications'])

# Rebuild connected publication/fire components independently from the model table.
input_rows = read(root / 'data/derived/lasso_model_table.csv')
parent = {row['Study_ID']: row['Study_ID'] for row in input_rows}
def find(study):
    while parent[study] != study:
        study = parent[study]
    return study
fires = defaultdict(set)
for row in input_rows:
    fires[row['Fire_ID_Analysis']].add(row['Study_ID'])
for studies in fires.values():
    studies = list(studies)
    for study in studies[1:]:
        parent[find(study)] = find(studies[0])
membership = read(output / 'component_membership.csv')
summary = read(output / 'overall_RMSE_improvement_uncertainty.csv')
for a, expected_components in [('DOC', 6), ('NO3', 11)]:
    members = [row for row in membership if row['response_var'] == a]
    by_component = defaultdict(list)
    for row in members:
        by_component[row['draw_column']].append(row['Study_ID'])
    assert len(by_component) == expected_components
    for studies in by_component.values():
        assert len({find(study) for study in studies}) == 1
    assert len({find(studies[0]) for studies in by_component.values()}) == expected_components
    linked = next(studies for studies in by_component.values() if 'Rhea et al. 2021' in studies)
    assert set(linked) == {'Rhea et al. 2021', 'Writer et al. 2014'}
    overall = {model: math.sqrt(sum(mse[a, model, row['Study_ID']] for row in members) / len(members))
               for model in models}
    sums = {(component, model): sum(mse[a, model, study] for study in studies)
            for component, studies in by_component.items() for model in models}
    delta_draws = defaultdict(list)
    percent_draws = defaultdict(list)
    count = 0
    with gzip.open(output / f'bootstrap_errors_{a}.csv.gz', 'rt') as stream:
        for row in csv.DictReader(stream):
            count += 1
            multiplicity = {component: int(row[component]) for component in by_component}
            assert sum(multiplicity.values()) == expected_components
            publications = sum(multiplicity[component] * len(studies) for component, studies in by_component.items())
            assert publications == int(row['publications'])
            for model in models:
                expected = math.sqrt(sum(multiplicity[component] * sums[component, model]
                                         for component in by_component) / publications)
                assert math.isclose(float(row[f'RMSE_{model}']), expected, abs_tol=1e-12)
            for comparison in comparisons:
                ref, candidate = comparison['reference_model'], comparison['candidate_model']
                delta = float(row[f'RMSE_{ref}']) - float(row[f'RMSE_{candidate}'])
                if abs(delta) <= tol:
                    delta = 0
                delta_draws[ref, candidate].append(delta)
                percent_draws[ref, candidate].append(100 * delta / float(row[f'RMSE_{ref}']))
    assert count == draws
    for row in [r for r in summary if r['response_var'] == a]:
        key = row['reference_model'], row['candidate_model']
        point = overall[key[0]] - overall[key[1]]
        if abs(point) <= tol:
            point = 0
        assert math.isclose(float(row['RMSE_improvement']), point, abs_tol=1e-12)
        assert math.isclose(float(row['improvement_percent']), 100 * point / overall[key[0]], abs_tol=1e-10)
        for prefix, probability in [('lower', .025), ('upper', .975)]:
            assert math.isclose(float(row[f'{prefix}_RMSE_improvement']), quantile(delta_draws[key], probability), abs_tol=1e-10)
            assert math.isclose(float(row[f'{prefix}_improvement_percent']), quantile(percent_draws[key], probability), abs_tol=1e-8)
for row in read(output / 'input_manifest.csv'):
    assert hashlib.md5((root / row['path']).read_bytes()).hexdigest() == row['md5']
print('PASS: all 190 publication comparisons, improvement counts, independently rebuilt fire components, '
      '200,000 paired bootstrap draws, equal-publication RMSE targets, percentile intervals and provenance.')
