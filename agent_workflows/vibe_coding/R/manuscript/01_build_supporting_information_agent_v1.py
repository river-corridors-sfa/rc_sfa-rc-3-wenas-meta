"""Build the statistical SI for Results 3.1–3.3 from saved, verified outputs.

No models are fitted. Publication tables retain full precision in companion CSVs.
"""
import csv
import gzip
import hashlib
import math
import re
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PAPER = ROOT / 'output/paper'
OUT = PAPER / 'supporting_information'
(OUT / 'tables').mkdir(parents=True, exist_ok=True)
(OUT / 'data').mkdir(exist_ok=True)
sources = set()
table_index = []

def read(relative):
    path = ROOT / relative
    sources.add(relative)
    with path.open() as stream:
        return list(csv.DictReader(stream))

def text_source(relative):
    sources.add(relative)
    return (ROOT / relative).read_text()

def write_rows(path, rows):
    assert rows, path
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

def number(value, digits=3):
    if value is None or str(value) in ('', 'NA', 'nan'):
        return '—'
    formatted = f'{float(value):.{digits}f}'
    if float(formatted) == 0:
        formatted = formatted.lstrip('-')
    return formatted.replace('-', '−')

def interval(estimate, lower, upper, digits=3):
    return f'{number(estimate, digits)} [{number(lower, digits)}, {number(upper, digits)}]'

def percent(value, digits=1):
    return number(100 * float(value), digits)

def quantile(values, p):
    ordered = sorted(values)
    index = (len(ordered) - 1) * p
    lo = int(index)
    hi = min(lo + 1, len(ordered) - 1)
    return ordered[lo] + (ordered[hi] - ordered[lo]) * (index - lo)

def table(identifier, title, machine_rows, headers, display_rows, note='', embedded=True):
    filename = f'table_{identifier.lower()}.csv'
    write_rows(OUT / 'tables' / filename, machine_rows)
    table_index.append(dict(table=identifier, title=title, file=f'tables/{filename}', rows=len(machine_rows)))
    body = f'### Table {identifier} {title}\n\n'
    if embedded:
        body += '| ' + ' | '.join(headers) + ' |\n'
        body += '| ' + ' | '.join('---' for _ in headers) + ' |\n'
        for row in display_rows:
            assert len(row) == len(headers), identifier
            body += '| ' + ' | '.join(str(v).replace('|', '\\|').replace('\n', ' ') for v in row) + ' |\n'
        body += '\n'
    if note:
        body += note + '\n\n'
    return body + f'[Full-precision Table {identifier}](tables/{filename}).\n\n'

ANALYTES = ('DOC', 'NO3')
NAMES = {'DOC': 'DOC', 'NO3': 'Nitrate'}
MODEL_ORDER = ['intercept_only', 'time_only', 'time_wildfire', 'time_watershed', 'time_full']
MODEL = {'intercept_only': 'Intercept only', 'time_only': 'Time', 'time_wildfire': 'Time + wildfire',
         'time_watershed': 'Time + watershed', 'time_full': 'Time + watershed + wildfire',
         'lasso': 'Full set with time penalized', 'time_plus_fire': 'Time + one fire metric benchmark'}
SCENARIO = {'primary': 'Primary interpolation inventory', 'observed_only': 'Observed only, matched inventory',
            'matched_interpolated': 'Measured + interpolated, matched inventory'}
CLUSTER = {'study': 'Publication', 'study_fire_component': 'Connected publication–fire group'}
PREDICTOR = {'post_fire_year': 'Time since fire', 'burn_percent_fire_year': 'Watershed burned percentage',
             'burn_sev_high': 'High-severity burn percentage', 'Area_watershed_km': 'Watershed area',
             'runoffws': 'Mean annual runoff', 'bfiws': 'Baseflow index', 'forest_cover': 'Forest cover',
             'omws': 'Soil organic matter', 'clayws': 'Soil clay content'}

model_table = read('data/derived/lasso_model_table.csv')
observed_table = read('data/derived/paper_observed_only_model_table.csv')
inventory = read('output/paper/tables/paper_study_inventory.csv')
balanced = read('output/paper/balanced_synthesis/balanced_CR2_results.csv')
pooled = [r for r in balanced if r['specification'] == 'pooled']
deletions = [r for r in read('output/paper/balanced_synthesis/leave_component_out.csv')
             if r['scenario'] == 'primary' and r['specification'] == 'pooled']
weights = read('output/paper/balanced_synthesis/balanced_weights.csv')
components = read('output/paper/balanced_synthesis/study_fire_clusters.csv')
component_by_study = {r['Study_ID']: r['dependence_cluster'] for r in components}
assert len(model_table) == 103 and len(observed_table) == 88 and len(inventory) == 12
assert sum(int(r['DOC_responses']) for r in inventory) == 35
assert sum(int(r['NO3_responses']) for r in inventory) == 68
primary_pooled = {r['response_var']: r for r in pooled if r['status'] == 'primary_synthesis'}

document = '''# Supporting information for statistical analyses

This supporting information accompanies Results Sections 3.1–3.3. Section S1 reports pooled wildfire responses and their dependence, interpolation, and influence sensitivities. Section S2 reports predictor definitions, cross-validation performance, publication-specific errors, and conditional uncertainty in predictive improvement. Section S3 reports predictor-selection stability, coefficient distributions, alternative weighting, and structured predictor omissions.

## S1 Pooled wildfire responses

### Analysis inventory and annual effect sizes

The analysis includes 12 publications, 14 unique wildfires, 28 approved burned–reference watershed pairs, and 46 study-specific physical watersheds. The retained dataset contains 35 DOC and 68 nitrate annual responses. Shared reference watersheds are counted once within each publication and role; pair counts therefore exceed reference-watershed counts. Six annual records with only one eligible matched date were excluded. One additional nitrate record from Crandall et al. (2021), Site_2–Control_2 in 2018, was excluded after inspection because of its extreme response (lnRR −4.507). This was a post hoc response-based decision, not a confirmed measurement error; its DOC counterpart was retained. Table S1 describes the retained publication inventory.

Annual lnRR is the natural logarithm of the ratio of burned and reference arithmetic-mean concentrations calculated over the same dates within a calendar year. Dates must have finite, strictly positive concentrations in both watersheds. Measured concentrations are preserved. Daily linear interpolation is restricted to observed endpoints separated by no more than 40 elapsed days, without extrapolation across longer gaps. Calendar-year means summarize available matched dates rather than complete annual concentration or discharge-weighted records.

'''
s1_rows = [dict(publication=r['Study_ID'], DOC_records=int(r['DOC_responses']), nitrate_records=int(r['NO3_responses']),
               pairs=int(r['pairs']), study_fire_records=int(r['fires']), burned_watersheds=int(r['burned_watersheds']),
               reference_watersheds=int(r['reference_watersheds'])) for r in inventory]
document += table('S1', 'Retained publication inventory', s1_rows,
                  ['Publication', 'DOC records', 'Nitrate records', 'Pairs', 'Fires within publication', 'Burned watersheds', 'Reference watersheds'],
                  [[r[k] for k in r] for r in s1_rows],
                  'There are 15 publication–fire records but 14 unique wildfires because Rhea and Writer both include High Park. Watershed totals are study-specific physical-site counts.')
document += '''### Balance weights and dependence groups

For an analyte with S publications, Pₛ eligible pairs in publication s, and Yₛₚ retained annual responses in pair p, the normalized weight of each annual response is wₛₚᵧ = 1/(S × Pₛ × Yₛₚ). Equivalently, annual responses are averaged within pairs, pair means within publications, and publication means across publications. The weighted intercept-only linear model yields this pooled lnRR. Weights are recalculated for each response inventory and are not inverse sampling-variance weights. Exponentiating the pooled lnRR gives a geometric summary of burned-to-reference response ratios, rather than a ratio of grand mean concentrations.

Uncertainty uses CR2 cluster-robust covariance estimates and Satterthwaite degrees of freedom. The working target for the correction is the identity matrix, with fitting weights explicitly treated as nonprecision weights. Within-group covariance is not estimated as a complete sampling covariance matrix. Publications linked directly or indirectly through any included wildfire form connected dependence groups; all observations from a publication remain together, including publications covering multiple fires. Rhea and Writer connect through High Park, producing six DOC groups from seven publications and eleven nitrate groups from twelve publications. Different groups are assumed independent. These links are working dependence assignments and do not imply that different publications measured the same physical watersheds. Small-sample adjustment does not eliminate the limitations of having few groups (Hedges et al., 2010; Tipton, 2015; Pustejovsky and Tipton, 2022).

### Sensitivity to the clustering definition

We recalculated CR2 intervals using publication identifiers as clusters. The analysis rows, weights, and pooled estimates were identical to those in the connected-group analysis. This comparison changes estimated uncertainty, not the point estimate. Nitrate intervals remained above zero under both definitions; DOC intervals included zero under both definitions (Table S2).

'''
s2_rows = [r for r in pooled if r['scenario'] == 'primary']
document += table('S2', 'Pooled responses under alternative clustering definitions', s2_rows,
                  ['Analyte', 'Clustering', 'Records', 'Publications', 'Groups', 'Pooled lnRR [95% CI]', 'Satterthwaite df'],
                  [[NAMES[r['response_var']], CLUSTER[r['clustering']], r['rows'], r['studies'], r['clusters'],
                    interval(r['estimate'], r['lower'], r['upper']), number(r['df'], 2)] for r in s2_rows],
                  'Connected publication–fire groups define primary uncertainty. Weights and estimates are identical between clustering definitions.')
document += '''### Sensitivity to interpolation on matched inventories

Observed-only responses were recomputed using dates when both watershed concentrations were measured and positive, with at least two such matched dates per pair–analyte–year. This retains 32 DOC and 56 nitrate responses, compared with 35 and 68 in the primary analysis. The 15 omitted annual records remain in the primary analysis. The interpolation-based comparison was restricted to exactly the same pair–analyte–year keys as observed-only, with balance weights recomputed in each inventory. Both use connected-group CR2 intervals. The direct observed-only versus matched-interpolated comparison holds annual-record membership fixed; a comparison with the primary inventory also reflects removal of sparsely observed records.

'''
matched_pooled = {(r['response_var'], r['scenario']): r for r in pooled if r['clustering'] == 'study_fire_component'}
doc_delta = float(matched_pooled['DOC', 'observed_only']['estimate']) - float(matched_pooled['DOC', 'matched_interpolated']['estimate'])
no_delta = float(matched_pooled['NO3', 'observed_only']['estimate']) - float(matched_pooled['NO3', 'matched_interpolated']['estimate'])
document += f'On the matched inventory, observed-only minus interpolation-based pooled lnRR was {number(doc_delta)} for DOC and {number(no_delta)} for nitrate. DOC intervals included zero and nitrate intervals remained above zero for both response constructions (Table S3). This comparison assesses sensitivity to interpolation and sampling support; it does not establish which construction most accurately represents the true annual mean.\n\n'
s3_rows = [matched_pooled[a, scenario] for a in ANALYTES for scenario in SCENARIO]
document += table('S3', 'Interpolation and annual record inventory comparisons', s3_rows,
                  ['Analyte', 'Response inventory', 'Records', 'Publications', 'Groups', 'Pooled lnRR [95% CI]'],
                  [[NAMES[r['response_var']], SCENARIO[r['scenario']], r['rows'], r['studies'], r['clusters'],
                    interval(r['estimate'], r['lower'], r['upper'])] for r in s3_rows])
document += '''### Influence of individual dependence groups

Each complete publication–fire dependence group was omitted in turn. Remaining publications retain equal total weight, with equal pairs and available years within publications. Removing a complete group preserves the relative balance weights among all remaining records; normalizing them again changes neither the fitted intercept nor the pooled estimate. We recorded the deletion estimate and its difference from the primary estimate. These diagnostics evaluate influence on the point estimate; deletion confidence intervals were not calculated.

'''
s4_rows = []
for r in deletions:
    removed = [d for d in model_table if d['response_var'] == r['response_var']
               and component_by_study[d['Study_ID']] == r['omitted_cluster']]
    s4_rows.append({**r, 'publications_removed': len({d['Study_ID'] for d in removed}), 'records_removed': len(removed)})
for a in ANALYTES:
    rr = [r for r in s4_rows if r['response_var'] == a]
    values = [float(r['deleted_estimate']) for r in rr]
    assert min(values) > 0
    largest = max(rr, key=lambda r: abs(float(r['change'])))
    document += (f'{NAMES[a]} deletion estimates ranged from {number(min(values))} to {number(max(values))} lnRR. '
                 f'The largest absolute change followed removal of {largest["omitted_cluster"]} '
                 f'({number(largest["change"])} lnRR). ')
document += 'All deletion point estimates remained positive (Table S4). This stability of direction does not demonstrate that deletion confidence intervals would exclude zero.\n\n'
document += table('S4', 'Deletion of each complete dependence group', s4_rows,
                  ['Analyte', 'Omitted group', 'Publications removed', 'Records removed', 'Deletion lnRR', 'Change from primary'],
                  [[NAMES[r['response_var']], r['omitted_cluster'], r['publications_removed'], r['records_removed'],
                    number(r['deleted_estimate']), number(r['change'])] for r in s4_rows],
                  'Change = deletion estimate minus primary estimate. No deletion intervals are implied.')

document += '''## S2 Predictive model specification and validation

### Candidate predictors and descriptive diagnostics

Each analyte uses eight specified predictors. Seven are shared: time since fire, watershed burned percentage, high-severity burn percentage, watershed area, mean annual runoff, baseflow index, and forest cover. The eighth is soil organic matter for DOC and soil clay content for nitrate (Table S5). Watershed area, runoff, baseflow index, forest cover, and the soil variable form the watershed group. The two burn metrics form the wildfire group. Time is separate. Both burn metrics and both hydrology metrics are retained; correlations were examined descriptively rather than used as an automatic exclusion rule.

The implemented time variable uses the reported time-since-fire field where available. If unavailable, it uses follow-up year, defined as calendar year minus the earliest calendar year for that pair plus one. This fallback is an index of follow-up and need not equal elapsed years since wildfire. Correlations use pairwise-complete Spearman coefficients across the 103 annual analyte records and repeat static attributes across years and analytes (SI Figure 1). Raw predictor distributions use one value per pair for static characteristics and pair–year values for time (SI Figure 2). These descriptive displays do not represent independent observations or the hierarchical fitting weights.

'''
config = read('config/issue6_revised_primary_predictors.csv')
dictionary = {r['predictor']: r for r in read('config/predictor_dictionary.csv')}
config_sets = {a: [r['predictor'] for r in sorted(config, key=lambda r: int(r['position'])) if r['response_var'] == a] for a in ANALYTES}
s5_rows = []
for predictor in dict.fromkeys(r['predictor'] for r in config):
    row = dict(predictor=predictor, label=PREDICTOR[predictor],
               group='time' if predictor == 'post_fire_year' else 'wildfire' if predictor in {'burn_percent_fire_year', 'burn_sev_high'} else 'watershed',
               transformation=dictionary[predictor]['transformation'])
    for a in ANALYTES:
        values = [r[predictor] for r in model_table if r['response_var'] == a]
        row[f'{a}_included'] = predictor in config_sets[a]
        row[f'{a}_missing'] = sum(v in ('', 'NA') or not math.isfinite(float(v)) for v in values)
        row[f'{a}_records'] = len(values)
    s5_rows.append(row)
document += table('S5', 'Predictor groups transformations and missingness', s5_rows,
                  ['Predictor', 'Group', 'Transformation', 'DOC included', 'Nitrate included', 'DOC missing / records', 'Nitrate missing / records'],
                  [[r['label'], r['group'], 'log(1 + max[x, 0])' if r['transformation'] == 'log1p' else 'None',
                    'Yes' if r['DOC_included'] else 'No', 'Yes' if r['NO3_included'] else 'No',
                    f"{r['DOC_missing']}/{r['DOC_records']}" if r['DOC_included'] else 'Not a candidate',
                    f"{r['NO3_missing']}/{r['NO3_records']}" if r['NO3_included'] else 'Not a candidate'] for r in s5_rows],
                  'Missingness is evaluated on annual analysis rows before training-fold imputation. Raw source units are retained before the indicated transformations; standardized coefficients describe a one-training-standard-deviation change in the transformed predictor.')
document += '''### Training fold preprocessing and penalty selection

Models use Gaussian LASSO regression in glmnet with α = 1 (Friedman et al., 2010). Transformations are specified before fitting. Within each training fold, a candidate must have at least three finite values and nonzero finite sample variation after transformation. Eligible predictors are median-imputed and centered/scaled using the training mean and sample standard deviation. The fitted transformation, median, center, and scale are applied to validation observations. Predictor eligibility, imputation, and scaling are repeated separately inside every inner fold, outer fit, and bootstrap fit; validation outcomes are not used for preprocessing.

Outer validation withholds each publication in turn, retaining all of its annual records together. Within outer training data, sorted publication identifiers are assigned cyclically to up to five inner folds. Training and inner-validation publications do not overlap. Penalties are selected from 80 logarithmically spaced values from 100 to 0.0001. For the primary weighting rule, inner squared-error loss balances pairs and available years within each validation publication, then averages publication losses equally. The selected penalty is the largest within one standard error of the minimum publication-level validation loss. Standard errors use the variation among publication losses. Fitting weights are recalculated from each training subset using equal publication → pair → annual-record balance.

### Five model comparisons in Figure 3

The intercept-only model predicts the hierarchically weighted training response mean. The time-only model is weighted linear regression on time. The remaining models add the wildfire group, watershed group, or both groups to time, with time unpenalized (penalty factor zero) and additional predictors penalized (factor one). These models share outer publication splits and inner fold assignments. If a fold lacks a usable additional predictor, the group model reduces to the applicable time-only or intercept-only model.

Figure 3 reports 100 × (RMSE_intercept − RMSE_model) / RMSE_intercept. Positive values mean improvement over intercept-only; negative values mean worse prediction. Comparing a group model with time alone separates the additional group contribution from time itself. This dedicated comparison differs from the full-set selection models in Section S3, where time and all other candidates are penalized.

For publication s, MSEₛ is the mean squared error across its held-out annual records. Equal-publication RMSE is √[(1/S) Σₛ MSEₛ]; equal-publication MAE averages publication-specific MAE values. RMSE therefore differs from the arithmetic mean of publication-specific RMSE. Pooled annual-record RMSE and MAE weight all held-out rows equally. Pooled out-of-sample R² is 1 − Σ(y − ŷ)² / Σ(y − mean[y])², using the pooled observed mean; it can be negative and is not an equal-publication score.

'''
predictions = read('output/paper/predictor_group_cv/heldout_predictions.csv')
group_performance = {(r['response_var'], r['model']): r for r in read('output/paper/predictor_group_cv/performance.csv')}
group_improvement = {(r['response_var'], r['model']): r for r in read('output/paper/predictor_group_cv/group_improvement.csv')}
s6_rows = []
for a in ANALYTES:
    for model in MODEL_ORDER:
        rr = [r for r in predictions if r['response_var'] == a and r['model'] == model]
        observed = [float(r['observed']) for r in rr]
        error = [float(r['observed']) - float(r['predicted']) for r in rr]
        denominator = sum((v - sum(observed) / len(observed)) ** 2 for v in observed)
        mse_by_study = defaultdict(list)
        mae_by_study = defaultdict(list)
        for r, e in zip(rr, error):
            mse_by_study[r['Study_ID']].append(e * e)
            mae_by_study[r['Study_ID']].append(abs(e))
        equal_rmse = math.sqrt(sum(sum(v) / len(v) for v in mse_by_study.values()) / len(mse_by_study))
        equal_mae = sum(sum(v) / len(v) for v in mae_by_study.values()) / len(mae_by_study)
        saved = group_performance[a, model]
        assert math.isclose(equal_rmse, float(saved['RMSE']), abs_tol=1e-12)
        assert math.isclose(equal_mae, float(saved['MAE']), abs_tol=1e-12)
        s6_rows.append(dict(response_var=a, model=model, records=len(rr), publications=len(mse_by_study),
                            equal_publication_RMSE=equal_rmse, equal_publication_MAE=equal_mae,
                            pooled_RMSE=math.sqrt(sum(e * e for e in error) / len(error)),
                            pooled_MAE=sum(abs(e) for e in error) / len(error),
                            pooled_R2=1 - sum(e * e for e in error) / denominator,
                            improvement_percent=100 * float(group_improvement[a, model]['relative_RMSE_improvement'])))
document += table('S6', 'Cross validated performance of the five time adjusted models', s6_rows,
                  ['Analyte', 'Model', 'Equal publication RMSE', 'Equal publication MAE', 'Pooled RMSE', 'Pooled MAE', 'Pooled R²', 'RMSE improvement %'],
                  [[NAMES[r['response_var']], MODEL[r['model']], number(r['equal_publication_RMSE'], 4),
                    number(r['equal_publication_MAE'], 4), number(r['pooled_RMSE'], 4), number(r['pooled_MAE'], 4),
                    number(r['pooled_R2'], 3), number(r['improvement_percent'], 1)] for r in s6_rows],
                  'Improvements use equal-publication RMSE relative to intercept-only. Pooled metrics and R² are descriptive alternatives with different row weights.')
document += '''The DOC time + watershed model reduces equal-publication RMSE by 22.5%, and the model with both groups reduces it by 27.3%, relative to intercept-only. Time alone and time + wildfire are 1.4% worse. For nitrate, every time-containing model improves by 8.5%; additional group coefficients are zero in all outer fits, so these gains are attributable to time rather than watershed or wildfire characteristics. These are descriptive cross-analyte comparisons because publication coverage, response variation, and the soil predictor differ.

### Publication specific errors and consistency of improvement

Table S7 reports publication-specific RMSE from the same held-out rows for all models. All ten pairwise model comparisons were calculated within each publication. RMSE difference is candidate minus reference; RMSE improvement has the opposite sign. Differences within 10⁻¹⁰ lnRR units are treated as ties. Counts describe publications and are not independent binomial trials because some publications share a wildfire.

'''
publication_errors = read('output/paper/predictor_group_cv/paired_uncertainty/publication_model_errors.csv')
s7_rows = []
for a in ANALYTES:
    for study in sorted({r['Study_ID'] for r in publication_errors if r['response_var'] == a}):
        rr = {r['model']: r for r in publication_errors if r['response_var'] == a and r['Study_ID'] == study}
        row = dict(response_var=a, publication=study, records=int(rr['intercept_only']['n_records']),
                   dependence_group=rr['intercept_only']['dependence_cluster'])
        row.update({m: float(rr[m]['RMSE']) for m in MODEL_ORDER})
        s7_rows.append(row)
document += table('S7', 'Publication specific held out RMSE', s7_rows,
                  ['Analyte', 'Publication', 'Records', 'Intercept', 'Time', 'Time + wildfire', 'Time + watershed', 'Both groups + time'],
                  [[NAMES[r['response_var']], r['publication'], r['records']] + [number(r[m], 4) for m in MODEL_ORDER] for r in s7_rows])
pair_differences = read('output/paper/predictor_group_cv/paired_uncertainty/paired_publication_RMSE_differences.csv')
write_rows(OUT / 'data/paired_publication_RMSE_differences.csv', pair_differences)
document += '''The full time-adjusted DOC model improves on intercept-only for six of seven held-out publications; Uzun et al. (2020) is the exception, with RMSE increasing by 0.0704. Time + watershed also improves six of seven, and the full model improves on time + watershed for five of seven. Nitrate time-containing models improve on intercept-only for six of twelve publications and worsen for six; they tie one another in all twelve. The [complete 190 publication-specific paired differences](data/paired_publication_RMSE_differences.csv) retain both sign conventions.

### Conditional uncertainty from resampling held out errors

We generated 100,000 paired resamples per analyte from the saved held-out publication MSE vectors (seed 20261027). Each resample draws the observed number of connected publication–fire groups with replacement: six for DOC and eleven for nitrate. All publications in a selected group enter together, once per group selection, so Rhea and Writer always have the same resampling multiplicity. Identical group draws are used for every model. Within a resampled inventory, overall RMSE is the square root of the publication-weighted mean MSE. The group containing two publications retains both publication contributions rather than being forced to one publication’s weight. Consequently, publication counts can vary between draws. Percentile intervals use the 2.5th and 97.5th percentiles of paired absolute or percentage improvements.

These intervals condition on the existing cross-validation predictions. They assess sensitivity to the represented publication–fire-group composition without refitting or retuning models. They do not fully capture covariance induced by overlapping cross-validation training sets or remove shared-fire information crossing the original publication splits. Only six independent DOC groups are available. Resample-positive fractions are descriptive frequencies, not posterior probabilities or formal p-values.

'''
s8_rows = read('output/paper/predictor_group_cv/paired_uncertainty/overall_RMSE_improvement_uncertainty.csv')
document += table('S8', 'Paired improvements and conditional error resampling intervals', s8_rows,
                  ['Analyte', 'Reference → candidate', 'Improved / tied / worse', 'RMSE improvement [95% interval]', 'Improvement % [95% interval]'],
                  [[NAMES[r['response_var']], f"{MODEL[r['reference_model']]} → {MODEL[r['candidate_model']]}",
                    f"{r['improved']} / {r['tied']} / {r['worsened']}",
                    interval(r['RMSE_improvement'], r['lower_RMSE_improvement'], r['upper_RMSE_improvement'], 4),
                    interval(r['improvement_percent'], r['lower_improvement_percent'], r['upper_improvement_percent'], 1)] for r in s8_rows],
                  'Positive improvement = reference minus candidate error. Intervals resample fixed errors in connected groups, with equal-publication weighting retained; they do not include refitting uncertainty.')
document += '''The full DOC RMSE improvement over intercept-only is 0.1021 lnRR units, with a conditional 95% interval of 0.0136–0.1601, or 27.3% [4.4%, 40.9%]. The corresponding time + watershed interval includes zero (0.0840 [−0.0150, 0.1492]). Nitrate’s time-containing models have improvement 0.0738 [−0.0088, 0.1597], also including zero. Thus, lower aggregate error does not imply that every held-out publication improves or that the same gain is assured in a new wildfire setting.

### Benchmark comparison for the full set selection models

The full-set models used for Figure 4 and the sensitivities in Section S3 penalize time as well as all other candidates. Their separate benchmark comparison uses intercept-only, time-only, and time plus one fire metric fitted by weighted linear regression on the same outer training data. The fire metric is high-severity burn percentage for DOC and watershed burned percentage for nitrate. This one-fire-metric benchmark differs from Figure 3’s two-fire-metric LASSO group model. Table S9 reports these full-set model comparisons without substituting their scores for Figure 3 results.

'''
primary_performance = read('output/paper/tables/performance.csv')
primary_equal = {(r['response_var'], r['model']): r for r in read('output/paper/tables/performance_equal_study.csv')}
s9_rows = [{**r, **{k: primary_equal[r['response_var'], r['model']][k] for k in ('equal_study_RMSE', 'equal_study_MAE')}} for r in primary_performance]
document += table('S9', 'Full set penalized model and linear benchmark performance', s9_rows,
                  ['Analyte', 'Model', 'Equal publication RMSE', 'Equal publication MAE', 'Pooled RMSE', 'Pooled MAE', 'Pooled R²'],
                  [[NAMES[r['response_var']], MODEL[r['model']], number(r['equal_study_RMSE'], 4), number(r['equal_study_MAE'], 4),
                    number(r['RMSE'], 4), number(r['MAE'], 4), number(r['R2'], 3)] for r in s9_rows],
                  'Time is penalized in the full-set LASSO, but not in the linear benchmarks. These are not the time-adjusted group-model scores in Table S6.')

document += '''## S3 Predictor stability and sensitivity analyses

### Publication bootstrap and coefficient distributions

For each analyte, 1,000 bootstrap attempts sample the original number of publications with replacement and retain all pair histories and annual records from each draw. Each draw receives equal fitting-weight mass, split equally among its pairs and available years. Repeated copies of an original publication are assigned to the same inner tuning fold, preventing a copy from appearing in both inner training and validation. Preprocessing and penalty tuning are repeated for each sample. Tuning-loss means retain bootstrap multiplicity, whereas their standard errors use unique original publications. Time and every other candidate are penalized in these full-set models.

Six DOC attempts contain fewer than three unique publications and cannot support inner tuning; they are logged and excluded from denominators. The retained counts are 994 successful DOC refits and 1,000 nitrate refits. A screened-out candidate counts as unselected in a successful fit, whereas a failed fit does not count as a zero coefficient. Selection frequency is the fraction of successful refits with a nonzero coefficient. Positive and negative frequencies use the same successful-refit denominator; their sum equals selection frequency. Coefficients correspond to predictors standardized within each bootstrap training dataset. Table S10 reports empirical coefficient summaries including zeros; its percentile ranges are distributions across resampled fits, not coefficient confidence intervals or significance tests.

'''
stability = read('output/paper/tables/selection_stability.csv')
bootstrap = read('output/paper/tables/bootstrap_summary.csv')
raw_coefficients = read('output/paper/tables/bootstrap_coefficients.csv')
iterations = {a: sorted({int(r['iteration']) for r in raw_coefficients if r['response_var'] == a}) for a in ANALYTES}
coef_lookup = {(r['response_var'], int(r['iteration']), r['predictor']): float(r['coefficient']) for r in raw_coefficients}
coefficient_rows = []
s10_rows = []
for r in stability:
    a, predictor = r['response_var'], r['predictor']
    values = [coef_lookup.get((a, i, predictor), 0.0) for i in iterations[a]]
    assert len(values) == int(r['completed_iterations'])
    assert math.isclose(sum(v != 0 for v in values) / len(values), float(r['selection_frequency']), abs_tol=1e-12)
    assert math.isclose(quantile(values, .5), float(r['median_coefficient']), abs_tol=1e-12)
    s10_rows.append({**r, 'coefficient_q025': quantile(values, .025), 'coefficient_q975': quantile(values, .975)})
    coefficient_rows.extend(dict(response_var=a, iteration=i, predictor=predictor, coefficient=v)
                            for i, v in zip(iterations[a], values))
with gzip.open(OUT / 'data/bootstrap_coefficient_distributions.csv.gz', 'wt', newline='') as stream:
    writer = csv.DictWriter(stream, fieldnames=list(coefficient_rows[0]))
    writer.writeheader()
    writer.writerows(coefficient_rows)
document += table('S10', 'Selection frequencies signs and coefficient distributions', s10_rows,
                  ['Analyte', 'Predictor', 'Successful refits', 'Selected %', 'Positive %', 'Negative %', 'Median coefficient [2.5th, 97.5th percentile]'],
                  [[NAMES[r['response_var']], PREDICTOR[r['predictor']], r['completed_iterations'], percent(r['selection_frequency']),
                    percent(r['positive_frequency']), percent(r['negative_frequency']),
                    interval(r['median_coefficient'], r['coefficient_q025'], r['coefficient_q975'], 4)] for r in s10_rows],
                  'Percentiles include zero coefficients. The [complete coefficient distributions](data/bootstrap_coefficient_distributions.csv.gz) include candidates screened out of successful fits as zeros.')
document += '''Soil organic matter is selected in 78.0% of DOC refits and always has a positive coefficient when selected. Runoff and watershed area are selected in 37.3% and 27.1%, respectively. The highest nitrate frequency is 3.7%. These summaries describe stability conditional on the measured candidate set and study composition; they do not establish causal effects or unique ecological importance.

### Alternative weighting comparisons

The equal-row sensitivity assigns equal fitting weight to every annual record. The precision/family sensitivity uses inverse provisional annual delta variances, replaces unavailable precision with the training median, caps precision at the training 95th percentile, and multiplies by 1 divided by the number of retained pairs sharing a reference. These variances assume independence of eligible daily values and remain unvalidated for temporal dependence or interpolation. The precision/family comparison changes both precision and the shared-reference multiplier, so it does not isolate a shared-reference weighting effect.

Both alternatives use the same candidate sets, transformations, grouped-validation framework, fixed penalty grid, one-SE rule, and publication bootstrap. Their inner validation losses weight annual rows equally within each publication, then average publication losses; the primary condition also balances pairs within publication. Thus these comparisons change fitting weights and within-publication tuning-loss weighting. They apply to the full-set models with all candidates penalized, not a new weighting analysis of Figure 3’s unpenalized-time group models. Observed-only and matched-interpolated LASSO diagnostics remain available in the analysis repository; the interpolation analysis selected for pooled responses is reported in Section S1.

'''
weight_performance = [r for r in read('output/paper/tables/weighting_interpolation_performance.csv')
                      if r['model'] == 'lasso' and r['scenario'] in ('primary', 'equal_row', 'precision_family')]
WEIGHT = {'primary': 'Hierarchical balance', 'equal_row': 'Equal annual rows', 'precision_family': 'Provisional precision + reference family'}
s11_rows = []
for r in weight_performance:
    baseline = next(x for x in read('output/paper/tables/weighting_interpolation_performance.csv')
                    if x['response_var'] == r['response_var'] and x['scenario'] == r['scenario'] and x['model'] == 'intercept_only')
    s11_rows.append({**r, 'intercept_RMSE': baseline['equal_study_RMSE']})
document += table('S11', 'Full set performance under alternative weighting', s11_rows,
                  ['Analyte', 'Fitting condition', 'LASSO RMSE', 'Intercept RMSE', 'LASSO MAE'],
                  [[NAMES[r['response_var']], WEIGHT[r['scenario']], number(r['equal_study_RMSE'], 4),
                    number(r['intercept_RMSE'], 4), number(r['equal_study_MAE'], 4)] for r in s11_rows],
                  'All reported errors give equal weight to held-out publications. Precision weights are provisional; the comparison does not validate annual variances.')
s12_rows = []
for scenario in ('primary', 'equal_row', 'precision_family'):
    path = 'output/paper/tables/selection_stability.csv' if scenario == 'primary' else f'output/paper/sensitivity/{scenario}/selection_stability.csv'
    s12_rows.extend({**r, 'scenario': scenario} for r in read(path))
weight_selection = {(r['response_var'], r['predictor'], r['scenario']): r for r in s12_rows}
document += table('S12', 'Predictor selection under alternative weighting', s12_rows,
                  ['Analyte', 'Predictor', 'Balance selected %', 'Equal rows selected %', 'Precision family selected %'],
                  [[NAMES[r['response_var']], PREDICTOR[r['predictor']]] +
                   [percent(weight_selection[r['response_var'], r['predictor'], s]['selection_frequency'])
                    for s in ('primary', 'equal_row', 'precision_family')] for r in stability],
                  'Successful-refit denominators and coefficient-sign summaries are retained in the machine-readable table. Every condition has 994 successful DOC and 1,000 nitrate refits from 1,000 attempts.')
document += '''DOC full-set RMSE is 0.2789 under hierarchical balance, 0.2592 under equal-row weighting, and 0.4184 under provisional precision/family weighting. Its performance advantage is therefore sensitive to the weighting condition, particularly provisional precision. Nitrate full-set predictions match their condition-specific intercept-only benchmarks under all three rules (Table S11). These weighting comparisons do not alter the main Figure 3 group-model scores.

### Individual and grouped predictor omissions

Twelve specifications per analyte comprise the full eight-predictor set, each of eight individual omissions, joint hydrology omission, complete wildfire-group omission, and complete watershed-group omission. Time remains available in both complete-group exclusions but is penalized, as in the full-set selection models. All specifications share ordered bootstrap training draws, original-publication inner tuning groups, and publications not selected in each draw for out-of-bag evaluation. Preprocessing and tuning are repeated for each candidate set. Sparse subsets fit the remaining usable predictor or an appropriate intercept if none vary; an internal constant padding column required by glmnet is not counted as a candidate.

Nested leave-one-publication-out errors provide the primary omission contrasts. Paired out-of-bag error differences describe sensitivity across resampled training and varying test-publication compositions. Eight successful DOC draws include every publication and have no out-of-bag test publication; they enter selection summaries but not error comparisons. Thus 994 DOC refits contribute selection frequencies and 986 contribute paired out-of-bag errors; all 1,000 nitrate refits contribute both. Out-of-bag medians and central 95% ranges in Table S13 and SI Figure 3 are descriptive resampling distributions, not formal confidence intervals. They differ from Table S8’s fixed-held-out-error bootstrap.

'''
omission_specs = read('output/paper/predictor_omission/specifications.csv')
omission_labels = {(r['response_var'], r['specification']): r['label'] for r in omission_specs}
omission_performance = read('output/paper/predictor_omission/performance_loso.csv')
omission_summary = {(r['response_var'], r['specification']): r for r in read('output/paper/predictor_omission/paired_omission_summary.csv')}
omission_bootstrap = {r['response_var']: r for r in read('output/paper/predictor_omission/bootstrap_summary.csv')}
s13_rows = []
for r in omission_performance:
    a, spec = r['response_var'], r['specification']
    if spec == 'full':
        extra = dict(delta_RMSE=0, median_delta_RMSE=0, lower_delta_RMSE=0, upper_delta_RMSE=0,
                     paired_oob_refits=omission_bootstrap[a]['paired_oob_refits'])
    else:
        extra = omission_summary[a, spec]
    s13_rows.append({**r, 'label': omission_labels[a, spec],
                    **{k: extra[k] for k in ('delta_RMSE', 'median_delta_RMSE', 'lower_delta_RMSE', 'upper_delta_RMSE', 'paired_oob_refits')}})
document += table('S13', 'Individual and group omission performance', s13_rows,
                  ['Analyte', 'Specification', 'LOSO RMSE', 'Change from full set', 'Median OOB change [central 95% range]', 'Paired OOB refits'],
                  [[NAMES[r['response_var']], r['label'], number(r['RMSE'], 4), number(r['delta_RMSE'], 4),
                    interval(r['median_delta_RMSE'], r['lower_delta_RMSE'], r['upper_delta_RMSE'], 4),
                    r['paired_oob_refits']] for r in s13_rows],
                  'Here change = omitted-set minus full-set RMSE, so positive means deterioration. This sign differs from positive-improvement Tables S6 and S8. Time is penalized in these omission fits. The full-set row has zero differences by definition.')
omission_stability = read('output/paper/predictor_omission/selection_stability.csv')
document += table('S14', 'Complete selection and sign summaries for predictor omissions', omission_stability,
                  [], [], 'The machine-readable table contains all 192 analyte–specification–predictor combinations, with selection frequency, median coefficient, sign frequencies and successful-refit denominator. Intentionally unavailable predictors have availability false and missing frequencies rather than zero selection. SI Figure 3 displays these selection percentages.', embedded=False)
document += '''Deleting the complete watershed group increases all-penalized DOC LOSO RMSE from 0.2789 to 0.3733; deleting the wildfire group leaves it unchanged. Removing soil organic matter alone increases RMSE to 0.3449, whereas removing runoff or both hydrology metrics lowers it to 0.2351. The prespecified full model is retained rather than replaced by a subset selected after examining these results. Soil organic matter remains selected in 75.1–82.0% of refits when available. Nitrate selection frequencies remain at or below 6.8% across these specifications. Removing its complete watershed group changes RMSE from 0.8666 to 0.8733; wildfire-group removal leaves it unchanged. Correlated-predictor substitutions and different tuning solutions can change error without identifying a unique causal driver.

### Supporting figure captions

'''
for number, directory, image_name in (
    (1, 'si_figure_1_predictor_correlations', 'si_figure_1_predictor_correlation_matrix'),
    (2, 'si_figure_2_predictor_violins', 'si_figure_2_predictor_violins'),
):
    caption = text_source(f'output/paper/figures/{directory}/si_figure_{number}_caption.txt').strip()
    document += caption + f'\n\n![SI Figure {number}](../figures/{directory}/{image_name}.png)\n\n'
    document += f'[Vector PDF](../figures/{directory}/{image_name}.pdf).\n\n'
si3_caption = text_source('output/paper/figures/si_figure_3_predictor_omission/si_figure_3_caption.txt').strip()
document += si3_caption + '\n\n![SI Figure 3 predictor omissions](../figures/si_figure_3_predictor_omission/si_figure_3_predictor_omission.png)\n\n'
document += '''### Software and reproducibility

Saved execution records identify R 4.6.1, glmnet 5.0, and clubSandwich 0.7.0. Model fitting, publication bootstrap, and paired error-resampling seeds and settings are retained with their outputs. The paired error-resampling seed is 20261027; the full-set selection and omission bootstrap seed is 20260929. All tables here are generated from saved outputs, with full-precision CSV companions. Conditional REML analyses remain exploratory and are not used as manuscript inference in these sections.

## References

Hedges, L. V., Tipton, E., and Johnson, M. C. (2010). Robust variance estimation in meta-regression with dependent effect size estimates. *Research Synthesis Methods*, 1(1), 39–65. [Article](https://doi.org/10.1002/jrsm.5).

Tipton, E. (2015). Small sample adjustments for robust variance estimation with meta-regression. *Psychological Methods*, 20(3), 375–393. [Article](https://doi.org/10.1037/met0000011).

Pustejovsky, J. E., and Tipton, E. (2022). Meta-analysis with robust variance estimation: Expanding the range of working models. *Prevention Science*, 23, 425–438. [Article](https://doi.org/10.1007/s11121-021-01246-3).

Friedman, J., Hastie, T., and Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1–22. [Article](https://doi.org/10.18637/jss.v033.i01).
'''

# Preserve numeric sources for descriptive predictor diagnostics and add hashes.
read('output/paper/tables/si_figure_1_predictor_correlations.csv')
read('output/paper/tables/si_figure_2_predictor_distribution_summary.csv')
group_session = text_source('output/paper/predictor_group_cv/session_info.txt')
synthesis_session = text_source('output/paper/balanced_synthesis/session_info.txt')
assert 'R version 4.6.1' in group_session and 'glmnet_5.0' in group_session
assert 'clubSandwich_0.7.0' in synthesis_session
assert len(table_index) == 14
assert len(s6_rows) == 10 and len(s7_rows) == 19 and len(s8_rows) == 20
assert len(s10_rows) == 16 and len(s13_rows) == 24 and len(omission_stability) == 192
assert len(pair_differences) == 190 and len(coefficient_rows) == (994 + 1000) * 8
assert all(int(r['successful']) == (994 if r['response_var'] == 'DOC' else 1000) for r in bootstrap)
assert 'REML' not in document.split('### Software and reproducibility')[0]
(OUT / 'statistical_supporting_information.md').write_text(document)
write_rows(OUT / 'table_index.csv', table_index)
sources.add('R/manuscript/01_build_supporting_information_agent_v1.py')
write_rows(OUT / 'source_manifest.csv', [dict(path=p, sha256=hashlib.sha256((ROOT / p).read_bytes()).hexdigest()) for p in sorted(sources)])
(OUT / 'main_text_si_cross_references.md').write_text('''# Main text references to statistical supporting information

## Results Section 3.1 and pooled synthesis Methods

Replace the generic pooled-response sensitivity reference “(SI)” with “(Supporting Information Section S1, Tables S1–S4)”. Table S2 covers clustering; Table S3 covers interpolation on matched annual inventories; Table S4 covers point-estimate influence. Deletion confidence intervals were not calculated.

## Results Section 3.2 and cross validation Methods

Use “(Supporting Information Section S2, Tables S5–S9; SI Figures 1 and 2)” for predictors, preprocessing, performance metrics, publication-specific errors and conditional uncertainty. In particular, “Publication-specific prediction errors and the consistency and conditional uncertainty of predictive improvements are reported in Supporting Information Section S2, Tables S7 and S8.”

For the new uncertainty calculation, add: “We quantified conditional uncertainty in RMSE improvement using 100,000 paired resamples of connected publication–wildfire groups, keeping publications sharing a wildfire together. Resampling used saved held-out publication MSE vectors without refitting models; overall RMSE was recomputed as the square root of the equally weighted mean publication MSE. We report 95% percentile intervals.” These intervals do not remove shared-fire information across the original publication validation splits or fully capture overlapping-training/refitting uncertainty.

The Figure 3 group models leave time unpenalized. Distinguish their two-fire-metric LASSO group from the one-fire-metric linear benchmark accompanying the all-penalized full-set models (Table S9). Pooled annual-record RMSE, MAE and R² for all five Figure 3 models are now explicitly calculated in Table S6 from their saved predictions.

## Results Section 3.3 and bootstrap Methods

Use “(Supporting Information Section S3, Tables S10–S14; SI Figure 3)” for full-set selection/sign summaries, coefficient distributions, alternative weighting and individual/group omissions. A replacement closing sentence is: “Selection frequencies, coefficient distributions, alternative-weighting comparisons, and structured individual and group predictor-omission analyses are reported in Supporting Information Section S3 (Tables S10–S14; SI Figure 3).”

Alternative weighting applies to the all-candidate-penalized full-set models, not to a rerun of the Figure 3 group models. Precision/family weights use provisional variances and change both precision and the reference multiplier. Within-publication inner scoring also differs between primary balance weights and alternative weighting conditions; these are not strictly isolated fitting-weight comparisons.
''')
(OUT / 'README.md').write_text('''# Statistical supporting information

The editable manuscript SI is [statistical_supporting_information.md](statistical_supporting_information.md), covering Results 3.1–3.3 and both supplied statistical Methods subsections. It contains three sections, fourteen numbered tables, SI figure captions/links, and verified references. Companion CSVs preserve precision. [Main text reference replacements](main_text_si_cross_references.md) provide exact section/table citations and necessary model distinctions.

Rebuild from saved outputs, without fitting models:

```sh
python3 agent_workflows/vibe_coding/R/manuscript/01_build_supporting_information_agent_v1.py
```

The runner’s `si` mode performs this step alone; `all` runs it after model/figure generation. Rebuild source analyses first when inputs change. `source_manifest.csv` hashes all tabular/text sources and the builder. `table_index.csv` lists every export. `data/` contains complete publication-specific paired differences and bootstrap coefficient distributions, including zero-filled screened candidates from successful fits.

Table labels S1–S14 are assigned within this statistical SI packet. SI Figure numbers 1–3 and main Figure numbers 1–4 are unchanged. Final journal-wide SI numbering, predictor-unit/provenance review against main Table 1, and Word/journal layout are authoring steps outside this Markdown build. The code’s follow-up-year fallback is explicitly described. Exploratory REML, unimplemented elastic net and unperformed inferential tests are not presented as completed manuscript analyses.
''')

# Verify saved local targets and contents after writing. No Page or remote edit.
assert (OUT / 'statistical_supporting_information.md').read_text() == document
for target in re.findall(r'\]\(([^)]+)\)', document):
    if not target.startswith(('https://', 'http://')):
        assert (OUT / target).exists(), target
for entry in table_index:
    with (OUT / entry['file']).open() as stream:
        assert len(list(csv.DictReader(stream))) == entry['rows']
print(f'Saved three SI sections and {len(table_index)} full-precision tables to {OUT}')
print('Verified source metrics, bootstrap denominators, table dimensions, saved readback and all local figure/data links.')
