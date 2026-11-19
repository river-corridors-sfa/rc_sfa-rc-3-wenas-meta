"""Rebuild model inputs from the local paired daily chemistry."""

import csv
from collections import Counter, defaultdict
from datetime import date
from math import isfinite, log
from pathlib import Path
from statistics import mean, variance


def read_csv(path):
    with path.open(newline="", encoding="utf-8") as file:
        return list(csv.DictReader(file))


def write_csv(path, fields, rows):
    with path.open("w", newline="", encoding="utf-8") as file:
        writer = csv.DictWriter(file, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def main():
    folder = Path(__file__).resolve().parent
    inputs = folder / "inputs"
    output = folder / "model_inputs"
    pair_rows = read_csv(inputs / "pairs.csv")
    predictor_rows = read_csv(inputs / "watershed_predictors.csv")
    pairs = {r["candidate_pair_id"]: r for r in pair_rows}
    predictors = {r["candidate_pair_id"]: r for r in predictor_rows}
    if len(pairs) != len(pair_rows) or len(predictors) != len(predictor_rows):
        raise ValueError("Pair and predictor inputs must have unique candidate_pair_id values.")
    if pairs.keys() != predictors.keys():
        raise ValueError("The predictor roster must match the pair roster.")

    analytes = ("DOC", "NO3")
    exclusions = {
        (r["candidate_pair_id"], r["response_var"], r["year"])
        for r in read_csv(inputs / "calendar_year_exclusions.csv")
    }
    dates_by_pair = defaultdict(list)
    seen = set()
    for row in read_csv(inputs / "daily_paired_chemistry.csv"):
        pair_id = row["candidate_pair_id"]
        analyte = row["response_var"]
        sample_date = date.fromisoformat(row["Sampling_Date"])
        daily_key = pair_id, analyte, sample_date
        if daily_key in seen or int(row["year"]) != sample_date.year or analyte not in analytes:
            raise ValueError(f"Duplicate or inconsistent daily row: {daily_key}")
        seen.add(daily_key)
        water_year = sample_date.year + (sample_date.month >= 10)
        if water_year != int(pairs[pair_id]["fire_water_year"]) + 1 or row["valid"] != "TRUE":
            continue
        if (pair_id, analyte, row["year"]) in exclusions:
            continue
        burn, reference = float(row["burn"]), float(row["reference"])
        if not all(isfinite(value) and value > 0 for value in (burn, reference)):
            raise ValueError(f"Invalid concentration flagged valid: {daily_key}")
        directly_observed = row["burn_observed"] == row["reference_observed"] == "TRUE"
        dates_by_pair[pair_id, analyte].append((sample_date, burn, reference, directly_observed))

    coverage = []
    results = {"first_water_year": [], "first_water_year_observed_only": []}
    for pair_id, pair in pairs.items():
        water_year = int(pair["fire_water_year"]) + 1
        for analyte in analytes:
            daily = sorted(dates_by_pair[pair_id, analyte])
            observed = [record for record in daily if record[-1]]
            coverage.append({
                **pair, "response_var": analyte, "year": water_year,
                "n_valid_paired_dates": len(daily), "n_observed_paired_dates": len(observed),
                "included": len(daily) >= 2, "observed_only_eligible": len(observed) >= 2,
            })
            for scenario, records in (("first_water_year", daily),
                                      ("first_water_year_observed_only", observed)):
                if len(records) < 2:
                    continue
                burn_mean = mean(record[1] for record in records)
                reference_mean = mean(record[2] for record in records)
                differences = [burn / burn_mean - reference / reference_mean
                               for _, burn, reference, _ in records]
                result = {
                    **pair, **predictors[pair_id], "response_var": analyte, "year": water_year,
                    "water_year_start": date(water_year - 1, 10, 1),
                    "water_year_end": date(water_year, 9, 30), "post_fire_year": 1,
                    "first_matched_date": records[0][0], "last_matched_date": records[-1][0],
                    "lnRR_n": len(records), "n_both_observed": len(observed),
                    "n_with_interpolation": len(records) - len(observed),
                    "annual_mean_burn": burn_mean, "annual_mean_reference": reference_mean,
                    "lnRR_mean": log(burn_mean / reference_mean),
                    "lnRR_var": variance(differences) / len(records),
                    "effect_size_definition": "log_ratio_annual_arithmetic_means",
                    "variance_method": "paired_delta_daily_independence_provisional",
                    "shared_control_year_id": f"{pair['shared_control_id']} | {analyte} | {water_year}",
                    "n_pairs_sharing_reference": 1, "reference_family_weight": 1,
                }
                results[scenario].append(result)

    output.mkdir(exist_ok=True)
    write_csv(output / "coverage.csv", coverage[0].keys(), coverage)
    for scenario, rows in results.items():
        if not rows:
            raise ValueError(f"No eligible rows for {scenario}.")
        effects_per_pair = Counter(r["candidate_pair_id"] for r in rows)
        for row in rows:
            row["matched_doc_no3"] = "TRUE" if effects_per_pair[row["candidate_pair_id"]] == 2 else "FALSE"
        rows.sort(key=lambda r: (r["response_var"], r["Study_ID"], r["Comparison_ID"]))
        write_csv(output / f"{scenario}.csv", rows[0].keys(), rows)
        print(f"{scenario}: {dict(Counter(r['response_var'] for r in rows))}")


if __name__ == "__main__":
    main()
