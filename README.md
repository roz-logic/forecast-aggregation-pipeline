# Forecast Aggregation & Model Evaluation

Improving crowd-forecast accuracy by combining forecaster track record and recency into a weighted aggregation method — benchmarked against five standard baselines on real forecasting-tournament data.

## Overview

This project analyses individual forecaster predictions from the IARPA Hybrid Forecasting Competition dataset, restricted to binary single-answer questions—a mathematical prerequisite because several baseline methods are scalar aggregators and cannot operate on probability vectors.

After cleaning, the working dataset covers 138,036 individual forecasts from 4,518 forecasters across 88 resolved binary questions, expanded via last-observation-carried-forward into 4,307,460 forecaster-question-day rows.

## Method

Five standard aggregation baselines were benchmarked by Brier score:

- Arithmetic mean
- Median
- Geometric mean
- Trimmed mean (10%)
- Geometric mean of odds

Geometric mean performed best among the unweighted baselines (0.0705). The analysis attributes this to the dataset's skewed 12.57% positive-resolution rate: geometric mean's downward compression is rewarded when most questions resolve negatively, while geometric mean of odds—often theoretically preferable for balanced distributions—underperforms here because this outcome distribution is not balanced.

A weighted aggregation method was then built on top of the best baseline, combining two signals per forecaster:

- **Accuracy weight:** `1 / (past_brier + 0.01)` for forecasters with five or more resolved question-day observations; otherwise 1. This treats unproven forecasters as unweighted rather than penalised.
- **Staleness weight:** `1 / (1 + age_days)`, downweighting older forecasts as a question approaches resolution.

The two weights are combined multiplicatively rather than additively, enforcing that a forecast must be both accurate and recent to receive a high weight. All weighting strictly respects a T-1 cutoff (`resolved_date < T`), so no forecast is weighted using information that would not have been available at the time.

## Results

| Method | Brier Score |
|---|---:|
| Weighted geometric mean (this method) | **0.0573** |
| Geometric mean (best baseline) | 0.0705 |
| Geometric mean of odds | 0.0851 |
| Median | 0.0972 |
| Trimmed mean (10%) | 0.1011 |
| Raw mean | 0.1104 |

The weighted method improves 19% over the best baseline overall, 27.6% on question-days that resolved negatively, and 12.2% on question-days that resolved positively.

## Data notes

- The source dataset is not included in this repository. Obtain the IARPA Hybrid Forecasting Competition data from its authorised/original source and update the input path in the R script before running the pipeline.
- Source data included a third file of approximately 9.2 million pre-aggregated team-level daily forecasts. It was deliberately excluded because it is already-aggregated output, not individual forecaster-level input.
- 994 duplicate rows with the same question, forecaster, and timestamp but differing probabilities were resolved by keeping the higher value. This was verified empirically to produce identical Brier scores to four decimal places against keeping the lower value.
- Exact 0 and 1 probabilities were clamped to 0.001 and 0.999 because `log(0)` is undefined and would break the geometric-mean calculation.

## Stack

- R
- tidyverse
- dplyr
- tidyr
- readr
- purrr

## Files

- `hfc_rcta_analysis.R` — full pipeline, from raw data to final weighted scores
- `hfc_rcta_memo.pdf` — written methodology and findings memo

## Reproducibility

1. Obtain the source data from its authorised provider.
2. Update the input-data path near the top of `hfc_rcta_analysis.R`.
3. Install the required R packages listed in the script.
4. Run the script in RStudio or from an R session.
5. Compare the generated scores with the values reported above.
