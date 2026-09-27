# Forecast Aggregation & Model Evaluation

Improving crowd-forecast accuracy by combining forecaster track record and 
recency into a weighted aggregation method — benchmarked against five standard 
baselines on real forecasting-tournament data.

## Overview

This project analyses individual forecaster predictions from the IARPA Hybrid 
Forecasting Competition dataset, restricted to binary single-answer questions 
(a mathematical prerequisite, since several baseline methods are scalar 
aggregators and cannot operate on probability vectors). After cleaning, the 
working dataset covers 138,036 individual forecasts from 4,518 forecasters 
across 88 resolved binary questions, expanded via last-observation-carried-
forward into 4,307,460 forecaster-question-day rows.

## Method

Five standard aggregation baselines were benchmarked by Brier score: arithmetic 
mean, median, geometric mean, trimmed mean (10%), and geometric mean of odds. 
Geometric mean performed best (0.0705), which the analysis traces to the 
dataset's skewed 12.57% positive-resolution rate — geometric mean's downward 
compression is rewarded when most questions resolve negatively, while geometric 
mean of odds (theoretically preferred for balanced distributions per Mellers et 
al., 2015) underperforms here precisely because this distribution isn't balanced.

A weighted aggregation method was then built on top of the best baseline, 
combining two signals per forecaster:

- **Accuracy weight** — `1 / (past_brier + 0.01)` for forecasters with 5 or 
  more resolved question-day observations, else 1 (a multiplicative identity, 
  so unproven forecasters aren't penalised, only unweighted).
- **Staleness weight** — `1 / (1 + age_days)`, downweighting older forecasts 
  as a question approaches resolution.

The two weights are combined multiplicatively rather than additively, enforcing 
that a forecast needs to be *both* accurate and recent to be trusted — 
either alone isn't sufficient. All weighting strictly respects a T-1 cutoff 
(`resolved_date < T`), so no forecast is ever weighted using information that 
wouldn't have been available at the time.

## Results

| Method | Brier Score |
|---|---|
| Weighted geometric mean (this method) | **0.0573** |
| Geometric mean (best baseline) | 0.0705 |
| Geometric mean of odds | 0.0851 |
| Median | 0.0972 |
| Trimmed mean (10%) | 0.1011 |
| Raw mean | 0.1104 |

The weighted method improves 19% over the best baseline overall, 27.6% on 
question-days that resolved negatively, and 12.2% on question-days that 
resolved positively.

## Data notes

- Source data included a third file of ~9.2 million pre-aggregated team-level 
  daily forecasts, which was deliberately excluded — it's the wrong level of 
  input for this analysis (already-aggregated output, not individual forecasts).
- 994 duplicate rows (same question/forecaster/timestamp, differing probability) 
  were resolved by keeping the higher value; this was verified empirically to 
  produce identical Brier scores to four decimal places against the alternative 
  (keeping the lower value), confirming the choice doesn't bias results.
- Exact 0 and 1 probabilities were clamped to 0.001 and 0.999, since 
  log(0) is undefined and would break the geometric mean calculation.

## Stack

R · lme4-adjacent tidy workflow · tidyverse for cleaning and transformation

## Files

- `hfc_rcta_analysis.R` — full pipeline, from raw data to final weighted scores
- `hfc_rcta_memo.pdf` — written methodology and findings memo

