# FRI Data Analyst Work Test (max 10 hours)
# Data = HFC RCTA : Comparing aggregation methods for individual forecasters

# R version: 4.6.1 (2026-06-24 ucrt)
# Rstudio version: 2026.7.1.147
# Required packages : "tidyverse","ggplot2","dplyr","lubridate", "purrr"

# In this script, I will load the RCTA data, clean individual human forecasts,
# compute five aggregation methods for each question-day pair,
# compute the Brier scores, and propose one improvement.


# A note on scope: I restrict to binary/single-answer questions. 
# Because the requested aggregation methods are single-number probability estimates,
# whereas ordinal or multinomial questions require distribution-level aggregation and different scoring decisions.
# I therefore exclude both ordinal questions and non-ordinal multiple answer questions.
# I'll justify my decisions in the memo.


#my path
#setwd("C:/Users/desau/Desktop/...") 
setwd("PATH/TO/YOUR/DATA/FOLDER")
#change only the path above before running the script:


#libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)


# *********************************************
# PART 1: LOAD THE 3 RCTA CSV FILES
# *********************************************

# Three files, three levels of the same data:
#  1. questions-answers : what was asked and what happened (the outcomes)
#  2. prediction-sets: every individual participant forecast ever submitted
#  3. daily-forecasts: performer method snapshot (kept for reference, not aggregation)

# The prediction-sets file is the big one at ~750k rows. This will take a while.

questions_answers_raw <- read.csv("rct-a-questions-answers.csv")
cat("Questions-Answers file loaded:" , nrow(questions_answers_raw), "rows\n")

prediction_sets_raw <- read.csv("rct-a-prediction-sets.csv")
cat("Prediction sets file loaded:" , nrow(prediction_sets_raw), "rows\n")

#daily_forecasts loaded for reference, context only - not used in aggregation
#daily_forecasts_raw <- read.csv("rct-a-daily-forecasts.csv") #9.2M rows, slow-load only
#n_distinct(daily_forecasts_raw$external.predictor.id) 
#320 distinct methods, computed during exploration. To verify please uncomment the line(s) above.

#check if everything loaded correctly
cat("\nQuestions_answers file columns:\n")
names(questions_answers_raw)   # 38 cols

cat("\nPrediction_sets file columns:\n")
names(prediction_sets_raw)   # 33 cols

#cat("\nDaily_forecasts file columns:\n")
#names(daily_forecasts_raw)   # 11 cols

cat("\nMissing values in key prediction sets columns:\n")
sum(is.na(prediction_sets_raw$forecasted.probability))
sum(is.na(prediction_sets_raw$membership.guid))
sum(is.na(prediction_sets_raw$discover.question.id))
# all are 0, shall continue


# ****************************************************
# PART 2: UNDERSTANDING THE QUESTIONS STRUCTURE
# ****************************************************

# This part identifies which questions are binary with a single answer option.
# The platform's use.ordinal.scoring flag identifies non-ordinal questions,
# but some non-ordinal questions have multiple answer options.
# I will filter to questions with exactly one answer option (n_answers == 1)
# to ensure all five aggregation methods receive a single focal probability.


#how many ordinal vs non-ordinal questions do we have?
questions_answers_raw %>%
  distinct(discover.question.id, use.ordinal.scoring) %>%
  count(use.ordinal.scoring)


#how many non-ordinal questions actually resolved?
questions_answers_raw %>%
  filter(use.ordinal.scoring == FALSE) %>%
  filter(!is.na(answer.resolved.probability)) %>%
  distinct(discover.question.id) %>%
  nrow()

#identify the binary/single-outcome questions
question_answer_counts <- questions_answers_raw %>%
  group_by(discover.question.id, use.ordinal.scoring) %>%
  summarise(
    n_answers = n_distinct(discover.answer.id),
    .groups = "drop"
  )

binary_question_ids <- question_answer_counts %>%
  filter(use.ordinal.scoring == FALSE, n_answers == 1) %>%
  pull(discover.question.id)

cat("Binary questions identified:", length(binary_question_ids), "\n")

# How many binary/single-outcome questions actually resolved?
questions_answers_raw %>%
  filter(discover.question.id %in% binary_question_ids) %>%
  filter(answer.sort.order == 0) %>%
  filter(!is.na(answer.resolved.probability)) %>%
  distinct(discover.question.id) %>%
  nrow()
# 135 of 166 binary/single-answer questions have resolved outcomes



# ****************************************************
# PART 3: CLEANING THE INDIVIDUAL FORECAST DATA
# ****************************************************

# Key decisions made here (explained in memo):
# 1. Binary/single-outcome questions only (use.ordinal.scoring == FALSE)
# 2. Exclude forecasts made after correctness was known (they are no longer genuine predictions)
# 3. Keep only answer.sort.order = 0 (one probability per forecaster per prediction set)
#    This avoids double-counting and gives us a single focal probability
# 4. Clamp probabilities away from 0 and 1 (required for geometric mean calculations)
#    A forecast of exactly 0 makes the geometric product = 0, wiping out all others
#    Replace 0 with 0.001 and 1 with 0.999 which counts as a minimal adjustment



forecasts_pre_clamp <- prediction_sets_raw %>%
  #keep binary/single-outcome questions only
  filter(discover.question.id %in% binary_question_ids) %>%
  
  #keep only the first answer option per prediction set
  #this gives one focal probability per forecaster per question
  #for binary qs, this is the probability of outcome A
  #the resolved probability of outcome A is in 'answer.resolved.probability'
  filter(answer.sort.order == 0) %>%
  
  #exclude forecasts made after the answer was already publicly known
  #these are not real predictions and would inflate accuracy scores
  filter(made.after.correctness.known == FALSE) %>%
  
  #drop rows with missing forecast probability
  filter(!is.na(forecasted.probability)) %>%
  
  #drop rows with missing resolution info
  filter(!is.na(answer.resolved.probability)) %>%
  
  #standardise the column names so the rest of the script is readable
  rename(
    question_id = discover.question.id,
    forecaster_id = membership.guid,
    forecast_prob = forecasted.probability,
    resolved_outcome = answer.resolved.probability,
    submitted_at = created.at
  ) %>%
  # created.at and filled.at are identical for all rows in RCTA, so created.at is used as the timestamp.
  #parse timestamps and convert numeric columns
  mutate(
    submitted_at = as_datetime(submitted_at),
    forecast_date = as_date(submitted_at),
    forecast_prob = as.numeric(forecast_prob),
    resolved_outcome = as.numeric(resolved_outcome)
  ) 

#check exact 0/1 counts before clamping
#geometric mean and GMO both require probabilities strictly between 0 and 1
cat("Exact zero forecasts before clamping:",
    sum(forecasts_pre_clamp$forecast_prob == 0), "\n")
cat("Exact one forecasts before clamping:",
    sum(forecasts_pre_clamp$forecast_prob == 1 ), "\n")

#clamp probabilities away from exactly 0 and 1 
#necessary for geometric mean calculations as stated above
#clamp only exact 0 and 1s.
#log(0) breaks the geometric mean, odds of 0/1 are 0 or Inf.
forecasts_cleaned <- forecasts_pre_clamp %>%
  mutate(
    forecast_prob = case_when(
      forecast_prob == 0 ~ 0.001,
      forecast_prob == 1 ~ 0.999,
      TRUE ~ forecast_prob
    )
  ) %>%  
  
  #finally keep only the columns I need
  select(question_id, forecaster_id, forecast_prob, 
         resolved_outcome, forecast_date, submitted_at) 

#confirm clamping worked
cat("Zeros after clamping:", 
    sum(forecasts_cleaned$forecast_prob == 0), "\n")
cat("Ones after clamping:", 
    sum(forecasts_cleaned$forecast_prob == 1), "\n")
cat("Values below 0.001:", 
    sum(forecasts_cleaned$forecast_prob < 0.001), "\n")
cat("Values above 0.999:", 
    sum(forecasts_cleaned$forecast_prob > 0.999), "\n")

#row count before sanity checks
cat("Row count:", nrow(forecasts_cleaned), "\n")

# duplicate check
dup_count <- forecasts_cleaned %>%
  group_by(question_id, forecaster_id, submitted_at) %>%
  filter(n() > 1) %>%
  nrow()
cat("Duplicate rows (same forecaster, question, timestamp):", dup_count, "\n")
#showed 994 rows initially, de-duplicated below 

# inspect a sample of duplicates before de-duplication
# optional development check (fast, non-essential)
# forecasts_cleaned %>%
#  group_by(question_id, forecaster_id, submitted_at) %>%
#  filter(n() > 1) %>%
#  arrange(question_id, forecaster_id, submitted_at) %>%
#  head(20)


# 994 rows share identical question_id, forecaster_id, and submitted_at
# but carry different forecast probabilities - a possible raw-data artefact 
# where multiple submissions share the same timestamp.
# Resolution: keep the higher probability value per duplicate group.
# This choice is an arbitrary judgement call under uncertainty;
# retaining the lower values would be equally defensible.
# Verification: the full run with slice_min produced identical Brier scores. 
# This affects 0.7% of rows (524 removed from 138,560).


# de-duplicate the flagged rows
forecasts_cleaned <- forecasts_cleaned %>%
  group_by(question_id, forecaster_id, submitted_at) %>%
  slice_max(forecast_prob, n = 1, with_ties = FALSE) %>%
  ungroup()

#check if cleaning worked
dup_count_after <- forecasts_cleaned %>%
  group_by(question_id, forecaster_id, submitted_at) %>%
  filter(n() > 1) %>%
  nrow()
cat("Duplicate rows after deduplication:", dup_count_after, "\n")
cat("Rows after deduplication:", nrow(forecasts_cleaned), "\n")

# full column class check
str(forecasts_cleaned)
# all valid

# value range check
cat("forecast_prob range:", 
    round(min(forecasts_cleaned$forecast_prob), 4), "to",
    round(max(forecasts_cleaned$forecast_prob), 4), "\n")
cat("resolved_outcome values:", 
    paste(sort(unique(forecasts_cleaned$resolved_outcome)), collapse = ", "), "\n")


#sanity checks after cleaning
stopifnot(is.numeric(forecasts_cleaned$forecast_prob))
stopifnot(is.numeric(forecasts_cleaned$resolved_outcome))
stopifnot(sum(is.na(forecasts_cleaned$forecast_prob)) == 0)
stopifnot(sum(is.na(forecasts_cleaned$resolved_outcome)) == 0)
stopifnot(all(forecasts_cleaned$forecast_prob > 0))
stopifnot(all(forecasts_cleaned$forecast_prob < 1))
stopifnot(all(prediction_sets_raw$created.at == prediction_sets_raw$filled.at))

cat("Rows after cleaning:", nrow(forecasts_cleaned), "\n")
cat("Unique Forecasters:", n_distinct(forecasts_cleaned$forecaster_id), "\n")
cat("Unique questions:", n_distinct(forecasts_cleaned$question_id), "\n")
cat("Missing values in forecast_prob:", sum(is.na(forecasts_cleaned$forecast_prob)), "\n")
cat("Missing values in resolved_outcome:", sum(is.na(forecasts_cleaned$resolved_outcome)), "\n")
cat("Any forecast_prob = 0?:", sum(forecasts_cleaned$forecast_prob == 0), "\n")
cat("Any forecast_prob = 1?:", sum(forecasts_cleaned$forecast_prob == 1), "\n")


#look at the distribution of forecast probabilities - reasonable or not?
hist(forecasts_cleaned$forecast_prob,
     main = "Distribution of individual forecast probabilities",
     xlab = "Forecast probability",
     breaks = 20,
     col = "#E07B54")
# exploratory check: individual forecasts are concentrated near low probabilities.
# consistent with the low positive-resolution rate in the evaluated binary set.


#calculate median of cleaned individual probability forecasts
#this provides the baseline distribution location discussed in Step 3 of the memo,
#verifying that the raw forecast distribution is heavily skewed toward low probabilities.
cat("Median individual forecast:", median(forecasts_cleaned$forecast_prob), "\n")



# *******************************************************
# PART 4: CARRY-FORWARD LOGIC
# Build most recent forecast per forecaster per question per day
# *******************************************************

# The task requires: for each day a question had any forecasts, use the most recent
# forecast from each forecaster as of that day.
# "Most recent as of that day" means carry-forward:
# If forecaster X submitted on Day 1 and never updated, their Day 1 forecast counts
# on day 1, day 2, day 3 and onward through the question's active period.

# How this will work:
# Part 4a - find all active question-days (days where anyone submitted anything)
# Part 4b - for each active question-day, find each forecaster's most recent
# submission on or before that day


# Step 4a: identify all days any question was active
# (at least one forecaster submitted on this question on this day)
active_days <- forecasts_cleaned %>%
  distinct(question_id, forecast_date)

cat("Total active question-day pairs:", nrow(active_days), "\n")


# Step 4b: Carry each forecaster's most recent forecast forward
# Logic: take the latest forecast submitted by each forecaster on
# or before each active day.
# If they never submitted by that day, they don't contribute(filter out NA)

forecasts_daily <- forecasts_cleaned %>%
  group_by(question_id, forecaster_id, forecast_date) %>%
  slice_max(submitted_at, n=1, with_ties = FALSE) %>%
  ungroup()

#find first forecast date per forecaster per question
first_forecast <- forecasts_daily %>%
  group_by(question_id, forecaster_id) %>%
  summarise(first_date = min(forecast_date), .groups = "drop")

# for each active q-day keep only forecasters who had already submitted at least one by that day
active_forecaster_days <- active_days %>%
  inner_join(first_forecast, by = "question_id",
             relationship = 'many-to-many') %>%
  #adding many-to-many relationship is intentional here
  #each question can have multiple active days and each question can also have
  #multiple forecasters so one question_id maps to many rows in both tables
  filter(forecast_date >= first_date) %>%
  rename(active_day = forecast_date)

#fill each forecaster's last known forecast forward to all subsequent active days
most_recent <- active_forecaster_days %>%
  left_join(
    forecasts_daily %>%
      select(question_id, forecaster_id, forecast_date,
             forecast_prob, resolved_outcome, submitted_at),
    by = c("question_id", "forecaster_id", "active_day" = "forecast_date")
  ) %>%
  arrange(question_id, forecaster_id, active_day) %>%
  group_by(question_id, forecaster_id) %>%
  fill(forecast_prob, resolved_outcome, submitted_at, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(forecast_prob))

cat("Question-day-forecaster rows after carry-forward:" , nrow(most_recent), "\n")


#sanity check: how many distinct q-day pairs do we have?
#carry-forward must neither drop nor invent question-days
n_qd_pairs <- most_recent %>%
  distinct(question_id, active_day) %>%
  nrow()
cat("Distinct question-day pairs in most recent:" , n_qd_pairs, "\n")
# equal to active_days(6,667), continue

#sanity check: on a given question-day, how many forecasters contribute on average
most_recent %>%
  count(question_id, active_day) %>%
  summarise(
    mean_forecasters_per_qd = round(mean(n),1),
    min_forecasters_per_qd = min(n),
    max_forecasters_per_qd = max(n)
  ) %>%
  print()


# sanity check: duplicated in most_recent
# exactly one row per forecaster per q-day  
duplicates <- anyDuplicated(paste(most_recent$question_id,
                                  most_recent$active_day,
                                  most_recent$forecaster_id))
cat("Duplicate question-day-forecaster rows:", duplicates, "\n")  
# must return 0


# carry-forward demonstration:
# pick one forecaster who appears multiple times
sample_forecaster <- most_recent %>%
  count(forecaster_id) %>%
  filter(n > 10) %>%
  slice(1) %>%
  pull(forecaster_id)

sample_question <- most_recent %>%
  filter(forecaster_id == sample_forecaster) %>%
  count(question_id) %>%
  slice(1) %>%
  pull(question_id)

most_recent %>%
  filter(forecaster_id == sample_forecaster,
         question_id == sample_question) %>%
  select(active_day, forecast_prob, submitted_at) %>%
  arrange(active_day) %>%
  print(n = 20)
#expected: forecast_prob repeats and submitted_at stays unchanged
#at the single submission timestamp while active_day advances.


# sanity check: row accounting
cat("forecasts_daily rows:", nrow(forecasts_daily), "\n")
cat("first_forecast rows:", nrow(first_forecast), "\n")
cat("active_forecaster_days rows:", nrow(active_forecaster_days), "\n")
# most_recent rows already checked: 4,307,460



# **********************************************************
# PART 5(STEP 2 IN THE TASK): COMPUTE FIVE AGGREGATION METHODS
# **********************************************************

# For each q-day pair, aggregate individual forecasts five ways
# Raw Mean: simple average, everyone weighted equally 
# Median = middle value, robust to extremes
# Geometric Mean = exp(mean(log(p))), pulls toward lower probabilities
# Trimmed Mean = remove top and bottom 10% before averaging
# Geometric Mean of Odds: convert to odds first, then geometric mean, then back
#                         this is symmetric: treats 0.1 and 0.9 as equally extreme
# No minimum forecaster filter applied here.
# The carry-forward outputs in Part 4 ensured every q-day pair
# had at least 3 contributing forecasters.

aggregated <- most_recent %>%
  group_by(question_id, active_day, resolved_outcome) %>%
  summarise(n_forecasters = n(),
            agg_mean = mean(forecast_prob),                       #method 1:mean
            #sort all forecasts and take the middle one
            agg_median = median(forecast_prob),                   #method 2:median
            #equivalent to the nth root of the product of all forecasts
            #tends to produce lower values than the raw mean
            #only possible because of the clamped probabilities handled in part 3
            agg_geomean = exp(mean(log(forecast_prob))),          #method 3:geometric mean
            #trims the most extreme forecasters from both ends
            agg_trimmed = mean(forecast_prob, trim = 0.10),       #method 4:trimmed mean
            #best understood by comparison: raw mean treats 0.1 and 0.9 as symmetric
            #but in odds space: 0.1 = 1:9 odds, 0.9 = 9:1 odds -these ARE symmetric
            #GMO respects this symmetry : convert prob --> odds --> geometric mean --> back to prob
            #See: https://forum.effectivealtruism.org/posts/sMjcjnnpoAQCcedL2/
            #the GMO is preferred in balanced forecasting environments
            #on this heavily skewed dataset the geometric mean of probabilities outperforms it (See Part 6)
            agg_geomean_odds = {                                  # method 5: Geometric mean of odds
              odds <- forecast_prob / (1 - forecast_prob)
              geom_odds <- exp(mean(log(odds)))
              geom_odds  / (1 + geom_odds)
            },                                                    
            .groups = "drop"
  )


cat("Question-day pairs in aggregated output:", nrow(aggregated), "\n")
cat("Questions covered:", n_distinct(aggregated$question_id), "\n")

#also check if the aggregated values look sensible
aggregated %>%
  select(agg_mean,agg_median,agg_geomean,
         agg_trimmed,agg_geomean_odds) %>%
  summary()

#check: do all aggregated values fall between 0 and 1?
problems <- aggregated %>%
  filter(agg_mean < 0 | agg_mean > 1 |
           agg_median < 0 | agg_median > 1 |
           agg_geomean < 0 | agg_geomean > 1 |
           agg_trimmed < 0 | agg_trimmed > 1 |
           agg_geomean_odds < 0 | agg_geomean_odds > 1 )
cat("Rows with aggregated values outside [0,1]:", nrow(problems), "\n")
# expect 0

# verify five aggregated values exist for every question-day pair
# all counts should be 0
cat("Missing values in any aggregation:\n")
aggregated %>%
  summarise(across(starts_with("agg_"), ~sum(is.na(.)))) %>%
  print()



# *********************************************************
# PART 6(STEP 3 IN THE TASK): CALCULATE BRIER SCORES
# Compare aggregated forecasts to resolved outcomes
# *********************************************************

# Brier Score for one forecast = (forecast_probability - outcome)^2
# outcome = 1 if this answer option was correct, 0 otherwise
# Mean Brier score: avg across all question-day pairs
# Lower = better , 0 = perfect , 0.25 = equivalent to always guessing 0.5
# I will use answer.sort.order = 0 consistently as the focal outcome.
# This means my Brier score = (forecast_for_answer_0 - resolved_prob_of_answer_0)^2

scored <- aggregated %>%
  mutate(
    BS_mean = (agg_mean - resolved_outcome)^2,
    BS_median = (agg_median - resolved_outcome)^2,
    BS_geomean = (agg_geomean - resolved_outcome)^2,
    BS_trimmed = (agg_trimmed - resolved_outcome)^2,
    BS_geomean_odds = (agg_geomean_odds - resolved_outcome)^2
  )

#summarise mean Brier scores across all q-day pairs
brier_results <- scored %>%
  summarise(
    `Raw Mean` = mean(BS_mean, na.rm = TRUE),
    `Median` = mean(BS_median, na.rm = TRUE),
    `Geometric Mean` = mean(BS_geomean, na.rm = TRUE),
    `Trimmed Mean` = mean(BS_trimmed, na.rm = TRUE),
    `Geom. Mean of Odds` = mean(BS_geomean_odds, na.rm = TRUE)
  )  %>%
  pivot_longer(everything(),
               names_to = "Method",
               values_to = "Mean_Brier_Score") %>%
  arrange(Mean_Brier_Score)

#print results: LOWER IS BETTER!
print(brier_results)

# sanity check: resolved outcomes should be binary (0 or 1 only)
cat("Unique resolved_outcome values:", 
    paste(sort(unique(aggregated$resolved_outcome)), collapse = ", "), "\n")
cat("Fraction resolving to 1:", 
    round(mean(aggregated$resolved_outcome == 1), 4), "\n")
# 12.57% of question-days resolve to 1:
# the low base rate explains why methods pushing forecasts toward zero perform better

# verify Brier scores are in plausible range (0 to 1)
cat("Brier score range check:\n")
scored %>%
  summarise(across(starts_with("BS_"), 
                   list(min = min, max = max, mean = mean),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything()) %>%
  print()

#save for the memo
write.csv(brier_results, "step3_results_table.csv", row.names = FALSE)
cat("Results saved to step3_results_table.csv\n")

#bar charts of the results for the memo
p_brier <- ggplot(brier_results,
                  aes(x = reorder(Method, Mean_Brier_Score),
                      y = Mean_Brier_Score)) +
  geom_col(fill = "#E07B54", width = 0.6, colour = "black") +
  geom_text(aes(label = round(Mean_Brier_Score, 4)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(
    limits = c(0, max(brier_results$Mean_Brier_Score) * 1.15),
    expand = c(0, 0)
  ) +
  labs(
    title = "Mean Brier Score by Aggregation Method",
    subtitle = "RCTA binary questions; lower scores are better",
    x = "Aggregation Method",
    y = "Mean Brier Score"
  ) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("step3_brier_chart.png",p_brier, width = 8, height = 5, dpi = 300)
cat("Chart saved to step3_brier_chart.png\n")



# **********************************************************
# PART 7(STEP 4 IN THE TASK): IMPROVEMENT - 
# ACCURACY AND STALENESS WEIGHTED GEOMETRIC MEAN
# **********************************************************

# The idea: not all forecasters are equally skilled. If we weight better
# historical forecasters more heavily, the aggregate should be more accurate.
# And not all forecasts are equally valid as well, carried-forward forecasts
# can sit unchanged for weeks.
# Weighting by both past accuracy AND recency should produce 
# better aggregate than either dimension alone. 

# Implementation: 
# 1. Calculate each forecaster's past mean Brier score across all their forecasts
# 2. Calculate how many days old each forecast is 
# 3. Combined weight = accuracy_weight * staleness_weight
# 4. Apply those weights in a weighted geometric mean of probabilities
# 5. Strictly enforce T-1 constraint: only questions resolved before day T
#    enter the accuracy calculation
# 6. Staleness uses only submitted_at and active_day, no future information anywhere.

# Strict T-1 constraint: for each question-day pair (question Q, day T)
# can only use accuracy information from questions that resolved BEFORE day T.
# No future information is used at any point.

# Forecasters with fewer than 5 resolved question-day pairs 
# in their history receive weight 1 (treated as default)
# Their staleness weight is still applied normally, only the accuracy component defaults
# not the combined weight.
# A new forecaster with a 10-day-old forecast still receives
# a combined weight of 1 * (1/11) = 0.091, not 1.


# PART 7a: get resolution date for each question from questions_answer_raw
# restrict to binary/single-outcome questions, answer.sort.order == 0, resolved only

resolution_dates <- questions_answers_raw %>%
  filter(discover.question.id %in% binary_question_ids) %>%
  filter(answer.sort.order == 0) %>%
  filter(!is.na(answer.resolved.probability)) %>%
  transmute(
    question_id = discover.question.id,
    resolved_date = as_date(question.resolved.at)
  )

cat("Questions with resolution dates:", nrow(resolution_dates), "\n")

# Part 7b: join resolution dates onto most_recent
# Each forecast row now carries the date its question was resolved

most_recent_dated <- most_recent %>%
  left_join(resolution_dates, by = "question_id")

cat("Rows with resolution date after join:", 
    sum(!is.na(most_recent_dated$resolved_date)), "\n")
#must equal to most_recent rows: no rows lost or duplicated


# Part 7c: compute individual Brier scores on resolved questions
# These are the scores to build forecaster weights ultimately

individual_scored <- most_recent_dated %>%
  filter(!is.na(resolved_date)) %>%
  mutate(individual_brier = (forecast_prob - resolved_outcome)^2)

cat("Individual scored rows:", nrow(individual_scored), "\n")

#Part 7d: get all unique active days
all_days <- sort(unique(most_recent_dated$active_day))
cat("Unique active days to process:", length(all_days), "\n")


# Part 7e: for each active day T, compute each forecaster's mean Brier score
# using only questions resolved strictly before T.
# This loop will take under a min - this is expected

cat("Computing rolling weights... this will take half a minute. \n")

#note: this loop is iterated over 186 active days. A vectorised implementation using data.table
#or pre-computed cumulative summaries would be faster.
#but this approach prioritises readability for a 10-hour submission

rolling_weights <- map_dfr(all_days, function(T) {
  individual_scored %>%
    filter(resolved_date < T) %>%
    group_by(forecaster_id) %>%
    summarise(
      past_brier = mean(individual_brier, na.rm = TRUE),
      n_resolved = n(),
      .groups = "drop"
    ) %>%
    filter(n_resolved >= 5) %>%
    mutate(
      active_day = T,
      #the small constant 0.01 prevents division by zero for near-perfect forecasters
      #and ensures the weight function is continuous and finite across all Brier values
      weight = 1 / (past_brier + 0.01)
    )
})

#weight sanity check
rolling_weights %>%
  summarise(
    min_weight = min(weight, na.rm = TRUE),
    q25_weight = quantile(weight, 0.25, na.rm = TRUE),
    median_weight = median(weight, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    q75_weight = quantile(weight, 0.75, na.rm = TRUE),
    max_weight = max(weight, na.rm = TRUE)
  ) %>%
  print()

cat("Rolling weight rows computed:", nrow(rolling_weights), "\n")
cat("Unique days with weight data:",
    n_distinct(rolling_weights$active_day), "\n")


# PART 7f: compute staleness weights
# carried-forward forecasts lose weight as they age
# age = number of days between submission and the active scoring day
# weight = 1 / (1 + age): same-day submission gets full weight
# a forecast 9 days old gets 1/10 of the weight
# this uses only submitted_at and active_day, no future information

most_recent_stale <- most_recent %>%
  mutate(
    #the number of days between submission and the scoring day
    age_days = as.numeric(active_day - as_date(submitted_at)),
    #pmax defensively prevents any negative age from producing a weight above 1
    #stale_w ranges from 1(submitted today) to near 0(submitted many weeks ago)
    #a forecast 9 days old receives weight 1/10
    stale_w = 1 / (1+ pmax(age_days, 0)) 
  )

# Staleness weight validation — run during development, all confirmed
# Check 1- age range (min= 0, max= 184, mean= 36.5, median= 26) — reasonable
# Check 2- stale_w range (min= 0.0054, max= 1, mean= 0.104, median= 0.037)
# Check 3- same-day forecasts all receive stale_w = 1 exactly — TRUE
# Check 4- no negative age_days — 0 negative rows

# Uncomment to reproduce:
# most_recent_stale %>%
#   summarise(min_age = min(age_days), max_age = max(age_days),
#             mean_age = round(mean(age_days),1),
#             median_age = median(age_days)) %>% print()
# most_recent_stale %>%
#   summarise(min_stale_w = round(min(stale_w),4),
#             max_stale_w = max(stale_w),
#             mean_stale_w = round(mean(stale_w),4),
#             median_stale_w = round(median(stale_w),4)) %>% print()
# most_recent_stale %>%
#   filter(age_days == 0) %>%
#   summarise(all_weight_one = all(stale_w == 1)) %>% print()
# cat("Negative age rows:", sum(most_recent_stale$age_days < 0), "\n")



# Part 7g: combine accuracy and staleness weights
# accuracy weight : from rolling Brier history (Part 7e)
# staleness weight: from forecast age (Part 7f)
# combined weight = accuracy_weight * staleness_weight
# a forecast from a skilled forecaster submitted recently gets the highest weight
# a forecast from a poor forecaster submitted weeks ago gets the lowest
# forecasters with insufficient history receive accuracy weight = 1 (default)

improved <- most_recent_stale %>%
  left_join(
    rolling_weights %>% 
      select(forecaster_id, active_day, weight),
    by = c("forecaster_id", "active_day")
  ) %>%
  mutate(
    acc_w = if_else(is.na(weight), 1, weight),
    #combined_w rewards forecasters who are both skilled and timely
    #the product means neither signal alone is sufficient for high weight
    combined_w = acc_w * stale_w
  ) %>%
  group_by(question_id, active_day, resolved_outcome) %>%
  summarise(
    n_forecasters = n(),
    agg_weighted_geomean = exp(
      sum(combined_w * log(forecast_prob)) / sum(combined_w)
    ),
    .groups = "drop"
  )

cat("Improved aggregation rows:", nrow(improved), "\n")
#should match n_qd_pairs (6667), each q-day produces one weighted aggregate


# Part 7h: score the improvement

improved_scored <- improved %>%
  mutate(BS_weighted_geomean = (agg_weighted_geomean - resolved_outcome)^2)

improved_brier <- mean(improved_scored$BS_weighted_geomean, na.rm = TRUE)


# Part 7i: final comparison table

best_baseline_row <- brier_results[1, ]
best_method_name <- best_baseline_row$Method
best_method_brier <- best_baseline_row$Mean_Brier_Score

cat("\n** STEP 4:IMPROVEMENT RESULTS **\n")
cat("Best baseline method:", best_method_name, "\n")
cat("Baseline Brier score:", round(best_method_brier, 5), "\n")
cat("Accuracy + Staleness Weighted Geometric Mean Brier score:",
    round(improved_brier, 5), "\n")
cat("Improvement:", round(best_method_brier - improved_brier, 5),
    "(positive = better)\n")


final_table <- brier_results %>%
  bind_rows(tibble(
    Method = "Accuracy + Staleness Weighted Geometric Mean",
    Mean_Brier_Score = improved_brier
  )) %>%
  arrange(Mean_Brier_Score) %>%
  mutate(Rank = row_number())

print(final_table)
write.csv(final_table, "step4_final_comparison.csv", row.names = FALSE)
cat("Final table saved.\n")
# 12.57% of q-days resolve to 1 (event occurred)
# this low base rate is the reason why downward-compressing methods
# outperform the raw mean
cat("Fraction resolving to 1:", mean(aggregated$resolved_outcome == 1), "\n")

#disaggregate improvement by resolved outcome
outcome_comparison <- improved_scored %>%
  left_join(
    scored %>%
      select(question_id, active_day, BS_geomean),
    by = c("question_id", "active_day")
  ) %>%
  group_by(resolved_outcome) %>%
  summarise(
    BS_weighted = mean(BS_weighted_geomean, na.rm = TRUE),
    BS_baseline = mean(BS_geomean , na.rm = TRUE),
    pct_improvement = round(
      100 * (BS_baseline - BS_weighted) / BS_baseline, 1),
    n_qd_pairs = n(),
    .groups = "drop"
    )
  
cat("\nBrier score breakdown by resolved outcome:\n")
print(outcome_comparison)

cat("\nScript complete. Output files: step3_results_table.csv, step4_final_comparison.csv, step3_brier_chart.png\n")
# end of script


