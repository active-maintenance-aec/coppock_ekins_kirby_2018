# coppock_ekins_kirby_2018/ground_truth/transcribe_published_claims.R
# Output: ground_truth/published_claims.csv
# Depends on: nothing; every value below was read out of the published article
# Description: The extraction. Every numeric token the published article prints, with a
#   claim id, the section or float it sits in, its class, and whether it requires a block
#   in maintained/in_text_claims.R. Float cells were read off 150 dpi renders of the
#   published pages rather than off pdftotext, which mis-maps a multi-column regression
#   row by one column and has manufactured a phantom typeset erratum in this paper before.
#   Spelled-out numbers ("five hypotheses", "two to three op-eds per day", "four or five
#   questions") were swept for separately; no token scan sees them.
#
#   value_paper is the string the article prints, with the leading zero and the minus sign
#   normalised and thousands separators removed, and with the printed number of decimals
#   preserved: 0.80 and 0.8 are the same double, so the string is the only record of the
#   precision a comparison has to match.
#
#   This file is run once and its output is the committed artifact. Nothing downstream
#   reads it.

library(here)
library(tidyverse)

here::i_am("ground_truth/transcribe_published_claims.R")

slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("^_") |>
    str_remove("_$")
}

rows <- list()
add <- function(claim_id, location, claim_type, value_paper, needs_block) {
  rows[[length(rows) + 1]] <<- tibble(claim_id = claim_id, location = location,
                                      claim_type = claim_type,
                                      value_paper = as.character(value_paper),
                                      needs_block = needs_block)
}

# Design tables (Tables 1 and 5) ----
design_table <- function(prefix, location, arms, rows_mat) {
  for (i in seq_len(nrow(rows_mat$values))) {
    wave_row <- rows_mat$labels[i]
    for (j in seq_along(arms)) {
      add(str_c(prefix, "_", slug(str_c(wave_row, " / ", arms[j]))), location,
          "pipeline", rows_mat$values[i, j], TRUE)
    }
  }
}

t1 <- list(
  labels = c("Wave 1 / N", "Wave 2 / Distraction", "Wave 2 / No distraction",
             "Wave 2 / Responded", "Wave 2 / Did not respond",
             "Wave 3 / Responded", "Wave 3 / Did not respond"),
  values = rbind(
    c(622, 597, 570, 587, 592, 603, 3571),
    c(263, 240, 252, 243, 256, 243, 1497),
    c(253, 247, 240, 243, 257, 261, 1501),
    c(516, 487, 492, 486, 513, 504, 2998),
    c(106, 110, 78, 101, 79, 99, 573),
    c(433, 412, 386, 386, 412, 422, 2451),
    c(189, 185, 184, 201, 180, 181, 1120)
  )
)
design_table("t1", "Table 1 (p. 67)",
             c("Control", "Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street", "Totals"),
             t1)

t5 <- list(
  labels = c("Wave 1 / N", "Wave 2 / Distraction", "Wave 2 / No distraction",
             "Wave 2 / Responded", "Wave 2 / Did not respond"),
  values = rbind(
    c(448, 407, 463, 438, 425, 2181),
    c(139, 135, 126, 132, 138, 670),
    c(141, 126, 141, 148, 132, 688),
    c(280, 261, 267, 280, 270, 1358),
    c(168, 146, 196, 158, 155, 823)
  )
)
design_table("t5", "Table 5 (p. 72)",
             c("Control", "Amtrak", "Flat Tax", "Veterans", "Wall Street", "Totals"),
             t5)

# Regression tables (Tables 3, 4, 6, 7, 8) ----
# est and se are character matrices so the article's printed precision survives.
reg_table <- function(prefix, location, dvs, terms, est, se, n, r2) {
  for (j in seq_along(dvs)) {
    for (i in seq_along(terms)) {
      base <- str_c(prefix, "_", slug(str_c(dvs[j], " / ", terms[i])))
      add(str_c(base, "_estimate"), location, "pipeline", est[i, j], TRUE)
      add(str_c(base, "_std_error"), location, "pipeline", se[i, j], TRUE)
    }
  }
  for (j in seq_along(dvs)) {
    add(str_c(prefix, "_", slug(dvs[j]), "_r2"), location, "pipeline", r2[j], TRUE)
  }
  for (j in seq_along(dvs)) {
    add(str_c(prefix, "_", slug(dvs[j]), "_n"), location, "pipeline", n[j], TRUE)
  }
}

mturk_dvs <- c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street")
mturk_terms <- c("Op-ed: Amtrak", "Op-ed: Climate", "Op-ed: Flat Tax",
                 "Op-ed: Veterans", "Op-ed: Wall Street", "Constant")

reg_table(
  "t3", "Table 3 (p. 70)", mturk_dvs, mturk_terms,
  est = rbind(
    c("0.440", "-0.020", "0.056", "-0.056", "-0.035"),
    c("0.054", "0.427", "0.132", "-0.009", "0.026"),
    c("0.195", "0.130", "0.850", "-0.033", "-0.018"),
    c("0.047", "0.032", "-0.106", "0.770", "-0.124"),
    c("0.038", "0.079", "0.151", "-0.210", "0.915"),
    c("2.902", "2.773", "3.738", "4.502", "2.616")
  ),
  se = rbind(
    c("0.085", "0.095", "0.108", "0.082", "0.079"),
    c("0.079", "0.098", "0.110", "0.082", "0.080"),
    c("0.078", "0.097", "0.109", "0.082", "0.079"),
    c("0.079", "0.095", "0.110", "0.078", "0.078"),
    c("0.078", "0.095", "0.107", "0.084", "0.082"),
    c("0.054", "0.065", "0.076", "0.058", "0.054")
  ),
  n = rep("3571", 5),
  r2 = c("0.012", "0.008", "0.026", "0.048", "0.061")
)

reg_table(
  "t4", "Table 4 (p. 71)", mturk_dvs, mturk_terms,
  est = rbind(
    c("0.501", "0.018", "0.005", "0.051", "-0.034"),
    c("0.078", "0.276", "0.045", "-0.007", "0.008"),
    c("0.080", "0.110", "0.488", "0.048", "-0.023"),
    c("0.044", "0.003", "-0.026", "0.646", "-0.090"),
    c("0.055", "0.067", "0.097", "-0.113", "0.698"),
    c("-0.125", "-0.079", "-0.101", "-0.103", "-0.096")
  ),
  se = rbind(
    c("0.059", "0.056", "0.056", "0.056", "0.054"),
    c("0.056", "0.057", "0.057", "0.056", "0.055"),
    c("0.055", "0.056", "0.060", "0.056", "0.056"),
    c("0.058", "0.057", "0.057", "0.054", "0.054"),
    c("0.055", "0.057", "0.057", "0.057", "0.056"),
    c("0.039", "0.039", "0.039", "0.040", "0.038")
  ),
  n = rep("3571", 5),
  r2 = c("0.029", "0.009", "0.030", "0.063", "0.076")
)

elite_dvs <- c("Amtrak", "Flat Tax", "Veterans", "Wall Street")
elite_terms <- c("Op-ed: Amtrak", "Op-ed: Flat Tax", "Op-ed: Veterans",
                 "Op-ed: Wall Street", "Constant")

reg_table(
  "t6", "Table 6 (p. 73)", elite_dvs, elite_terms,
  est = rbind(
    c("0.438", "-0.094", "0.069", "0.071"),
    c("-0.023", "0.411", "-0.060", "-0.004"),
    c("-0.004", "-0.156", "0.045", "0.119"),
    c("0.042", "0.067", "-0.055", "0.791"),
    c("2.304", "3.578", "4.585", "2.926")
  ),
  se = rbind(
    c("0.110", "0.150", "0.093", "0.103"),
    c("0.101", "0.144", "0.091", "0.101"),
    c("0.103", "0.147", "0.091", "0.101"),
    c("0.109", "0.148", "0.093", "0.105"),
    c("0.073", "0.103", "0.064", "0.071")
  ),
  n = rep("2181", 4),
  r2 = c("0.012", "0.008", "0.001", "0.037")
)

reg_table(
  "t7", "Table 7 (p. 74)", elite_dvs, elite_terms,
  est = rbind(
    c("0.303", "-0.014", "0.131", "0.010"),
    c("-0.084", "0.104", "-0.0005", "-0.039"),
    c("-0.041", "-0.044", "0.153", "0.066"),
    c("-0.021", "0.077", "-0.032", "0.571"),
    c("0.012", "0.002", "-0.043", "-0.123")
  ),
  se = rbind(
    c("0.073", "0.070", "0.065", "0.063"),
    c("0.067", "0.070", "0.062", "0.062"),
    c("0.069", "0.070", "0.063", "0.063"),
    c("0.071", "0.069", "0.064", "0.060"),
    c("0.048", "0.049", "0.044", "0.044")
  ),
  n = rep("2181", 4),
  r2 = c("0.016", "0.003", "0.006", "0.057")
)

t8_terms <- c("Op-ed: Amtrak", "Op-ed: Flat Tax", "Op-ed: Veterans", "Op-ed: Wall Street",
              "Elite Experiment", "Elite X Amtrak", "Elite X Flat Tax",
              "Elite X Veterans", "Elite X Wall Street", "Constant")

reg_table(
  "t8", "Table 8 (p. 75)", elite_dvs, t8_terms,
  est = rbind(
    c("0.501", "0.005", "0.051", "-0.034"),
    c("0.080", "0.488", "0.048", "-0.023"),
    c("0.044", "-0.026", "0.646", "-0.090"),
    c("0.055", "0.097", "-0.113", "0.698"),
    c("0.137", "0.103", "0.060", "-0.027"),
    c("-0.198", "-0.018", "0.080", "0.044"),
    c("-0.164", "-0.384", "-0.048", "-0.016"),
    c("-0.085", "-0.019", "-0.493", "0.156"),
    c("-0.076", "-0.020", "0.081", "-0.127"),
    c("-0.125", "-0.101", "-0.103", "-0.096")
  ),
  se = rbind(
    c("0.059", "0.056", "0.056", "0.054"),
    c("0.055", "0.060", "0.056", "0.056"),
    c("0.058", "0.057", "0.054", "0.054"),
    c("0.055", "0.057", "0.057", "0.056"),
    c("0.062", "0.063", "0.059", "0.058"),
    c("0.094", "0.090", "0.086", "0.083"),
    c("0.087", "0.092", "0.084", "0.083"),
    c("0.090", "0.090", "0.083", "0.083"),
    c("0.090", "0.089", "0.086", "0.082"),
    c("0.039", "0.039", "0.040", "0.038")
  ),
  n = rep("5182", 4),
  r2 = c("0.026", "0.021", "0.045", "0.076")
)

# Table 9 ----
t9 <- tribble(
  ~sample,  ~issue,        ~dv_type,   ~value,
  "MTurk",  "Amtrak",      "Main DV",  "0.045",
  "MTurk",  "Climate",     "Main DV",  "0.845",
  "MTurk",  "Flat Tax",    "Main DV",  "0.009",
  "MTurk",  "Veterans",    "Main DV",  "0.878",
  "MTurk",  "Wall Street", "Main DV",  "0.310",
  "MTurk",  "Amtrak",      "Scale DV", "0.029",
  "MTurk",  "Climate",     "Scale DV", "0.243",
  "MTurk",  "Flat Tax",    "Scale DV", "0.001",
  "MTurk",  "Veterans",    "Scale DV", "0.562",
  "MTurk",  "Wall Street", "Scale DV", "0.371",
  "Elite",  "Amtrak",      "Main DV",  "0.436",
  "Elite",  "Flat Tax",    "Main DV",  "0.146",
  "Elite",  "Veterans",    "Main DV",  "0.434",
  "Elite",  "Wall Street", "Main DV",  "0.316",
  "Elite",  "Amtrak",      "Scale DV", "0.342",
  "Elite",  "Flat Tax",    "Scale DV", "0.293",
  "Elite",  "Veterans",    "Scale DV", "0.291",
  "Elite",  "Wall Street", "Scale DV", "0.265"
)
for (i in seq_len(nrow(t9))) {
  add(str_c("t9_", slug(str_c(t9$sample[i], " / ", t9$issue[i], " / ", t9$dv_type[i],
                              " / F test p"))),
      "Table 9 (p. 77)", "pipeline", t9$value[i], TRUE)
}

# Table 11 ----
t11 <- tribble(
  ~issue,                        ~sample, ~split, ~est,   ~se,
  "Amtrak",                      "Elite", "25th", "0.04", "0.03",
  "Amtrak",                      "MTurk", "25th", "0.12", "0.02",
  "Amtrak",                      "Elite", "50th", "0.10", "0.03",
  "Amtrak",                      "MTurk", "50th", "0.15", "0.03",
  "Amtrak",                      "Elite", "75th", "0.09", "0.03",
  "Amtrak",                      "MTurk", "75th", "0.23", "0.03",
  "Climate",                     "MTurk", "25th", "0.10", "0.02",
  "Climate",                     "MTurk", "50th", "0.11", "0.03",
  "Climate",                     "MTurk", "75th", "0.10", "0.03",
  "Flat Tax",                    "Elite", "25th", "0.02", "0.03",
  "Flat Tax",                    "MTurk", "25th", "0.08", "0.02",
  "Flat Tax",                    "Elite", "50th", "0.02", "0.03",
  "Flat Tax",                    "MTurk", "50th", "0.20", "0.03",
  "Flat Tax",                    "Elite", "75th", "0.06", "0.03",
  "Flat Tax",                    "MTurk", "75th", "0.18", "0.03",
  "Veterans",                    "Elite", "25th", "0.02", "0.03",
  "Veterans",                    "MTurk", "25th", "0.17", "0.02",
  "Veterans",                    "Elite", "50th", "0.08", "0.03",
  "Veterans",                    "MTurk", "50th", "0.28", "0.03",
  "Veterans",                    "Elite", "75th", "0.09", "0.03",
  "Veterans",                    "MTurk", "75th", "0.29", "0.03",
  "Wall Street",                 "Elite", "25th", "0.14", "0.02",
  "Wall Street",                 "MTurk", "25th", "0.16", "0.02",
  "Wall Street",                 "Elite", "50th", "0.26", "0.03",
  "Wall Street",                 "MTurk", "50th", "0.28", "0.03",
  "Wall Street",                 "Elite", "75th", "0.21", "0.03",
  "Wall Street",                 "MTurk", "75th", "0.27", "0.03",
  "Precision weighted average",  "Elite", "25th", "0.06", "0.01",
  "Precision weighted average",  "MTurk", "25th", "0.13", "0.01",
  "Precision weighted average",  "Elite", "50th", "0.12", "0.02",
  "Precision weighted average",  "MTurk", "50th", "0.20", "0.01",
  "Precision weighted average",  "Elite", "75th", "0.11", "0.02",
  "Precision weighted average",  "MTurk", "75th", "0.21", "0.01"
)
for (i in seq_len(nrow(t11))) {
  base <- str_c("t11_", slug(str_c(t11$sample[i], " / ", t11$issue[i], " / ", t11$split[i])))
  add(str_c(base, "_estimate"), "Table 11 (p. 81)", "pipeline", t11$est[i], TRUE)
  add(str_c(base, "_std_error"), "Table 11 (p. 81)", "pipeline", t11$se[i], TRUE)
}

# Table 10, hand-entered circulation arithmetic the deposit was never going to record ----
t10 <- tribble(
  ~outlet,               ~circulation, ~share, ~read, ~unique,
  "New York Times",      "2134150",    "95",   "0.25", "506861",
  "Wall Street Journal", "2276207",    "97",   "0.25", "551980",
  "USA Today",           "4139380",    "50",   "0.25", "517423",
  "Newsweek",            "433333",     "50",   "0.25", "54167"
)
for (i in seq_len(nrow(t10))) {
  o <- slug(t10$outlet[i])
  add(str_c("t10_", o, "_circulation"), "Table 10 (p. 80)", "definitional", t10$circulation[i], FALSE)
  add(str_c("t10_", o, "_opinion_share"), "Table 10 (p. 80)", "definitional", t10$share[i], FALSE)
  add(str_c("t10_", o, "_opeds_read"), "Table 10 (p. 80)", "definitional", t10$read[i], FALSE)
  add(str_c("t10_", o, "_unique_readers"), "Table 10 (p. 80)", "definitional", t10$unique[i], FALSE)
}
add("t10_average_unique_readers", "Table 10 (p. 80)", "definitional", "407608", FALSE)

# Table 2 prints no numbers at all: it is op-ed titles, authors and outlets ----
add("t2_no_numeric_content", "Table 2 (p. 68)", "structural", NA, FALSE)

# Figures 1 and 2 ----
add("fig1_n_estimates", "Figure 1 (p. 76)", "pipeline", NA, TRUE)
add("fig1_axis_break_low", "Figure 1 (p. 76)", "structural", "0.0", FALSE)
add("fig1_axis_break_high", "Figure 1 (p. 76)", "structural", "1.5", FALSE)
add("fig2_n_group_means", "Figure 2 (p. 79)", "pipeline", NA, TRUE)
add("fig2_axis_day_0", "Figure 2 (p. 79)", "structural", "0", FALSE)
add("fig2_axis_day_10", "Figure 2 (p. 79)", "structural", "10", FALSE)
add("fig2_axis_day_30", "Figure 2 (p. 79)", "structural", "30", FALSE)

# Prose ----
prose <- tribble(
  ~claim_id,                   ~location,                      ~claim_type,    ~value_paper, ~needs_block,
  # Abstract, p. 59
  "ab_n_experiments",          "Abstract (p. 59)",             "structural",   "2",     FALSE,
  "ab_n_opeds",                "Abstract (p. 59)",             "structural",   "5",     FALSE,
  "ab_target_shift",           "Abstract (p. 59)",             "descriptive",  "0.5",   TRUE,
  "ab_scale_points",           "Abstract (p. 59)",             "definitional", "7",     FALSE,
  "ab_persistence_months",     "Abstract (p. 59)",             "structural",   "1",     FALSE,
  "ab_nontarget_small",        "Abstract (p. 59)",             "descriptive",  NA,      TRUE,
  "ab_parties_similar",        "Abstract (p. 59)",             "descriptive",  NA,      TRUE,
  # Introduction, pp. 60-65
  "in_oped_debut_day",         "Introduction (p. 60)",         "transcribed",  "21",    FALSE,
  "in_oped_debut_year",        "Introduction (p. 60)",         "transcribed",  "1970",  FALSE,
  "in_years_since_debut",      "Introduction (p. 60)",         "structural",   "40",    FALSE,
  "in_opeds_per_day_low",      "Introduction (p. 60)",         "transcribed",  "2",     FALSE,
  "in_opeds_per_day_high",     "Introduction (p. 60)",         "transcribed",  "3",     FALSE,
  "in_form_decade",            "Introduction (p. 60)",         "transcribed",  "1970",  FALSE,
  "fn1_chicago_year",          "Footnote 1 (p. 60)",           "transcribed",  "1912",  FALSE,
  "fn1_post_decade",           "Footnote 1 (p. 60)",           "transcribed",  "1930",  FALSE,
  "fn1_lat_decade",            "Footnote 1 (p. 60)",           "transcribed",  "1950",  FALSE,
  "in_cato_year",              "Introduction (p. 61)",         "transcribed",  "2015",  FALSE,
  "in_cato_opeds",             "Introduction (p. 61)",         "transcribed",  "944",   FALSE,
  "in_cato_top_opeds",         "Introduction (p. 61)",         "transcribed",  "73",    FALSE,
  "in_cato_top_papers",        "Introduction (p. 61)",         "transcribed",  "10",    FALSE,
  "in_brookings_opeds",        "Introduction (p. 61)",         "transcribed",  "116",   FALSE,
  "in_brookings_year",         "Introduction (p. 61)",         "transcribed",  "2015",  FALSE,
  "in_aei_opeds",              "Introduction (p. 61)",         "transcribed",  "3385",  FALSE,
  "fn3_start_year",            "Footnote 3 (p. 61)",           "structural",   "2015",  FALSE,
  "fn3_end_year",              "Footnote 3 (p. 61)",           "structural",   "2015",  FALSE,
  "in_ghostwrite_low",         "Introduction (p. 62)",         "definitional", "5000",  FALSE,
  "in_ghostwrite_high",        "Introduction (p. 62)",         "definitional", "25000", FALSE,
  "in_n_experiments",          "Introduction (p. 62)",         "structural",   "2",     FALSE,
  "in_n_followups_mturk",      "Introduction (p. 62)",         "structural",   "2",     FALSE,
  "in_n_followups_elite",      "Introduction (p. 62)",         "structural",   "1",     FALSE,
  "in_n_results",              "Introduction (p. 62)",         "structural",   "3",     FALSE,
  "lit_jerit_outcomes",        "Previous Literature (p. 64)",  "transcribed",  "17",    FALSE,
  "lit_jerit_significant",     "Previous Literature (p. 64)",  "transcribed",  "1",     FALSE,
  "lit_n_hypotheses",          "Previous Literature (p. 65)",  "structural",   "5",     FALSE,
  # Study 1: Mechanical Turk, p. 66
  "s1_enrolled",               "Study 1: Mechanical Turk (p. 66)", "pipeline",    "3567", TRUE,
  "s1_n_waves",                "Study 1: Mechanical Turk (p. 66)", "structural",  "3",    FALSE,
  "s1_payment_per_wave",       "Study 1: Mechanical Turk (p. 66)", "definitional","1.00", FALSE,
  "s1_n_treatment_opeds",      "Study 1: Mechanical Turk (p. 66)", "structural",  "5",    FALSE,
  "s1_w2_delay_days",          "Study 1: Mechanical Turk (p. 66)", "definitional","10",   FALSE,
  "s1_w3_delay_days",          "Study 1: Mechanical Turk (p. 66)", "definitional","30",   FALSE,
  "s1_recontact_w2",           "Study 1: Mechanical Turk (p. 66)", "pipeline",    "84",   TRUE,
  "s1_recontact_w3",           "Study 1: Mechanical Turk (p. 66)", "pipeline",    "69",   TRUE,
  "s1_chisq_w2_p",             "Study 1: Mechanical Turk (p. 66)", "pipeline",    "0.10", TRUE,
  "s1_chisq_w3_p",             "Study 1: Mechanical Turk (p. 66)", "pipeline",    "0.61", TRUE,
  "s1_n_analysis_approaches",  "Study 1: Mechanical Turk (p. 66)", "structural",  "2",    FALSE,
  # Study 1: Treatments, pp. 67-68
  "s1_cato_authored",          "Study 1: Treatments (p. 67)",  "structural",   "4",     FALSE,
  "s1_paul_authored",          "Study 1: Treatments (p. 67)",  "structural",   "5",     FALSE,
  "s1_flat_tax_rate",          "Study 1: Treatments (p. 67)",  "transcribed",  "14",    FALSE,
  "s1_flat_tax_threshold",     "Study 1: Treatments (p. 68)",  "transcribed",  "50000", FALSE,
  "s1_reading_minutes_low",    "Study 1: Treatments (p. 68)",  "pipeline",     "2",     TRUE,
  "s1_reading_minutes_high",   "Study 1: Treatments (p. 68)",  "pipeline",     "4",     TRUE,
  # Study 1: Outcome Measures, pp. 68-69
  "s1_questions_per_issue_low", "Study 1: Outcome Measures (p. 68)", "definitional", "4", FALSE,
  "s1_questions_per_issue_high","Study 1: Outcome Measures (p. 68)", "definitional", "5", FALSE,
  "s1_n_presentations",        "Study 1: Outcome Measures (p. 68)", "structural",   "3", FALSE,
  "s1_main_dv_scale_points",   "Study 1: Outcome Measures (p. 68)", "definitional", "7", FALSE,
  "s1_n_factors",              "Study 1: Outcome Measures (p. 69)", "definitional", "2", FALSE,
  "s1_control_agreement_share","Study 1: Outcome Measures (p. 69)", "definitional", "50", FALSE,
  "s1_robustness_split_low",   "Study 1: Outcome Measures (p. 69)", "definitional", "25", FALSE,
  "s1_robustness_split_high",  "Study 1: Outcome Measures (p. 69)", "definitional", "75", FALSE,
  "dv_amtrak_scale_points",    "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_amtrak_scale_low",       "Study 1: Outcome Measures (p. 69)", "definitional", "1", FALSE,
  "dv_amtrak_scale_high",      "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_climate_scale_points",   "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_climate_scale_low",      "Study 1: Outcome Measures (p. 69)", "definitional", "1", FALSE,
  "dv_climate_scale_high",     "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_flat_scale_points",      "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_flat_scale_low",         "Study 1: Outcome Measures (p. 69)", "definitional", "1", FALSE,
  "dv_flat_scale_high",        "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_flat_threshold",         "Study 1: Outcome Measures (p. 69)", "definitional", "50000", FALSE,
  "dv_vets_scale_points",      "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_vets_scale_low",         "Study 1: Outcome Measures (p. 69)", "definitional", "1", FALSE,
  "dv_vets_scale_high",        "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_wall_scale_points",      "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  "dv_wall_scale_low",         "Study 1: Outcome Measures (p. 69)", "definitional", "1", FALSE,
  "dv_wall_scale_high",        "Study 1: Outcome Measures (p. 69)", "definitional", "7", FALSE,
  # Study 1: Results, pp. 69-71
  "s1_all_dvs",                "Study 1: Results (p. 70)",     "structural",   "21",    FALSE,
  "s1_n_treatments",           "Study 1: Results (p. 70)",     "structural",   "5",     FALSE,
  "s1_main_target_min",        "Study 1: Results (p. 70)",     "pipeline",     "0.427", TRUE,
  "s1_main_scale_low",         "Study 1: Results (p. 70)",     "definitional", "1",     FALSE,
  "s1_main_scale_high",        "Study 1: Results (p. 70)",     "definitional", "7",     FALSE,
  "s1_main_target_max",        "Study 1: Results (p. 70)",     "pipeline",     "0.915", TRUE,
  "s1_main_target_sig_p",      "Study 1: Results (p. 70)",     "descriptive",  "0.001", TRUE,
  "s1_nontarget_cells",        "Study 1: Results (p. 70)",     "structural",   "20",    TRUE,
  "s1_nontarget_sig_count",    "Study 1: Results (p. 70)",     "descriptive",  "2",     TRUE,
  "s1_scale_target_min",       "Study 1: Results (p. 70)",     "pipeline",     "0.276", TRUE,
  "s1_scale_target_range_low", "Study 1: Results (p. 70)",     "descriptive",  "0.5",   TRUE,
  "s1_scale_target_range_high","Study 1: Results (p. 70)",     "descriptive",  "0.7",   TRUE,
  "s1_scale_cross_issue_small","Study 1: Results (p. 70)",     "descriptive",  NA,      TRUE,
  # Study 2: Elites, pp. 71-72
  "s2_email_list",             "Study 2: Elites (p. 71)",      "definitional", "32498", FALSE,
  "s2_full_design_arms",       "Study 2: Elites (p. 72)",      "structural",   "6",     FALSE,
  "s2_reminders_wave1",        "Study 2: Elites (p. 72)",      "structural",   "2",     FALSE,
  "s2_completed",              "Study 2: Elites (p. 72)",      "pipeline",     "2169",  TRUE,
  "s2_followup_delay_days",    "Study 2: Elites (p. 72)",      "definitional", "10",    FALSE,
  "s2_reminders_wave2",        "Study 2: Elites (p. 72)",      "structural",   "2",     FALSE,
  "s2_w2_complete",            "Study 2: Elites (p. 72)",      "pipeline",     "1349",  TRUE,
  "s2_reading_gap_low",        "Study 2: Elites (p. 72)",      "descriptive",  "25",    TRUE,
  "s2_reading_gap_high",       "Study 2: Elites (p. 72)",      "descriptive",  "45",    TRUE,
  # Study 2: Results, pp. 72-74
  "s2_n_opeds",                "Study 2: Results (p. 73)",     "structural",   "4",     FALSE,
  "s2_main_sig_count",         "Study 2: Results (p. 73)",     "descriptive",  "3",     TRUE,
  "s2_main_total_count",       "Study 2: Results (p. 73)",     "structural",   "4",     FALSE,
  "s2_main_sig_p",             "Study 2: Results (p. 73)",     "descriptive",  "0.001", TRUE,
  "s2_main_target_min",        "Study 2: Results (p. 73)",     "pipeline",     "0.411", TRUE,
  "s2_main_scale_low",         "Study 2: Results (p. 73)",     "definitional", "1",     FALSE,
  "s2_main_scale_high",        "Study 2: Results (p. 73)",     "definitional", "7",     FALSE,
  "s2_main_target_max",        "Study 2: Results (p. 73)",     "pipeline",     "0.791", TRUE,
  "s2_main_no_cross_issue",    "Study 2: Results (p. 73)",     "descriptive",  "0",     TRUE,
  "s2_scale_sig_count",        "Study 2: Results (p. 73)",     "descriptive",  "3",     TRUE,
  "s2_scale_total_count",      "Study 2: Results (p. 73)",     "structural",   "4",     FALSE,
  "s2_scale_sig_p_strong",     "Study 2: Results (p. 73)",     "descriptive",  "0.001", TRUE,
  "s2_scale_sig_p_weak",       "Study 2: Results (p. 73)",     "descriptive",  "0.05",  TRUE,
  "s2_all_dvs",                "Study 2: Results (p. 73)",     "structural",   "16",    FALSE,
  "s2_scale_target_min",       "Study 2: Results (p. 74)",     "pipeline",     "0.104", TRUE,
  "s2_scale_target_max",       "Study 2: Results (p. 74)",     "pipeline",     "0.571", TRUE,
  "s2_nontarget_cells",        "Study 2: Results (p. 74)",     "structural",   "16",    TRUE,
  "s2_nontarget_sig_count",    "Study 2: Results (p. 74)",     "descriptive",  "1",     TRUE,
  # Studies 1 & 2: Heterogeneous Effects by Experimental Sample, pp. 74-75
  "s12_all_smaller_for_elites","Heterogeneous Effects by Experimental Sample (p. 74)", "descriptive", "4", TRUE,
  "s12_amtrak_interaction",    "Heterogeneous Effects by Experimental Sample (p. 74)", "pipeline", "0.198", TRUE,
  "s12_sig_interactions",      "Heterogeneous Effects by Experimental Sample (p. 74)", "descriptive", NA, TRUE,
  "s12_wall_interaction_null", "Heterogeneous Effects by Experimental Sample (p. 75)", "descriptive", NA, TRUE,
  # Studies 1 & 2: Heterogeneous Effects by Partisanship, pp. 75-77
  "s12_flat_tax_rep_over_dem", "Heterogeneous Effects by Partisanship (p. 77)", "descriptive", NA, TRUE,
  "s12_dem_pw_average",        "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.58", TRUE,
  "s12_dem_pw_average_se",     "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.05", TRUE,
  "s12_rep_pw_average",        "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.88", TRUE,
  "s12_rep_pw_average_se",     "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.08", TRUE,
  "s12_rep_dem_difference",    "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.3",  TRUE,
  "s12_rep_dem_difference_se", "Heterogeneous Effects by Partisanship (p. 77)", "pipeline", "0.09", TRUE,
  "s12_n_party_groups",        "Heterogeneous Effects by Partisanship (p. 77)", "structural", "3", FALSE,
  "s12_table9_exceptions",     "Heterogeneous Effects by Partisanship (p. 77)", "descriptive", NA, TRUE,
  # Studies 1 & 2: Long Term Effects, pp. 77-78
  "lt_wave2_days",             "Long Term Effects (p. 78)",    "definitional", "10",    FALSE,
  "lt_wave3_days",             "Long Term Effects (p. 78)",    "definitional", "30",    FALSE,
  "lt_decay_share",            "Long Term Effects (p. 78)",    "descriptive",  "50",    TRUE,
  "lt_still_significant",      "Long Term Effects (p. 78)",    "descriptive",  NA,      TRUE,
  "lt_little_later_decay",     "Long Term Effects (p. 78)",    "descriptive",  NA,      TRUE,
  "lt_n_waves_measured",       "Long Term Effects (p. 78)",    "structural",   "3",     FALSE,
  "lt_hockey_stick_30",        "Long Term Effects (p. 78)",    "structural",   "30",    FALSE,
  "lt_hockey_stick_10",        "Long Term Effects (p. 78)",    "structural",   "10",    FALSE,
  "lt_n_mechanisms",           "Long Term Effects (p. 78)",    "structural",   "2",     FALSE,
  # Cost Per Mind Changed, pp. 78-82
  "cost_n_arguments",          "Cost Per Mind Changed (p. 78)","structural",   "3",     FALSE,
  "cost_ghostwrite_low",       "Cost Per Mind Changed (p. 80)","definitional", "5000",  FALSE,
  "cost_ghostwrite_high",      "Cost Per Mind Changed (p. 80)","definitional", "25000", FALSE,
  "cost_advertisement",        "Cost Per Mind Changed (p. 80)","definitional", "50000", FALSE,
  "cost_assumed_opinion_share","Cost Per Mind Changed (p. 80)","definitional", "50",    FALSE,
  "cost_readers_per_oped",     "Cost Per Mind Changed (p. 80)","definitional", "4",     FALSE,
  "cost_opeds_per_day_low",    "Cost Per Mind Changed (p. 80)","definitional", "3",     FALSE,
  "cost_opeds_per_day_high",   "Cost Per Mind Changed (p. 80)","definitional", "6",     FALSE,
  "cost_newsweek_readers",     "Cost Per Mind Changed (p. 81)","definitional", "50000", FALSE,
  "cost_assumed_audience",     "Cost Per Mind Changed (p. 81)","definitional", "400000",FALSE,
  "cost_n_issue_areas",        "Cost Per Mind Changed (p. 81)","structural",   "5",     FALSE,
  "cost_split_25",             "Cost Per Mind Changed (p. 81)","definitional", "25",    FALSE,
  "cost_split_50",             "Cost Per Mind Changed (p. 81)","definitional", "50",    FALSE,
  "cost_split_75",             "Cost Per Mind Changed (p. 81)","definitional", "75",    FALSE,
  "cost_mturk_pw_average",     "Cost Per Mind Changed (p. 81)","pipeline",     "20",    TRUE,
  "cost_control_agreement",    "Cost Per Mind Changed (p. 81)","definitional", "50",    FALSE,
  "cost_treated_agreement",    "Cost Per Mind Changed (p. 81)","pipeline",     "70",    TRUE,
  "cost_elite_pw_average",     "Cost Per Mind Changed (p. 81)","pipeline",     "12",    TRUE,
  "cost_rosy_cost",            "Cost Per Mind Changed (p. 81)","definitional", "5000",  FALSE,
  "cost_rosy_reach",           "Cost Per Mind Changed (p. 81)","definitional", "400000",FALSE,
  "cost_rosy_share",           "Cost Per Mind Changed (p. 81)","definitional", "20",    FALSE,
  "cost_rosy_cents",           "Cost Per Mind Changed (p. 82)","definitional", "6",     TRUE,
  "cost_conservative_multiple","Cost Per Mind Changed (p. 82)","definitional", "10",    FALSE,
  "cost_conservative_cost",    "Cost Per Mind Changed (p. 82)","definitional", "50000", FALSE,
  "cost_conservative_reach",   "Cost Per Mind Changed (p. 82)","definitional", "200000",FALSE,
  "cost_conservative_share",   "Cost Per Mind Changed (p. 82)","definitional", "10",    FALSE,
  "cost_conservative_dollars", "Cost Per Mind Changed (p. 82)","definitional", "2.50",  TRUE,
  "cost_range_cents",          "Cost Per Mind Changed (p. 82)","definitional", "50",    FALSE,
  "cost_range_dollars",        "Cost Per Mind Changed (p. 82)","definitional", "3",     FALSE,
  # Discussion, pp. 82-84
  "disc_years_since_debut",    "Discussion (p. 82)",           "structural",   "40",    FALSE,
  "disc_effect_low",           "Discussion (p. 82)",           "descriptive",  "0.30",  TRUE,
  "disc_effect_high",          "Discussion (p. 82)",           "descriptive",  "0.50",  TRUE,
  "disc_persistence_months",   "Discussion (p. 82)",           "structural",   "1",     FALSE,
  "disc_wave_pair_early_a",    "Discussion (p. 83)",           "structural",   "1",     FALSE,
  "disc_wave_pair_early_b",    "Discussion (p. 83)",           "structural",   "2",     FALSE,
  "disc_wave_pair_late_a",     "Discussion (p. 83)",           "structural",   "2",     FALSE,
  "disc_wave_pair_late_b",     "Discussion (p. 83)",           "structural",   "3",     FALSE,
  "disc_wave_stable_a",        "Discussion (p. 83)",           "structural",   "2",     FALSE,
  "disc_wave_stable_b",        "Discussion (p. 83)",           "structural",   "3",     FALSE,
  "disc_cost_range_cents",     "Discussion (p. 83)",           "definitional", "50",    FALSE,
  "disc_cost_range_dollars",   "Discussion (p. 83)",           "definitional", "3",     FALSE,
  "disc_questions_low",        "Discussion (p. 83)",           "definitional", "4",     FALSE,
  "disc_questions_high",       "Discussion (p. 83)",           "definitional", "5",     FALSE,
  "disc_n_populations",        "Discussion (p. 83)",           "structural",   "2",     FALSE,
  "disc_two_sided",            "Discussion (p. 83)",           "structural",   "2",     FALSE,
  "disc_aggregate_shift_low",  "Discussion (p. 83)",           "definitional", "10",    FALSE,
  "disc_aggregate_shift_high", "Discussion (p. 83)",           "definitional", "20",    FALSE,
  "disc_us_adult_share",       "Discussion (p. 84)",           "definitional", "0.2",   FALSE,
  "disc_changed_share",        "Discussion (p. 84)",           "definitional", "10",    FALSE,
  "disc_aggregate_shift",      "Discussion (p. 84)",           "definitional", "0.02",  FALSE
)

published_claims <- bind_rows(list_rbind(rows), prose)

stopifnot(!anyDuplicated(published_claims$claim_id))

write_csv(published_claims, here::here("ground_truth", "published_claims.csv"), na = "")

print(count(published_claims, claim_type, needs_block))
print(nrow(published_claims))
