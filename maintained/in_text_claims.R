# coppock_ekins_kirby_2018/maintained/in_text_claims.R
# Output: printed to the console; no file
# Depends on: helpers.R, output/table_1_mturk_design.csv, output/table_3_mturk_main_dv.csv,
#   output/table_4_mturk_scale_dv.csv, output/table_5_elite_design.csv,
#   output/table_6_elite_main_dv.csv, output/table_7_elite_scale_dv.csv,
#   output/table_8_compare_samples.csv, output/table_9_het_fx_ftests.csv,
#   output/table_11_agreement.csv, output/text_agreement_pw_averages.csv,
#   output/text_recontact_rates.csv, output/text_reading_time.csv,
#   output/text_reading_time_differences.csv, output/text_partisan_pw_averages.csv,
#   output/text_partisan_rep_dem_difference.csv, output/figure_1_het_fx_by_party.csv,
#   output/figure_2_persistence.csv, ground_truth/published_claims.csv
# Description: Every number the published article prints, beside the sentence that prints
#   it, in the order a reader meets them.
#
#   This file recomputes. It reads the same maintained/output/ files the ground truth
#   reads, does its own selection, unit conversion and rounding, and never refits:
#   estimation happens once, in the analysis scripts, and only derivation happens twice.
#   ground_truth/build_ground_truth.R runs this file non-interactively, counts the CLAIM
#   lines it printed, and stops if any of them disagrees with the ground truth's own
#   value. It never reads the ground truth itself. Where the two disagree, one of them is
#   wrong, and that is the whole point of running both.
#
#   It does read ground_truth/published_claims.csv, which is the extraction rather than
#   the comparison. A block cannot print a number at the article's own precision without
#   the string the article printed, and quoting that string beside the computed value is
#   what makes the output readable without the article open.
#
#   Every claim prints one line, CLAIM <id> = <value> || <label>. That printed id is the
#   only link between a block and the claim it covers, and it is load bearing: the gate
#   reads this file as a program and matches on what it printed. The "covers:" comments
#   below are for a reader and nothing reads them.
#
#   Verbatim sentences are carried for prose claims, where a wrong number is a wrong
#   sentence. They are not carried for the 487 published table cells, where the cell's own
#   row and column label locates it exactly and quoting a sentence would be meaningless.
#
#   cat() is used because a labelled line per claim is what makes the output scannable
#   beside the sentences; it is permitted here and in no other file in this repository.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

t1 <- out("table_1_mturk_design.csv")
t3 <- out("table_3_mturk_main_dv.csv")
t4 <- out("table_4_mturk_scale_dv.csv")
t5 <- out("table_5_elite_design.csv")
t6 <- out("table_6_elite_main_dv.csv")
t7 <- out("table_7_elite_scale_dv.csv")
t8 <- out("table_8_compare_samples.csv")
t9 <- out("table_9_het_fx_ftests.csv")
t11 <- out("table_11_agreement.csv")
t11_pw <- out("text_agreement_pw_averages.csv")
recontact <- out("text_recontact_rates.csv")
reading <- out("text_reading_time.csv")
reading_gaps <- out("text_reading_time_differences.csv")
party_pw <- out("text_partisan_pw_averages.csv")
rep_dem <- out("text_partisan_rep_dem_difference.csv")
fig1 <- out("figure_1_het_fx_by_party.csv")
fig2 <- out("figure_2_persistence.csv")

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character())
)

# Reporting ----
# The article's own precision governs how a value is printed, and the article's own string
# is the only place that precision is recorded: 0.80 and 0.8 are the same double. digits
# is given explicitly for a claim the article states without a number, and for one where
# the page's precision would destroy the evidence.

page <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  value
}

report <- function(id, value, gloss, digits = NULL) {
  stopifnot(length(value) == 1, !is.null(value))
  target <- page(id)
  if (is.null(digits)) {
    digits <- if (is.na(target)) 0L
    else if (str_detect(target, fixed("."))) nchar(str_remove(target, "^[^.]*[.]")) else 0L
  }
  text <- if (is.na(value)) "NA" else sprintf(str_c("%.", digits, "f"), value)
  if (str_detect(text, "^-0[.]?0*$")) text <- str_remove(text, "^-")
  cat("CLAIM ", id, " = ", text, " || ", gloss,
      if (is.na(target)) "" else str_c(" [article: ", target, "]"), "\n", sep = "")
}

slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("^_") |>
    str_remove("_$")
}

# Accessors ----
# Each stops unless the filter selects exactly one row, so a claim cannot quietly read a
# row that is not there and print a plausible number from the wrong place.

one <- function(data, ...) {
  rows <- filter(data, ...)
  stopifnot(nrow(rows) == 1)
  rows
}

scalar <- function(data, column, ...) {
  one(data, ...)[[column]]
}

# On-target and off-target cells of a cross-effects table. The op-ed label carries the
# issue name, so a cell is on target when the treatment and the outcome name the same one.
on_target <- function(tbl) filter(tbl, op_ed != "Constant", str_remove(op_ed, "^Op-ed: ") == dv)
off_target <- function(tbl) filter(tbl, op_ed != "Constant", str_remove(op_ed, "^Op-ed: ") != dv)

# Table 9's published column is the classical F test. The archive's own line returns an
# HC2 Wald test under current estimatr, and that is the p_hc2 column; the published table
# is p_classical. See the report.
t9_published <- select(t9, sample, issue, dv_type, p = p_classical)

# Table 8's terms come out of the model matrix; these are the labels the article prints.
t8_labels <- c(
  Z_matchamtrak = "Op-ed: Amtrak",
  Z_matchflat = "Op-ed: Flat Tax",
  Z_matchvets = "Op-ed: Veterans",
  Z_matchwall = "Op-ed: Wall Street",
  experimental_sampleelite = "Elite Experiment",
  `Z_matchamtrak:experimental_sampleelite` = "Elite X Amtrak",
  `Z_matchflat:experimental_sampleelite` = "Elite X Flat Tax",
  `Z_matchvets:experimental_sampleelite` = "Elite X Veterans",
  `Z_matchwall:experimental_sampleelite` = "Elite X Wall Street",
  `(Intercept)` = "Constant"
)

t8_named <- mutate(t8, op_ed = unname(t8_labels[term]))

# Table 8's interaction rows carry the elite-minus-MTurk difference for each op-ed on each
# outcome. The on-target ones are what the text discusses.
t8_interactions <-
  t8_named |>
  filter(str_starts(op_ed, "Elite X")) |>
  mutate(issue = str_remove(op_ed, "^Elite X ")) |>
  filter(issue == dv)

# Abstract ----

# "We find very large average treatment effects on target issues, equivalent to shifts of
# approximately 0.5 scale points on a 7-point scale, that persist for at least one month."
# covers: ab_target_shift
# The sentence does not say which average it means and the nine on-target effects on the
# main dependent variables run from 0.045 to 0.915, so the mean is printed with the range
# beside it and no verdict is drawn.
abstract_main_target <- bind_rows(on_target(t3), on_target(t6))
report(
  "ab_target_shift", mean(abstract_main_target$estimate),
  str_glue(
    "mean of the {nrow(abstract_main_target)} on-target effects on the main dependent ",
    "variables, both samples, range {sprintf('%.3f', min(abstract_main_target$estimate))} ",
    "to {sprintf('%.3f', max(abstract_main_target$estimate))}; MTurk mean ",
    "{sprintf('%.3f', mean(on_target(t3)$estimate))}, elite mean ",
    "{sprintf('%.3f', mean(on_target(t6)$estimate))}"
  ),
  digits = 1
)

# "We find very small and insignificant average treatment effects on non-target issues,
# suggesting that our subjects read, understood, and were persuaded by the arguments
# presented in these op-eds."
# covers: ab_nontarget_small
abstract_nontarget <- bind_rows(off_target(t3), off_target(t6))
report(
  "ab_nontarget_small", sum(abstract_nontarget$p.value < 0.05),
  str_glue(
    "non-target effects on the main dependent variables significant at 0.05, of ",
    "{nrow(abstract_nontarget)} in both samples; largest absolute estimate ",
    "{sprintf('%.3f', max(abs(abstract_nontarget$estimate)))}"
  ),
  digits = 0
)

# "We find limited evidence of treatment effect heterogeneity by party identification:
# Democrats, Republicans, and independents all appear to move in the predicted direction
# by similar magnitudes."
# covers: ab_parties_similar
party_cells <-
  fig1 |>
  summarise(all_positive = all(estimate > 0), .by = c(sample, issue, dv_type))
report(
  "ab_parties_similar", sum(party_cells$all_positive),
  str_glue(
    "of {nrow(party_cells)} sample-by-issue-by-outcome cells in which all three party ",
    "groups move in the predicted direction; smallest party estimate anywhere ",
    "{sprintf('%.3f', min(fig1$estimate))}"
  ),
  digits = 0
)

# Study 1: Mechanical Turk ----

# "We enrolled 3,567 subjects on Amazon's Mechanical Turk (MTurk) in a three-wave panel
# survey."
# covers: s1_enrolled
report(
  "s1_enrolled", scalar(recontact, "value", claim == "MTurk subjects enrolled"),
  "subjects in the deposited MTurk data, which is also the total Table 1 prints"
)

# "This design is summarized in Table 1. We obtained a recontact rate of 84% in Wave 2 and
# 69% in Wave 3."
# covers: s1_recontact_w2, s1_recontact_w3
report(
  "s1_recontact_w2",
  scalar(recontact, "value", claim == "MTurk Wave 2 recontact rate (percent)"),
  "per cent of enrolled MTurk subjects responding in Wave 2"
)
report(
  "s1_recontact_w3",
  scalar(recontact, "value", claim == "MTurk Wave 3 recontact rate (percent)"),
  "per cent of enrolled MTurk subjects responding in Wave 3"
)

# "Using chi-square tests, we fail to reject the null hypothesis that treatment status and
# response in follow-up waves are independent (Wave 2: p = 0.10; Wave 3: p = 0.61)."
# covers: s1_chisq_w2_p, s1_chisq_w3_p
report(
  "s1_chisq_w2_p",
  scalar(recontact, "value", claim == "MTurk Wave 2 treatment-by-response chi-square p"),
  "chi-square p for treatment by Wave 2 response, MTurk"
)
report(
  "s1_chisq_w3_p",
  scalar(recontact, "value", claim == "MTurk Wave 3 treatment-by-response chi-square p"),
  "chi-square p for treatment by Wave 3 response, MTurk"
)

# Study 1: Treatments ----

# "Our Mechanical Turk subjects spent an average of 2 to 4 minutes reading, depending on
# the length of the op-ed."
# covers: s1_reading_minutes_low, s1_reading_minutes_high
mturk_reading <- filter(reading, sample == "MTurk")
report(
  "s1_reading_minutes_low", min(mturk_reading$trimmed_minutes),
  str_glue(
    "shortest arm's trimmed mean reading time in minutes ",
    "({sprintf('%.2f', min(mturk_reading$trimmed_minutes))} before rounding)"
  )
)
report(
  "s1_reading_minutes_high", max(mturk_reading$trimmed_minutes),
  str_glue(
    "longest arm's trimmed mean reading time in minutes ",
    "({sprintf('%.2f', max(mturk_reading$trimmed_minutes))} before rounding)"
  )
)

# Study 1: Results ----

# "Turning first to the main dependent variables, the size of the treatment effects of the
# op-eds on their target issues varied from 0.427 scale points (on a 1-7 scale) for the
# climate piece to 0.915 scale points for the Wall Street piece."
# covers: s1_main_target_min, s1_main_target_max
report("s1_main_target_min", min(on_target(t3)$estimate),
       "smallest MTurk on-target effect on a main dependent variable (climate)")
report("s1_main_target_max", max(on_target(t3)$estimate),
       "largest MTurk on-target effect on a main dependent variable (Wall Street)")

# "All five of the effects on their target dependent variables are statistically
# significant at p < 0.001."
# covers: s1_main_target_sig_p
report(
  "s1_main_target_sig_p", as.numeric(all(on_target(t3)$p.value < 0.001)),
  str_glue(
    "1 if all five MTurk on-target main-DV effects have p below 0.001; largest of the ",
    "five p-values is {format(max(on_target(t3)$p.value), digits = 3, scientific = TRUE)}"
  ),
  digits = 0
)

# "Of the 20 treatment effects on non target issues, only two are statistically significant
# (Flat Tax op-ed on Amtrak outcome and Wall Street op-ed on Veterans outcome)."
# covers: s1_nontarget_cells, s1_nontarget_sig_count
report("s1_nontarget_cells", nrow(off_target(t3)),
       "off-diagonal cells in Table 3, five op-eds by five main dependent variables",
       digits = 0)
t3_sig <- filter(off_target(t3), p.value < 0.05)
report(
  "s1_nontarget_sig_count", nrow(t3_sig),
  str_glue(
    "MTurk non-target main-DV effects significant at 0.05: ",
    "{str_c(str_c(t3_sig$op_ed, ' on ', t3_sig$dv), collapse = '; ')}"
  ),
  digits = 0
)

# "The smallest is again the effect of the climate op-ed on the climate scale, at 0.276
# standard deviations, while the remainder range from about 0.5 to 0.7 standard
# deviations."
# covers: s1_scale_target_min, s1_scale_target_range_low, s1_scale_target_range_high
t4_target <- on_target(t4)
t4_remainder <- filter(t4_target, dv != "Climate")
report("s1_scale_target_min", min(t4_target$estimate),
       "smallest MTurk on-target effect on a composite scale (climate)")
report(
  "s1_scale_target_range_low", min(t4_remainder$estimate),
  str_glue(
    "smallest of the four remaining MTurk on-target scale effects ",
    "({sprintf('%.3f', min(t4_remainder$estimate))} before rounding)"
  )
)
report(
  "s1_scale_target_range_high", max(t4_remainder$estimate),
  str_glue(
    "largest of the four remaining MTurk on-target scale effects ",
    "({sprintf('%.3f', max(t4_remainder$estimate))} before rounding)"
  )
)

# "By this measure as well, the cross-issue effects are small to non-existent."
# covers: s1_scale_cross_issue_small
t4_off <- off_target(t4)
report(
  "s1_scale_cross_issue_small", sum(t4_off$p.value < 0.05),
  str_glue(
    "of {nrow(t4_off)} MTurk non-target scale effects significant at 0.05 ",
    "({sum(t4_off$p.value < 0.1)} at 0.1); largest absolute estimate ",
    "{sprintf('%.3f', max(abs(t4_off$estimate)))}"
  ),
  digits = 0
)

# Study 2: Elites ----

# "This procedure yielded 2,169 subjects who completed our survey."
# covers: s2_completed
report("s2_completed", scalar(recontact, "value", claim == "Elite subjects enrolled"),
       "subjects in the deposited elite data, which is also the total Table 5 prints")

# "As in the MTurk study, we asked subjects to participate in a follow-up survey after 10
# days; after two reminders, we obtained 1,349 complete responses."
# covers: s2_w2_complete
report("s2_w2_complete", scalar(recontact, "value", claim == "Elite responded in Wave 2"),
       "elite subjects responding in Wave 2, which is also the total Table 5 prints")

# "On average, our elite sample spent 25-45 seconds less time reading our treatment
# articles than the MTurk sample."
# covers: s2_reading_gap_low, s2_reading_gap_high
# The published range is a range across op-eds, so the two ends of the actual range are
# what it has to be compared against. A positive number is seconds less for elites.
gaps <- arrange(reading_gaps, mturk_minus_elite_seconds, .locale = "en")
gap_list <- str_c(str_c(gaps$Z, " ", sprintf("%.1f", gaps$mturk_minus_elite_seconds)),
                  collapse = "; ")
report(
  "s2_reading_gap_low", min(gaps$mturk_minus_elite_seconds),
  str_glue("smallest elite deficit in seconds across the four shared op-eds ({gap_list}); ",
           "mean {sprintf('%.1f', mean(gaps$mturk_minus_elite_seconds))}")
)
report(
  "s2_reading_gap_high", max(gaps$mturk_minus_elite_seconds),
  str_glue("largest elite deficit in seconds across the four shared op-eds; mean ",
           "{sprintf('%.1f', mean(gaps$mturk_minus_elite_seconds))}")
)

# Study 2: Results ----

# "Turning first to main dependent variables, we found statistically significant treatment
# effects for three of the four treatments (Amtrak, Flat Tax, Wall Street) at p < 0.001,
# but did not for the Veterans op-ed treatment."
# covers: s2_main_sig_count, s2_main_sig_p
t6_target <- on_target(t6)
report("s2_main_sig_count", sum(t6_target$p.value < 0.05),
       "of four elite on-target main-DV effects significant at 0.05", digits = 0)
t6_named <- filter(t6_target, dv %in% c("Amtrak", "Flat Tax", "Wall Street"))
report(
  "s2_main_sig_p", as.numeric(all(t6_named$p.value < 0.001)),
  str_glue(
    "1 if all three named elite on-target main-DV effects have p below 0.001; ",
    "{str_c(str_c(t6_named$dv, ' p = ', sprintf('%.4f', t6_named$p.value)), collapse = '; ')}"
  ),
  digits = 0
)

# "The size of the treatment effects of the op-eds on their target issues ranged from 0.411
# scale points (on a 1-7 scale) for the Flat Tax treatment to 0.791 scale points for the
# Wall Street op-ed."
# covers: s2_main_target_min, s2_main_target_max
# The article's range starts at the Flat Tax estimate, not at the smallest of the four:
# the Veterans effect is smaller and is the one the previous sentence calls null.
report("s2_main_target_min", scalar(t6_target, "estimate", dv == "Flat Tax"),
       "elite on-target main-DV effect for the Flat Tax op-ed")
report("s2_main_target_max", scalar(t6_target, "estimate", dv == "Wall Street"),
       "elite on-target main-DV effect for the Wall Street op-ed")

# "We find no evidence of cross-issue effects of the treatments on non-target issues for
# the main dependent variables."
# covers: s2_main_no_cross_issue
t6_off <- off_target(t6)
report(
  "s2_main_no_cross_issue", sum(t6_off$p.value < 0.1),
  str_glue("of {nrow(t6_off)} elite non-target main-DV effects significant at 0.1; ",
           "largest absolute estimate {sprintf('%.3f', max(abs(t6_off$estimate)))}"),
  digits = 0
)

# "We find statistically significant treatment effects for three of the four treatments
# (Amtrak and Wall Street at p < 0.001; Veterans at p < 0.05), but did not for the Flat Tax
# treatment."
# covers: s2_scale_sig_count, s2_scale_sig_p_strong, s2_scale_sig_p_weak
# The same construction as the main-DV sentence two paragraphs earlier, which names a level
# the estimates do not support, so the two levels this one names are checked the same way.
t7_target <- on_target(t7)
report("s2_scale_sig_count", sum(t7_target$p.value < 0.05),
       "of four elite on-target composite-scale effects significant at 0.05", digits = 0)
t7_strong <- filter(t7_target, dv %in% c("Amtrak", "Wall Street"))
report(
  "s2_scale_sig_p_strong", as.numeric(all(t7_strong$p.value < 0.001)),
  str_glue(
    "1 if both named elite on-target composite-scale effects have p below 0.001; ",
    "{str_c(str_c(t7_strong$dv, ' p = ', sprintf('%.6f', t7_strong$p.value)), collapse = '; ')}"
  ),
  digits = 0
)
t7_weak <- one(t7_target, dv == "Veterans")
report(
  "s2_scale_sig_p_weak", as.numeric(t7_weak$p.value < 0.05),
  str_glue("1 if the elite on-target Veterans composite-scale effect has p below 0.05; ",
           "p = {sprintf('%.4f', t7_weak$p.value)}"),
  digits = 0
)

# "The treatment effect sizes on their target issues range from 0.104 for the Flat Tax
# treatment to 0.571 for the Wall Street treatment."
# covers: s2_scale_target_min, s2_scale_target_max
report("s2_scale_target_min", scalar(t7_target, "estimate", dv == "Flat Tax"),
       "elite on-target composite-scale effect for the Flat Tax op-ed")
report("s2_scale_target_max", scalar(t7_target, "estimate", dv == "Wall Street"),
       "elite on-target composite-scale effect for the Wall Street op-ed")

# "Of the 16 treatment effects on non-target issues, only one is statistically significant
# (Amtrak op-ed on Veterans outcome)."
# covers: s2_nontarget_cells, s2_nontarget_sig_count
t7_off <- off_target(t7)
report(
  "s2_nontarget_cells", nrow(t7_off),
  str_glue(
    "off-diagonal cells in Table 7: four op-eds by four composite scales is ",
    "{nrow(t7_target) + nrow(t7_off)} coefficients, of which {nrow(t7_target)} are on ",
    "target"
  ),
  digits = 0
)
t7_sig <- filter(t7_off, p.value < 0.05)
report(
  "s2_nontarget_sig_count", nrow(t7_sig),
  str_glue(
    "elite non-target scale effects significant at 0.05: ",
    "{str_c(str_c(t7_sig$op_ed, ' on ', t7_sig$dv), collapse = '; ')}"
  ),
  digits = 0
)

# Studies 1 & 2: Heterogeneous Effects by Experimental Sample ----

# "On each of the target dependent variables, the treatment effect for elites was smaller.
# For example, the effect of the Amtrak op-ed on the Amtrak composite scale dependent
# variable was 0.198 scale points smaller for elites."
# covers: s12_all_smaller_for_elites, s12_amtrak_interaction
report(
  "s12_all_smaller_for_elites", sum(t8_interactions$estimate < 0),
  str_glue(
    "of {nrow(t8_interactions)} on-target elite-minus-MTurk interactions that are ",
    "negative: {str_c(str_c(t8_interactions$dv, ' ', sprintf('%.3f', t8_interactions$estimate)), collapse = '; ')}"
  ),
  digits = 0
)
report(
  "s12_amtrak_interaction", abs(scalar(t8_interactions, "estimate", dv == "Amtrak")),
  "size of the Amtrak on-target interaction, which the sentence states as a shortfall"
)

# "This difference is statistically significant, as it is for the effects of the Flat Tax
# and Veterans treatments on their target outcomes."
# covers: s12_sig_interactions
t8_sig <- filter(t8_interactions, p.value < 0.05)
report(
  "s12_sig_interactions", nrow(t8_sig),
  str_glue("on-target interactions significant at 0.05: {str_c(t8_sig$dv, collapse = '; ')}"),
  digits = 0
)

# "While the interaction is negative for the effect of the Wall Street op-ed on its target
# dependent variable, the difference is not statistically significant."
# covers: s12_wall_interaction_null
wall_interaction <- one(t8_interactions, dv == "Wall Street")
report(
  "s12_wall_interaction_null",
  as.numeric(wall_interaction$estimate < 0 && wall_interaction$p.value >= 0.05),
  str_glue(
    "1 if the Wall Street on-target interaction is negative and not significant at 0.05: ",
    "estimate {sprintf('%.3f', wall_interaction$estimate)}, p = ",
    "{sprintf('%.3f', wall_interaction$p.value)}"
  ),
  digits = 0
)

# Studies 1 & 2: Heterogeneous Effects by Partisanship ----

# "Some cases of treatment effect moderation are clear cut: in the Mechanical Turk sample,
# the effect of the Flat Tax treatment is larger among Republicans than it is among
# Democrats."
# covers: s12_flat_tax_rep_over_dem
flat_tax_party <- filter(fig1, sample == "Mechanical Turk", issue == "Flat Tax",
                         dv_type == "Main DV")
flat_tax_gap <- scalar(flat_tax_party, "estimate", party == "Republican") -
  scalar(flat_tax_party, "estimate", party == "Democrat")
report(
  "s12_flat_tax_rep_over_dem", flat_tax_gap,
  str_glue(
    "Republican minus Democrat effect of the Flat Tax op-ed on its main DV, MTurk: ",
    "{sprintf('%.3f', scalar(flat_tax_party, 'estimate', party == 'Republican'))} against ",
    "{sprintf('%.3f', scalar(flat_tax_party, 'estimate', party == 'Democrat'))}"
  ),
  digits = 3
)

# "The precision weighted average of the treatment effects on the Main DVs for is 0.58
# (SE = 0.05) among Democrats on MTurk and 0.88 (SE = 0.08) among Republicans. The standard
# error of this 0.3 point difference is 0.09, indicating that the difference-in-difference
# is statistically significant."
# covers: s12_dem_pw_average, s12_dem_pw_average_se, s12_rep_pw_average,
#   s12_rep_pw_average_se, s12_rep_dem_difference, s12_rep_dem_difference_se
report("s12_dem_pw_average", scalar(rep_dem, "value", claim == "Democrat average"),
       "precision-weighted average of the MTurk main-DV effects among Democrats")
report("s12_dem_pw_average_se", scalar(rep_dem, "value", claim == "Democrat SE"),
       "standard error of that average")
report("s12_rep_pw_average", scalar(rep_dem, "value", claim == "Republican average"),
       "precision-weighted average of the MTurk main-DV effects among Republicans")
report("s12_rep_pw_average_se", scalar(rep_dem, "value", claim == "Republican SE"),
       "standard error of that average")
report("s12_rep_dem_difference",
       scalar(rep_dem, "value", claim == "Republican minus Democrat"),
       "Republican minus Democrat difference in the precision-weighted averages")
report("s12_rep_dem_difference_se", scalar(rep_dem, "value", claim == "Difference SE"),
       "standard error of that difference")

# "With the exceptions of the Amtrak and Flat Tax treatments in the Mechanical Turk sample,
# we fail to reject this null: treatment effects do not appear to vary dramatically by
# partisanship."
# covers: s12_table9_exceptions
t9_rejected <- t9_published |> filter(p < 0.05) |> distinct(sample, issue)
report(
  "s12_table9_exceptions", nrow(t9_rejected),
  str_glue(
    "sample-by-issue combinations in Table 9 with any p below 0.05: ",
    "{str_c(str_c(t9_rejected$sample, ' ', t9_rejected$issue), collapse = '; ')}"
  ),
  digits = 0
)

# Studies 1 & 2: Long Term Effects ----
# Figure 2 plots group means, so an effect is the target-arm mean less the control mean in
# the same study, outcome and wave, and its standard error is the root sum of squares
# because the two groups are disjoint.

target_arms <- tribble(
  ~Z, ~dv,
  "amtrak", "amtrak",
  "climate", "climate",
  "paul", "flat",
  "veterans", "vets",
  "wallstreet", "wall"
)

persistence <-
  fig2 |>
  inner_join(target_arms, by = c("Z", "dv")) |>
  inner_join(
    fig2 |> filter(Z == "control") |> select(study, dv, wave, control = estimate,
                                             control_se = std.error),
    by = c("study", "dv", "wave")
  ) |>
  mutate(
    effect = estimate - control,
    se = sqrt(std.error^2 + control_se^2),
    p.value = 2 * pnorm(-abs(effect / se))
  ) |>
  select(study, dv, wave, effect, se, p.value)

persistence_wide <-
  persistence |>
  select(study, dv, wave, effect) |>
  pivot_wider(names_from = wave, values_from = effect)

# "While the treatment effects are indeed smaller (approximately 50% the original
# magnitudes in each case), they remain statistically significant in most cases."
# covers: lt_decay_share, lt_still_significant
decay <- persistence_wide$w2 / persistence_wide$w1
report(
  "lt_decay_share", 100 * mean(decay),
  str_glue(
    "mean Wave 2 effect as a percentage of the Wave 1 effect across the nine target ",
    "series, range {sprintf('%.0f', 100 * min(decay))} to {sprintf('%.0f', 100 * max(decay))}"
  ),
  digits = 0
)
later_waves <- filter(persistence, wave != "w1")
report(
  "lt_still_significant", sum(later_waves$p.value < 0.05),
  str_glue("of {nrow(later_waves)} post-treatment-wave target effects significant at 0.05"),
  digits = 0
)

# "However, we do not observe much decay at all after the initial decline."
# covers: lt_little_later_decay
mturk_decay <- filter(persistence_wide, study == "MTurk Study")
report(
  "lt_little_later_decay", 100 * mean(mturk_decay$w3 / mturk_decay$w2),
  str_glue(
    "mean Wave 3 effect as a percentage of the Wave 2 effect on MTurk, range ",
    "{sprintf('%.0f', 100 * min(mturk_decay$w3 / mturk_decay$w2))} to ",
    "{sprintf('%.0f', 100 * max(mturk_decay$w3 / mturk_decay$w2))}"
  ),
  digits = 0
)

# Cost Per Mind Changed ----

# "On Mechanical Turk, the precision weighted average of the estimated effects (using the
# 50th percentile split) indicates that on average, the treatments increased agreement with
# the authors by 20 percentage points. That is, if 50% of the control group agrees with the
# author, then we can say that approximately 70% of the target issue treatment group agrees
# with the author. In the elite sample, the estimate is smaller, but is still impressive at
# 12 percentage points."
# covers: cost_mturk_pw_average, cost_treated_agreement, cost_elite_pw_average
mturk_agreement <- 100 * scalar(t11_pw, "estimate", sample == "MTurk", split == 50)
elite_agreement <- 100 * scalar(t11_pw, "estimate", sample == "Elite", split == 50)
control_agreement <- as.numeric(page("cost_control_agreement"))
report("cost_mturk_pw_average", mturk_agreement,
       "precision-weighted average agreement effect on MTurk at the median split, in points")
report(
  "cost_treated_agreement", control_agreement + mturk_agreement,
  str_glue(
    "control-group agreement of {control_agreement} per cent, which the design fixes, ",
    "plus the estimated increase"
  )
)
report("cost_elite_pw_average", elite_agreement,
       "precision-weighted average agreement effect for elites at the median split")

# "Under the rosiest scenario, an op-ed costs $5,000 to produce, reaches 400,000 people,
# and changes the mind of 20% of them. Plugging these values into Equation (1), we obtain
# that the cost per mind changed is a mere 6 cents."
# covers: cost_rosy_cents
# The share is the article's own rounding of the estimate above, so it is taken from the
# pipeline and rounded the way the sentence rounds it. The cost and the readership are
# design assumptions the article states and the deposit could never record, so they come
# from the extraction.
rounded_agreement <- round(mturk_agreement)
rosy_cost <- page("cost_rosy_cost")
rosy_reach <- page("cost_rosy_reach")
rosy_cents <- 100 * as.numeric(rosy_cost) /
  (as.numeric(rosy_reach) * rounded_agreement / 100)
report(
  "cost_rosy_cents", rosy_cents,
  str_glue(
    "cents per mind changed at ${rosy_cost}, {rosy_reach} readers and the pipeline's ",
    "{rounded_agreement} point effect ({sprintf('%.2f', rosy_cents)} before rounding)"
  )
)

# "Under a more conservative set of assumptions, an op-ed costs 10 times as much ($50,000),
# reaches half as many people (200,000), and changes half as many minds (10%). The
# resulting cost per mind changed would work out to $2.50."
# covers: cost_conservative_dollars
conservative_cost <- page("cost_conservative_cost")
conservative_reach <- page("cost_conservative_reach")
report(
  "cost_conservative_dollars",
  as.numeric(conservative_cost) /
    (as.numeric(conservative_reach) * (rounded_agreement / 2) / 100),
  str_glue(
    "dollars per mind changed at ${conservative_cost}, {conservative_reach} readers and ",
    "half the pipeline's {rounded_agreement} point effect"
  )
)

# Discussion ----

# "In both studies of the mass public and elites, we find large, statistically significant
# average treatment effects of op-eds between 0.30 and 0.50 standard deviations on policy
# attitudes."
# covers: disc_effect_low, disc_effect_high
# Read as a range over the on-target composite-scale effects, which are the estimates in
# standard deviations. The two sample means are printed beside them because the sentence
# does not say which of the two it means, and no verdict is drawn.
discussion_scale <- bind_rows(t4_target, t7_target)
report(
  "disc_effect_low", min(discussion_scale$estimate),
  str_glue(
    "smallest of the {nrow(discussion_scale)} on-target composite-scale effects; MTurk ",
    "mean {sprintf('%.3f', mean(t4_target$estimate))}, elite mean ",
    "{sprintf('%.3f', mean(t7_target$estimate))}"
  )
)
report(
  "disc_effect_high", max(discussion_scale$estimate),
  str_glue(
    "largest of the {nrow(discussion_scale)} on-target composite-scale effects; all nine: ",
    "{str_c(sprintf('%.3f', sort(discussion_scale$estimate)), collapse = ', ')}"
  )
)

# Figures ----
# Neither figure prints a number on its face, and both still assert a countable quantity.

# covers: fig1_n_estimates
report("fig1_n_estimates", nrow(fig1),
       "party-specific estimates plotted in Figure 1, with a confidence interval each",
       digits = 0)

# covers: fig2_n_group_means
report("fig2_n_group_means", nrow(fig2),
       "group means plotted in Figure 2, with a confidence ribbon each", digits = 0)

# Published table cells ----
# The 487 cells of Tables 1, 3, 4, 5, 6, 7, 8, 9 and 11. A verbatim sentence is not carried
# for these: the row and column labels locate a cell exactly, and there is no sentence.
# Each block derives its claim ids from the table's own labels rather than reading them out
# of the extraction, so a label that has drifted shows up as an id the gate does not
# recognise instead of silently matching.

design_cells <- function(tbl, prefix) {
  tbl |>
    pivot_longer(-c(wave, row), names_to = "arm", values_to = "count") |>
    mutate(claim_id = str_c(prefix, "_", slug(str_c(wave, " / ", row, " / ", arm))))
}

walk2(
  list(t1, t5), c("t1", "t5"),
  \(tbl, prefix) {
    cells <- design_cells(tbl, prefix)
    pwalk(list(cells$claim_id, cells$count, cells$wave, cells$row, cells$arm),
          \(id, count, wave, row, arm)
          report(id, count, str_glue("Table {str_remove(prefix, 't')}: {wave}, {row}, {arm}"),
                 digits = 0))
  }
)

regression_cells <- function(tbl, prefix) {
  coefficients <-
    tbl |>
    select(dv, op_ed, estimate, std.error) |>
    pivot_longer(c(estimate, std.error), names_to = "quantity") |>
    mutate(
      claim_id = str_c(prefix, "_", slug(str_c(dv, " / ", op_ed)), "_",
                       if_else(quantity == "estimate", "estimate", "std_error")),
      label = str_glue("Table {str_remove(prefix, 't')}: {dv} column, {op_ed} row, {quantity}")
    )
  fit_statistics <-
    tbl |>
    distinct(dv, nobs, r_squared) |>
    pivot_longer(c(r_squared, nobs), names_to = "quantity") |>
    mutate(
      claim_id = str_c(prefix, "_", slug(dv), "_",
                       if_else(quantity == "r_squared", "r2", "n")),
      label = str_glue("Table {str_remove(prefix, 't')}: {dv} column, {quantity}")
    )
  bind_rows(select(coefficients, claim_id, value, label),
            select(fit_statistics, claim_id, value, label))
}

walk2(
  list(t3, t4, t6, t7, t8_named), c("t3", "t4", "t6", "t7", "t8"),
  \(tbl, prefix) {
    cells <- regression_cells(tbl, prefix)
    pwalk(list(cells$claim_id, cells$value, cells$label), report)
  }
)

t9_cells <-
  t9_published |>
  mutate(claim_id = str_c("t9_", slug(str_c(sample, " / ", issue, " / ", dv_type,
                                            " / F test p"))))
pwalk(
  list(t9_cells$claim_id, t9_cells$p, t9_cells$sample, t9_cells$issue, t9_cells$dv_type),
  \(id, p, sample, issue, dv_type)
  report(id, p, str_glue("Table 9: {sample}, {issue}, {dv_type}, classical F test"))
)

t11_cells <-
  bind_rows(select(t11, sample, issue, split, estimate, std.error), t11_pw) |>
  pivot_longer(c(estimate, std.error), names_to = "quantity") |>
  mutate(
    claim_id = str_c("t11_", slug(str_c(sample, " / ", issue, " / ", split, "th")), "_",
                     if_else(quantity == "estimate", "estimate", "std_error")),
    label = str_glue("Table 11: {issue}, {sample}, {split}th percentile split, {quantity}")
  )
pwalk(list(t11_cells$claim_id, t11_cells$value, t11_cells$label), report)
