# coppock_ekins_kirby_2018/ground_truth/build_ground_truth.R
# Output: printed to the console; no file
# Depends on: ground_truth/published_claims.csv,
#   ground_truth/coppock_ekins_kirby_2018_ground_truth.csv, maintained/in_text_claims.R,
#   everything in maintained/output/
# Description: The gate over the ground truth. It checks the committed comparison against
#   the extraction and against a live run of the second instrument, and it stops the
#   pipeline when they disagree.
#
#   It is a gate and not a generator, and the distinction matters. Stage 2 of this
#   programme's procedures asks that the ground truth be written by a script whose only
#   hardcoded numbers are the ones read out of the article, with value_script recovered by
#   running the deposit and value_rewrite read back out of maintained/output/. That is not
#   what this repository has: its value_script and value_rewrite columns are committed
#   data. What this file does instead is close the channel that matters most. Every
#   value_rewrite is compared, on every run, against a number maintained/in_text_claims.R
#   computes live from maintained/output/ by a path of its own, so a committed value that
#   has drifted from the scripts cannot survive a run.
#
#   The checks, in order:
#     1. the two files agree on what the article says
#     2. every claim id is real, unique and accounted for
#     3. every published float is covered, and by what fraction of its cells
#     4. a verdict and a defect locus go together
#     5. the second instrument ran, printed, and named exactly the claims it should
#     6. the second instrument agrees with the ground truth, value by value

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

# value_paper is a string on purpose. Guessed as a double it silently becomes 0.09 where
# the article prints 0.090, and the printed precision is the only thing the comparison has
# to match against.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character(), needs_block = col_logical())
)

gt <- read_csv(
  here::here("ground_truth", "coppock_ekins_kirby_2018_ground_truth.csv"),
  col_types = cols(.default = col_character())
)

stopifnot(
  identical(
    names(gt),
    c("paper_id", "claim_id", "table_figure", "claim", "value_script", "value_paper",
      "match", "value_rewrite", "match_rewrite", "defect_locus", "notes")
  ),
  !anyDuplicated(published_claims$claim_id),
  !any(is.na(published_claims$claim_id))
)

# 1. The two files agree on what the article says ----
# The extraction is the reading of the article. A ground truth carrying a different string
# for the same claim is one of them having been edited without the other.

paper_disagreements <-
  gt |>
  drop_na(claim_id) |>
  select(claim_id, gt_value = value_paper) |>
  inner_join(select(published_claims, claim_id, extraction_value = value_paper),
             by = "claim_id") |>
  filter(!identical(gt_value, extraction_value),
         is.na(gt_value) != is.na(extraction_value) |
           (!is.na(gt_value) & gt_value != extraction_value))

if (nrow(paper_disagreements) > 0) {
  print(paper_disagreements, n = 40)
  stop(str_glue(
    "The ground truth and the extraction disagree on the published value of ",
    "{nrow(paper_disagreements)} claims."
  ))
}

# 2. Every claim id is real, unique and accounted for ----

stopifnot(
  !anyDuplicated(na.omit(gt$claim_id)),
  all(na.omit(gt$claim_id) %in% published_claims$claim_id)
)

must_check <- filter(published_claims, needs_block)

rowless <- setdiff(must_check$claim_id, na.omit(gt$claim_id))
if (length(rowless) > 0) {
  stop(str_glue(
    "Claims requiring a block with no ground truth row ({length(rowless)}): ",
    "{str_c(head(rowless, 40), collapse = ', ')}."
  ))
}

# 3. Every published float is covered ----
# Taken from the article's own list of floats rather than from what the pipeline happens
# to produce. "At least one row" is not coverage, so the covered fraction is computed per
# float and printed: a float with one row against a hundred published cells passes a
# presence check and says nothing.

published_float_inventory <- str_c(
  "Table ",
  c("1 (p. 67)", "2 (p. 68)", "3 (p. 70)", "4 (p. 71)", "5 (p. 72)", "6 (p. 73)",
    "7 (p. 74)", "8 (p. 75)", "9 (p. 77)", "10 (p. 80)", "11 (p. 81)")
) |>
  c("Figure 1 (p. 76)", "Figure 2 (p. 79)")

# Floats whose numbers the deposit can produce. Table 2 is op-ed titles, authors and
# outlets and prints no number at all; Table 10 is circulation figures taken from an
# auditing bureau and two publishers' internal surveys, multiplied by a readership share
# the article assumes, none of which is in the deposit or could be.
reproducible_floats <- setdiff(published_float_inventory,
                               c("Table 2 (p. 68)", "Table 10 (p. 80)"))

# Three columns, not one. published_values counts every number the float prints;
# needing_coverage counts the ones the pipeline is expected to reproduce; covered_values
# counts the ones with a ground truth row. The gap between the first two is the axis
# breaks and panel counts, which are verified where they are written rather than here, and
# it is printed so that it is a stated exemption rather than an unnoticed shortfall.
float_coverage <-
  published_claims |>
  filter(location %in% published_float_inventory) |>
  left_join(mutate(drop_na(gt, claim_id), covered = TRUE) |> select(claim_id, covered),
            by = "claim_id") |>
  summarise(
    published_values = n(),
    needing_coverage = sum(needs_block),
    covered_values = sum(!is.na(covered)),
    .by = location
  ) |>
  mutate(covered_fraction = if_else(needing_coverage == 0, NA_real_,
                                    covered_values / needing_coverage)) |>
  arrange(location, .locale = "en")

missing_floats <- setdiff(published_float_inventory, float_coverage$location)
if (length(missing_floats) > 0) {
  stop(str_glue("Published floats absent from the extraction: ",
                "{str_c(missing_floats, collapse = ', ')}."))
}

partial <- filter(float_coverage, location %in% reproducible_floats,
                  !is.na(covered_fraction), covered_fraction < 1)
if (nrow(partial) > 0) {
  print(partial, n = 20)
  stop(str_glue("{nrow(partial)} reproducible floats are only partly covered."))
}

# 4. A verdict and a defect locus go together ----
# A zero without a locus reads as a failure of the rewrite, which it almost never is; a
# locus on a clean match is a verdict nothing supports. Both columns are checked, not just
# match_rewrite: a row where the archive no longer reproduces the article and the rewrite
# does is exactly Table 9's dispatch drift, and a locus is what records where the fault
# lies.

locus_missing <- filter(gt, (!is.na(match) & match == "0") |
                          (!is.na(match_rewrite) & match_rewrite == "0"),
                        is.na(defect_locus))
locus_spurious <- filter(gt, !is.na(match), match == "1", !is.na(match_rewrite),
                         match_rewrite == "1", !is.na(defect_locus))

if (nrow(locus_missing) > 0 || nrow(locus_spurious) > 0) {
  stop(str_glue(
    "Rows disagreeing with the article and carrying no defect locus: ",
    "{nrow(locus_missing)}. Clean matches carrying one: {nrow(locus_spurious)}."
  ))
}

stopifnot(all(na.omit(gt$defect_locus) %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved")))

# 5. The second instrument ran, printed, and named the right claims ----
# It is run here rather than read. A block that errors at its first line, or that ends in a
# bare expression and so prints nothing under source(), satisfies a scan for markers
# completely while checking nothing at all, so what is counted is what it printed.

claims_output <- capture.output(source(here::here("maintained", "in_text_claims.R")))

printed_claims <-
  tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM ")) |>
  transmute(
    claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
    value_in_text = str_match(line, "^CLAIM [^ ]+ = (.*?) \\|\\| ")[, 2]
  )

stopifnot(
  !anyDuplicated(printed_claims$claim_id),
  !any(is.na(printed_claims$claim_id)),
  !any(is.na(printed_claims$value_in_text))
)

blockless <- setdiff(must_check$claim_id, printed_claims$claim_id)
invented <- setdiff(printed_claims$claim_id, must_check$claim_id)

if (length(blockless) > 0 || length(invented) > 0) {
  stop(str_glue(
    "Coverage gate failed. ",
    "Claims with no block in maintained/in_text_claims.R ({length(blockless)}): ",
    "{str_c(head(blockless, 40), collapse = ', ')}. ",
    "Blocks naming a claim the extraction does not require ({length(invented)}): ",
    "{str_c(head(invented, 40), collapse = ', ')}."
  ))
}

stopifnot(nrow(printed_claims) == nrow(must_check))

# 6. The two instruments agree, value by value ----
# in_text_claims.R prints each number at the precision the block chose, so the ground
# truth's value is formatted to that same precision and the two strings must be identical.
# A disagreement is one of the two being wrong, and it stops the run.

decimals_of <- function(text) {
  if_else(str_detect(text, fixed(".")), nchar(str_remove(text, "^[^.]*[.]")), 0L)
}

instrument_disagreements <-
  gt |>
  drop_na(claim_id) |>
  inner_join(printed_claims, by = "claim_id") |>
  filter(!is.na(value_rewrite)) |>
  mutate(
    formatted = sprintf(str_c("%.", decimals_of(value_in_text), "f"),
                        as.numeric(value_rewrite)),
    formatted = if_else(str_detect(formatted, "^-0[.]?0*$"),
                        str_remove(formatted, "^-"), formatted)
  ) |>
  filter(formatted != value_in_text)

if (nrow(instrument_disagreements) > 0) {
  print(select(instrument_disagreements, claim_id, value_paper, value_rewrite,
               value_in_text, formatted), n = 40)
  stop(str_glue(
    "The ground truth and maintained/in_text_claims.R disagree on ",
    "{nrow(instrument_disagreements)} claims."
  ))
}

# Summary ----

print(float_coverage, n = nrow(float_coverage))

print(
  gt |>
    count(match, match_rewrite, defect_locus) |>
    arrange(defect_locus, .locale = "en")
)

print(
  gt |>
    filter(!is.na(match_rewrite), match_rewrite == "0") |>
    select(claim_id, claim, value_paper, value_rewrite, defect_locus),
  n = 20
)

print(tibble(
  claims_extracted = nrow(published_claims),
  blocks_required = nrow(must_check),
  blocks_printed = nrow(printed_claims),
  ground_truth_rows = nrow(gt),
  instruments_compared = nrow(inner_join(drop_na(gt, claim_id), printed_claims,
                                         by = "claim_id"))
))
