# coppock_ekins_kirby_2018/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_ekins_kirby_2018_ground_truth.csv (its derived columns),
#   ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv,
#   ground_truth/coppock_ekins_kirby_2018_ground_truth.csv, errata_entries.csv,
#   maintained/in_text_claims.R, everything in maintained/output/
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
#   It writes the columns that are derived rather than observed: location, claim_type,
#   and digits from the extraction, and disposition from the errata spine. Everything else in
#   the ground truth is read and written back unchanged, so a run that changes no input
#   leaves the file byte for byte as it was.
#
#   The checks, in order:
#     1. the two files agree on what the article says
#     2. every claim id is real, unique and accounted for
#     3. every published float is covered, and by what fraction of its cells
#     4. a verdict and a defect locus go together
#     5. the second instrument ran, printed, and named exactly the claims it should
#     6. the second instrument agrees with the ground truth, value by value
#     7. every paper_internal defect carries a disposition

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

gt_path <- here::here("ground_truth", "coppock_ekins_kirby_2018_ground_truth.csv")

gt <- read_csv(
  gt_path,
  col_types = cols(.default = col_character(), match = col_logical(),
                   match_rewrite = col_logical(), holds = col_logical())
)

gt_columns <- c("paper_id", "claim_id", "location", "claim_type", "claim", "value_paper",
                "digits", "value_script", "match", "value_rewrite", "match_rewrite", "holds",
                "defect_locus", "disposition", "disposition_reason", "notes")

stopifnot(
  identical(names(gt), gt_columns),
  identical(
    names(published_claims),
    c("claim_id", "location", "claim_type", "claim", "value_paper", "digits", "comparison",
      "expect", "needs_block", "notes")
  ),
  all(published_claims$claim_type %in%
        c("pipeline", "definitional", "structural", "descriptive", "transcribed")),
  all(na.omit(published_claims$comparison) %in% c("==", "<", "<=", ">", ">=", "approx")),
  all(is.na(published_claims$comparison) == is.na(published_claims$value_paper)),
  all(na.omit(published_claims$expect) %in% c("range", "derived", "absent", "shape")),
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
float_check <-
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

missing_floats <- setdiff(published_float_inventory, float_check$location)
if (length(missing_floats) > 0) {
  stop(str_glue("Published floats absent from the extraction: ",
                "{str_c(missing_floats, collapse = ', ')}."))
}

partial <- filter(float_check, location %in% reproducible_floats,
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

locus_missing <- filter(gt, (!is.na(match) & !match) |
                          (!is.na(match_rewrite) & !match_rewrite),
                        is.na(defect_locus))
locus_spurious <- filter(gt, !is.na(match), match, !is.na(match_rewrite),
                         match_rewrite, !is.na(defect_locus))

if (nrow(locus_missing) > 0 || nrow(locus_spurious) > 0) {
  stop(str_glue(
    "Rows disagreeing with the article and carrying no defect locus: ",
    "{nrow(locus_missing)}. Clean matches carrying one: {nrow(locus_spurious)}."
  ))
}

stopifnot(all(na.omit(gt$defect_locus) %in%
                c("archive", "paper_internal", "environment", "rewrite", "unresolved",
                  "upstream")))

# 5. The second instrument ran, printed, and named the right claims ----
# It is run here rather than read. A block that errors at its first line, or that ends in a
# bare expression and so prints nothing under source(), satisfies a scan for markers
# completely while checking nothing at all, so what is counted is what it printed.

claims_output <- capture.output(source(here::here("maintained", "in_text_claims.R")))

printed_claims <-
  tibble(line = claims_output) |>
  # The CLAIM SUMMARY lines claim_summary() prints carry no " = " and are not claims.
  filter(str_detect(line, "^CLAIM [^ ]+ = ")) |>
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

# The errata spine's claim_ids ----
# errata_entries.csv names, for every published entry, the ground-truth claims it corrects.
# Every one of those ids has to exist here: a missing one is a typo or a claim that has since
# been renamed, and a dangling reference inside a document whose whole purpose is correcting
# the record is worse than a failed build.
cited_ids <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE)$claim_ids |>
  str_split(";") |>
  unlist() |>
  str_trim() |>
  discard(\(x) is.na(x) | x == "")
dangling <- setdiff(cited_ids, gt$claim_id)
if (length(dangling) > 0) print(dangling)
stopifnot(length(dangling) == 0)

# Location, claim type, and printed precision, carried from the extraction ----
# Every keyed row has an extraction row, float cells included, so each takes the
# extraction's location, class, and digit count. The two unkeyed rows (the reading-time
# gaps for the two op-eds the sentence on p. 72 does not bound) keep the location they
# carry. Nothing here overrides the extraction's digits.
gt <- gt |>
  select(-claim_type, -digits) |>
  left_join(select(published_claims, claim_id, extracted_location = location, claim_type,
                   digits),
            by = "claim_id") |>
  mutate(
    location = if_else(is.na(claim_id), location, extracted_location),
    claim_type = if_else(is.na(claim_id), "pipeline", claim_type),
    digits = if_else(is.na(claim_id), as.character(excheckr::claim_digits(value_paper)),
                     digits)
  )

stopifnot(!anyNA(gt$location), !anyNA(gt$claim_type))

# 7. Disposition: what became of each defect ----
# corrected_here is computed from the errata spine and never typed. Every other judgment is
# one the report already makes, keyed by defect locus; a defect nobody has judged stays NA.
disposition_judgments <- tribble(
  ~defect_locus, ~judgment, ~reason,
  "environment", "documented_only",
  str_c("The report's Table 9 section documents the drift: current estimatr gives the ",
        "archive's waldtest() call an HC2 variance, so the deposited line no longer returns ",
        "the classical F test the article prints. The rewrite reports both tests and its ",
        "classical column reproduces every published cell, so the article needs no erratum.")
)

gt <- gt |>
  left_join(disposition_judgments, by = "defect_locus") |>
  mutate(
    disposition = case_when(
      is.na(defect_locus) ~ NA_character_,
      !is.na(claim_id) & claim_id %in% cited_ids ~ "corrected_here",
      .default = judgment
    ),
    disposition_reason = if_else(!is.na(disposition) & disposition != "corrected_here",
                                 reason, NA_character_)
  )

undisposed <- filter(gt, defect_locus == "paper_internal", is.na(disposition))
if (nrow(undisposed) > 0) {
  print(select(undisposed, claim_id, location, claim), n = nrow(undisposed))
  stop(str_glue("{nrow(undisposed)} paper_internal rows carry no disposition."))
}
print(count(filter(gt, !is.na(defect_locus)), defect_locus, disposition))

# Float coverage ----
# One row per published float, labelled as printed. A table's published_numbers counts the
# numbers the extraction transcribed off its face; covered counts those with a ground truth
# row, and the two reproduced columns count the ones each pipeline matches. Neither figure
# prints a number on its face (the axis labels are scale, not data), so each carries the
# count of quantities it plots instead.
float_labels <- c(str_c("Table ", 1:11), "Figure 1", "Figure 2")

float_cells <- gt |>
  mutate(float = str_remove(location, " \\(p\\. \\d+\\)$")) |>
  filter(float %in% float_labels, str_starts(float, "Table"))

cell_counts <- float_cells |>
  summarise(
    covered = sum(!is.na(value_paper)),
    reproduced_by_rewrite = sum(match_rewrite, na.rm = TRUE),
    reproduced_by_archive = if_else(all(is.na(match)), NA_integer_,
                                    sum(match, na.rm = TRUE)),
    .by = float
  )

printed_numbers <- published_claims |>
  mutate(float = str_remove(location, " \\(p\\. \\d+\\)$")) |>
  filter(float %in% float_labels) |>
  summarise(published_numbers = sum(!is.na(value_paper)), .by = float)

plotted_counts <- tibble(
  float = c("Figure 1", "Figure 2"),
  plotted = map_int(c("fig1_n_estimates", "fig2_n_group_means"),
                    \(id) as.integer(gt$value_rewrite[which(gt$claim_id == id)]))
)

float_notes <- c(
  "Table 2" = "Op-ed topics, titles, authors, and outlets; prints no number.",
  "Table 10" = str_c("Circulation figures from an auditing bureau and two publishers' ",
                     "surveys times an assumed readership share; none is in the deposit."),
  "Figure 1" = str_c("Prints no number on its face. The article states no count to compare ",
                     "against, so the ground truth carries no verdict on the plotted count."),
  "Figure 2" = str_c("Prints no number on its face. The article states no count to compare ",
                     "against, so the ground truth carries no verdict on the plotted count.")
)

float_coverage <- tibble(float = float_labels) |>
  left_join(printed_numbers, by = "float") |>
  left_join(cell_counts, by = "float") |>
  left_join(plotted_counts, by = "float") |>
  mutate(
    is_figure = str_starts(float, "Figure"),
    published_numbers = if_else(is_figure, 0L, published_numbers),
    covered = replace_na(covered, 0L),
    reproduced_by_rewrite = replace_na(reproduced_by_rewrite, 0L),
    plotted_reproduced = NA_integer_,
    note = unname(float_notes[float])
  ) |>
  select(float, published_numbers, covered, reproduced_by_rewrite, reproduced_by_archive,
         plotted, plotted_reproduced, note)

# Every table the deposit can produce is covered cell for cell.
stopifnot(
  !anyNA(float_coverage$published_numbers),
  with(filter(float_coverage, !float %in% c("Table 2", "Table 10"), str_starts(float, "Table")),
       all(covered == published_numbers & published_numbers > 0))
)

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"), na = "NA")

# The ground truth, in the canonical column order ----
gt <- select(gt, all_of(gt_columns))
write_csv(gt, gt_path, na = "NA")

# Summary ----

print(float_check, n = nrow(float_check))
print(float_coverage, n = nrow(float_coverage), width = 200)

print(
  gt |>
    count(match, match_rewrite, defect_locus) |>
    arrange(defect_locus, .locale = "en")
)

print(
  gt |>
    filter(!is.na(match_rewrite), !match_rewrite) |>
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
