# coppock_ekins_kirby_2018/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Runs every deposited script, twice, and records whether each one finishes
#   and what each pass leaves behind in the folder it runs in.
#
#   The deposit is flat and every script loads its data by bare file name after a comment
#   telling the reader to set the working directory to the archive folder, so each pass
#   runs in a copy of the deposit made in a temporary directory. Running in original/
#   itself would leave the deposit's own plot output beside the deposited files, which is
#   how the copy that seeded this repository came to hold an Rplots.pdf the deposit does
#   not.
#
#   The two passes are the stripped pass, which gives the deposit only what no deposited
#   script writes, and the as-shipped pass, which gives it everything. Here they are the
#   same copy: the deposit ships three data files and no deposited script writes any of
#   them, so nothing is withheld from the stripped pass. That is asserted below rather than
#   assumed: no deposited script calls a write function, and no pass changes a deposited
#   file.
#
#   opeds_source.R is a helper that every other script sources, so it is run by them and
#   has no row of its own. opeds_monkey.R draws a figure that appears nowhere in the
#   article or its appendix, so its role is other rather than analysis.
#
#   value_script in the ground truth is committed data and is not read back from this run.
#   This script is not part of run_all.R: it needs packages the maintained rewrite no
#   longer uses (coefplot, reshape2, stargazer, xtable), and its two tables are committed
#   so that nothing downstream has to run it.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_archive.R")

deposit <- here::here("original")
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

helper_scripts <- "opeds_source.R"
other_scripts <- "opeds_monkey.R"

scripts <- manifest |>
  filter(str_ends(file, "\\.R"), !file %in% helper_scripts) |>
  transmute(script = file,
            role = if_else(script %in% other_scripts, "other", "analysis")) |>
  arrange(script, .locale = "en")

stopifnot(nrow(scripts) == 15)

# Files no deposited script writes, so the stripped pass withholds nothing ----
# A shipped intermediate is a file the deposit's own code writes. None of the deposited
# scripts calls save(), saveRDS(), or a write function, so the withheld set is empty.
deposited_code <- map_chr(file.path(deposit, manifest$file[str_ends(manifest$file, "\\.R")]),
                          \(f) str_c(read_lines(f), collapse = "\n"))
stopifnot(!any(str_detect(deposited_code, "\\bsave(RDS)?\\(|write\\.|write_|saveRDS")))
withheld <- character(0)

# Stage a copy of the deposit ----
stage <- function(label) {
  # normalizePath, because tempdir() can carry a doubled slash and macOS resolves it
  # through /private, either of which would defeat the path scrubbing below.
  root <- file.path(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                    str_c("coppock_ekins_kirby_2018_", label))
  unlink(root, recursive = TRUE)
  dir.create(root, recursive = TRUE)
  file.copy(file.path(deposit, setdiff(manifest$file, withheld)), root)
  root
}

snapshot <- function(root) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  tibble(file = files, md5 = unname(tools::md5sum(file.path(root, files))))
}

# Run each script in its own process ----
# Every deposited script opens with rm(list = ls()), which would empty this session's
# environment if they were sourced into it. print.eval reproduces what a reader running
# the script interactively sees, which is also what draws the figures.
run_one <- function(root, script) {
  log_file <- file.path(dirname(root), str_c(basename(root), "_", str_remove(script, "\\.R$"),
                                             ".log"))
  expr <- str_c("setwd(", deparse(root), "); source(", deparse(script),
                ", print.eval = TRUE)")
  status <- system2(file.path(R.home("bin"), "Rscript"), args = c("-e", shQuote(expr)),
                    stdout = log_file, stderr = log_file, timeout = 600)
  lines <- read_lines(log_file)
  first <- which(str_starts(lines, "Error"))
  stopped_at <- if (length(first) == 0) {
    NA_character_
  } else {
    lines[first[1]:min(first[1] + 3, length(lines))] |>
      discard(\(x) str_detect(x, "Execution halted")) |>
      str_c(collapse = " ") |>
      str_replace_all(fixed(root), "<archive>") |>
      str_replace_all(fixed(str_remove(root, "^/private")), "<archive>") |>
      str_squish() |>
      str_trunc(220)
  }
  tibble(script = script, exit_status = status, stopped_at = stopped_at)
}

run_pass <- function(label) {
  root <- stage(label)
  before <- snapshot(root)
  status <- map(scripts$script, \(s) run_one(root, s)) |> list_rbind()
  after <- snapshot(root)
  list(status = status, before = before, after = after)
}

passes <- list(stripped = run_pass("stripped"), as_shipped = run_pass("as_shipped"))

# The archive tables, in the canonical long form ----
run_status <- passes |>
  map(\(p) p$status) |>
  list_rbind(names_to = "pass") |>
  left_join(scripts, by = "script") |>
  transmute(
    pass, script, role, exit_status,
    outcome = case_when(
      exit_status == 0 ~ "clean",
      exit_status == 124 ~ "timeout",
      .default = "error"
    ),
    stopped_at
  )

stopifnot(nrow(run_status) == 2 * nrow(scripts), !anyNA(run_status$role))

# A file has a row only where a pass did something to it: added if the deposit's code
# wrote a file the deposit does not ship, overwritten if it wrote over a deposited file,
# deleted if it removed one. A withheld file would have a row under the stripped pass,
# and there is none.
pass_effects <- function(p) {
  full_join(p$before, p$after, by = "file", suffix = c("_before", "_after")) |>
    mutate(
      deposited_md5 = manifest$md5_served[match(file, manifest$file)],
      effect = case_when(
        is.na(md5_before) ~ "added",
        is.na(md5_after) ~ "deleted",
        md5_before != md5_after ~ "overwritten",
        .default = NA_character_
      ),
      matches_deposit = if_else(effect == "overwritten", md5_after == deposited_md5, NA)
    ) |>
    filter(!is.na(effect)) |>
    select(file, effect, matches_deposit)
}

overwrites <- passes |>
  map(pass_effects) |>
  list_rbind(names_to = "pass") |>
  bind_rows(tibble(pass = "stripped", file = withheld, effect = "withheld",
                   matches_deposit = NA)) |>
  select(pass, file, effect, matches_deposit) |>
  arrange(pass, file, .locale = "en")

stopifnot(!any(overwrites$effect %in% c("overwritten", "deleted") &
                 overwrites$file %in% manifest$file))

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status |> select(pass, script, role, outcome, stopped_at), n = nrow(run_status),
      width = 200)
print(overwrites)
