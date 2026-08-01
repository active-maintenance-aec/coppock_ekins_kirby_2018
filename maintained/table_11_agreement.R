# coppock_ekins_kirby_2018/maintained/table_11_agreement.R
# Output: output/table_11_agreement.csv, output/table_11_agreement.tex,
#   output/text_agreement_pw_averages.csv
# Depends on: helpers.R
# Description: Effects of each op-ed on its own "agreement" dependent variable (Table 11),
#   dichotomized at the 25th, 50th and 75th percentiles of the MTurk control group, plus
#   the precision-weighted averages the text quotes as 20 and 12 percentage points.

source(here::here("maintained", "helpers.R"))

specs <- bind_rows(
  tribble(
    ~sample, ~issue, ~arm, ~stem,
    "MTurk", "Amtrak", "amtrak", "amtrak",
    "MTurk", "Climate", "climate", "climate",
    "MTurk", "Flat Tax", "paul", "flat",
    "MTurk", "Veterans", "veterans", "vets",
    "MTurk", "Wall Street", "wallstreet", "wall"
  ),
  tribble(
    ~sample, ~issue, ~arm, ~stem,
    "Elite", "Amtrak", "amtrak", "amtrak",
    "Elite", "Flat Tax", "paul", "flat",
    "Elite", "Veterans", "veterans", "vets",
    "Elite", "Wall Street", "wallstreet", "wall"
  )
) |>
  crossing(split = c(25, 50, 75)) |>
  mutate(dv = if_else(split == 50,
                      paste0("dv_", stem, "_agree_w1"),
                      paste0("dv_", stem, "_agree_", split, "_w1")))

table_11 <- specs |>
  mutate(
    dat = map(sample, \(s) if (s == "MTurk") mturk_opeds else elite_opeds),
    fit = pmap(list(dv, arm, dat), tidy_single_arm)
  ) |>
  select(sample, issue, split, dv, fit) |>
  unnest(fit)

write_csv(table_11, here::here("maintained", "output", "table_11_agreement.csv"))

# Precision-weighted average across issues, within sample and split ----
pw_averages <- table_11 |>
  summarise(
    estimate = weighted.mean(estimate, 1 / std.error^2),
    std.error = sqrt(1 / sum(1 / std.error^2)),
    .by = c(sample, split)
  ) |>
  mutate(issue = "Precision weighted average") |>
  select(sample, issue, split, estimate, std.error)

write_csv(pw_averages, here::here("maintained", "output", "text_agreement_pw_averages.csv"))

# Published layout: issue rows, six columns of "estimate (se)" ----
table_11_display <- bind_rows(select(table_11, sample, issue, split, estimate, std.error),
                              pw_averages) |>
  mutate(
    entry = paste0(sprintf("%.2f", estimate), " (", sprintf("%.2f", std.error), ")"),
    column = paste0(split, "th ", sample)
  ) |>
  select(issue, column, entry) |>
  pivot_wider(names_from = column, values_from = entry) |>
  select(issue,
         `25th Elite`, `25th MTurk`, `50th Elite`, `50th MTurk`,
         `75th Elite`, `75th MTurk`) |>
  arrange(match(issue, c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street",
                         "Precision weighted average")))

write_lines(
  kable(table_11_display, format = "latex", booktabs = TRUE,
        caption = "Effects of op-eds on \"agreement\" dependent variables.") |>
    kable_styling(latex_options = c("hold_position", "scale_down")),
  here::here("maintained", "output", "table_11_agreement.tex")
)

print(table_11, n = nrow(table_11))
print(table_11_display)
