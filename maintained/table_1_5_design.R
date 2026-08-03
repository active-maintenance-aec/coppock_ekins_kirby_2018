# coppock_ekins_kirby_2018/maintained/table_1_5_design.R
# Output: output/table_1_mturk_design.csv, output/table_1_mturk_design.tex,
#   output/table_5_elite_design.csv, output/table_5_elite_design.tex
# Depends on: helpers.R
# Description: Experimental design and recontact tables for the MTurk study (Table 1)
#   and the elite study (Table 5). The two tables share a layout, so they share a script.

source(here::here("maintained", "helpers.R"))

# Counts by arm for one grouping variable ----
count_by_arm <- function(data, arms, group_var, wave, labels) {
  data |>
    count(Z, .data[[group_var]]) |>
    filter(Z %in% arms, !is.na(.data[[group_var]])) |>
    pivot_wider(names_from = Z, values_from = n, values_fill = 0) |>
    mutate(
      row = labels[as.character(.data[[group_var]])],
      wave = wave,
      totals = rowSums(across(all_of(arms)))
    ) |>
    select(wave, row, all_of(arms), totals)
}

# MTurk (Table 1) ----
mturk_n <- mturk_opeds |>
  count(Z) |>
  filter(Z %in% mturk_arms) |>
  pivot_wider(names_from = Z, values_from = n) |>
  mutate(wave = "Wave 1", row = "N", totals = rowSums(across(all_of(mturk_arms)))) |>
  select(wave, row, all_of(mturk_arms), totals)

table_1 <- bind_rows(
  mturk_n,
  count_by_arm(filter(mturk_opeds, responded_w2), mturk_arms, "Z_distract", "Wave 2",
               c(`1` = "Distraction", `0` = "No distraction")),
  count_by_arm(mturk_opeds, mturk_arms, "responded_w2", "Wave 2",
               c(`TRUE` = "Responded", `FALSE` = "Did not respond")),
  count_by_arm(mturk_opeds, mturk_arms, "responded_w3", "Wave 3",
               c(`TRUE` = "Responded", `FALSE` = "Did not respond"))
) |>
  mutate(row = factor(row, levels = c("N", "Distraction", "No distraction",
                                      "Responded", "Did not respond"))) |>
  arrange(wave, row, .locale = "en") |>
  rename_with(\(x) mturk_labels, all_of(mturk_arms)) |>
  rename(Totals = totals)

write_csv(table_1, here::here("maintained", "output", "table_1_mturk_design.csv"))

write_lines(
  kable(table_1, format = "latex", booktabs = TRUE,
        caption = "MTurk experimental design.") |>
    kable_styling(latex_options = c("hold_position", "scale_down")),
  here::here("maintained", "output", "table_1_mturk_design.tex")
)

# Elite (Table 5) ----
elite_n <- elite_opeds |>
  count(Z) |>
  filter(Z %in% elite_arms) |>
  pivot_wider(names_from = Z, values_from = n) |>
  mutate(wave = "Wave 1", row = "N", totals = rowSums(across(all_of(elite_arms)))) |>
  select(wave, row, all_of(elite_arms), totals)

table_5 <- bind_rows(
  elite_n,
  count_by_arm(filter(elite_opeds, responded_w2), elite_arms, "Z_distract", "Wave 2",
               c(`1` = "Distraction", `0` = "No distraction")),
  count_by_arm(elite_opeds, elite_arms, "responded_w2", "Wave 2",
               c(`TRUE` = "Responded", `FALSE` = "Did not respond"))
) |>
  mutate(row = factor(row, levels = c("N", "Distraction", "No distraction",
                                      "Responded", "Did not respond"))) |>
  arrange(wave, row, .locale = "en") |>
  rename_with(\(x) elite_labels, all_of(elite_arms)) |>
  rename(Totals = totals)

write_csv(table_5, here::here("maintained", "output", "table_5_elite_design.csv"))

write_lines(
  kable(table_5, format = "latex", booktabs = TRUE,
        caption = "Elite experimental design.") |>
    kable_styling(latex_options = c("hold_position", "scale_down")),
  here::here("maintained", "output", "table_5_elite_design.tex")
)

print(table_1)
print(table_5)
