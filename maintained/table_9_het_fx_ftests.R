# coppock_ekins_kirby_2018/maintained/table_9_het_fx_ftests.R
# Output: output/table_9_het_fx_ftests.csv, output/table_9_het_fx_ftests.tex
# Depends on: helpers.R
# Description: Joint F tests of the null that treatment effects are equal for
#   Democrats, Independents and Republicans (Table 9). Two p-values are reported per
#   cell. p_classical is the published quantity: when the archive was written,
#   lmtest::waldtest() had no robust variance to take from an lm_robust fit and fell
#   back to the classical F. p_hc2 is what the same archive code returns today, now
#   that estimatr supplies a vcov method. The choice moves every cell and changes no
#   conclusion; see the report.

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
  crossing(dv_type = c("Main DV", "Scale DV")) |>
  mutate(dv = paste0("dv_", stem, "_", if_else(dv_type == "Main DV", "main", "scale"), "_w1"))

table_9 <- specs |>
  mutate(
    dat = map(sample, \(s) if (s == "MTurk") mturk_opeds else elite_opeds),
    p_classical = pmap_dbl(list(dv, arm, dat), \(dv, arm, dat)
                           party_ftest(dv, arm, dat, se_type = "classical")),
    p_hc2 = pmap_dbl(list(dv, arm, dat), \(dv, arm, dat)
                     party_ftest(dv, arm, dat, se_type = "HC2"))
  ) |>
  select(sample, issue, dv_type, dv, p_classical, p_hc2) |>
  arrange(desc(sample), issue, dv_type, .locale = "en")

write_csv(table_9, here::here("maintained", "output", "table_9_het_fx_ftests.csv"))

# Published layout: one row per issue, four p-value columns ----
table_9_display <- table_9 |>
  mutate(column = paste(sample, dv_type)) |>
  select(issue, column, p_classical) |>
  pivot_wider(names_from = column, values_from = p_classical) |>
  select(issue, `MTurk Main DV`, `MTurk Scale DV`, `Elite Main DV`, `Elite Scale DV`) |>
  arrange(match(issue, c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street")))

write_lines(
  kable(table_9_display, format = "latex", booktabs = TRUE, digits = 3,
        caption = "Joint tests of treatment effect heterogeneity by partisanship.") |>
    kable_styling(latex_options = c("hold_position")),
  here::here("maintained", "output", "table_9_het_fx_ftests.tex")
)

print(table_9, n = nrow(table_9))
print(table_9_display)
