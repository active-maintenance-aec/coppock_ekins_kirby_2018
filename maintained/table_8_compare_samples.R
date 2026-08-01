# coppock_ekins_kirby_2018/maintained/table_8_compare_samples.R
# Output: output/table_8_compare_samples.csv, output/table_8_compare_samples.tex
# Depends on: helpers.R
# Description: Interaction models comparing MTurk and elite treatment effects on the
#   composite scale dependent variables (Table 8). The climate arm is dropped because
#   the elite study did not run it.

source(here::here("maintained", "helpers.R"))

stacked <- bind_rows(mturk = mturk_opeds, elite = elite_opeds, .id = "experimental_sample") |>
  filter(Z != "climate") |>
  mutate(
    experimental_sample = factor(experimental_sample, levels = c("mturk", "elite")),
    Z_match = factor(Z,
                     levels = c("control", "amtrak", "paul", "veterans", "wallstreet"),
                     labels = c("control", "amtrak", "flat", "vets", "wall"))
  ) |>
  filter(responded_w1)

scale_dvs <- c(Amtrak = "dv_amtrak_scale_w1", `Flat Tax` = "dv_flat_scale_w1",
               Veterans = "dv_vets_scale_w1", `Wall Street` = "dv_wall_scale_w1")

fits <- map(scale_dvs, \(dv) lm_robust(
  as.formula(paste0(dv, " ~ Z_match * experimental_sample")),
  data = stacked, se_type = "HC2"
))

table_8 <- tibble(dv = names(fits), fit = fits) |>
  mutate(tidied = map(fit, \(f) tidy(f) |>
                        select(term, estimate, std.error, p.value, conf.low, conf.high) |>
                        mutate(nobs = f$nobs, r_squared = f$r.squared))) |>
  select(dv, tidied) |>
  unnest(tidied)

write_csv(table_8, here::here("maintained", "output", "table_8_compare_samples.csv"))

interaction_labels <- c(
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

modelsummary(
  fits,
  output = here::here("maintained", "output", "table_8_compare_samples.tex"),
  coef_map = interaction_labels,
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Comparison of treatment effects on composite scale dependent variables.",
  notes = c("Models estimated via OLS. HC2 robust standard errors in parentheses.",
            "Dependent variables are constructed via factor analysis and have unit variance.")
)

print(table_8)
