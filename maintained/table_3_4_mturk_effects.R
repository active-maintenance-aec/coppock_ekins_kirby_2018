# coppock_ekins_kirby_2018/maintained/table_3_4_mturk_effects.R
# Output: output/table_3_mturk_main_dv.csv, output/table_3_mturk_main_dv.tex,
#   output/table_4_mturk_scale_dv.csv, output/table_4_mturk_scale_dv.tex
# Depends on: helpers.R
# Description: MTurk treatment effects on the five main dependent variables (Table 3)
#   and the five composite scale dependent variables (Table 4).

source(here::here("maintained", "helpers.R"))

mturk_w1 <- filter(mturk_opeds, responded_w1)

main_dvs <- c(Amtrak = "dv_amtrak_main_w1", Climate = "dv_climate_main_w1",
              `Flat Tax` = "dv_flat_main_w1", Veterans = "dv_vets_main_w1",
              `Wall Street` = "dv_wall_main_w1")

scale_dvs <- c(Amtrak = "dv_amtrak_scale_w1", Climate = "dv_climate_scale_w1",
               `Flat Tax` = "dv_flat_scale_w1", Veterans = "dv_vets_scale_w1",
               `Wall Street` = "dv_wall_scale_w1")

# Table 3: main dependent variables ----
table_3 <- tibble(dv = names(main_dvs), dv_col = unname(main_dvs)) |>
  mutate(fits = map(dv_col, tidy_arm_effects, data = mturk_w1)) |>
  unnest(fits) |>
  mutate(op_ed = if_else(term == "(Intercept)", "Constant", unname(oped_labels[term]))) |>
  select(dv, op_ed, estimate, std.error, p.value, conf.low, conf.high, nobs, r_squared)

write_csv(table_3, here::here("maintained", "output", "table_3_mturk_main_dv.csv"))

fits_main <- map(main_dvs, \(dv) lm_robust(as.formula(paste0(dv, " ~ Z")),
                                           data = mturk_w1, se_type = "HC2"))

modelsummary(
  fits_main,
  output = here::here("maintained", "output", "table_3_mturk_main_dv.tex"),
  coef_map = c(oped_labels, `(Intercept)` = "Constant"),
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "MTurk experiment: Treatment effects on main dependent variables.",
  notes = "Models estimated via OLS. HC2 robust standard errors in parentheses."
)

# Table 4: composite scale dependent variables ----
table_4 <- tibble(dv = names(scale_dvs), dv_col = unname(scale_dvs)) |>
  mutate(fits = map(dv_col, tidy_arm_effects, data = mturk_w1)) |>
  unnest(fits) |>
  mutate(op_ed = if_else(term == "(Intercept)", "Constant", unname(oped_labels[term]))) |>
  select(dv, op_ed, estimate, std.error, p.value, conf.low, conf.high, nobs, r_squared)

write_csv(table_4, here::here("maintained", "output", "table_4_mturk_scale_dv.csv"))

fits_scale <- map(scale_dvs, \(dv) lm_robust(as.formula(paste0(dv, " ~ Z")),
                                             data = mturk_w1, se_type = "HC2"))

modelsummary(
  fits_scale,
  output = here::here("maintained", "output", "table_4_mturk_scale_dv.tex"),
  coef_map = c(oped_labels, `(Intercept)` = "Constant"),
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "MTurk experiment: Treatment effects on composite scale dependent variables.",
  notes = c("Models estimated via OLS. HC2 robust standard errors in parentheses.",
            "Dependent variables are constructed via factor analysis and have unit variance.")
)

print(table_3)
print(table_4)
