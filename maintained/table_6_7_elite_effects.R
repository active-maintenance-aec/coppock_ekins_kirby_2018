# coppock_ekins_kirby_2018/maintained/table_6_7_elite_effects.R
# Output: output/table_6_elite_main_dv.csv, output/table_6_elite_main_dv.tex,
#   output/table_7_elite_scale_dv.csv, output/table_7_elite_scale_dv.tex
# Depends on: helpers.R
# Description: Elite treatment effects on the four main dependent variables (Table 6)
#   and the four composite scale dependent variables (Table 7). The elite study has
#   no climate arm.

source(here::here("maintained", "helpers.R"))

elite_w1 <- filter(elite_opeds, responded_w1)

main_dvs <- c(Amtrak = "dv_amtrak_main_w1", `Flat Tax` = "dv_flat_main_w1",
              Veterans = "dv_vets_main_w1", `Wall Street` = "dv_wall_main_w1")

scale_dvs <- c(Amtrak = "dv_amtrak_scale_w1", `Flat Tax` = "dv_flat_scale_w1",
               Veterans = "dv_vets_scale_w1", `Wall Street` = "dv_wall_scale_w1")

elite_oped_labels <- oped_labels[names(oped_labels) != "Zclimate"]

# Table 6: main dependent variables ----
table_6 <- tibble(dv = names(main_dvs), dv_col = unname(main_dvs)) |>
  mutate(fits = map(dv_col, tidy_arm_effects, data = elite_w1)) |>
  unnest(fits) |>
  mutate(op_ed = if_else(term == "(Intercept)", "Constant", unname(oped_labels[term]))) |>
  select(dv, op_ed, estimate, std.error, p.value, conf.low, conf.high, nobs, r_squared)

write_csv(table_6, here::here("maintained", "output", "table_6_elite_main_dv.csv"))

fits_main <- map(main_dvs, \(dv) lm_robust(as.formula(paste0(dv, " ~ Z")),
                                           data = elite_w1, se_type = "HC2"))

modelsummary(
  fits_main,
  output = here::here("maintained", "output", "table_6_elite_main_dv.tex"),
  coef_map = c(elite_oped_labels, `(Intercept)` = "Constant"),
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Elite experiment: Treatment effects on main dependent variables.",
  notes = "Models estimated via OLS. HC2 robust standard errors in parentheses."
)

# Table 7: composite scale dependent variables ----
table_7 <- tibble(dv = names(scale_dvs), dv_col = unname(scale_dvs)) |>
  mutate(fits = map(dv_col, tidy_arm_effects, data = elite_w1)) |>
  unnest(fits) |>
  mutate(op_ed = if_else(term == "(Intercept)", "Constant", unname(oped_labels[term]))) |>
  select(dv, op_ed, estimate, std.error, p.value, conf.low, conf.high, nobs, r_squared)

write_csv(table_7, here::here("maintained", "output", "table_7_elite_scale_dv.csv"))

fits_scale <- map(scale_dvs, \(dv) lm_robust(as.formula(paste0(dv, " ~ Z")),
                                             data = elite_w1, se_type = "HC2"))

modelsummary(
  fits_scale,
  output = here::here("maintained", "output", "table_7_elite_scale_dv.tex"),
  coef_map = c(elite_oped_labels, `(Intercept)` = "Constant"),
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Elite experiment: Treatment effects on composite scale dependent variables.",
  notes = c("Models estimated via OLS. HC2 robust standard errors in parentheses.",
            "Dependent variables are constructed via factor analysis and have unit variance.")
)

print(table_6)
print(table_7)
