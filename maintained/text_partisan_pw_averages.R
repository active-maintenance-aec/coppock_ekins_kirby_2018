# coppock_ekins_kirby_2018/maintained/text_partisan_pw_averages.R
# Output: output/text_partisan_pw_averages.csv, output/text_partisan_rep_dem_difference.csv
# Depends on: helpers.R, figure_1_het_fx_by_party.R output
# Description: Precision-weighted averages of the by-party treatment effects plotted in
#   Figure 1, and the Republican-minus-Democrat difference on the MTurk main dependent
#   variables quoted on p. 77.

source(here::here("maintained", "helpers.R"))

party_estimates <- read_csv(
  here::here("maintained", "output", "figure_1_het_fx_by_party.csv"),
  show_col_types = FALSE
)

pw_averages <- party_estimates |>
  summarise(
    average_ate = weighted.mean(estimate, 1 / std.error^2),
    average_ate_se = sqrt(1 / sum(1 / std.error^2)),
    .by = c(sample, dv_type, party)
  ) |>
  arrange(sample, dv_type, party)

write_csv(pw_averages, here::here("maintained", "output", "text_partisan_pw_averages.csv"))

# Republican minus Democrat, MTurk main dependent variables ----
mturk_main <- filter(pw_averages, sample == "Mechanical Turk", dv_type == "Main DV")

rep_dem <- tibble(
  claim = c("Democrat average", "Democrat SE",
            "Republican average", "Republican SE",
            "Republican minus Democrat", "Difference SE"),
  value = c(
    mturk_main$average_ate[mturk_main$party == "Democrat"],
    mturk_main$average_ate_se[mturk_main$party == "Democrat"],
    mturk_main$average_ate[mturk_main$party == "Republican"],
    mturk_main$average_ate_se[mturk_main$party == "Republican"],
    mturk_main$average_ate[mturk_main$party == "Republican"] -
      mturk_main$average_ate[mturk_main$party == "Democrat"],
    sqrt(mturk_main$average_ate_se[mturk_main$party == "Republican"]^2 +
           mturk_main$average_ate_se[mturk_main$party == "Democrat"]^2)
  )
)

write_csv(rep_dem, here::here("maintained", "output", "text_partisan_rep_dem_difference.csv"))

print(pw_averages, n = nrow(pw_averages))
print(rep_dem)
