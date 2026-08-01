# coppock_ekins_kirby_2018/maintained/text_reading_time.R
# Output: output/text_reading_time.csv, output/text_reading_time_differences.csv
# Depends on: helpers.R
# Description: Time spent reading the treatment op-eds, supporting the claims on
#   pp. 69 and 72 that subjects read for two to four minutes and that elites spent
#   25 to 45 seconds less than MTurk subjects. The archive's opeds_compliance.R ends
#   with two subtractions of numbers typed in by hand; here both are computed.

source(here::here("maintained", "helpers.R"))

# Trimmed mean seconds by arm, matching the archive's trim = 0.01 ----
reading_by_arm <- bind_rows(
  mturk_opeds |> mutate(sample = "MTurk"),
  elite_opeds |> mutate(sample = "Elite")
) |>
  as_tibble() |>
  filter(Z != "control") |>
  summarise(
    n = sum(!is.na(time_spent_reading)),
    mean_seconds = mean(time_spent_reading, na.rm = TRUE),
    trimmed_seconds = mean(time_spent_reading, na.rm = TRUE, trim = 0.01),
    trimmed_minutes = mean(time_spent_reading, na.rm = TRUE, trim = 0.01) / 60,
    .by = c(sample, Z)
  ) |>
  arrange(sample, Z)

write_csv(reading_by_arm, here::here("maintained", "output", "text_reading_time.csv"))

# MTurk minus elite, by arm, in seconds ----
reading_differences <- reading_by_arm |>
  select(sample, Z, trimmed_seconds) |>
  pivot_wider(names_from = sample, values_from = trimmed_seconds) |>
  filter(!is.na(Elite)) |>
  mutate(mturk_minus_elite_seconds = MTurk - Elite)

write_csv(reading_differences,
          here::here("maintained", "output", "text_reading_time_differences.csv"))

print(reading_by_arm, n = nrow(reading_by_arm))
print(reading_differences)
