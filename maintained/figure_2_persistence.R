# coppock_ekins_kirby_2018/maintained/figure_2_persistence.R
# Output: output/figure_2_persistence.pdf, output/figure_2_persistence.png,
#   output/figure_2_persistence.csv
# Depends on: helpers.R
# Description: Group means on the composite scale dependent variables at 0, 10 and 30
#   days after treatment, for both experimental samples (Figure 2). Panel subjects only,
#   so the same people contribute to every wave.

source(here::here("maintained", "helpers.R"))

long_mturk <- mturk_opeds |>
  filter(responded_w3, responded_w2) |>
  select(subject_id, Z, matches("_scale_w[123]$")) |>
  select(-contains("mj")) |>
  pivot_longer(-c(subject_id, Z), names_to = "variable") |>
  mutate(study = "MTurk Study")

long_elite <- elite_opeds |>
  filter(responded_w2) |>
  select(subject_id, Z, matches("_scale_w[12]$")) |>
  select(-contains("mj")) |>
  pivot_longer(-c(subject_id, Z), names_to = "variable") |>
  mutate(study = "Elite Study")

long_df <- bind_rows(long_mturk, long_elite) |>
  mutate(
    dv = str_split_i(variable, "_", 2),
    wave = str_split_i(variable, "_", 4)
  )

group_means <- long_df |>
  group_by(study, Z, dv, wave) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  select(study, Z, dv, wave, estimate, std.error, conf.low, conf.high)

write_csv(group_means, here::here("maintained", "output", "figure_2_persistence.csv"))

gg_df <- group_means |>
  mutate(
    Z = factor(Z,
               levels = c("control", "amtrak", "veterans", "climate", "wallstreet", "paul"),
               labels = c("Control", "Treatment: Amtrak", "Treatment: Veterans",
                          "Treatment: Climate", "Treatment: Wall Street",
                          "Treatment: Flat Tax")),
    dv = factor(dv,
                levels = c("amtrak", "vets", "wall", "flat", "climate"),
                labels = c("Outcomes: Amtrak", "Outcomes: Veterans",
                           "Outcomes: Wall Street", "Outcomes: Flat Tax",
                           "Outcomes: Climate")),
    days = as.numeric(as.character(factor(wave, levels = c("w1", "w2", "w3"),
                                          labels = c("0", "10", "30")))),
    study = factor(study, levels = c("MTurk Study", "Elite Study"))
  )

g <- ggplot(gg_df, aes(x = days, y = estimate, group = Z, color = Z, fill = Z, shape = Z)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, show.legend = FALSE) +
  geom_line(alpha = 0.1) +
  geom_point(size = 4) +
  facet_grid(dv ~ study) +
  scale_x_continuous(breaks = c(0, 10, 30)) +
  theme_bw() +
  ylab("Composite Attitude Scale") +
  xlab("Days Since Treatment") +
  guides(linetype = "none", fill = "none", alpha = "none") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank()
  )

ggsave(here::here("maintained", "output", "figure_2_persistence.pdf"),
       plot = g, width = 9, height = 10)
ggsave(here::here("maintained", "output", "figure_2_persistence.png"),
       plot = g, width = 9, height = 10, dpi = 300)
