# coppock_ekins_kirby_2018/maintained/figure_1_het_fx_by_party.R
# Output: output/figure_1_het_fx_by_party.pdf, output/figure_1_het_fx_by_party.png,
#   output/figure_1_het_fx_by_party.csv
# Depends on: helpers.R
# Description: Treatment effects on each target dependent variable estimated separately
#   within each party group, in both experimental samples (Figure 1). The estimates are
#   written to CSV because text_partisan_pw_averages.R averages them.

source(here::here("maintained", "helpers.R"))

specs <- bind_rows(
  crossing(
    tibble(sample = "Mechanical Turk",
           issue = c("Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street"),
           arm = c("amtrak", "climate", "paul", "veterans", "wallstreet"),
           stem = c("amtrak", "climate", "flat", "vets", "wall")),
    tibble(dv_type = c("Main DV", "Scale DV"))
  ),
  crossing(
    tibble(sample = "Elites",
           issue = c("Amtrak", "Flat Tax", "Veterans", "Wall Street"),
           arm = c("amtrak", "paul", "veterans", "wallstreet"),
           stem = c("amtrak", "flat", "vets", "wall")),
    tibble(dv_type = c("Main DV", "Scale DV"))
  )
) |>
  crossing(party = parties) |>
  mutate(dv = paste0("dv_", stem, "_", if_else(dv_type == "Main DV", "main", "scale"), "_w1"))

estimates <- specs |>
  mutate(
    dat = map(sample, \(s) if (s == "Mechanical Turk") mturk_opeds else elite_opeds),
    fit = pmap(list(dv, arm, party, dat), function(dv, arm, party, dat) {
      d <- filter(dat, Z %in% c("control", arm), pid_3_cat == party)
      lm_robust(as.formula(paste0(dv, " ~ Z")), data = d, se_type = "HC2") |>
        tidy() |>
        filter(term != "(Intercept)") |>
        select(estimate, std.error, p.value, conf.low, conf.high)
    })
  ) |>
  select(sample, issue, dv_type, party, dv, fit) |>
  unnest(fit)

write_csv(estimates, here::here("maintained", "output", "figure_1_het_fx_by_party.csv"))

gg_df <- estimates |>
  mutate(
    party = factor(party, levels = parties),
    sample = factor(sample, levels = c("Mechanical Turk", "Elites")),
    issue = factor(issue, levels = rev(c("Amtrak", "Climate", "Flat Tax",
                                         "Veterans", "Wall Street")))
  )

g <- ggplot(gg_df, aes(y = issue, x = estimate, color = party, shape = party)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_color_manual(values = c(Democrat = "blue", Independent = "purple",
                                Republican = "red")) +
  coord_cartesian(xlim = c(-0.1, 1.5)) +
  facet_grid(sample ~ dv_type, scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    legend.key.width = unit(4, "lines"),
    strip.background = element_blank()
  )

ggsave(here::here("maintained", "output", "figure_1_het_fx_by_party.pdf"),
       plot = g, width = 9, height = 7)
ggsave(here::here("maintained", "output", "figure_1_het_fx_by_party.png"),
       plot = g, width = 9, height = 7, dpi = 300)
