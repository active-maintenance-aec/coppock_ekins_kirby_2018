# coppock_ekins_kirby_2018/maintained/text_recontact_rates.R
# Output: output/text_recontact_rates.csv
# Depends on: helpers.R
# Description: Enrolment counts, recontact rates and the chi-square tests of
#   independence between treatment assignment and later-wave response, quoted on
#   pp. 66 and 72 of the article.

source(here::here("maintained", "helpers.R"))

chisq_w2_mturk <- chisq.test(table(mturk_opeds$responded_w2, mturk_opeds$Z))
chisq_w3_mturk <- chisq.test(table(mturk_opeds$responded_w3, mturk_opeds$Z))
chisq_w2_elite <- chisq.test(table(elite_opeds$responded_w2, elite_opeds$Z))

text_recontact <- tibble(
  claim = c(
    "MTurk subjects enrolled",
    "MTurk responded in Wave 2",
    "MTurk Wave 2 recontact rate (percent)",
    "MTurk responded in Wave 3",
    "MTurk Wave 3 recontact rate (percent)",
    "MTurk Wave 2 treatment-by-response chi-square p",
    "MTurk Wave 3 treatment-by-response chi-square p",
    "Elite subjects enrolled",
    "Elite responded in Wave 2",
    "Elite Wave 2 recontact rate (percent)",
    "Elite Wave 2 treatment-by-response chi-square p"
  ),
  value = c(
    nrow(mturk_opeds),
    sum(mturk_opeds$responded_w2),
    100 * mean(mturk_opeds$responded_w2),
    sum(mturk_opeds$responded_w3),
    100 * mean(mturk_opeds$responded_w3),
    chisq_w2_mturk$p.value,
    chisq_w3_mturk$p.value,
    nrow(elite_opeds),
    sum(elite_opeds$responded_w2),
    100 * mean(elite_opeds$responded_w2),
    chisq_w2_elite$p.value
  )
)

write_csv(text_recontact, here::here("maintained", "output", "text_recontact_rates.csv"))

print(text_recontact, n = nrow(text_recontact))
