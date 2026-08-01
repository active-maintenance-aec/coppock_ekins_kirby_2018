# coppock_ekins_kirby_2018/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then every published table and figure, then the in-text quantities.
# Every script is self-contained and can also be run on its own, with the one
# exception noted below.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Tables ----
source(here::here("maintained", "table_1_5_design.R"))
source(here::here("maintained", "table_3_4_mturk_effects.R"))
source(here::here("maintained", "table_6_7_elite_effects.R"))
source(here::here("maintained", "table_8_compare_samples.R"))
source(here::here("maintained", "table_9_het_fx_ftests.R"))
source(here::here("maintained", "table_11_agreement.R"))

# Figures ----
source(here::here("maintained", "figure_1_het_fx_by_party.R"))
source(here::here("maintained", "figure_2_persistence.R"))

# In-text quantities ----
# text_partisan_pw_averages.R averages the estimates figure_1 writes, so it runs after it.
source(here::here("maintained", "text_recontact_rates.R"))
source(here::here("maintained", "text_partisan_pw_averages.R"))
source(here::here("maintained", "text_reading_time.R"))
