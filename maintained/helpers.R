# coppock_ekins_kirby_2018/maintained/helpers.R
# Output: none
# Depends on: original/mturk_opeds_cleaned.RData, original/elite_opeds_cleaned.RData
# Description: Packages, deposited data, and shared helpers for every script in maintained/.

library(here)
library(tidyverse)
library(estimatr)
library(lmtest)
library(modelsummary)
library(knitr)
library(kableExtra)

here::i_am("maintained/helpers.R")

options(modelsummary_format_numeric_latex = "plain")

# Deposited data ----
# File names are spelled exactly as deposited. The archive's own scripts ask for
# "mturk_opeds_cleaned.rdata" and "elite_opeds_cleaned.rdata", which resolve on a
# case-insensitive filesystem and fail on a case-sensitive one.
load(here::here("original", "mturk_opeds_cleaned.RData"))
load(here::here("original", "elite_opeds_cleaned.RData"))
load(here::here("original", "dv_df.rdata"))

# Labels ----
mturk_arms <- c("control", "amtrak", "climate", "paul", "veterans", "wallstreet")
mturk_labels <- c("Control", "Amtrak", "Climate", "Flat Tax", "Veterans", "Wall Street")
elite_arms <- c("control", "amtrak", "paul", "veterans", "wallstreet")
elite_labels <- c("Control", "Amtrak", "Flat Tax", "Veterans", "Wall Street")
parties <- c("Democrat", "Independent", "Republican")

oped_labels <- c(
  Zamtrak = "Op-ed: Amtrak",
  Zclimate = "Op-ed: Climate",
  Zpaul = "Op-ed: Flat Tax",
  Zveterans = "Op-ed: Veterans",
  Zwallstreet = "Op-ed: Wall Street"
)

# Effects of every op-ed on one dependent variable ----
# Returns one column of a cross-effects table: one row per treatment arm, plus the
# control-group mean as the intercept row, and the N and R-squared the article prints
# at the foot of each column.
tidy_arm_effects <- function(dv, data) {
  fit <- lm_robust(as.formula(paste0(dv, " ~ Z")), data = data, se_type = "HC2")
  tidy(fit) |>
    select(term, estimate, std.error, p.value, conf.low, conf.high) |>
    mutate(nobs = fit$nobs, r_squared = fit$r.squared)
}

# Effect of one op-ed on one dependent variable, against the control arm only ----
tidy_single_arm <- function(dv, arm, data) {
  d <- filter(data, responded_w1, Z %in% c("control", arm))
  lm_robust(as.formula(paste0(dv, " ~ Z")), data = d, se_type = "HC2") |>
    tidy() |>
    filter(term != "(Intercept)") |>
    select(estimate, std.error)
}

# Joint F test that treatment effects are equal across the three party groups ----
# se_type is an explicit argument because it is the whole finding for Table 9: the
# published p-values are classical F tests, and the same archive code returns HC2
# ones under current estimatr. Both are reported rather than one hiding in a default.
party_ftest <- function(dv, arm, data, se_type) {
  d <- filter(data, Z %in% c("control", arm))
  fit_r <- lm_robust(as.formula(paste0(dv, " ~ Z + pid_3_cat")), data = d, se_type = se_type)
  fit_u <- lm_robust(as.formula(paste0(dv, " ~ Z * pid_3_cat")), data = d, se_type = se_type)
  waldtest(fit_r, fit_u, test = "F")$`Pr(>F)`[2]
}
