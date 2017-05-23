library(dplyr)
library(readr)
library(survey)

# Load IPUMS data (extract #35 from my account).
# It contains: "Demographic, education, and employment data for Texas residents (1990, 2000, 2005-2015) w/ CSV"
ipums.orig <- read_csv("data/usa_00035.csv")

# Reduce to contingous years and delete unneeded columns
ipums <- ipums.orig %>%
  filter(
    COUNTYFIPS == 423,
    AGE >= 25
  ) %>%
  select(-STATEFIP, -COUNTYFIPS, -RACED, -HISPAND)

# Convert factors
ipums$year <- as.factor(as.character(ipums$YEAR))

ipums$sex <- NA
ipums$sex[ipums$SEX == 1] <- "Male"
ipums$sex[ipums$SEX == 2] <- "Female"
ipums$sex <- factor(ipums$sex)

ipums$age_group <- NA
ipums$age_group[ipums$AGE >= 25 && ipums$AGE < 35] <- "25-34"
ipums$age_group[ipums$AGE >= 35 && ipums$AGE < 45] <- "35-44"
ipums$age_group[ipums$AGE >= 45 && ipums$AGE < 55] <- "45-54"
ipums$age_group[ipums$AGE >= 55 && ipums$AGE < 65] <- "55-64"
ipums$age_group[ipums$AGE >= 65] <- "65+"
ipums$age_group <- factor(ipums$age_group)

ipums$educ <- NA
ipums$educ[ipums$EDUCD < 81] <- "No college"
ipums$educ[ipums$EDUCD >= 81 && ipums$EDUCD < 101] <- "Associate's degree"
ipums$educ[ipums$EDUCD >= 101 && ipums$EDUCD < 114] <- "Bachelor's degree"
ipums$educ[ipums$EDUCD >= 114] <- "Advanced degree"
ipums$educ <- factor(ipums$educ)

# Add a column of ones for weighted counts
ipums$one <- 1

# Survey design for single-year estimates
design <- svydesign(
  ids = ~ 1,
  data = ipums,
  weights = ~ PERWT
)

# Count observations by year
ipums_counts <- svyby(~ one, ~ year, design, svytotal, vartype = "ci")

ipums_counts_age_sex <- svyby(~ one, ~ year + age_group + sex + educ, design, svytotal, vartype = "ci")

# Compute mean transit time
ipums.means <- svyby(~ trantime, ~ year, design, svymean, vartype = c("se", "ci"))

# Compute median transit times
ipums.medians <- svyby(~ trantime, ~ year, design, svyquantile, quantiles=0.5, ci=TRUE)