
# =====================================================
# 03_aim1_analysis.R
# Greenwashing and Customer ESG Project
#
# Aim 1:
# Examine annual patterns and consecutive-year
# within-firm changes in GW and CESG.
#
# Input: data/raw/data.dta
# Outputs: results/tables/
# =====================================================

# 1. Load packages
library(haven)
library(dplyr)

# 2. Import original dataset
data <- read_dta("data/raw/data.dta")

# 3. Verify identifiers
stopifnot(
  !anyNA(data$id),
  !anyNA(data$year),
  !anyDuplicated(data[c("id", "year")])
)

# 4. Summarize annual GW and CESG levels
annual_trends <- data %>%
  group_by(year) %>%
  summarise(
    n_firms = n_distinct(id),
    mean_GW = mean(GW, na.rm = TRUE),
    median_GW = median(GW, na.rm = TRUE),
    mean_CESG = mean(CESG, na.rm = TRUE),
    median_CESG = median(CESG, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Calculate changes within each firm
# Only consecutive years represent annual changes.

firm_changes <- data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    year_gap = year - lag(year),
    delta_GW = if_else(
      year_gap == 1,
      GW - lag(GW),
      NA_real_
    ),
    delta_CESG = if_else(
      year_gap == 1,
      CESG - lag(CESG),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(year_gap == 1)

# 6. Verify available consecutive-year pairs
stopifnot(nrow(firm_changes) == 2186)

# 7. Summarize within-firm changes
change_summary <- data.frame(
  variable = c("GW", "CESG"),
  n_pairs = c(
    sum(!is.na(firm_changes$delta_GW)),
    sum(!is.na(firm_changes$delta_CESG))
  ),
  mean_change = c(
    mean(firm_changes$delta_GW, na.rm = TRUE),
    mean(firm_changes$delta_CESG, na.rm = TRUE)
  ),
  median_change = c(
    median(firm_changes$delta_GW, na.rm = TRUE),
    median(firm_changes$delta_CESG, na.rm = TRUE)
  ),
  sd_change = c(
    sd(firm_changes$delta_GW, na.rm = TRUE),
    sd(firm_changes$delta_CESG, na.rm = TRUE)
  )
)

# 8. Summarize changes by calendar year
yearly_changes <- firm_changes %>%
  group_by(year) %>%
  summarise(
    n_pairs = n(),
    mean_delta_GW = mean(delta_GW, na.rm = TRUE),
    median_delta_GW = median(delta_GW, na.rm = TRUE),
    mean_delta_CESG = mean(delta_CESG, na.rm = TRUE),
    median_delta_CESG = median(delta_CESG, na.rm = TRUE),
    .groups = "drop"
  )

# 9. Examine variable distributions
print(summary(data[c("GW", "CESG")]))
print(summary(firm_changes[c("delta_GW", "delta_CESG")]))

# 10. Display analysis results
print(annual_trends)
print(change_summary)
print(yearly_changes)

# 11. Export summary tables
dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  annual_trends,
  "results/tables/03_annual_trends.csv",
  row.names = FALSE
)

write.csv(
  change_summary,
  "results/tables/03_change_summary.csv",
  row.names = FALSE
)

write.csv(
  yearly_changes,
  "results/tables/03_yearly_changes.csv",
  row.names = FALSE
)

# No observations in the source file are modified.
# Only consecutive firm-year pairs are used for changes.

