
# =====================================================
# 02_panel_structure.R
# Greenwashing and Customer ESG Project
#
# Purpose:
# Examine panel coverage, identify gaps, and determine
# which firm-year observations have consecutive years.
#
# Input: data/raw/data.dta
# Outputs: results/tables/
# =====================================================

# 1. Check packages
for (pkg in c("haven", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install the package: ", pkg)
  }
}

library(dplyr)

# 2. Import original data
panel_data <- haven::read_dta("data/raw/data.dta")

# 3. Verify firm-year identifiers
stopifnot(
  !anyNA(panel_data$id),
  !anyNA(panel_data$year),
  !anyDuplicated(panel_data[c("id", "year")])
)

# 4. Identify consecutive-year observations
panel_data <- panel_data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    previous_year = lag(year),
    year_gap = year - previous_year,
    consecutive_year = !is.na(year_gap) & year_gap == 1
  ) %>%
  ungroup()

# 5. Summarize coverage for each firm
firm_coverage <- panel_data %>%
  group_by(id) %>%
  summarise(
    n_years = n(),
    first_year = min(year),
    last_year = max(year),
    n_consecutive_pairs = sum(consecutive_year),
    n_gaps = sum(year_gap > 1, na.rm = TRUE),
    missing_years_within_span =
      sum(pmax(year_gap - 1, 0), na.rm = TRUE),
    .groups = "drop"
  )

# 6. Count firms observed in each calendar year
annual_coverage <- panel_data %>%
  group_by(year) %>%
  summarise(
    n_firms = n_distinct(id),
    .groups = "drop"
  )

# 7. Create panel structure summary
panel_summary <- data.frame(
  metric = c(
    "Total firm-year observations",
    "Unique firms",
    "Firms observed in only one year",
    "Consecutive-year pairs",
    "Transitions with a panel gap",
    "Missing years within observed firm spans"
  ),
  value = c(
    nrow(panel_data),
    n_distinct(panel_data$id),
    sum(firm_coverage$n_years == 1),
    sum(panel_data$consecutive_year),
    sum(panel_data$year_gap > 1, na.rm = TRUE),
    sum(firm_coverage$missing_years_within_span)
  )
)

# 8. Display results
print(panel_summary)
print(annual_coverage)

# 9. Save audit outputs
dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  panel_summary,
  "results/tables/02_panel_summary.csv",
  row.names = FALSE
)

write.csv(
  firm_coverage,
  "results/tables/02_firm_coverage.csv",
  row.names = FALSE
)

write.csv(
  annual_coverage,
  "results/tables/02_annual_coverage.csv",
  row.names = FALSE
)

# No original observations are removed.
# Year-to-year changes will only use consecutive years.
