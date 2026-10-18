
# =====================================================
# 01_import_and_audit.R
# Greenwashing and Customer ESG Project
#
# Purpose:
# Import the original Stata dataset and check data
# structure, panel identifiers, and missing values.
#
# Input: data/raw/data.dta
# Outputs: results/tables/
# =====================================================

# 1. Check required package
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Please install the haven package.")
}

# 2. Import original data
raw_data <- haven::read_dta("data/raw/data.dta")

# 3. Check required variables
required_vars <- c(
  "id", "year", "GW", "GWA",
  "CESG", "CESGM", "sct_num"
)

missing_vars <- setdiff(required_vars, names(raw_data))

if (length(missing_vars) > 0) {
  stop("Missing required variables: ",
       paste(missing_vars, collapse = ", "))
}

# 4. Summarize dataset structure
data_summary <- data.frame(
  metric = c(
    "Observations",
    "Variables",
    "Unique firms",
    "First year",
    "Last year",
    "Missing firm-year identifiers",
    "Duplicate firm-year records"
  ),
  value = c(
    nrow(raw_data),
    ncol(raw_data),
    length(unique(raw_data$id[!is.na(raw_data$id)])),
    min(raw_data$year, na.rm = TRUE),
    max(raw_data$year, na.rm = TRUE),
    sum(is.na(raw_data$id) | is.na(raw_data$year)),
    sum(duplicated(raw_data[c("id", "year")]))
  )
)

print(data_summary)

# 5. Check missing values for every variable
missing_summary <- data.frame(
  variable = names(raw_data),
  missing_n = vapply(
    raw_data,
    function(x) sum(is.na(x)),
    integer(1)
  )
)

missing_summary$missing_pct <- round(
  100 * missing_summary$missing_n / nrow(raw_data),
  2
)

print(missing_summary)

# 6. Export audit results
dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  data_summary,
  "results/tables/01_data_summary.csv",
  row.names = FALSE
)

write.csv(
  missing_summary,
  "results/tables/01_missing_summary.csv",
  row.names = FALSE
)

# No observations or variables are modified.
# The original Stata file remains unchanged.
