
# =====================================================
# 04_aim2_analysis.R
# Greenwashing and Customer ESG Project
#
# Aim 2: Supply-chain transparency
# Step 1: Examine transparency distribution
# and construct descriptive comparison groups.
#
# Input: data/raw/data.dta
# Outputs: results/tables/
# =====================================================

library(haven)
library(dplyr)

# 1. Import original dataset
aim2_data <- read_dta("data/raw/data.dta")

# 2. Validate the transparency variable
stopifnot(
  !anyNA(aim2_data$sct_num),
  all(aim2_data$sct_num %in% 0:10)
)

# 3. Examine original transparency values
sct_frequency <- aim2_data %>%
  count(sct_num, name = "n") %>%
  mutate(
    percentage = round(100 * n / sum(n), 2)
  )

print(sct_frequency)

# 4. Create transparency groups
# Intermediate values are combined because they
# have relatively few observations.

aim2_data <- aim2_data %>%
  mutate(
    transparency_group = case_when(
      sct_num == 0 ~ "0",
      sct_num >= 1 & sct_num <= 9 ~ "1-9",
      sct_num == 10 ~ "10"
    )
  )

# 5. Summarize each group
sct_group_summary <- aim2_data %>%
  group_by(transparency_group) %>%
  summarise(
    n_observations = n(),
    n_firms = n_distinct(id),
    mean_GW = mean(GW),
    median_GW = median(GW),
    mean_CESG = mean(CESG),
    median_CESG = median(CESG),
    .groups = "drop"
  )

print(sct_group_summary)

# 6. Validate group counts
stopifnot(
  sum(sct_group_summary$n_observations) ==
    nrow(aim2_data)
)

# 7. Save results
dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  sct_frequency,
  "results/tables/04_sct_frequency.csv",
  row.names = FALSE
)

write.csv(
  sct_group_summary,
  "results/tables/04_sct_group_summary.csv",
  row.names = FALSE
)

# No observations are removed or imputed.
# Original sct_num values remain unchanged.


# =====================================================
# 8. Compare CESG-GW associations across groups
# =====================================================

# Compute descriptive associations within each
# supply-chain transparency group.

sct_association <- aim2_data %>%
  group_by(transparency_group) %>%
  summarise(
    n_observations = n(),
    n_firms = n_distinct(id),
    
    # Variation in customer ESG
    sd_CESG = sd(CESG),
    
    # Linear association
    pearson_r = cor(
      CESG, GW,
      method = "pearson"
    ),
    
    # Rank-based association
    spearman_r = cor(
      CESG, GW,
      method = "spearman"
    ),
    
    # Descriptive linear slope:
    # GW units per one-unit difference in CESG
    linear_slope = cov(CESG, GW) / var(CESG),
    
    .groups = "drop"
  )

# Display results
print(sct_association)

# Export results
write.csv(
  sct_association,
  "results/tables/04_sct_association.csv",
  row.names = FALSE
)

# =====================================================
# 9. Alternative-measure robustness checks
# =====================================================

# Confirm all four measures are available and complete
measure_vars <- c("CESG", "CESGM", "GW", "GWA")

stopifnot(
  all(measure_vars %in% names(aim2_data)),
  !anyNA(aim2_data[measure_vars])
)

# Reuse the same calculation for different measures
compare_measures <- function(x_var, y_var) {
  
  aim2_data %>%
    group_by(transparency_group) %>%
    summarise(
      n_observations = n(),
      
      pearson_r = cor(
        .data[[x_var]],
        .data[[y_var]],
        method = "pearson"
      ),
      
      spearman_r = cor(
        .data[[x_var]],
        .data[[y_var]],
        method = "spearman"
      ),
      
      linear_slope = cov(
        .data[[x_var]],
        .data[[y_var]]
      ) / var(.data[[x_var]]),
      
      .groups = "drop"
    ) %>%
    mutate(
      customer_measure = x_var,
      greenwashing_measure = y_var,
      .before = 1
    )
}

# Compare the original and alternative measures
alternative_results <- bind_rows(
  compare_measures("CESG", "GW"),
  compare_measures("CESGM", "GW"),
  compare_measures("CESG", "GWA"),
  compare_measures("CESGM", "GWA")
)

# Display results
print(alternative_results, n = 12)

# Export reproducible results
write.csv(
  alternative_results,
  "results/tables/04_alternative_measures.csv",
  row.names = FALSE
)


# =====================================================
# 10. Diagnostics for alternative measures
# =====================================================

# Compare the distributions of the four measures.
measure_distribution <- bind_rows(
  lapply(c("GW", "GWA", "CESG", "CESGM"), function(v) {
    x <- aim2_data[[v]]
    
    data.frame(
      variable = v,
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      min = min(x),
      max = max(x),
      n_zero = sum(x == 0)
    )
  })
)

# Examine how strongly the original and alternative
# measures correspond within transparency groups.
measure_agreement <- aim2_data %>%
  group_by(transparency_group) %>%
  summarise(
    pearson_GW_GWA = cor(GW, GWA),
    spearman_GW_GWA = cor(
      GW, GWA, method = "spearman"
    ),
    pearson_CESG_CESGM = cor(CESG, CESGM),
    spearman_CESG_CESGM = cor(
      CESG, CESGM, method = "spearman"
    ),
    .groups = "drop"
  )

print(measure_distribution)
print(measure_agreement)

# Save diagnostic results for reproducibility.
write.csv(
  measure_distribution,
  "results/tables/04_measure_distribution.csv",
  row.names = FALSE
)

write.csv(
  measure_agreement,
  "results/tables/04_measure_agreement.csv",
  row.names = FALSE
)

# =====================================================
# 11. GWA outlier sensitivity analysis
#
# Purpose:
# Examine whether extreme GWA observations affect
# the descriptive CESG-GWA correlations.
#
# The original dataset is not modified.
# =====================================================

# 1. Calculate pooled GWA percentile thresholds
gwa_bounds <- quantile(
  aim2_data$GWA,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

print(gwa_bounds)

# 2. Create a sensitivity sample
# Exclude observations outside the pooled 1%-99% range.
gwa_trimmed <- aim2_data %>%
  filter(
    GWA >= gwa_bounds[1],
    GWA <= gwa_bounds[2]
  )

# 3. Compare full and trimmed samples
gwa_sensitivity <- bind_rows(
  aim2_data %>%
    mutate(sample = "Full sample"),
  
  gwa_trimmed %>%
    mutate(sample = "Trimmed GWA 1%-99%")
) %>%
  group_by(sample, transparency_group) %>%
  summarise(
    n_observations = n(),
    pearson_r = cor(CESG, GWA),
    spearman_r = cor(
      CESG, GWA,
      method = "spearman"
    ),
    .groups = "drop"
  )

# 4. Display results
print(gwa_sensitivity, n = 6)

# 5. Save reproducible results
write.csv(
  gwa_sensitivity,
  "results/tables/04_GWA_sensitivity.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    percentile = c("1%", "99%"),
    GWA_threshold = as.numeric(gwa_bounds)
  ),
  "results/tables/04_GWA_thresholds.csv",
  row.names = FALSE
)
