# source from main script
source("sudan_health_nutrition.R")

# Total number of children that got BCG vaccine

BCG_positive <- sum(child$coverageBCG == 1, na.rm = TRUE)
print(BCG_positive)

# Total number of children eligible for BCG vaccine
BCG_eligible <- nrow(child)

# Calculate % BCG coverage in all children 

BCG_coverage <- (BCG_positive / BCG_eligible) * 100
print(BCG_coverage)


