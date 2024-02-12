install.packages("dplyr")
library(dplyr)

# source from main script
source("sudan_health_nutrition.R")

# Total number of children that got BCG vaccine

BCG_positive <- sum(child$coverageBCG == 1, na.rm = TRUE)
print(BCG_positive)

# Total number of children eligible for BCG vaccine
BCG_eligible <- nrow(child)
print(BCG_eligible)

# Calculate % BCG coverage in all children 

BCG_coverage <- (BCG_positive / BCG_eligible) * 100
print(BCG_coverage)


# Total number of eligible children that got Penta1 

Penta1_positive <- sum(child$coveragePenta1 == 1, na.rm = TRUE)
print(Penta1_positive)

# Total number of children eligible for Penta1 vaccine

Penta1_eligible <- sum(child$coveragePenta1 %in% c(0, 1), na.rm = TRUE)
print(Penta1_eligible)

# Calculate % Penta1 coverage in eligible children 

Penta1_coverage <- (Penta1_positive / Penta1_eligible) * 100
print(Penta1_coverage)


# Total number of eligible children that got Penta3 

Penta3_positive <- sum(child$coveragePenta3 == 1, na.rm = TRUE)
print(Penta3_positive)

# Total number of children eligible for Penta3 vaccine

Penta3_eligible <- sum(child$coveragePenta3 %in% c(0, 1), na.rm = TRUE)
print(Penta3_eligible)

# Calculate % Penta3 coverage in eligible children 

Penta3_coverage <- (Penta3_positive / Penta3_eligible) * 100
print(Penta3_coverage)


# Total number of eligible children that got OPV1 

OPV1_positive <- sum(child$coverageOPV1 == 1, na.rm = TRUE)
print(OPV1_positive)

# Total number of children eligible for OPV1 vaccine

OPV1_eligible <- sum(child$coverageOPV1 %in% c(0, 1), na.rm = TRUE)
print(OPV1_eligible)

# Calculate % OPV1 coverage in eligible children 

OPV1_coverage <- (OPV1_positive / OPV1_eligible) * 100
print(OPV1_coverage)


# Total number of eligible children that got OPV3 

OPV3_positive <- sum(child$coverageOPV3 == 1, na.rm = TRUE)
print(OPV3_positive)

# Total number of children eligible for OPV3 vaccine

OPV3_eligible <- sum(child$coverageOPV3 %in% c(0, 1), na.rm = TRUE)
print(OPV3_eligible)

# Calculate % OPV3 coverage in eligible children 

OPV3_coverage <- (OPV3_positive / OPV3_eligible) * 100
print(OPV3_coverage)

# Total number of eligible children that got Measles1 

Measles1_positive <- sum(child$coverageMeasles1 == 1, na.rm = TRUE)
print(Measles1_positive)

# Total number of children eligible for Measles1 vaccine

Measles1_eligible <- sum(child$coverageMeasles1 %in% c(0, 1), na.rm = TRUE)
print(Measles1_eligible)

# Calculate % Measles1 coverage in eligible children 

Measles1_coverage <- (Measles1_positive / Measles1_eligible) * 100
print(Measles1_coverage)

# Total number of eligible children that got Measles2 

Measles2_positive <- sum(child$coverageMeasles2 == 1, na.rm = TRUE)
print(Measles2_positive)

# Total number of children eligible for Measles2 vaccine

Measles2_eligible <- sum(child$coverageMeasles2 %in% c(0, 1), na.rm = TRUE)
print(Measles2_eligible)

# Calculate % Measles2 coverage in eligible children 

Measles2_coverage <- (Measles2_positive / Measles2_eligible) * 100
print(Measles2_coverage)