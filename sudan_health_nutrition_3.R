


# GENERAL CHARACTERISTICS 
tbl_summary(child_health, include = c(accessEducation, accessBasicEducation, everAttendedSchool))

# INDIVIDUAL FACTORS 
tbl_summary(child_health, include = c(age, sex), by = accessEducation)
tbl_summary(child_health, include = c(age, sex), by = accessBasicEducation)
tbl_summary(child_health, include = c(age, sex), by = accessBasicEducation)

# HEALTH-RELATED FACTORS 
tbl_summary(child_health, include = c(vaccineRecord, diarrhoea), by = accessEducation)

# Barriers to basic pre-school education ---------------------------------------

#################Create a table of na values per variable####################

# create a loop to determine number of na values per variable
na_list<-c()
for (i in 1:ncol(child)) {
  na_count<- sum(is.na(child[,i])) 
  print(na_count)
  na_list<-c(na_list,na_count)
}

colname_list<-colnames(child)

##create a table of na values
na_tbl <- data.frame(colname_list, na_list)

##naming cols
colnames(na_tbl) = c("variable", "values missing") 

##calc percentage of na variables
na_tbl[,3]<-round( na_tbl$`values missing`/nrow(child) *100,2)

na_tbl[,4]<-nrow(child)-na_tbl$`values missing`

##naming cols
colnames(na_tbl) = c("variable", "missing","missing %","present")


child_health <- child

# exclude rows where 'age' is NA
child_health_clean <- child_health[!is.na(child_health$age),]

# INDIVIDUAL FACTORS
# 2x2 Table for Sex and Access to Basic Education
table(child_health$sex, child_health$accessBasicEducation, useNA = "ifany")
# Chi=square test to check the association
chisq.test(table(child_health$sex, child_health$accessBasicEducation, useNA = "no"))

# 2x2 Table for sex and Access Education
table(child_health$sex, child_health$accessEducation, useNA = "ifany")
# Chi=square test
chisq.test(table(child_health$sex, child_health$accessEducation, useNA = "no"))


# 2x2 Table for sex and Access Preschool
table(child_health$sex, child_health$accessPreSchool, useNA = "ifany")
# Chi=square test
chisq.test(table(child_health$sex, child_health$accessPreSchool, useNA = "no"))

# HEALTHCARE FACTORS
# 2x2 table for Vaccine Record and Access to Basic Education
table(child_health$vaccineRecord, child_health$accessBasicEducation, useNA = "ifany")
# Chi-square test for association
chisq.test(table(child_health$vaccineRecord, child_health$accessBasicEducation, useNA = "no"))

# 2x2 table for Diarrhoea and Access to Basic Education
table(child_health$diarrhoea, child_health$accessBasicEducation, useNA = "ifany")
# Chi-square test for association
chisq.test(table(child_health$diarrhoea, child_health$accessBasicEducation, useNA = "no"))

# 2x2 table for Fever and Access to Basic Education
table(child_health$fever, child_health$accessBasicEducation, useNA = "ifany")
# Chi-square test for association
chisq.test(table(child_health$fever, child_health$accessBasicEducation, useNA = "no"))

# SOCIALCULTURAL FACTORS

# 2x2 table for early marriage and access to basic education
table(child_health$earlyMarriage, child_health$accessBasicEducation, useNA = "ifany")
# Chi-square test for association
chisq.test(table(child_health$earlyMarriage, child_health$accessBasicEducation, useNA = "no"))

# 2x2 table for displacement and access to basic education
table(child_health$displacement, child_health$accessBasicEducation, useNA = "ifany")
# Chi-square test for association
chisq.test(table(child_health$displacement, child_health$accessBasicEducation, useNA = "no"))


# CONDUCTING MULTIPLE LOGISTIC REGRESSION

# Convert categorical variables to factors
child_health$sex <- as.factor(child_health$sex)
child_health$vaccineRecord <- as.factor(ifelse(is.na(child_health$vaccineRecord), NA, ifelse(child_health$vaccineRecord == 1, "Yes", "No")))
child_health$earlyMarriage <- as.factor(ifelse(is.na(child_health$earlyMarriage), NA, ifelse(child_health$earlyMarriage == 1, "Yes", "No")))

# Logistic regression model
model <- glm(accessBasicEducation ~ age + sex + vaccineRecord, data = child_health, family = binomial())
summary(model)

# Calculate and plot deviance residuals
residuals <- residuals(model, type = "deviance")
plot(residuals, type = "b", main = "Deviance Residuals", xlab = "Observation", ylab = "Deviance Residual")


# SUMMARIZING DISTRIBUTION OF KEY VARIABLES 

# Summary statistics for age 
summary (child_health$age)

# Distribution of sex
table(child_health$sex)

# Distribution of diarrhoea and ARI cases
table(child_health$diarrhoea)
table(child_health$ari)

# Exploring relationships between demographic factors and health outcomes

# Average age for diarrhoe cases
aggregate(age ~ diarrhoea, data = child_health, FUN = mean, na.rm = TRUE)

# Cross-tabulation of sex and diarrhoea cases
xtabs(~ sex + diarrhoea, data = child_health)

# Cross-tabulation of sex and ARI cases
xtabs(~ sex + ari, data = child_health)

# Visual exploration

# Age distribution
ggplot(child_health, aes(x = age)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") + labs(title = "Age Distribution", x = "Age", y = "Count")

# Diarrhoea cases by sex
ggplot(child_health, aes(x = sex, fill = factor(ari))) + geom_bar(position = "fill") + scale_fill_manual(values = c("0" = "grey", "1" = "red"), labels = c("no", "yes")) + labs(title = "Diarrhoe Cases by Sex", x = "Sex", y = "Proportion", fill = "Diarrhoea")

# ARI cases by sex
ggplot(child_health, aes(x = sex, fill = factor(ari))) + geom_bar(position = "fill") + scale_fill_manual(values = c("0" = "grey", "1" = "green"), labels = c("No", "Yes")) + labs(title = "ARI Cases by Sex", x = "Sex", y = "Proportion", fill = "ARI")

