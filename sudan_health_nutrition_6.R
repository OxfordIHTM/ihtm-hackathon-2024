######### Determinants of maternal and child undernutrition #########
#########                   Team Sakura                     #########


### load packages
library(lme4) #for regression
library(forcats)

############ Maternal Undernutrition
### categorizing the outcome variable

# < 23.5 cm: underweight
# 29.2 cm <: obese

maternal1 <- maternal %>%
  mutate(muac_category = ifelse(muac<235, "Underweight", ifelse(muac>234 & muac<293, "Normal", 
                                                                ifelse(muac>292, "Obese", NA)))) %>%
  mutate(muac_category1 = ifelse(muac_category %in% "Underweight", "Underweight", 
                                 ifelse(muac_category %in% "Normal" | muac_category %in% "Obese", "Not Underweight", NA)))


### descriptive analysis
#### TO BE DONE!!!!

### logistic regression - Univariable
#### set reference variable
maternal1$muac_category1 <- as.factor(fct_relevel(maternal1$muac_category1,  "Underweight"))

#### model for age at first parity VS Nutrition category
maternal_model1 <- glm(muac_category1~ageFirstparity, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model1), confint(maternal_model1)))

# | Odds Ratio | 95% CI |
# | ----       | ----   |
# | 1.044589.  | 1.040393,1.048821 |

### model for  proteinrich VS nutrition category (RICHMONDA)
maternal_model2 <- glm(muac_category1~proteinRich, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model2), confint(maternal_model2)))
# OR    2.5 %   97.5 %
# (Intercept) 2.305487 2.232524 2.381162
# proteinRich 1.278784 1.233113 1.326016
