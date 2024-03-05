######### Determinants of maternal and child undernutrition #########
#########                   Team Sakura                     #########


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


#MATERNAL FACTORS

#### model for age at first parity VS Nutrition category
maternal_model1 <- glm(muac_category1~ageFirstparity, family = binomial, data = maternal1)
#### get model coefficients
m1 <- as.data.frame(exp(cbind(OR = coef(maternal_model1), confint(maternal_model1)))) %>%
  cbind(Variable = "Age at First Parity") 

#m1 <- kableExtra::kable(m1)

# | Odds Ratio | 95% CI |
# | ----       | ----   |
# | 1.044589.  | 1.040393,1.048821 |


#GEOGRAPHY

#model for state vs. nutrition
maternal_model2 <- glm(muac_category1~state_name, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model2), confint(maternal_model2)))

m2 <- as.data.frame(exp(cbind(OR = coef(maternal_model2), confint(maternal_model2)))) %>%
  cbind(Variable = "State") 

#                               OR     2.5 %    97.5 %
#(Intercept)               4.1253219 3.8711419 4.3998598
#state_nameAl-Gazeera      1.0088971 0.9124778 1.1159480
#state_nameBlue Nile       0.5085591 0.4622647 0.5594620
#state_nameCentral Darfur  0.6716053 0.6124696 0.7364169
#state_nameEast Darfur     0.5132646 0.4683982 0.5623443
#state_nameKassala         0.5483236 0.5036620 0.5967645
#state_nameKhartoum        2.5401230 2.2257614 2.9067781
#state_nameNorth Darfur    0.4931012 0.4553026 0.5337970
#state_nameNorth Kourdofan 0.7671439 0.7002338 0.8404157
#state_nameNorthern        1.9136918 1.6893442 2.1722310
#state_nameRed Sea         0.2599032 0.2366918 0.2852871
#state_nameRiver Nile      1.2419437 1.1167745 1.3821699
#state_nameSinar           0.9469317 0.8577470 1.0457142
#state_nameSouth Darfur    0.5605003 0.5181183 0.6060667
#state_nameSouth Kourdofan 0.6781559 0.6222742 0.7388837
#state_nameWest Darfur     0.7191160 0.6565337 0.7876148
#state_nameWest Kourdofan  0.4213609 0.3881469 0.4572257
#state_nameWhite Nile      1.0601193 0.9625668 1.1678520

### model for locality name vs nutrition category
maternal_model3 <- glm(muac_category1~locality_name, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model3), confint(maternal_model3)))

#m3 <- as.data.frame(exp(cbind(OR = coef(maternal_model3), confint(maternal_model3)))) %>%
  #cbind(Variable = "Age at First Parity") 


#ANTENATAL AND POSTNATAL CARE

#model for ANC visits vs. nutrition
maternal_model4 <- glm(muac_category1~visitsANC, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model4), confint(maternal_model4)))
#                 OR      2.5 %   97.5 %
# (Intercept) 2.050422 2.001899 2.100220
# visitsANC   1.102524 1.095795 1.109309

m4 <- as.data.frame(exp(cbind(OR = coef(maternal_model4), confint(maternal_model4)))) %>%
  cbind(Variable = "ANC visits") 

#model for postnatal visits vs. nutrition (Jillian)
maternal_model5 <- glm(muac_category1~postNatal, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model5), confint(maternal_model5)))
#               OR      2.5 %   97.5 %
#(Intercept) 2.646587 2.600468 2.693650
#postNatal   1.214305 1.175154 1.254864

m5 <- as.data.frame(exp(cbind(OR = coef(maternal_model5), confint(maternal_model5)))) %>%
  cbind(Variable = "PostNatal visits") 

#DIET

### model for  proteinRich VS nutrition category (RICHMONDA)
maternal_model6 <- glm(muac_category1~proteinRich, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model6), confint(maternal_model6)))
#                 OR    2.5 %   97.5 %
# (Intercept) 2.305487 2.232524 2.381162
# proteinRich 1.278784 1.233113 1.326016

m6 <- as.data.frame(exp(cbind(OR = coef(maternal_model6), confint(maternal_model6)))) %>%
  cbind(Variable = "Protein Rich Diet") 

#model for MDDW vs. nutrition (Jillian)
maternal_model7 <- glm(muac_category1~MDDW, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model7), confint(maternal_model7)))
#              OR      2.5 %     97.5 %
#(Intercept) 2.711873 2.670837 2.753639
#MDDW        1.739938 1.627560 1.861713

m7 <- as.data.frame(exp(cbind(OR = coef(maternal_model7), confint(maternal_model7)))) %>%
  cbind(Variable = "MDDW")

#model for iron v folic acid for 90 days
maternal_model8 <- glm(muac_category1~ifa90, family = binomial, data = maternal1)
#### get model coefficients
exp(cbind(OR = coef(maternal_model8), confint(maternal_model8)))
#                OR    2.5 %   97.5 %
# (Intercept) 2.396972 2.353898 2.440945
# ifa90       1.562997 1.514119 1.613572

m8 <- as.data.frame(exp(cbind(OR = coef(maternal_model8), confint(maternal_model8)))) %>%
  cbind(Variable = "Iron & Folic Acid")

#model for vitA
maternal_model9 <- glm(muac_category1~vitA, family = binomial, data = maternal1)
#### get model coefficients
m9 <- exp(cbind(OR = coef(maternal_model9), confint(maternal_model9)))
#                OR    2.5 %   97.5 %
# (Intercept) 2.698061 2.654360 2.742593
# vitA        1.235080 1.187802 1.284472

m9 <- as.data.frame(exp(cbind(OR = coef(maternal_model9), confint(maternal_model9)))) %>%
  cbind(Variable = "Vitamin A")

# tables for models

model_outputs <- bind_rows(m1,m2,m4,m5,m6,m7,m8,m9)
#tt2 <- gridExtra::ttheme_default(core=list(fg_params=list(hjust=1, x=0.9)),
                      #rowhead=list(fg_params=list(hjust=1, x=0.95)))
#gridExtra::grid.table(model_outputs, theme=tt2)

gridExtra::grid.table(model_outputs)


