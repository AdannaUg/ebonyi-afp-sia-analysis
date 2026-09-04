

#Linear Regression : SIA Exposure and Reported OPV Doses

model1 <- lm(opv_dose ~ sia_experienced,
             data = polio)

summary(model1)

## Model 1 shows for each additional SIA experienced was associated with an average increase of 0.27 reported OPV doses among the NP-AFP cases in Ebonyi state. (beta = 0.27, 95% CI, P<0.001, R squared = 11%)

#Modified Poisson Regression : SIA Exposure and Under-immunized cases using robust standard errors.

#load packages

library(sandwich)
library(lmtest)
library(broom)
library(purrr)


# Model 2: Under_immunized 

model2 <- glm(
  under_imm ~ sia_experienced,
  family = poisson(link = "log"),
  data = polio
)

model2

#Model Summary

summary(model2)

#Robust Standard Errors

coeftest(
  model2,
  vcov = vcovHC(model2, type = "HC0")
)

#We can interpret that for additional SIA experienced, there was a 24% relative
#reduction in risk of being under-immunized.



#Modified Poisson Regression : SIA Exposure and Zero - Dose using robust 
#standard errors

#Model 3
model3 <- glm(
  zero_dose ~ sia_experienced,
  family = poisson(link = "log"),
  data = polio
)
model3

#Model summary 
summary(model3)

#Robust standard errors
coeftest(
  model3,
  vcov = vcovHC(model3, type = "HC0")
)

#Risk Ratio: Model 2 (Under_Immunized)

#For additional SIA experienced, AFP cases had 24% lower risk of being 
#under-immunized (RR = 0.76).

robust <- coeftest(
  model2,
  vcov = vcovHC(model2, type = "HC0")
)

beta <- robust["sia_experienced", "Estimate"]
se   <- robust["sia_experienced", "Std. Error"]

RR <- exp(beta)
Lower <- exp(beta - 1.96 * se)
Upper <- exp(beta + 1.96 * se)

data.frame(
  Variable = "SIA experienced",
  RR = RR,
  Lower95CI = Lower,
  Upper95CI = Upper,
  p_value = robust["sia_experienced", "Pr(>|z|)"]
  
)

## Risk Ratio for MOdel 3 (Zero_dose)

```{r}
robust <- coeftest(
  model3,
  vcov = vcovHC(model3, type = "HC0")
)

beta <- robust["sia_experienced", "Estimate"]
se   <- robust["sia_experienced", "Std. Error"]

RR <- exp(beta)
Lower <- exp(beta - 1.96 * se)
Upper <- exp(beta + 1.96 * se)

data.frame(
  Variable = "SIA experienced",
  RR = RR,
  Lower95CI = Lower,
  Upper95CI = Upper,
  p_value = robust["sia_experienced", "Pr(>|z|)"]
)

## Summary:Association between reported doses and campaigns, based on 
#NP-AFP data, January 2020 and January 2025, Ebonyi State

```{r}
LGA2 <- polio %>% 
  group_by(local_govt_area) %>% 
  summarise(
    "NP-AFP Cases" = n(),
    "Under-immunized" = sum(under_imm == 1, na.rm = TRUE),
    "zero-dose"  = sum(zero_dose == 1, na.rm = TRUE),
  )

LGA2


LGA2_model1 <- forest_opv %>% 
  select(
    local_govt_area,
    estimate,
    conf.high,
    conf.low
  ) %>% 
  
  rename(
    model1_estimate = estimate,
    model1_low = conf.low,
    model1_high = conf.high
  )

LGA2_model1


LGA2_model2 <- forest_under %>% 
  select(
    local_govt_area,
    estimate,
    conf.high,
    conf.low
  ) %>% 
  
  rename(
    model2_estimate = estimate,
    model2_low = conf.low,
    model2_high = conf.high
  )

LGA2_model2


LGA2_model3 <- forest_under %>% 
  select(
    local_govt_area,
    estimate,
    conf.high,
    conf.low
  ) %>% 
  
  rename(
    model3_estimate = estimate,
    model3_low = conf.low,
    model3_high = conf.high
  )

LGA2_model3


#join the three (3) models 

LGA_assoc <- LGA2 %>%
  left_join(LGA2_model1, by = "local_govt_area") %>%
  left_join(LGA2_model2, by = "local_govt_area") %>%
  left_join(LGA2_model3, by = "local_govt_area")

flextable(LGA_assoc)   

