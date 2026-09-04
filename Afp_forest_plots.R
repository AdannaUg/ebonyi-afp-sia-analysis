##Forest Plot Model 1 


forest_opv <- polio %>%
  group_by(local_govt_area) %>%
  group_modify(~{
    
    model <- lm(opv_dose ~ sia_experienced,
                data = .x)
    
    tidy(model, conf.int = TRUE) %>%
      filter(term == "sia_experienced")
    
  }) %>%
  ungroup()
#graph

ggplot(forest_opv,
       #plot with variable on y axis and estimate on x - axis   
       aes(y =
             reorder(local_govt_area, estimate),
           x = estimate)) +
  #show estimate as a point
  geom_point(size = 3) +
  #add in an error bar for the CI
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "red") +
  
  labs(
    x = "Regression coefficient (β)",
    y = "Local Government Area",
    title = "Association between SIAs experienced and average OPV doses"
  ) +
  
  theme_bw()


##Plot for model 2

forest_under <- polio %>%
  group_by(local_govt_area) %>%
  group_modify(~{
    
    model <- glm(
      under_imm ~ sia_experienced,
      family = poisson(link = "log"),
      data = .x
    )
    
    robust <- coeftest(
      model,
      vcov = vcovHC(model, type = "HC0")
    )
    
    beta <- robust["sia_experienced", "Estimate"]
    se   <- robust["sia_experienced", "Std. Error"]
    
    tibble(
      estimate = exp(beta),
      conf.low = exp(beta - 1.96 * se),
      conf.high = exp(beta + 1.96 * se),
      p.value = robust["sia_experienced", "Pr(>|z|)"]
    )
    
  }) %>%
  ungroup()

#graph

ggplot(forest_under,
       aes(y = reorder(local_govt_area, estimate),
           x = estimate)) +
  
  geom_point(
    size = 2.8,
    colour = "#F8766D"
  ) +
  
  geom_errorbarh(
    aes(xmin = conf.low,
        xmax = conf.high),
    height = 0.15,
    linewidth = 0.6,
    colour = "#F8766D"
  ) +
  
  geom_vline(
    xintercept = 1,
    colour = "red",
    linewidth = 0.6
  ) +
  
  labs(
    title = "Under-immunized",
    x = "Risk Ratio (95% CI)",
    y = NULL
  ) +
  
  theme_bw()


##Plot model 3


forest_zero <- polio %>%
  group_by(local_govt_area) %>%
  group_modify(~{
    
    model <- glm(
      zero_dose ~ sia_experienced,
      family = poisson(link = "log"),
      data = .x
    )
    
    robust <- coeftest(
      model,
      vcov = vcovHC(model, type = "HC0")
    )
    
    beta <- robust["sia_experienced", "Estimate"]
    se   <- robust["sia_experienced", "Std. Error"]
    
    tibble(
      estimate = exp(beta),
      conf.low = exp(beta - 1.96 * se),
      conf.high = exp(beta + 1.96 * se),
      p.value = robust["sia_experienced", "Pr(>|z|)"]
    )
    
  }) %>%
  ungroup()

#graph

ggplot(forest_zero,
       aes(y = reorder(local_govt_area, estimate),
           x = estimate)) +
  
  geom_point(
    size = 2.8,
    colour = "#00BA38"
  ) +
  
  geom_errorbarh(
    aes(xmin = conf.low,
        xmax = conf.high),
    height = 0.15,
    linewidth = 0.6,
    colour = "#00BA38"
  ) +
  
  geom_vline(
    xintercept = 1,
    colour = "blue",
    linewidth = 0.6
  ) +
  
  labs(
    title = "Zero-dose",
    x = "Risk Ratio (95% CI)",
    y = NULL
  ) +
  
  theme_bw()


