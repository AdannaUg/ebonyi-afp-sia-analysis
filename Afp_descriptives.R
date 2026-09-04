
# Descriptive Analysis 
 
polio %>% 
get_summary_stats(age_years, type = "full")


 #Age Distribution of NP-AFP cases by Year (2020 -2025)


polio %>%
  group_by(year, age_cat) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )
``
#Annual Distribution and Selected Characteristics of AFP cases

year_summary <- polio %>% 
  group_by(year) %>% 
  summarise(
    n_rows  = n(),
    age_avg = round(mean(age_years, na.rm = TRUE), 0),
    max_onset = max(date_onset, na.rm = TRUE, default = NA),
    
    n_female = sum(sex == "F", na.rm = TRUE),
    n_male   = sum(sex == "M", na.rm = TRUE),
    
    pct_female = round((n_female / n_rows) * 100, 0),
    pct_male   = round((n_male / n_rows) * 100, 0)
  ) %>% 
  arrange(desc(n_rows)) %>% 
  flextable()

(year_summary)


#Characteristics Cases between January 2020 - December 2025 by LGA, Ebonyi State,
Nigeria

polio <- polio %>%
  mutate(
    age_months = floor(as.numeric(date_onset - date_of_birth) / 30.4375)
  )


lga_table <- polio %>%
  group_by(local_govt_area) %>%
  summarise(
    `NP-AFP cases` = n(),
    
    `Age (months)` =
      paste0(
        median(age_months, na.rm = TRUE),
        " (",
        quantile(age_months, .25, na.rm = TRUE),
        "-",
        quantile(age_months, .75, na.rm = TRUE),
        ")"
      ),
    
    `% Male` =
      round(mean(sex == "M", na.rm = TRUE) * 100, 1),
    
    `% Female` =
      round(mean(sex == "F", na.rm = TRUE) * 100, 1),
    
    `Under-immunized` =
      paste0(
        sum(under_imm == 1, na.rm = TRUE),
        " (",
        round(mean(under_imm == 1, na.rm = TRUE) * 100, 1),
        "%)"
      ),
    
    `Zero-dose` =
      paste0(
        sum(zero_dose == 1, na.rm = TRUE),
        " (",
        round(mean(zero_dose == 1, na.rm = TRUE) * 100, 1),
        "%)"
      ),
    
    `OPV doses` =
      paste0(
        median(opv_dose, na.rm = TRUE),
        " (",
        quantile(opv_dose, .25, na.rm = TRUE),
        "-",
        quantile(opv_dose, .75, na.rm = TRUE),
        ")"
      ),
    
    `SIAs experienced` =
      paste0(
        median(sia_experienced, na.rm = TRUE),
        " (",
        quantile(sia_experienced, .25, na.rm = TRUE),
        "-",
        quantile(sia_experienced, .75, na.rm = TRUE),
        ")"
        
      ), 
    .groups = "drop"
    
  )
flextable(lga_table)

