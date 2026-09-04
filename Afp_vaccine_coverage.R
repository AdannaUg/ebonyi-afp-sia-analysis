
# NP-AFP Vaccine Coverage, 2020 - 2025

# Zero - Dose Distribution 

polio <- polio %>% 
  rename(opv_dose = no_of_opv_doses) %>% 
  
  mutate(zero_dose = ifelse(opv_dose == 0, 1, 0)
  )
table(polio$zero_dose)

prop.table(table(polio$zero_dose)) * 100

#  Under -Immunized Distribution (<3 doses) 

polio <- polio %>% 
  mutate(under_imm = ifelse(opv_dose < 3, 1, 0)
  )   

table(polio$under_imm)

prop.table(table(polio$under_imm))*100


# Average Immunized (>=3 doses)

polio <- polio %>% 
  mutate(adeq_immun = ifelse(opv_dose >= 3, 1, 0)
  )   

table(polio$adeq_immun)

prop.table(table(polio$adeq_immun))*100

# Annual OPV Vaccine Coverage 

polio_yr <- polio %>%
  group_by(year) %>%
  summarise(
    Zero_dose = round(mean(zero_dose == 1, na.rm = TRUE) * 100, 0),
    Under_immunized = round(mean(under_imm == 1, na.rm = TRUE) * 100, 0),
    Average_immunised = round(mean(adeq_immun == 1, na.rm = TRUE) * 100, 0)
  )
flextable(polio_yr)


# OPV Vaccine Coverage by LGA

polio %>%
  group_by(local_govt_area) %>%
  summarise(
    NPAFP_cases = n(),
    Zero_dose = round(mean(zero_dose == "Yes", na.rm = TRUE) * 100, 0),
    Under_immunized = round(mean(under_imm == "Yes", na.rm = TRUE) * 100, 0),
    Adequately_immunised = round(mean(adeq_immun == "Yes", na.rm = TRUE) * 100, 0)
  )

