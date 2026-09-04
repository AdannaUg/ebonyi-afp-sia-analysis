
#SIA exposure against NP-AFP OPV Doses

#load campaign data
campaign <- import("Campaign.xlsx") #load campaign data

#create a function 

SIA_count <- function(date_of_birth, date_onset, local_govt_area) {
  
  eligible <- campaign %>% 
    filter(
      start_date >= date_of_birth ,   # keep campaign dates after child was born
      start_date <= date_onset, #keep campaign dates before paralysis onset
      LGA == "All" | LGA == local_govt_area
    )
  
  nrow(eligible)
  

#Apply  function to AFP cases

#load  function
library(purrr)

polio <- polio %>%
  mutate(
    sia_experienced = pmap_int(  #create new column
      list(date_of_birth,
           date_onset,
           local_govt_area),
      SIA_count
    )
  )

#Plot OPV dose by SIAs

ggplot(polio,
       aes(x = sia_experienced,
           y = opv_dose)) +
  geom_jitter(width = 0.15,
              height = 0.15,
              alpha = 0.3,
              colour = "black") +
  geom_smooth(method = "lm",
              colour = "red",
              se = FALSE,
              linewidth = 1.2) +
  labs(
    x = "SIAs experienced",
    y = "Reported OPV doses"
  ) +
  
  theme_bw()





