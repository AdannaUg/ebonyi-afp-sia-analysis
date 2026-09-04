
#AFP Surveillance Data Cleaning 

#Read AFP data

polio_dt <- import("polio_linelist.xlsx")

#create a function 

convert_dates <- function(x) {
  
  # If already Date → return as it is
  if (inherits(x, "Date")) return(x)
  
  # If POSIXct → convert to Date
  if (inherits(x, "POSIXt")) return(as_date(x))
  
  # Use numeric (Excel format)
  num <- suppressWarnings(as.numeric(x))
  num_date <- as_date(num, origin = "1899-12-30")
  
  #  character parsing
  text_date <- parse_date_time(
    x,
    orders = c("dmy", "mdy", "ymd", "d/m/Y", "m/d/Y", "Y-m-d"),
    quiet = TRUE
  ) %>%
    as_date()
  
  coalesce(num_date, text_date)
}

polio <- polio_dt %>%
  
  # clean names & remove duplicates
  clean_names() %>%
  distinct() %>%
  
  # rename variables
  rename(
    date_onset = 'onset_of_paralysis',
    date_notification = 'date_of_notification',
    date_investigation = 'date_case_investigated',
    date_1st_stool = 'date_1st_stool_collected',
    date_2nd_stool = 'date_2nd_stool_collected',
    date_sent_lab = 'date_stool_sent_to_the_lab',
    date_stool_rec_lab = 'date_stool_received_in_lab',
    date_of_birth = 'date_of_birth',
    local_govt_area = 'name_of_lga',
    virus_isolated = 'type_of_virus_isolated_p1_p2_p3_npent'
  ) %>%
  
  # fix all dates variables
  
  mutate(across(
    c(date_onset,
      date_notification,
      date_investigation,
      date_1st_stool,
      date_2nd_stool,
      date_sent_lab,
      date_stool_rec_lab,
      date_of_birth),
    convert_dates
  )) %>%
  
  
  
  
  mutate(
    age_years = floor(as.numeric(difftime(date_onset, date_of_birth, units = "days")) / 365.25),
    
    age_cat = case_when(
      age_years < 1 ~ "<1",
      age_years <= 4 ~ "1–4",
      age_years <= 9 ~ "5–9",
      age_years >= 10 ~ "10+",
      TRUE ~ NA_character_
    )
  )


``
