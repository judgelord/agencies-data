build_regextable <- function(sheet_url){

  library(googlesheets4)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(here)

  crosswalk <- read_sheet(sheet_url)

  crosswalk <- crosswalk %>%
    mutate(agency_short = agency) %>%
    select(department, department_acronym, agency, agency_short, 
           department_agency_acronym, regulationsdotgov_agency, regulationsdotgov_acronym,
           ACUS_agency, department_agency_acronyms_prior, other_acronyms, other_names)

  crosswalk <- crosswalk %>%
    rowwise() %>%
    mutate(pattern = paste(
      c_across(department:other_names) %>% 
        str_trim() %>%                     
        discard(~ .x == "" | is.na(.x)),  
      collapse = "|"
    )) %>%
      ungroup()
  
  all_aliases <- crosswalk %>%
    rowwise() %>%
    mutate(alias_list = list(
      c_across(department:other_names) %>%
        str_trim() %>%                 
        discard(~ .x == "" | is.na(.x)) 
    )) %>%
    ungroup() %>%
    select(agency_short, alias_list) %>%
    unnest(alias_list) %>%             
    rename(alias = alias_list)

  duplicates <- all_aliases %>%
    group_by(alias) %>%
    summarize(n_agencies = n_distinct(agency_short), .groups = "drop") %>%
    filter(n_agencies > 1)
  
  return(list(
    regextable = crosswalk %>% select(agency = agency_short, pattern),
    duplicates = duplicates
  ))
  
}
