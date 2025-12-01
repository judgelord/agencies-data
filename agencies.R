build_regextable <- function(sheet_url){

  library(googlesheets4)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(here)
  library(readxl)
  library(readr)
  library(urltools)
  library(fuzzyjoin)

  crosswalk <- read_sheet(sheet_url)

  crosswalk <- crosswalk %>%
    mutate(agency_short = agency) %>%
    select(department, department_acronym, agency, agency_short, 
           department_agency_acronym, regulationsdotgov_agency, regulationsdotgov_acronym,
           ACUS_agency, department_agency_acronyms_prior, other_acronyms, other_names)

  web_xlsx <- here::here("data", "Enviro Fed Web Tracker 2025-09-30.xlsx")
  
  webtracker <- read_excel(web_xlsx, sheet = 1) %>%
    as_tibble()
  
  web_clean <- webtracker %>%
    rename(
      acronym = Agency,
      url = URL
      ) %>%
    mutate(
      original_url = url,
      url = if_else(
        str_detect(original_url, "^https?://"),
        original_url,
        paste0("https://", original_url)
      ),
      root = paste0(scheme(url), "://", domain(url)),
      url_length = nchar(url)
    )
    
  landing_by_root <- web_clean %>%
    group_by(acronym, root) %>%
    summarize(
      n_urls = n(),
      shortest_url = url[which.min(url_length)][1],
      .groups = "drop"
    ) %>%
    group_by(acronym) %>%
    arrange(acronym, desc(n_urls), nchar(shortest_url)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(acronym, envirodatagov_url = root)
  
  crosswalk2 <- crosswalk %>%
    mutate(acronym = str_trim(toupper(department_agency_acronym)))
  
  landing_by_root2 <- landing_by_root %>%
    mutate(acronym = str_trim(toupper(acronym)))
  
  crosswalk_with_enviro <- crosswalk2 %>%
    left_join(landing_by_root2, by = "acronym")
    
  
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
    filter(n_agencies > 1) %>%
    pull(alias)
  
  aliases_unique <- all_aliases %>%
    filter(!(alias %in% duplicates))
  
  patterns_unique <- aliases_unique %>%
    group_by(agency_short) %>%
    summarize(
      pattern = paste(unique(alias), collapse = "|"),
      .groups = "drop"
    )
  
  final_table <- crosswalk_with_enviro %>%
    left_join(patterns_unique, by = "agency_short")
  
  return(list(
    regextable = final_table %>% select(
      agency = agency_short,
      envirodatagov_url,
      pattern),
    duplicates = duplicates
  ))
  
}
