clean_acs_data <- function(acs_vars_file) {
  library(tidycensus)
  
  acs_vars <- read_csv(acs_vars_file, show_col_types = FALSE)
  
  acs_raw_bg <- get_acs(
    geography = "block group",
    variables = filter(acs_vars, geography == "block group")$name,
    state = 53, year = 2019, survey = "acs5"
  )

  acs_raw_tract <- get_acs(
    geography = "tract",
    variables = filter(acs_vars, geography == "tract")$name,
    state = 53, year = 2019, survey = "acs5"
  )
  
  acs_bg <- acs_raw_bg %>% 
    select(-NAME, -moe) %>% 
    pivot_wider(names_from = "variable", values_from = "estimate") %>% 
    mutate(
      bg_pct_married = (B12001_004 + B12001_013) / B12001_001,
      bg_pct_kids = B11005_002 / B11005_001,
      bg_pct_hs_plus = (B15003_017 + B15003_018 + B15003_019 + B15003_020 + B15003_021 + 
                          B15003_022 + B15003_023 + B15003_024 + B15003_025) / B15003_001,
      bg_pct_enrolled_college = (B14002_019 + B14002_022 + B14002_043 + B14002_046) / B14002_001,
      bg_pct_veteran = B21001_002 / B21001_001,
      bg_pct_computer_internet = B28003_004 / B28003_001,
      bg_income_percapita = B19301_001,
      bg_pct_employed = B23025_002 / B23025_001,
      bg_pct_commute_public = B08301_010 / B08301_001,
      bg_pct_work_home = B08301_021 / B08301_001,
      bg_pct_ind_construction = (C24030_006 + C24030_033) / C24030_001,
      bg_pct_ind_manufacturing = (C24030_007 + C24030_034) / C24030_001,
      bg_pct_ind_retail = (C24030_009 + C24030_036) / C24030_001,
      bg_pct_ind_edu_health_social = (C24030_021 + C24030_048) / C24030_001,
      bg_pct_ind_acc_food = (C24030_026 + C24030_053) / C24030_001,
      bg_pct_ind_covid = (C24030_006 + C24030_033 + C24030_007 + C24030_034 + 
                            C24030_009 + C24030_036 + C24030_021 + C24030_048 + 
                            C24030_026 + C24030_053) / C24030_001,
      bg_pct_uninsured = (B27010_017 + B27010_033 + B27010_050 + B27010_066) / B27010_001,
      bg_pct_poverty100 = (C17002_002 + C17002_003) / C17002_001,
      bg_pct_poverty200 = 1 - (C17002_008 / C17002_001),
      bg_high_rent = (B25070_008 + B25070_009 + B25070_010) / B25070_001,
      bg_population = B01003_001,
      bg_pct_female = B01001_026 / B01001_001,
      bg_pct_working_age = (B01001_007 + B01001_008 + B01001_009 + B01001_010 + B01001_011 + 
                              B01001_012 + B01001_013 + B01001_014 + B01001_015 + B01001_016 + 
                              B01001_017 + B01001_018 + B01001_019 + B01001_020 +
                              B01001_031 + B01001_032 + B01001_033 + B01001_034 + B01001_035 + 
                              B01001_036 + B01001_037 + B01001_038 + B01001_039 + B01001_040 + 
                              B01001_041 + B01001_042 + B01001_043 + B01001_044) / B01001_001,
      bg_pct_nonwhite = 1 - (B03002_003 / B03002_001)
    ) %>% 
    mutate(tract = str_sub(GEOID, start = 1, end = -2)) %>% 
    select(GEOID, tract, starts_with("bg_")) %>% 
    mutate(across(starts_with("bg_"), ~ifelse(is.nan(.), 0, .)))
  
  acs_tract <- acs_raw_tract %>% 
    select(-NAME, -moe) %>% 
    pivot_wider(names_from = "variable", values_from = "estimate") %>% 
    mutate(
      tr_pct_snap = B22003_002 / B22003_001,
      tr_pct_no_car = B08014_002 / B08014_001
    ) %>% 
    select(GEOID, starts_with("tr_")) %>% 
    mutate(across(starts_with("tr_"), ~ifelse(is.nan(.), 0, .)))
  
  # This joins the tract-level values to the block group-level rows, which isn't
  # ideal, but it'll have to do for these two tract-level variables
  acs_clean <- acs_bg %>% 
    left_join(acs_tract, by = join_by(tract == GEOID))
  
  return(acs_clean)
}
