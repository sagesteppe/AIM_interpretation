
soil_coRe <- function(x){
  
  ## this functions converts soil data into a long format for plotting with
  ## ggplot2.
  
  # INPUTS: x = a dataframe(/tibble), group_var = first grouping variable 
  # column name, 
  
  breaks2cmIncrements <- function(x){
    # this helper function servers to convert input in a rowwise manner. 
    # input data should contain a Horizon Start and Horizon end column, 
    # as well as a column containing attributes of interest (e.g. texture, 
    # percent rock), optionally this should also contain a horizon number.
    cbind(
      x %>% 
        dplyr::select(-Horizon_Start , -Horizon_End),
      'Depth' = seq(from = x$Horizon_Start, to = x$Horizon_End, by = 1)
    )
  }
  
  final_row <- dummydf %>% 
    group_by(Soil_pit) %>% 
    arrange(Horizon) %>% 
    slice_tail() %>% 
    mutate(Horizon_End = 65)
  
  all_rows <- dummydf %>% 
    bind_rows(., final_row) %>% 
    group_by(Soil_pit) %>% 
    arrange(Horizon_End, .by_group = T) %>% 
    mutate('Horizon_Start' = lag(Horizon_End) + 1) %>% 
    relocate(Horizon_Start, .before = Horizon_End)
  
  all_rows$Horizon_Start <- all_rows$Horizon_Start %>% 
    replace_na(0)
  
  soils <- all_rows %>% 
    rowid_to_column() %>% 
    split(., .$rowid) %>% 
    lapply(., breaks2cmIncrements) %>% 
    bind_rows()
    
  return(soils)
}

