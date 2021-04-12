# Import necessary packages and functions
# Data from the stacker
# Dany 20210409

myconditions <- readr::read_csv(path_to_data_list,
                                col_types = cols(
                                  date=col_character(),
                                  description=col_character(),
                                  data_path=col_character(),
                                  plate_map_path=col_character()))

# Import the data
mydata <- myconditions %>%
  group_by(date,description,plate) %>% #date here is as per datalist, this is not the date of the measurement, description and plate should refer, together, to a unique experiment
    do((function(.df){
    plate_ <- as.character(unique(.df$plate))
    date_ <- as.character(unique(.df$date))
    description_ <- as.character(unique(.df$description))
    path_ <- as.character(unique(.df$data_path))
    new_df <- lapply(channels, function(c.){read_Biotek_Synergy_stacker(path_,c.)}) %>%
      bind_rows() %>% 
      mutate(date=date_,
             description=description_,
             plate=plate_)
    return(new_df)})(.)) %>% 
  select(date,description,plate,row,col,value,channel,time,step) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=c("date","description","plate","col","row","step"),names_from=c("channel"),values_from=c("value","time")) %>% 
  rename(od=value_OD600,
         fluo=value_GFP,column=col) %>% 
  mutate(time_min_od=time_OD600/60,
         time_min_fluo=time_GFP/60,time_min=15*(step-1)) %>% 
  select(-c(time_OD600,time_GFP))

# Import plate layout
mylayouts <- myconditions %>%
  group_by(date,description,plate) %>% 
  do((function(.df){
    plate_ <- as.character(unique(.df$plate))
    date_ <- as.character(unique(.df$date))
    description_ <- as.character(unique(.df$description))
    path_ <- as.character(unique(.df$plate_map_path))
    new_df <- read_plate_layout(path_) %>% 
      bind_rows() %>% 
      mutate(date=date_,
             description=description_,
             plate=plate_)
    return(new_df)})(.)) %>% 
  ungroup()
  #rename(exp_start_date=date)

# Completed data

mydata <- mydata %>% 
  left_join(mylayouts,by=c("date","plate","description","row","column"))

