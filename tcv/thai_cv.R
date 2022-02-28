
## Load archived report : 22-01-2020 to 31-12-2021 
province_daily_performance <- read_csv("performance/province_daily_performance.csv")
province_weekly_performance <- read_csv("performance/province_weekly_performance.csv")
province_monthly_performance <- read_csv("performance/province_monthly_performance.csv")
thai_daily_performance <- read_csv("performance/thai_daily_performance.csv")
thai_weekly_performance <- read_csv("performance/thai_weekly_performance.csv")
thai_monthly_performance <- read_csv("performance/thai_monthly_performance.csv")

province <- read_csv("data/province.csv")

# # get population
pop <- readr::read_csv("data/thai_population_2020.csv") %>%
  as_tibble()%>%
  na.omit()%>%
  mutate(ADM1_PCODE= str_to_upper(ADM1_Pcode))%>%
  select(ADM1_PCODE, 8)



## download dataset and add province code into data
province_daily2 <- jsonlite::fromJSON("https://covid19.ddc.moph.go.th/api/Cases/timeline-cases-by-provinces") %>%
  as_tibble() %>%
  mutate(date = as_date(txn_date),
         ADM1_TH = province) %>%
  relocate(date, .before = txn_date) %>%
  relocate(ADM1_TH, .after = date) %>%
  ## I found data has problem (There are duplicate row since 28-12-2021) --> I fixed it below)
  group_by(date, ADM1_TH) %>% 
  filter(total_case == max(total_case)) %>% 
  filter(!duplicated(ADM1_TH)) %>% 
  ungroup() %>%
  select(-3, -4, -7, -8, -11)

## Function for pre-processing
# add counter(column = update) to data
counter <- function(tbl){
  df <- unique(tbl$date)%>%
    bind_cols(c(1:length((unique(tbl$date)))))%>%
    set_names(c("date", "update"))%>%
    right_join(tbl, by = "date")
  
  df
}

# Calculate accumulate for new_case and new_death for weekly, monthly
accum_cal <- function(tbl, collated_data){
  
  # collated <- NULL #Placeholder table --> if not used archive data comment out
  collated <- collated_data
  
  for (i in 1: length(unique(tbl$update))){
    
    last <- collated %>%
      filter(date == max(date))
      
    present <- tbl %>%
      filter(update == i)%>%
      mutate(
        new_case = total_case - last$total_case,
        new_death = total_death - last$total_death) 
      
    collated <- collated %>%
      bind_rows(present)

  }
  
  collated
}

# create province daily cases
province_daily <- province_daily2 %>%
  left_join(province, by = "ADM1_TH")%>%
  left_join(pop, by = "ADM1_PCODE")%>%
  na.omit()%>%
  mutate(Population = Both_TOTAL)%>%
  select(-Both_TOTAL)%>%
  filter(date > max(province_daily_performance$date)) %>%
  counter()%>%
  accum_cal(province_daily_performance)

# create province weekly cases
province_weekly <- province_daily%>%
  filter(date > max(province_daily_performance$date)) %>%
  mutate(week_day = wday(date, label =TRUE))%>%
  filter(week_day == "Sun")%>%
  select(-week_day, -update)%>%
  counter()%>%
  accum_cal(province_weekly_performance)

# create province monthly cases
province_monthly <- province_daily%>%
  filter(date > max(province_daily_performance$date)) %>%
  group_by(month = month(date), year = year(date))%>%
  filter(date == max(date))%>%
  arrange(date, ADM1_TH)%>%
  ungroup()%>%
  select(-month, -year, -update)%>%
  counter()%>%
  accum_cal(province_monthly_performance )

# create whole country
thai_daily <-province_daily%>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  select(-update)

thai_weekly <- province_weekly %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  select(-update)

thai_monthly <- province_monthly %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  select(-update)

#check: plot --> provide specific case
# province_monthly %>%
#   group_by(date, ADM1_PCODE) %>%
#   as_data_frame()%>%
#   ggplot(aes(x = date, y = total_case, color = ADM1_PCODE))+
#   geom_line()

# for test : xplot(thai_monthly, max_date, "new_case")

write.csv(province_daily, "data/province_daily.csv", row.names = FALSE)
write.csv(province_weekly, "data/province_weekly.csv", row.names = FALSE)
write.csv(province_monthly, "data/province_monthly.csv", row.names = FALSE)
write.csv(thai_daily, "data/thai_daily.csv", row.names = FALSE)
write.csv(thai_weekly, "data/thai_weekly.csv", row.names = FALSE)
write.csv(thai_monthly, "data/thai_monthly.csv", row.names = FALSE)

## If you want new archived report comment out below
# write.csv(province_daily %>%filter(date < ymd("2021-12-28")), "performance/province_daily_performance.csv", row.names = FALSE)
# write.csv(province_weekly %>%filter(date < ymd("2021-12-28")), "performance/province_weekly_performance.csv", row.names = FALSE)
# write.csv(province_monthly %>%filter(date < ymd("2021-12-28")), "performance/province_monthly_performance.csv", row.names = FALSE)
# write.csv(thai_daily %>%filter(date < ymd("2021-12-28")), "performance/thai_daily_performance.csv", row.names = FALSE)
# write.csv(thai_weekly %>%filter(date < ymd("2021-12-28")), "performance/thai_weekly_performance.csv", row.names = FALSE)
# write.csv(thai_monthly %>%filter(date < ymd("2021-12-28")), "performance/thai_monthly_performance.csv", row.names = FALSE)

rm(list = ls())



#################################################################################################
### If you want start from first day report used these code instead all above
## get map
## see basic geographic data analysis from the book "Geocomputer with R" authored by Ribin Lovelace, Jakub Nowosad and Jannes Muenchow
## https://geocompr.robinlovelace.net/index.html

# thai <- sf::read_sf(dsn = "data/tha_adm_rtsd_itos_20190221_SHP_PART_1/tha_admbnda_adm1_rtsd_20190221.shp")%>%
#   rmapshaper::ms_simplify(keep = 0.25) #resize
# 
# # read coordinate
# coor <- readr::read_csv("data/province_coordinate.csv") #Thailand province coordinates
# country_coor <- readr::read_csv("data/countries_codes_and_coordinates.csv") %>% na.omit() #Country coordinates
# 
# # add coordinate into map
# thai <- thai %>%
#   left_join(coor %>%
#               select(3:5), by = "ADM1_PCODE")%>%
#   select(3:5, 18, 19)
# 
# # get population
# pop <- readr::read_csv("data/thai_population_2020.csv") %>%
#   as_tibble()%>%
#   na.omit()%>%
#   mutate(ADM1_PCODE= str_to_upper(ADM1_Pcode))%>%
#   select(ADM1_PCODE, 8)
# 
# # created province code table
# province <- tibble(rep(0, 77))%>%
#   mutate(
#     ADM1_EN = unique(thai$ADM1_EN),
#     ADM1_PCODE = unique(thai$ADM1_PCODE),
#     ADM1_TH = unique(thai$ADM1_TH)
#   )%>%
#   select(-1)
# 
# # download dataset and add province code into data
# province_daily1 <- jsonlite::fromJSON("https://covid19.ddc.moph.go.th/api/Cases/round-1to2-by-provinces") %>%
#   as_tibble() %>%
#   mutate(date = as_date(txn_date),
#          ADM1_TH = province) %>%
#   relocate(date, .before = txn_date) %>%
#   relocate(ADM1_TH, .after = date) %>%
#   select(-3, -4, -7, -8)
# 
# province_daily2 <- jsonlite::fromJSON("https://covid19.ddc.moph.go.th/api/Cases/timeline-cases-by-provinces") %>%
#   as_tibble() %>%
#   mutate(date = as_date(txn_date),
#          ADM1_TH = province) %>%
#   relocate(date, .before = txn_date) %>%
#   relocate(ADM1_TH, .after = date) %>%
#   select(-3, -4, -7, -8, -11)
# 
# ## Function for pre-processing
# # add counter(column = update) to data
# counter <- function(tbl){
#   df <- unique(tbl$date)%>%
#     bind_cols(c(1:length((unique(tbl$date)))))%>%
#     set_names(c("date", "update"))%>%
#     right_join(tbl, by = "date")
# 
#   df
# }
# 
# # Calculate accumulate for new_case and new_death for weekly, monthly
# accum_cal <- function(tbl){
#   collated <- NULL #Placeholder table
# 
#   for (i in 1: length(unique(tbl$update))){
#     #for the first day accum. = that day report
#     if (i == 1){
#       first <- tbl %>%
#         filter(update == i)%>%
#         mutate(
#           new_case = total_case,
#           new_death = total_death)
#       collated <- collated %>%
#         bind_rows(first)
# 
#     }else {
#       #pull yesterday report
#       last <- tbl %>%
#         filter(update == i-1)
# 
#       #pull today report and calculated
#       present <- tbl %>%
#         filter(update == i)%>%
#         mutate(
#           new_case = total_case - last$total_case,
#           new_death = total_death - last$total_death)
# 
#       #merge data and collected to placeholder
#       collated <- collated %>%
#         bind_rows(present)
#     }
#   }
# 
#   collated
# }
# 
# # add province code to data and we will use this code for joining with the map later
# province_daily <- province_daily1 %>%
#   bind_rows(province_daily2)%>%
#   left_join(province, by = "ADM1_TH")%>%
#   left_join(pop, by = "ADM1_PCODE")%>%
#   na.omit()%>%
#   mutate(Population = Both_TOTAL)%>%
#   select(-Both_TOTAL)%>%
#   counter()%>%
#   accum_cal()
# 
# # create province weekly cases
# province_weekly <- province_daily%>%
#   mutate(week_day = wday(date, label =TRUE))%>%
#   filter(week_day == "Mon")%>%
#   select(-week_day, -update)%>%
#   counter()%>%
#   accum_cal()
# 
# # create province monthly cases
# province_monthly <- province_daily%>%
#   group_by(month = month(date), year = year(date))%>%
#   filter(date == max(date))%>%
#   arrange(date, ADM1_TH)%>%
#   ungroup()%>%
#   select(-month, -year, -update)%>%
#   counter()%>%
#   accum_cal()
# 
# # create whole country
# thai_daily <-province_daily%>%
#   group_by(date) %>%
#   summarise(across(where(is.numeric), sum)) %>%
#   select(-update)
# 
# thai_weekly <- province_weekly %>%
#   group_by(date) %>%
#   summarise(across(where(is.numeric), sum)) %>%
#   select(-update)
# 
# thai_monthly <- province_monthly %>%
#   group_by(date) %>%
#   summarise(across(where(is.numeric), sum)) %>%
#   select(-update)
# 
# write.csv(province_daily, "data/province_daily.csv", row.names = FALSE)
# write.csv(province_weekly, "data/province_weekly.csv", row.names = FALSE)
# write.csv(province_monthly, "data/province_monthly.csv", row.names = FALSE)
# write.csv(thai_daily, "data/thai_daily.csv", row.names = FALSE)
# write.csv(thai_weekly, "data/thai_weekly.csv", row.names = FALSE)
# write.csv(thai_monthly, "data/thai_monthly.csv", row.names = FALSE)
# 
# rm(list = ls())



