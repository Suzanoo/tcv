## Covid-2019 interactive mapping tool: script to reformat JHU data from scratch

## This App adapted from R-App of: https://github.com/eparker12/nCoV_tracker which create by:
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, March 2019

## data extracted from Johns Hopkins data obtained from following Github repository
# https://github.com/CSSEGISandData/COVID-19

# Thank you to Edward Parker and Quentic Leclerc.


#########################################################################

### Process below will start from 1-01-2022 (I used archived report for reduce processing)
# If you want to start from first day report please comment out code at lowest part.

## Load archived report : 22-01-2020 to 31-12-2021 
jhu_Daily_Performance <- read_csv("performance/jhu_Daily_Performance.csv")
jhu_Weekly_Performance <- read_csv("performance/jhu_Weekly_Performance.csv")
jhu_Monthly_Performance <- read_csv("performance/jhu_Monthly_Performance.csv")


# Function to update jhu input data according to mapping base format
update_jhu <- function(input_df){
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
  input_df$Country[input_df$Province=="Macau"] = "Macao"
  input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
  input_df$Country[input_df$Country=="Korea, South"] = "Republic of Korea"
  input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
  input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
  input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
  input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
  input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
  input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
  input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
  input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "")
  
  #Group by country and sum cumulative for each patient
  input_df <- input_df%>%
    select(-c(Province, Lat, Long))%>%
    group_by(Country)%>%
    summarise_each(funs(sum))%>%
    #transpose table
    pivot_longer(-1) %>%
    pivot_wider(names_from = 1, values_from = value)%>%
    mutate(
      date = as_date(name, tz = NULL, format = "%m/%d/%y"),
    )%>%
    relocate(date, .before = name)%>%
    select(-name)
  
  input_df
}

#--------------------------------------------------------------------
# Load latest Covid-2019 data: confirmed cases from Johns Hopkins
jhu_cases <-   readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 

# Check if there are new report (diff from archived data)
new_report <- which(names(jhu_cases %>%
          as_tibble() %>%
          select(-c(1:4))) %>% 
          mdy() > max(jhu_Daily_Performance$date)) 

##Group data to daily, weekly and monthly case
jhu_cases_daily <- jhu_cases%>%
  as_tibble() %>%
  select(1:4, new_report + 4) %>% # select only new report (diff from archived)
  update_jhu()

jhu_cases_weekly <- jhu_cases_daily%>%
  mutate(
    week_day = wday(date, label = TRUE)# added week day label for weekly report, select later
  )%>%
  filter(week_day == "Sun")%>%
  select(-week_day)

jhu_cases_monthly <- jhu_cases_daily %>%
  filter(date == days_in_month(date))

# Load latest Covid-2019 data: deaths cases
jhu_deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") 

jhu_deaths_daily <- jhu_deaths%>%
  as_tibble() %>%
  select(1:4, new_report+4) %>% # select only new report (diff from archived)
  update_jhu()

jhu_deaths_weekly <- jhu_deaths_daily%>%
  mutate(
    week_day = wday(date, label = TRUE)#added week day label for weekly report, select later
  )%>%
  filter(week_day == "Sun")%>%
  select(-week_day)

jhu_deaths_monthly <- jhu_deaths_daily %>%
  filter(date == days_in_month(date))

# Load latest Covid-2019 data: recovered
jhu_rec <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",)

jhu_rec_daily <- jhu_rec%>%
  as_tibble() %>%
  select(1:4, new_report+4) %>% # select only new report (diff from archived)
  update_jhu()

jhu_rec_weekly <- jhu_rec_daily%>%
  mutate(
    week_day = wday(date, label = TRUE)#added week day label for weekly report, select later
  )%>%
  filter(week_day == "Sun")%>%
  select(-week_day)

jhu_rec_monthly <- jhu_rec_daily %>%
  filter(date == days_in_month(date))

# Create function for transpose table 
flip <- function(tbl){
  tbl <- tbl%>%
    pivot_longer(-1) %>%
    pivot_wider(names_from = 1, values_from = value)
  
  names(tbl)[1] <- "Country"
  tbl
}

# Transpose all table
jhu_cases_daily <- flip(jhu_cases_daily)
jhu_cases_weekly <- flip(jhu_cases_weekly)
jhu_cases_monthly <- flip(jhu_cases_monthly)

jhu_deaths_daily <- flip(jhu_deaths_daily)
jhu_deaths_weekly <- flip(jhu_deaths_weekly)
jhu_deaths_monthly <- flip(jhu_deaths_monthly)

jhu_rec_daily <- flip(jhu_rec_daily)
jhu_rec_weekly <- flip(jhu_rec_weekly)
jhu_rec_monthly <- flip(jhu_rec_monthly)

##Country name in JHU report must be match country name in map file.
# Process for clean
# Load country data
countries <- readr::read_csv("data/countries_codes_and_coordinates.csv")

merge <- function(cases, deaths, rec){
  jhu_merge <- cases%>%
    left_join(deaths, by='Country')%>%
    left_join(rec, by = "Country")
  
  jhu_merge
}

jhu_Daily <- merge(jhu_cases_daily, jhu_deaths_daily, jhu_rec_daily )%>%
  filter(Country %in% countries$jhu_ID)

jhu_Weekly <- merge(jhu_cases_weekly, jhu_deaths_weekly, jhu_rec_weekly )%>%
  filter(Country %in% countries$jhu_ID)

jhu_Monthly <- merge(jhu_cases_monthly, jhu_deaths_monthly, jhu_rec_monthly )%>%
  filter(Country %in% countries$jhu_ID)

#--------------------------------------------------------------------
## Re-organize table
#Create Collated Table
data_collect <- function(merge_tbl, case_tbl, collated_data){
  
  #For Test
  # merge_tbl <- jhu_Daily
  # case_tbl <- jhu_cases_daily
  # collated_data <- jhu_Daily_Performance
  # i = 4
  
  for (i in c(1: (ncol(case_tbl )-1))){ 
  
    #create placeholder df
    new_data <- merge_tbl%>%
      select(1, i+1, i+ncol(case_tbl), i-1 +2*ncol(case_tbl ))%>% #select column match date for temporary
      as_tibble()
    date <- as.Date(names(new_data)[4], "%Y-%m-%d")
    
    names(new_data) <- c('Country', 'Dcase', 'Ddeath', 'Drec')
    new_data <- new_data%>%
      filter(Dcase > 0 | Ddeath > 0 )%>%# cut zero cases
      #filter((.[[2]] > 0) | (.[[3]] > 0) )%>%# cut zero cases
      mutate(
        date = date,
        update = max(collated_data$update)+1,
        cases = Dcase,
        new_cases = 0,
        deaths = Ddeath,
        new_deaths = 0,
        recovered = Drec, 
        new_recovered =0
      )%>%
      dplyr::select(Country, date:new_recovered)
      # select(-names(.)[2:4]) #cut temporary
      
    #New country report
    new_country <- new_data%>%
      filter(!Country %in% collated_data$Country)%>%
      mutate(
        new_cases = cases,
        new_deaths = deaths,
        new_recovered = recovered
      )
    
    #New data of ever recorded country
    old_country <- new_data%>%
      filter(Country %in% collated_data$Country)
    
    #Pull last report of ever recorded country 
    Umax = max(collated_data$update)
    last_report <- collated_data%>%
      filter(Country %in% old_country$Country)%>%
      arrange(Country, cases)%>%
      filter(update == Umax)
      # filter(max(update))
    
    #Calculate new_... for ever recorded country && finally merge all
    collated_data <- old_country%>%
      mutate(
        new_cases = cases - last_report$cases,
        new_deaths = deaths - last_report$deaths,
        new_recovered = recovered - last_report$recovered)%>%
      bind_rows(new_country)%>%
      bind_rows(collated_data)%>%
      arrange(date, Country)
    
  }#for loop
  
  collated_data
  
}

jhu_Daily <- data_collect(jhu_Daily, jhu_cases_daily, jhu_Daily_Performance)

jhu_Weekly <- data_collect(jhu_Weekly, jhu_cases_weekly, jhu_Weekly_Performance)

jhu_Monthly <- data_collect(jhu_Monthly, jhu_cases_monthly, jhu_Monthly_Performance)

#--------------------------------------------------------------------
# Clean negative value : new_cases < 0 , new_deaths < 0
neg_clean <- function(tbl){
  tbl <- tbl%>%
    mutate(
      new_cases = replace(new_cases, new_cases < 0, 0),
      new_deaths = replace(new_deaths, new_deaths < 0, 0),
      new_recovered = replace(new_recovered, new_recovered < 0, 0))
  
  tbl
}

jhu_Daily <- neg_clean(jhu_Daily)
jhu_Weekly <- neg_clean(jhu_Weekly)
jhu_Monthly <- neg_clean(jhu_Monthly)

final <- function(tbl){
  #set new names
  names(tbl) <- c("jhu_ID", "date", "update", "cases", "new_cases",
                  "deaths", "new_deaths", "recovered", "new_recovered")
  tbl <- tbl%>%
    arrange(jhu_ID)%>%
    left_join(countries[, c("jhu_ID", "country")], by = "jhu_ID")%>% #join with country
    arrange(date, -cases, country)%>%
    mutate(
      last_update = NA)#added column
  
  tbl$last_update[nrow(tbl)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))#update time stamp
  
  tbl
}

jhu_Daily <- final(jhu_Daily)
jhu_Weekly <- final(jhu_Weekly)
jhu_Monthly <- final(jhu_Monthly)         

#--------------------------------------------------------------------
# save file
#write.csv(collated_data, "input_data/coronavirus_full.csv", row.names=F)
write.csv(jhu_Daily, "data/world_Daily.csv", row.names = FALSE)
write.csv(jhu_Weekly, "data/world_Weekly.csv", row.names = FALSE)
write.csv(jhu_Monthly, "data/world_Monthly.csv", row.names = FALSE)

## If you want new archived report comment out below
# write.csv(jhu_Daily %>% select(1:9), "performance/jhu_Daily_Performance.csv", row.names = FALSE)
# write.csv(jhu_Weekly %>% select(1:9), "performance/jhu_Weekly_Performance.csv", row.names = FALSE)
# write.csv(jhu_Monthly %>% select(1:9), "performance/jhu_Monthly_Performance.csv", row.names = FALSE)

# Clear all 
rm(list = ls())





#################################################################################################
### If you want start from first day of report used these code instead all above

# # Function to update jhu input data according to mapping base format
# update_jhu <- function(input_df){
#   names(input_df)[1:2] = c("Province", "Country")
#   input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
#   input_df$Country[input_df$Province=="Macau"] = "Macao"
#   input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
#   input_df$Country[input_df$Country=="Korea, South"] = "Republic of Korea"
#   input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
#   input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
#   input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
#   input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
#   input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
#   input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
#   input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
#   input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
#   input_df$Country = input_df$Country %>% str_replace_all(., " ", "")
#   
#   #Group by country and sum cumulative for each patient
#   input_df <- input_df%>%
#     select(-c(Province, Lat, Long))%>%
#     group_by(Country)%>%
#     summarise_each(funs(sum))%>%
#     #transpose table
#     pivot_longer(-1) %>%
#     pivot_wider(names_from = 1, values_from = value)%>%
#     mutate(
#       date = as_date(name, tz = NULL, format = "%m/%d/%y"),
#     )%>%
#     relocate(date, .before = name)%>%
#     select(-name)
#   
#   input_df
# }
# 
# #--------------------------------------------------------------------
# ## Group data to daily, weekly and monthly case
# 
# # Load latest Covid-2019 data: confirmed cases
# jhu_cases <-   readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#   
# jhu_cases_daily <- jhu_cases%>%
#   as_tibble()%>%
#   update_jhu()
# 
# jhu_cases_weekly <- jhu_cases_daily%>%
#   mutate(
#     week_day = wday(date, label = TRUE)#added week day label for weekly report, select later
#   )%>%
#   filter(week_day == "Sun")%>%
#   select(-week_day)
# 
# jhu_cases_monthly <- jhu_cases_daily%>%
#   group_by(month = month(date), year = year(date)) %>%
#   slice(which.max(day(date))) %>%
#   ungroup() %>%
#   select(-month, -year)%>%
#   arrange(date)
# 
# # Load latest Covid-2019 data: deaths
# jhu_deaths <- readr::read_csv(
#   "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# 
# jhu_deaths_daily <- jhu_deaths%>%
#   as_tibble()%>%
#   update_jhu()
# 
# jhu_deaths_weekly <- jhu_deaths_daily%>%
#   mutate(
#     week_day = wday(date, label = TRUE)#added week day label for weekly report, select later
#   )%>%
#   filter(week_day == "Sun")%>%
#   select(-week_day)
# 
# jhu_deaths_monthly <- jhu_deaths_daily%>%
#   group_by(month = month(date), year = year(date)) %>%
#   slice(which.max(day(date))) %>%
#   ungroup() %>%
#   select(-month, -year)%>%
#   arrange(date)
# 
# # Load latest Covid-2019 data: recovered
# jhu_rec <- readr::read_csv(
#   "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
# 
# jhu_rec_daily <- jhu_rec%>%
#   as_tibble()%>%
#   update_jhu()
# 
# jhu_rec_weekly <- jhu_rec_daily%>%
#   mutate(
#     week_day = wday(date, label = TRUE)#added week day label for weekly report, select later
#   )%>%
#   filter(week_day == "Sun")%>%
#   select(-week_day)
# 
# jhu_rec_monthly <- jhu_rec_daily%>%
#   group_by(month = month(date), year = year(date)) %>%
#   slice(which.max(day(date))) %>%
#   ungroup() %>%
#   select(-month, -year)%>%
#   arrange(date)
# 
# # Transpose table
# flip <- function(tbl){
#   tbl <- tbl%>%
#     pivot_longer(-1) %>%
#     pivot_wider(names_from = 1, values_from = value)
#   
#   names(tbl)[1] <- "Country"
#   tbl
# }
# 
# jhu_cases_daily <- flip(jhu_cases_daily)
# jhu_cases_weekly <- flip(jhu_cases_weekly)
# jhu_cases_monthly <- flip(jhu_cases_monthly)
# 
# jhu_deaths_daily <- flip(jhu_deaths_daily)
# jhu_deaths_weekly <- flip(jhu_deaths_weekly)
# jhu_deaths_monthly <- flip(jhu_deaths_monthly)
# 
# jhu_rec_daily <- flip(jhu_rec_daily)
# jhu_rec_weekly <- flip(jhu_rec_weekly)
# jhu_rec_monthly <- flip(jhu_rec_monthly)
# 
# # Clean jhu country names have not corresponding country data
# # Load country data
# countries <- readr::read_csv("data/countries_codes_and_coordinates.csv")
# 
# merge <- function(cases, deaths, rec){
#   jhu_merge <- cases%>%
#     left_join(deaths, by='Country')%>%
#     left_join(rec, by = "Country")
#   
#   jhu_merge
# }
# 
# jhu_Daily <- merge(jhu_cases_daily, jhu_deaths_daily, jhu_rec_daily )%>%
#   filter(Country %in% countries$jhu_ID)
# 
# jhu_Weekly <- merge(jhu_cases_weekly, jhu_deaths_weekly, jhu_rec_weekly )%>%
#   filter(Country %in% countries$jhu_ID)
# 
# jhu_Monthly <- merge(jhu_cases_monthly, jhu_deaths_monthly, jhu_rec_monthly )%>%
#   filter(Country %in% countries$jhu_ID)
# 
# #--------------------------------------------------------------------
# ## Re-organize table
# 
# #Create Collated Table
# data_collect <- function(merge_tbl, case_tbl ){
#   collated_data <- NULL
#   
#   for (i in c(1: (ncol(case_tbl )-1))){ #c(1: (ncol(case_tbl )-1))
#     #create placeholder df
#     new_data <- merge_tbl%>%
#       select(1, i+1, i+ncol(case_tbl), i-1 +2*ncol(case_tbl ))%>% #select column match date for temporary
#       as_tibble()%>%
#       filter(.[[2]] > 0 | .[[3]] > 0 )%>%# cut zero cases
#       mutate(
#         date = as_datetime(str_replace(names(.)[2], ".x", ""), tz = "UTC", format = NULL),
#         update = i,
#         cases = .[[2]],
#         new_cases = 0,
#         deaths = .[[3]],
#         new_deaths = 0,
#         recovered = .[[4]], 
#         new_recovered =0
#       )%>%
#       select(-names(.)[2:4]) #cut temporary
#     
#     if(i == 1){
#       collated_data <- new_data%>%
#         mutate(
#           new_cases = cases,
#           new_deaths = deaths,
#           new_recovered = recovered
#         )
#     }else{
#       #New country
#       new_country <- new_data%>%
#         filter(!Country %in% collated_data$Country)%>%
#         mutate(
#           new_cases = cases,
#           new_deaths = deaths,
#           new_recovered = recovered
#         )
#       
#       #New data of ever recorded country
#       old_country <- new_data%>%
#         filter(Country %in% collated_data$Country)
#       
#       #Pull last report of ever recorded country 
#       last_report <- collated_data%>%
#         filter(Country %in% old_country$Country)%>%
#         arrange(Country, cases)%>%
#         filter(update == max(update))
#       
#       #Calculate new_... for ever recorded country && finally merge all
#       collated_data <- old_country%>%
#         mutate(
#           new_cases = cases - last_report$cases,
#           new_deaths = deaths - last_report$deaths,
#           new_recovered = recovered - last_report$recovered)%>%
#         bind_rows(new_country)%>%
#         bind_rows(collated_data)%>%
#         arrange(date, Country)
#       
#     }#if...else
#     
#   }#for loop
#   
#   collated_data
#   
# }
# 
# jhu_Daily <- data_collect(jhu_Daily, jhu_cases_daily)
# jhu_Weekly <- data_collect(jhu_Weekly, jhu_cases_weekly)
# jhu_Monthly <- data_collect(jhu_Monthly, jhu_cases_monthly)
# 
# #--------------------------------------------------------------------na_
# #Clean negative value : new_cases < 0 , new_deaths < 0
# neg_clean <- function(tbl){
#   tbl <- tbl%>%
#     mutate(
#       new_cases = replace(new_cases, new_cases < 0, 0),
#       new_deaths = replace(new_deaths, new_deaths < 0, 0),
#       new_recovered = replace(new_recovered, new_recovered < 0, 0))
#   
#   tbl
# }
# 
# jhu_Daily <- neg_clean(jhu_Daily)
# jhu_Weekly <- neg_clean(jhu_Weekly)
# jhu_Monthly <- neg_clean(jhu_Monthly)
# 
# #######################################################################_
# ##
# final <- function(tbl){
#   #set new names
#   names(tbl) <- c("jhu_ID", "date", "update", "cases", "new_cases",
#                   "deaths", "new_deaths", "recovered", "new_recovered")
#   tbl <- tbl%>%
#     arrange(jhu_ID)%>%
#     left_join(countries[, c("jhu_ID", "country")], by = "jhu_ID")%>% #join with country
#     arrange(date, -cases, country)%>%
#     mutate(
#       last_update = NA)#added column
#   
#   tbl$last_update[nrow(tbl)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))#update time stamp
#   
#   tbl
# }
# 
# jhu_Daily <- final(jhu_Daily)
# jhu_Weekly <- final(jhu_Weekly)
# jhu_Monthly <- final(jhu_Monthly)         
# 
# # save file
# write.csv(jhu_Daily, "data/world_Daily.csv", row.names = FALSE)
# write.csv(jhu_Weekly, "data/world_Weekly.csv", row.names = FALSE)
# write.csv(jhu_Monthly, "data/world_Monthly.csv", row.names = FALSE)
# 
# # Clear all 
# rm(list = ls())







