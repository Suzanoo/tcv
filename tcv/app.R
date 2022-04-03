
## Covid-2019 interactive mapping tool: script to reformat JHU data from scratch

## This App adapted from R-App of: https://github.com/eparker12/nCoV_tracker which create by:
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, March 2019

## data extracted from Johns Hopkins data obtained from following Github repository
## https://github.com/CSSEGISandData/COVID-19

## And from Thailand Department of Disease Control: https://covid19.ddc.moph.go.th/

library(shiny)
library(tidyverse)
library(lubridate)
library(rlang)
library(sf)
library(leaflet)
library(rmapshaper)
library(RColorBrewer)
library(plotly)
library(highcharter)

library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(bslib)

## to prevent cross over from old runs
rm(list = ls(), envir = globalenv()) 

source("vbox.R")
source("sparkobj.R")
#------------------------------------------------------------ 
## Data pre-processing
## Data from last archive 
world_daily <- readr::read_csv("data/world_Daily.csv") 

## Update world report from Johns Hopkins
jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 

jhu_date <- lubridate::mdy(last(names(jhu_cases)))
last_report_date <- lubridate::ymd(max(world_daily$date))

## Process if our archived data is not update with Johns Hopkins
if (jhu_date != last_report_date) {
  paste0("Ther are new cases")
  source("world_cv.R", local =TRUE)
  source("thai_cv.R", local =TRUE)
  
  world_daily <- readr::read_csv("data/world_Daily.csv") 
  world_weekly <- readr::read_csv("data/world_Weekly.csv")
  world_monthly <- readr::read_csv("data/world_Monthly.csv")
  
  province_daily <-readr::read_csv("data/province_daily.csv")
  province_weekly <-readr::read_csv("data/province_weekly.csv")
  province_monthly <-readr::read_csv("data/province_monthly.csv")
  
  thai_daily <-readr::read_csv("data/thai_daily.csv")
  thai_weekly <-readr::read_csv("data/thai_weekly.csv")
  thai_monthly <-readr::read_csv("data/thai_monthly.csv")

## Process if data is up to date  
}else{
  paste0('Data is up to date.No action Talk only!!!')
  # world_daily <- readr::read_csv("data/world_Daily.csv") 
  world_weekly <- readr::read_csv("data/world_Weekly.csv")
  world_monthly <- readr::read_csv("data/world_Monthly.csv")
  
  province_daily <- readr::read_csv("data/province_daily.csv")
  province_weekly <- readr::read_csv("data/province_weekly.csv")
  province_monthly <- readr::read_csv("data/province_monthly.csv")
  
  thai_daily <- readr::read_csv("data/thai_daily.csv")
  thai_weekly <- readr::read_csv("data/thai_weekly.csv")
  thai_monthly <- readr::read_csv("data/thai_monthly.csv")
}

# get thai map
# see basic geographic data analysis from the book "Geocomputer with R" authored by Ribin Lovelace, Jakub Nowosad and Jannes Muenchow
# https://geocompr.robinlovelace.net/index.html
thai <- sf::read_sf(dsn = "data/tha_adm_rtsd_itos_20190221_SHP_PART_1/tha_admbnda_adm1_rtsd_20190221.shp")%>%
  rmapshaper::ms_simplify(keep = 0.25) #resize

# read coordinate
coor <- read.csv("data/province_coordinate.csv") #Thailand province coordinates

#World country coordinates 
country_coor <- read.csv("data/countries_codes_and_coordinates.csv") %>% 
  na.omit()

# add coordinate into map 
thai <- thai %>%
  left_join(coor %>%
              select(c(3, 4, 5)), by = "ADM1_PCODE")%>%
  select(c(3:5, 18, 19))

min_date <- min(province_daily$date)
max_date <- max(province_daily$date)

#------------------------------------------------------------ 
#create ui 
#source('ui.R') #Desktop version
source('ui2.R') #Mobile

#------------------------------------------------------------
server <- function(input, output, session) {
  
  # addClass(selector = "body", class = "sidebar-collapse")
  # 
  # # Reset Button
  # # Need to exclude the buttons from themselves being bookmarked
  # setBookmarkExclude(c("bookmark1", "bookmark2"))
  # 
  # # Trigger bookmarking with either button
  # observeEvent(input$bookmark1, {
  #   session$doBookmark()
  # })
  # observeEvent(input$bookmark2, {
  #   session$doBookmark()
  # })
  # 
  # js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  # 
  # observeEvent(input$reset_button, {
  #   reset("form")
  # })
  # 
  # id <- NULL
  # 
  # observeEvent(input$reset_button, {
  #   id <<- showNotification(
  #     paste("Filters are Reset"),
  #     duration = 5,
  #     type = "message"
  #   )
  # })
  # 
  #------------------------------------------------------------
  # Date input
  date_filter <- eventReactive(input$date, {
    input$date
  })
  
  # Case input
  case_choose <- eventReactive(input$cases, {
    case_when(input$cases == "New Cases" ~ "new_case",
              input$cases == "New Deaths" ~  "new_death",
              input$cases == "Total Cases" ~ "total_case",
              input$cases == "Total Deaths" ~  "total_death"
    )
  })
  # Province data render into map
  df <- eventReactive(input$timeline, {
    if (input$timeline == "Daily"){
      df <-province_daily
    }else if (input$timeline == "Weekly"){
      df <- province_weekly
    }else if (input$timeline == "Monthly"){
      df <-province_monthly
    }
    df %>%
      filter(date <= date_filter()) %>%
      filter(date == max(date)) %>%
      # We filter date because if user choose weekly or monthly and date is not match Sunday or month end,
      # app will shift report on Sunday for weekly and month end for monthly
      select(date, new_case, new_death, total_case, total_death, ADM1_EN)
    
  })
  
  # Country data 
  df_country <- eventReactive(input$timeline, {
    if (input$timeline == "Daily"){
      df_country <- thai_daily
    }else if (input$timeline == "Weekly"){
      df_country <- thai_weekly
    }else if (input$timeline == "Monthly"){
      df_country <- thai_monthly
    } 
    df_country
  })
  
  # Color pallete
  pallete <- reactive({
    bins1 <- case_when(
      case_choose() == "new_case" ~ c(0, 50, 100, 500, 1000, 2000, Inf),
      case_choose() == "new_death" ~ c(0, 10, 30, 50, 100, 200, Inf),
      case_choose() == "total_case" ~ c(0, 1e4, 2e4, 5e4, 1e5, 2e5, Inf),
      case_choose() == "total_death" ~ c(0, 1e3, 2e2, 3e3, 4e3, 5e3, Inf)
    )
    
    colorBin(palette = "YlOrRd",
             domain = df() %>%
               select(matches(case_choose())),
             bins = bins1, na.color = "green")
  })
  
  # Value Boxes 1-4
  output$count1 <- renderValueBox({
    value <- df() %>%
      select(matches("new_case")) %>%
      pull() %>% sum()
    vbox_render(value,
                df_country(),
                label = "new_case" ,
                chart_type = "area",
                subtitle = paste0(input$timeline, " Report"),
                info = "|||",
                icon = icon("plane"),
                color = "teal"
    )
  })
  
  output$count2 <- renderValueBox({
    value <- df() %>%
      select(matches("new_death")) %>%
      pull() %>% sum()
    vbox_render(value,
                df_country(),
                label = "new_death" ,
                chart_type = "area",
                subtitle = paste0(input$timeline, " Report"),
                info = "|||",
                icon = icon("plane"),
                color = "orange"
    )
  })
  
  output$count3 <- renderValueBox({
    value <- df() %>%
      select(matches("total_case")) %>%
      pull() %>% sum()
    vbox_render(value,
                df_country(),
                label = "total_case" ,
                chart_type = "line",
                subtitle = paste0(input$timeline, " Report"),
                info = "|||",
                icon = icon("plane"),
                color = "olive"
    )
  })
  
  output$count4 <- renderValueBox({
    value <- df() %>%
      select(matches("total_death")) %>%
      pull() %>% sum()
    vbox_render(value,
                df_country(),
                label = "total_death" ,
                chart_type = "line",
                subtitle = paste0(input$timeline, " Report"),
                info = "|||",
                icon = icon("plane"),
                color = "red"
    )
  })
  ## skip 
  # Plot graph
  # output$plot_xxx <- renderPlotly({
  #   
  #   # function for thai pandemic plot
  #   df_country() %>%
  #     filter(date <= date_filter()) %>%
  #     as_data_frame() %>%
  #     ggplot(aes(x = date, y = !!rlang::sym(case_choose()),
  #     ), show.legend = FALSE)+
  #     geom_line(color = "orange")+
  #     geom_point(size = .5, color = "#823a14", alpha = 0.5)+
  #     labs(tittle = 'Scale in Log Scale',
  #          subtitle  = paste0("source: ", "https://covid19.ddc.moph.go.th/"),
  #          x = "Date", y = str_to_title(case_choose()))+
  #     theme_bw()+
  #     scale_y_continuous(trans = "log10")
  #   
  # })
  
  # Plot map
  #Initial Map --> I set default = daily & total cases, you can change at ui "render_thai" input setting
  output$render_thai <- renderLeaflet({
    pal <- pallete()
    
    #More leaflet detail from document https://rstudio.github.io/leaflet/
    #Merge map and data and pass to render in leaflet
    thai_st <- thai %>%
      left_join(df(), by = "ADM1_EN")
    
    thai_st %>%
      leaflet() %>%
      
      #You can choose other based map from https://leaflet-extras.github.io/leaflet-providers/preview/
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))%>%
      setView(lng = 100.5018, lat = 13.7563 ,zoom = 5)%>% #set at Bangkok coordinate
      addLegend("bottomright", title = paste0(input$cases, " |||"),
                pal = pal,
                values = ~df() %>%
                  select(matches(case_choose())) %>%
                  pull())%>%
      addPolygons(fillColor = ~pal(thai_st %>%
                                     select(matches(case_choose())) %>%
                                     st_drop_geometry() %>%
                                     pull()),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  stroke = FALSE) %>%
      addCircleMarkers(data = thai_st, lng = ~LONG, lat = ~LAT,
                       weight = 1, radius = ~(total_case)^(1/4),
                       fillOpacity = 0.2, color = "olive",
                       label = as.list(sprintf(
                         "<strong>%s<strong><br/>New Cases: %d<br/>New Deaths: %d<br/>Total Cases: %d<br/>Total Deaths: %d",
                         thai_st$ADM1_EN, thai_st$new_case, thai_st$new_death, thai_st$total_case, thai_st$total_death
                       ))%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "olive"),
                         textsize = "15px", direction = "auto"))
    
    
  })

  ##Render world pandemic map
  output$render_world <- renderLeaflet({
    data = world_daily %>%
      filter(date == max(date)) %>%
      select(-last_update) %>%
      left_join(country_coor %>%
                  select(c( 6, 7, 10)), by = "jhu_ID")
    
    data %>%
      leaflet() %>%
      setView(lng = 9, lat = 34 , zoom = 2) %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = providerTileOptions(noWrap = TRUE))%>%
      
      addCircleMarkers(data = data,
                       lng = ~longitude, lat = ~latitude,
                       weight = 1, radius = ~(cases)^(1/6),
                       fillOpacity = 0.2, color = "olive",
                       label = as.list(sprintf(
                         "<strong>%s<strong><br/>New Cases: %g<br/>New Deaths: %d<br/>Total Cases: %d<br/> Total Deaths: %d",
                         data$jhu_ID, data$new_cases, data$new_deaths, data$cases, data$deaths
                       ))%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "olive"),
                         textsize = "15px", direction = "auto"))
    
  })
  # Value Boxes 5-8
  dt <- reactive({
    world_daily %>% 
      mutate(
        date = as_date(date)
      ) %>%
      select(date, cases, new_cases, deaths, new_deaths) %>%
      group_by(date) %>%
      summarise(across(where(is.numeric), sum), .groups = "drop") %>% 
      set_names(c("date", "total_case", "new_case", "total_death", "new_death"))
  })

  output$world1 <- renderValueBox({
    value <- dt() %>% 
      filter(date == max(date)) %>% 
      select(matches("new_case")) %>% 
      pull()
    vbox_render(value,
                dt(),
                "new_case",
                chart_type = "line",
                subtitle = "World Daily Report",
                info = "|||",
                icon = icon("plane"),
                color = "teal"
    )
  })
  
  output$world2 <- renderValueBox({
    value <- dt() %>% 
      filter(date == max(date)) %>% 
      select(matches("total_case")) %>% 
      pull()
    vbox_render(value,
                dt(),
                "total_case",
                chart_type = "line",
                subtitle = "World Daily Report",
                info = "|||",
                icon = icon("plane"),
                color = "orange"
    )
  })
  
  output$world3 <- renderValueBox({
    value <- dt() %>% 
      filter(date == max(date)) %>% 
      select(matches("new_death")) %>% 
      pull()
    vbox_render(value,
                dt(),
                "new_death",
                chart_type = "line",
                subtitle = "World Daily Report",
                info = "|||",
                icon = icon("plane"),
                color = "olive"
    )
  })
  
  output$world4 <- renderValueBox({
    value <- dt() %>% 
      filter(date == max(date)) %>% 
      select(matches("total_death")) %>% 
      pull()
    vbox_render(value,
                dt(),
                "total_death",
                chart_type = "line",
                subtitle = "World Daily Report",
                info = "|||",
                icon = icon("plane"),
                color = "red"
    )
  })
  # 
  ## Skip
  # World plot graph 
  # output$plot_world<- renderPlotly({
  #   world_daily %>%
  #     mutate(date = as_date(date),
  #            global_level = "global") %>%
  #     select(c(2, 4:7), global_level) %>%
  #     group_by(global_level, date) %>%
  #     summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  #     as_data_frame() %>%
  #     ggplot(aes(x = date, y = !!rlang::sym(input$rb) , group = 1), show.legend = FALSE)+
  #     geom_line(color = "orange")+
  #     geom_point(size = .5, color = "#823a14", alpha = 0.5)+
  #     labs(subtitle = "source from Johns Hopkins:  https://github.com/CSSEGISandData/COVID-19",
  #          ylab = paste0(input$rb, "/", "in weekly scale"))+
  #     theme_bw()
  #   # scale_y_continuous(trans = "log10")
  #   
  # })
  
}

# Run the application 

shinyApp(ui = ui, server = server)




