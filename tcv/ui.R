###The ui cannot see all the variables in the server, they have other ways to communicate.
###We reload necessary variable
province_daily <-readr::read_csv("data/province_daily.csv")
min_date <- min(province_daily$date)
max_date <- max(province_daily$date)

ui <- dashboardPage(skin='black',
                    dashboardHeader(title = "Thailand: Covid-19 Tracker"),
                    dashboardSidebar(width=275,
                                     
                                     # The dynamically-generated user panel
                                     uiOutput("userpanel"),
                                     
                                     # Side Bar Menu
                                     sidebarMenu(style = "position: Scroll; overflow: visible;", id = "sidebarmenu",
                                                 
                                                 menuItem("World Pandemic", tabName = "iaa", icon = icon("th", lib = "font-awesome")),
                                                 
                                                 menuItem("Thailand Dashboard", tabName = "cso", icon = icon("tachometer-alt", lib = "font-awesome"),
                                                          badgeLabel = "new",
                                                          badgeColor = "green"),
                                                 
                                                 conditionalPanel("input.sidebarmenu === 'cso'",
                                                   # a. FILTERS
                                                   useShinyjs(),
                                                   div(id = "form",
                                                       tags$hr(),
                                                       selectInput("timeline", "Timeline",
                                                                   choices = list("Daily", "Weekly", "Monthly"),
                                                                   bookmarkButton(id = "bookmark1")),
                                                       selectInput("cases", "Cases",
                                                                   choices = list( "Total Cases", "Total Deaths", "New Cases", "New Deaths"),
                                                                   bookmarkButton(id = "bookmark2")),
                                                       
                                                       sliderInput("date", label = h3("Date"), min = min_date,
                                                                   max = max_date, value = max_date),
                                                       
                                                       absolutePanel(
                                                         id = "control", class = "panel panel-default",
                                                         top = 550, left = 150, width = 500, fixed = TRUE,
                                                         draggable = TRUE, height = "auto",
                                                         
                                                         plotlyOutput("plot_xxx", height="250px", width="100%"),
                                                         
                                                         style = "opacity: 0.95; z-index: 10;" ## z-index modification
                                                       ),
                                                   ))
                                                 
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "iaa",
                                #WORLD PANDEMIC
                                fluidRow(column(10, offset = 0.5,h2("WORLD PANDEMIC"),
                                                h4("Source from Johns Hopkins repo: https://github.com/CSSEGISandData/COVID-19"),
                                                h6("Developer : Wittaya Somanant : highwaynumber12@gmail.com")
                                )),
                                
                                br(),
                                fluidRow(column(12, offset = 2.5, 
                                                leafletOutput('render_world', width = 1500, height = 600))),
                                absolutePanel(
                                  id = "control2", class = "panel panel-default",
                                  
                                  top = 425, left = 150, width = 450, fixed = TRUE,
                                  draggable = TRUE, height = "auto",
                                  
                                  plotlyOutput("plot_world", height="250px", width="100%"),
                                  
                                  radioButtons(
                                    inputId = "rb", label = "Cases", inline = TRUE,
                                    choiceNames = list("New Cases", "Total Cases", "New Deaths", "Total Deaths"),
                                    choiceValues = list("new_cases", "cases", "new_deaths", "deaths")
                                  ),
                                  
                                  style = "opacity: 0.75; z-index: 10;" ## z-index modification
                                ),
                                
                                br()
                        ),
                        tabItem(tabName = "cso",
                                #THAILAND REPORT
                                fluidRow(column(10, offset = 0.5, h2("THAILAND REPORT"),
                                                h4("Source from Thailand Department of Disease Control: https://covid19.ddc.moph.go.th/"),
                                                h6("For daily report --> report at that day"),
                                                h6("For weekly report --> report on Sunday of that week"),
                                                h6("For monthly report --> report at end day of previous month"))),
                                
                                
                                fluidRow(style="height:35px;",
                                         valueBoxOutput("count1",width = 3),
                                         valueBoxOutput("count2",width = 3),
                                         valueBoxOutput("count3",width = 3),
                                         valueBoxOutput("count4",width = 3)
                                ),
                                br(),
                                br(),
                                fluidRow(column(12, offset = 2.5, 
                                                leafletOutput('render_thai', width = 1200, height = 600))),
                        )
                      )
                    )
)