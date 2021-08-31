library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(aws.s3)
library(shinyBS)

options(shiny.maxRequestSize=35*1024^2)

## SHINY APP

ui <- fluidPage(
  
  titlePanel("Simplistic Stow Maps"),
  
  tags$head(
    tags$style(
      HTML(
        "#TargetAVG{height: auto; width: auto; font-size: 30px;}
        #WarehouseAVG{height: auto; width: auto; font-size: 30px;}
        #stdev{height: auto; width: auto; font-size: 30px;}
        #availAiles{height: auto; width: auto; font-size: 30px;}"
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(
          6,
          dateInput(
            "date", label = "Select Date", value = Sys.Date()+1
            )
          ),
        column(
          6,
          selectizeInput(
            "time", label = "Select Time", choices =  NULL
            )
          )
        ),
      selectInput("selectMode", label = "Select Mode", 
                  choices = list("Individual" = 1, "Batch" = 2), 
                  selected = 2),
      conditionalPanel(
        "input.selectMode == 1",
        selectInput("bintype", label = "Select Bin Type", 
                           choices = list("Apparels" = 'A', "Library" = 'L',
                                          "Library Deep" = 'DL', "Shoe Bins" = 'S',
                                          "Non Sort" = 'N'),
                           selected = 'A')
      ),
      sliderInput(
        inputId = "bins", label = "Select Number of Aisles",
        min = 1, value = 25, max = 50
      ),
      sliderInput(
        inputId = 'percentile', label = "Select Cutoff Space",
        min = 0, value = 0, max = 0
      ),
      h6('Number of Aisles greater than cutoff'),
      verbatimTextOutput("dummy"),
      hr(),
      fileInput("file", label = "Upload a new file", accept = ".csv"),
    ),
    
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("Overview",
                           column(
                             6,
                             br(),
                             br(),
                             fluidRow(
                               column(
                                 6,
                                 h6("Available Space"),
                                 verbatimTextOutput("WarehouseAVG"),
                                 h6("Standard Deviation"),
                                 verbatimTextOutput("stdev")
                               ),
                               column(
                                 6,
                                 h6("Space Change"),
                                 verbatimTextOutput("TargetAVG"),
                                 h6("Available Aisles"),
                                 verbatimTextOutput("availAiles")
                               )
                             ),
                             fluidRow(
                               column(
                                 12,
                                 h6("Summary Statistics"),
                                 verbatimTextOutput("sum")
                               )
                             )
                           ),
                           column(
                             6,
                             plotOutput("hist")
                           )
                           ),
                  tabPanel("Recommendations",
                           br(),
                           h4("Recommended Aisles:"),
                           textOutput("dummy2"),
                           tags$head(
                             tags$style(
                               "#dummy2{
                               font-size: 12px; font-style: italic;
                               }"
                             )
                           ),
                           hr(),
                  
                           fluidRow(
                             column(
                               8,
                               dataTableOutput("dataset")
                             ),
                             column(
                               4,
                               textOutput("aisles"),
                               tags$head(
                                 tags$style(
                                   "#aisles{ 
                           height: auto; background: WhiteSmoke;   padding: 10px;
                          border: 1px #E0E0E0; border-radius: 5px; border: 1px solid #DCDCDC}"
                                 )
                               ),
                             )
                           )
                  ),
                  tabPanel('Analytics',
                    br(),
                    
                    bsCollapse(id = "collapseExample", open = "Performance Metrics",
                               bsCollapsePanel("Stow Maps", 
                                               fluidRow(
                                                 column(
                                                   3,
                                                   selectInput("selectslice", label = "View", 
                                                               choices = list("By Aisles" = 1, "By Aisles and Bays" = 2), 
                                                               selected = 2),
                                                   selectInput("filteraisles", label = "Filter Aisles", 
                                                               choices = list("All" = 1, "Top 25%" = 2, "Lowest 25%" = 3), 
                                                               selected = 3),
                                                 ),
                                               ),
                                               style = "default"
                                               ),
                               bsCollapsePanel("Performance Metrics", 
                                               fluidRow(
                                                   column(
                                                     3,
                                                     h6("Apparel Space"),
                                                     verbatimTextOutput("AppSpace"),
                                                     h6("Apparel Target Utilization"),
                                                     verbatimTextOutput("targetA")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Library Space"),
                                                     verbatimTextOutput("LibSpace"),
                                                     h6("Library Target Utilization"),
                                                     verbatimTextOutput("targetL")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Deep Library Space"),
                                                     verbatimTextOutput("dlSpace"),
                                                     h6("Deep Library Target Utilization"),
                                                     verbatimTextOutput("targetDL")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Shoe Bin Space"),
                                                     verbatimTextOutput("sbSpace"),
                                                     h6("Shoe Bin Target Utilization"),
                                                     verbatimTextOutput("targetShoe")
                                                   )
                                               ),
                                               style = "default"
                                               )
                    )
                    
                  ),
                  selected = "Recommendations"
    )
    )
  )
)

server <- function(input,output,session) {
  
  ## checking files inside AWS S3 instance
  dates <- data.frame(get_bucket(bucket = "stowmaps")) %>% select(Key) %>% 
    separate(Key, c("Date","Time"), " ", remove = FALSE) %>% group_by(Date) %>%
    arrange(desc(Date)) %>% mutate(
      timeRound = format(
        round(strptime(Key,"%Y-%m-%d %H:%M:%S"), 
              units="hours"
        ), 
        format="%H:%M"
      )
    )
  
  ## Slider
  
  ## date
  
  observeEvent(
    input$date, {
      timeSelector <- reactive(
        dates %>% filter(Date == input$date)
      )
      updateSelectizeInput(session, 
                           'time', 
                           choices = timeSelector()$timeRound, 
                           server = TRUE)
    }
  )
  
  
  
  
  
  }

shinyApp(ui = ui, server = server)


