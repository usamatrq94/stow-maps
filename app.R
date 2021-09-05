library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(shinyBS)
library(shinyWidgets)

## Setting upload limit of 35Mb
options(shiny.maxRequestSize=35*1024^2)

## Fetching AWS Environment
source("aws_creds.R")
aws_creds()


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
            "date", label = "Select Date", value = Sys.Date()
            )
          ),
        column(
          6,
          selectizeInput(
            "time", label = "Select Time", choices =  NULL
            )
          )
        ),
      pickerInput(
        "Size", label = "Bin Size",
        choices = list("Apparel" = "A", "Library" = "L", "Deep Library" = "DL", "Shoes" = "S"),
        multiple = TRUE, selected = c("A", "L", "DL", "S"),
        choicesOpt = list(
          content = sprintf("<span class='label label-%s'>%s</span>",
                            c("info", "warning", "danger", "primary"),
                            c("Apparel", "Library", "Deep Library", "Shoes")
        ))
      ),
      pickerInput(
        "Mod", label = "Mods",
        choices = c("A", "B", "D", "N"),
        multiple = TRUE, selected = c("A", "B", "D"),
        choicesOpt = list(
          content = sprintf("<span class='label label-%s'>%s</span>",
                            c("info", "warning", "danger", "primary"),
                            c("A", "B", "D", "N")
          ))
      ),
      sliderInput(
        inputId = "bins", label = "Select Number of Aisles per Bin Type",
        min = 1, value = 5, max = 10
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
                               9,
                               dataTableOutput("dataset")
                             ),
                             column(
                               3,
                               htmlOutput("aisles"),
                               tags$head(
                                 tags$style(
                                   "#aisles{ 
                           height: auto; background: WhiteSmoke;   padding: 10px;   padding-right: 30px;
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
  
  ## Selecting database
  fileName <- reactive(
    dates %>% filter(Date == input$date) %>% filter(timeRound == input$time) %>% 
      arrange(desc(timeRound)) %>% head(1) %>% first(1)
  )
  sDatabase <- reactive(
    s3read_using(FUN = read.csv, bucket = 'stowmaps', object = fileName())
  )
  ## Database Wrangling
  rawDatabase <- reactive(
    sDatabase() %>% filter(Bin.Type %in% c("DRAWER", "LIBRARY-DEEP",
                                         "LIBRARY", "SHOES"),
                         !Bin.Usage == "DAMAGE") %>%
      mutate(
        Size = case_when(
          Bin.Type == "DRAWER" ~ "A", Bin.Type == "LIBRARY-DEEP" ~ "DL",
          Bin.Type == "LIBRARY" ~ "L", Bin.Type == "SHOES" ~ "S", TRUE ~ "ERROR"
        )
      ) %>% filter(!Dropzone %in% c("dz-P-Damage", "dz-P-DMGLAND", "dz-P-HRV")) %>%
      mutate(
        Dz = case_when(
          Dropzone == 'dz-P-A1LOW' ~ 'L', Dropzone == 'dz-P-NONSORT' ~ '*',
          Dropzone == 'dz-P-A1MED' ~ 'M', Dropzone == 'dz-P-A1HIGH' ~ 'H',
          Dropzone == 'dz-P-SORT' ~ '*', Dropzone == 'dz-P-1A' ~ '*',
          Dropzone == 'dz-P-PRIME' ~ '*', Dropzone == 'dz-P-LIBRARY_DEEP' ~ '*',
          TRUE ~ "ERROR"
        )
      ) %>% mutate(Space = 100 - Utilization..)
  )
  
  wDatabase <- reactive(
    rawDatabase() %>% group_by(Mod, Dz, Size, Aisle) %>%
      summarise(Aisle.Space = mean(Space)) %>% ungroup() %>% 
      mutate(Location = paste0(Mod, "-", Aisle, Dz, "-", Size))
  )
  
  slices <- reactive(
    wDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size) %>%
      group_by(Size) %>% arrange(Size, desc(Aisle.Space)) %>% slice(1:input$bins)
  )
 
  
  ## Slider
  
  # Date
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
  
  # Upload
  observeEvent(
    input$file, {
      put_object(
        file = input$file$datapath,
        object =toString(
          format(
            as.POSIXct(Sys.time()),
            tz="Australia/Melbourne",usetz=FALSE
          )
        ),
        bucket = "stowmaps",
        multipart = TRUE
      )
      session$reload()
      
    }
  )
  
  
  ### Main Panel
  
  ## Recommendation
  # Table
  output$dataset <- renderDataTable(
    slices()
  )
  # Aisle Text
  output$aisles <- renderUI(
    {
      HTML(
        paste(
          unlist(slices()$Location),
          collapse = " <br> "
        )
      )
    }
  )
  

  
  }

shinyApp(ui = ui, server = server)


