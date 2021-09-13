library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(purrr)

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
    
    sidebarPanel(width = 3,
      fluidRow(
        column(
          6,
          dateInput(
            "date", label = "Date", value = Sys.Date()
            )
          ),
        column(
          6,
          selectizeInput(
            "time", label = "Time", choices =  NULL
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
      pickerInput(
        "Velo", label = "Velocity",
        choice = c("Slow" = "S", "Fast" = "F", "Non-Velocity" = "NV"),
        multiple = TRUE, selected = c("S", "F", "NV"),
        choicesOpt = list(
          content = sprintf("<span class='label label-%s'>%s</span>",
                            c("info", "warning", "danger"),
                            c("Slow", "Fast", "Non-Velocity")
                            ))
      )
      ,
      sliderInput(
        inputId = "bins", label = "Number of Aisles per Bin Type",
        min = 1, value = 2, max = 10
      ),
      sliderInput(
        inputId = 'percentile', label = "Number of Top Aisles",
        min = 1, value = 30, max = 115
      ),
      h6('Cutoff Space'),
      verbatimTextOutput("dummy"),
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
                           fluidRow(
                             column(9,
                                    h4("Recommended Aisles:"),
                                    textOutput("dummy2"),
                                    tags$head(
                                      tags$style(
                                        "#dummy2{font-size: 12px; font-style: italic;}"
                                      )
                                    )
                             ),
                             column(3,
                                    pickerInput(
                                      "displayMode", label = "Datatable Display Mode:",
                                      choices = c("Basic", "Advanced"),
                                      selected = "Basic"
                                    )
                             ),
                           ),
                           hr(),
                           fluidRow(
                             column(
                               9,
                               dataTableOutput("dataset")
                             ),
                             column(
                               3,
                               h5("Aisles for Printmon"),
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
                                                               choices = list("Top 25%" = 0.75, "Lowest 25%" = 0.25), 
                                                               selected = 0.25),
                                                 ),
                                                 column(
                                                   9,
                                                   plotOutput("stowMap")
                                                 )
                                               ),
                                               style = "default"
                                               ),
                               bsCollapsePanel("Performance Metrics",
                                               fluidRow(
                                                 column(3,
                                                        selectizeInput(
                                                          "timeCompare", label = "Compare With:", choices =  NULL
                                                        )
                                                 )
                                               ),
                                               fluidRow(
                                                   column(
                                                     3,
                                                     h6("Current Apparel Space"),
                                                     verbatimTextOutput("AppSpace"),
                                                     h6("Compared Apparel Space"),
                                                     verbatimTextOutput("LastApp"),
                                                     h6("Apparel Target Utilization"),
                                                     verbatimTextOutput("targetA"),
                                                     h6("Apparel Aisle Similarity"),
                                                     verbatimTextOutput("AppSim")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Current Library Space"),
                                                     verbatimTextOutput("LibSpace"),
                                                     h6("Compared Library Space"),
                                                     verbatimTextOutput("LastLib"),
                                                     h6("Library Target Utilization"),
                                                     verbatimTextOutput("targetL"),
                                                     h6("Library Aisle Similarity"),
                                                     verbatimTextOutput("LibSim")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Current Deep Library Space"),
                                                     verbatimTextOutput("dlSpace"),
                                                     h6("Compared Deep Library Space"),
                                                     verbatimTextOutput("LastDL"),
                                                     h6("Deep Library Target Utilization"),
                                                     verbatimTextOutput("targetDL"),
                                                     h6("Deep Library Aisle Similarity"),
                                                     verbatimTextOutput("DLSim")
                                                   ),
                                                   column(
                                                     3,
                                                     h6("Current Shoe Bin Space"),
                                                     verbatimTextOutput("sbSpace"),
                                                     h6("Compared Shoe Bin Space"),
                                                     verbatimTextOutput("LastShoe"),
                                                     h6("Shoe Bin Target Utilization"),
                                                     verbatimTextOutput("targetShoe"),
                                                     h6("Shoe Bin Aisle Similarity"),
                                                     verbatimTextOutput("SBSim")
                                                   )
                                               ),
                                               style = "default"
                                               )
                    ),
                    textOutput("dummy3"),
                    tags$head(
                      tags$style(
                        "#dummy3{font-size: 12px; font-style: italic;}"
                      )
                    ),
                    textOutput("dummy4"),
                    tags$head(
                      tags$style(
                        "#dummy4{font-size: 12px; font-style: italic;}"
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
  
  ## Selecting current database
  fileName <- reactive({
    validate(
      need(input$time != "", "Please upload a new dataset or select date and time to get started.")
    )
    dates %>% filter(Date == input$date) %>% filter(timeRound == input$time) %>% 
      arrange(desc(timeRound)) %>% head(1) %>% first(1)
  })
  # Download Curent file from S3 Container
  sDatabase <- reactive(
    s3read_using(FUN = read.csv, bucket = 'stowmaps', object = fileName())
  )
  
  # Database Wrangling
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
      ) %>% mutate(Space = 100 - Utilization..) %>% mutate(Bay = as.numeric(substr(Bay.Id,14,16))) %>%
      mutate(
        Velocity = ifelse(Size %in% c('L', 'DL'), 
                         ifelse(Shelf %in% c('A', 'B','F', 'G', 'H', 'I'), 'S', 'F'),
                         'NV')
      )
  )
  # Grouping Data
  wDatabase <- reactive(
    rawDatabase() %>% group_by(Mod, Dz, Size, Velocity, Aisle) %>%
      summarise(Aisle.Space = mean(Space)) %>% ungroup() %>% 
      mutate(Location = paste0(Mod, "-", Aisle, Dz, "-", Size,"-",Velocity)) %>% distinct()
  )
  # Slicing data as per user inputs
  modifier <- reactive(
    wDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size, Velocity %in% input$Velo) %>%
      group_by(Size) %>% arrange(Size, desc(Aisle.Space)) 
  )
  # Printmon slicers
  slices <- reactive(
    modifier() %>% slice(1:input$percentile) %>% sample_n(input$bins)
  )
  
  # fileName <- reactive({

  # })
  # 
  ## Selecting Last Database
  lastFile <- reactive({
    validate(
      need(input$timeCompare != "", "Please select compared file to get started.")
    )
    dates %>% filter(Date == input$date) %>% filter(timeRound == input$timeCompare) %>%
      head(1) %>% first(1)
  })
  ## Downloading Last Database from S3
  lDatabase <- reactive(
    s3read_using(FUN = read.csv, bucket = 'stowmaps', object = lastFile())
  )
  ## Data Wrangling for last database
  lstDatabase <- reactive(
    lDatabase() %>% filter(Bin.Type %in% c("DRAWER", "LIBRARY-DEEP",
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
      ) %>% mutate(Space = 100 - Utilization..) %>% mutate(Bay = as.numeric(substr(Bay.Id,14,16))) %>%
      mutate(
        Velocity = ifelse(Size %in% c('L', 'DL'), 
                          ifelse(Shelf %in% c('A', 'B','F', 'G', 'H', 'I'), 'S', 'F'),
                          'NV')
      ) %>% group_by(Mod, Dz, Size, Velocity, Aisle) %>%
      summarise(Aisle.Space = mean(Space)) %>% ungroup() %>% 
      mutate(Location = paste0(Mod, "-", Aisle, Dz, "-", Size,"-",Velocity)) %>% distinct()
  )
  ## Current Overview dataset
  CurrOverview <- reactive(
    wDatabase() %>% group_by(Size) %>% arrange(desc(Aisle.Space)) %>% slice(1:(input$percentile*4)) %>% 
      summarise(Size.Space = mean(Aisle.Space)) %>% ungroup()
  )
  ## Last overview dataset
  LstOverview <- reactive(
    lstDatabase() %>% group_by(Size) %>% arrange(desc(Aisle.Space)) %>% slice(1:(input$percentile*4)) %>% 
      summarise(Size.Space = mean(Aisle.Space)) %>% ungroup()
  )
  ## Data table viewer element
  displayTable <- reactive(
    if(input$displayMode == "Basic"){
      slices() %>% select(Velocity, Location, Aisle.Space)
    }
    else {
      wDatabase()
    }
  )
  # Stow maps element
  mapPlot <- reactive(
    if(input$selectslice == 2){
      if(input$filteraisles == 0.75){
        rawDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size) %>%
          group_by(Size, Aisle, Bay) %>% summarise(Cat.Space = mean(Space)) %>%
          mutate(Aisle = -1 * Aisle, Bay = -1 * Bay) %>% ungroup() %>% select(Aisle, Bay, Cat.Space) %>%
          filter(Cat.Space > quantile(modifier()$Aisle.Space, 0.75), Aisle < -410) %>%
          `colnames<-`(c("Aisle", "Slicer", "Available.Space"))
      }
      else {
        rawDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size) %>%
          group_by(Size, Aisle, Bay) %>% summarise(Cat.Space = mean(Space)) %>%
          mutate(Aisle = -1 * Aisle, Bay = -1 * Bay) %>% ungroup() %>% select(Aisle, Bay, Cat.Space) %>%
          filter(Cat.Space <quantile(modifier()$Aisle.Space, 0.25), Aisle < -410) %>%
          `colnames<-`(c("Aisle", "Slicer", "Available.Space"))
      }
    }
    else {
      if(input$filteraisles == 0.75){
        rawDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size) %>%
          mutate(Dz = case_when(Dz == 'L' ~ -1, Dz == 'M' ~ -2, Dz == 'H' ~ -3)) %>%
          group_by(Size, Dz, Aisle) %>% summarise(Cat.Space = mean(Space)) %>%
          mutate(Aisle = -1 * Aisle) %>% ungroup() %>% select(Aisle, Dz, Cat.Space) %>%
          filter(Cat.Space >quantile(modifier()$Aisle.Space, 0.75), Aisle < -410) %>% 
          `colnames<-`(c("Aisle", "Slicer", "Available.Space"))
      }
      else {
        rawDatabase() %>% filter(Mod %in% input$Mod, Size %in% input$Size) %>%
          mutate(Dz = case_when(Dz == 'L' ~ -1, Dz == 'M' ~ -2, Dz == 'H' ~ -3)) %>%
          group_by(Size, Dz, Aisle) %>% summarise(Cat.Space = mean(Space)) %>%
          mutate(Aisle = -1 * Aisle) %>% ungroup() %>% select(Aisle, Dz, Cat.Space) %>%
          filter(Cat.Space <quantile(modifier()$Aisle.Space, 0.25), Aisle < -410) %>% 
          `colnames<-`(c("Aisle", "Slicer", "Available.Space"))
      }
    }
  )
  # Performance Metrics Metrics
  Curr_A <- reactive(
    CurrOverview()  %>% filter(Size == 'A') %>% select(Size.Space) %>% round(digits = 2)
  )
  Curr_L <- reactive(
    CurrOverview() %>% filter(Size == 'L') %>% select(Size.Space) %>% round(digits = 2)
  )
  Curr_Dl <- reactive(
    CurrOverview()  %>% filter(Size == 'DL') %>% select(Size.Space) %>% round(digits = 2)
  )
  Curr_Shoes <- reactive(
    CurrOverview() %>% filter(Size == 'S') %>% select(Size.Space) %>% round(digits = 2)
  )
  Lst_A <- reactive(
    LstOverview() %>% filter(Size == 'A') %>% select(Size.Space) %>% round(digits = 2)
  )
  Lst_L <- reactive(
    LstOverview() %>% filter(Size == 'L') %>% select(Size.Space) %>% round(digits = 2)
  )
  Lst_DL <- reactive(
    LstOverview() %>% filter(Size == 'DL') %>% select(Size.Space) %>% round(digits = 2) 
  )
  Lst_Shoe <- reactive(
    LstOverview() %>% filter(Size == 'S') %>% select(Size.Space) %>% round(digits = 2)
  )
  
  comparisionScore_A <- reactive({
    simLst <- lstDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'A') %>% 
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    simCurr <- wDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'A') %>%
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    sim <- ifelse(simLst == simCurr, 1, 0)
    score <- round(sum(sim)/length(sim)*100,2)
    return(score)
  })
  
  comparisionScore_L <- reactive({
    simLst <- lstDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'L') %>% 
        slice(1:(input$percentile)) %>% select(Location) %>% unique()
    simCurr <- wDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'L') %>%
        slice(1:(input$percentile)) %>% select(Location) %>% unique()
    sim <- ifelse(simLst == simCurr, 1, 0)
    score <- round(sum(sim)/length(sim)*100,2)
    return(score)
  })
  
  comparisionScore_DL <- reactive({
    simLst <- lstDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'DL') %>% 
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    simCurr <- wDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'DL') %>%
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    sim <- ifelse(simLst == simCurr, 1, 0)
    score <- round(sum(sim)/length(sim)*100,2)
    return(score)
  })
  
  comparisionScore_S <- reactive({
    simLst <- lstDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'S') %>% 
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    simCurr <- wDatabase() %>% arrange(desc(Aisle.Space)) %>% filter(Size == 'S') %>%
      slice(1:(input$percentile)) %>% select(Location) %>% unique()
    sim <- ifelse(simLst == simCurr, 1, 0)
    score <- round(sum(sim)/length(sim)*100,2)
    return(score)
  })
  
  output$AppSim <- renderText(
    paste(comparisionScore_A(), " %")
  )
  
  output$LibSim <- renderText(
    paste(comparisionScore_L(), " %")
  )
  
  output$DLSim <- renderText(
    paste(comparisionScore_DL(), " %")
  )
  
  output$SBSim <- renderText(
    paste(comparisionScore_S(), " %")
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
                           selected = unlist(timeSelector()$timeRound)[-1],
                           server = TRUE)
    }
  )
  
  # Upload File
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
  # Last Updated
  output$dummy2 <- renderText({
    paste(c("Last updated at: ",fileName()), collapse = " ")
  })
  output$dummy3 <- renderText({
    paste(c("Current updated at: ",fileName()), collapse = " ")
  })
  output$dummy4 <- renderText({
    paste(c("Compared at: ",lastFile()), collapse = " ")
  })
  # Table
  output$dataset <- renderDataTable(
    datatable(displayTable(), options = list(pageLength = 8))
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
  
  #Cutoff Space
  
  ctf <- reactive(
    modifier() %>% group_by(Size) %>% arrange(Size, desc(Aisle.Space)) %>% slice(1:input$percentile) %>%
      ungroup()
  )
  
  output$dummy <- renderText({
    mean(ctf()$Aisle.Space)
  })
  
  ## Overview
  # Available Space
  output$WarehouseAVG <- renderText(
    {
      paste(c(
        round(mean(modifier()$Aisle.Space),1), "%"),
        collapse = "")
    }
  )
  # Standard Deviation
  output$stdev <- renderText(
    paste(c(
      round(sd(modifier()$Aisle.Space),1),"%"),collapse = "")
  )
  # Summary
  output$sum <- renderPrint(
    {
      round(summary(modifier()$Aisle.Space),1)
    }
  )
  # Available Aisles
  output$availAiles <- renderText({
    count(modifier() %>% filter(Aisle.Space >quantile(modifier()$Aisle.Space, 0.75)) %>% 
            ungroup() %>% select(Aisle.Space))[[1]] / 4
  })
  # Target Aisles
  output$TargetAVG <- renderText("32.7%")
  # Histogram
  output$hist <- renderPlot(
    {
      hist(
        modifier()$Aisle.Space, main = "Distribution of Available Space",
        xlab = "Available Space", ylab = "Number of Aisles", 
        breaks = seq(-250,100,5)
      )
    }
  )
  
  ## Analytics
  # Stow Map
  output$stowMap <- renderPlot(
    mapPlot() %>% ggplot(.,aes(x=Slicer, y=Aisle, text = paste("Country:", Slicer))) + geom_point(
      color="purple",
      fill="#69b3a2",
      shape=22,
      alpha=0.5,
      size=2,
      stroke = 2
    ) + ylim(-580, -410)
  )
  
  ## Performance Metrics
  # Apparel Space
  output$AppSpace <- renderText(
    Curr_A() %>% toString() %>% paste("%")
  )

  output$LibSpace <- renderText(
    Curr_L() %>% toString() %>% paste("%")
  )
    
  output$dlSpace <- renderText(
    Curr_Dl() %>% toString() %>% paste("%")
  )
  
  output$sbSpace <- renderText(
    Curr_Shoes() %>% toString() %>% paste("%")
  )
  
  output$LastApp <- renderText(
    Lst_A() %>% toString() %>% paste("%")
  )
  output$LastLib <- renderText(
    Lst_L() %>% toString() %>% paste("%")
  )
  output$LastDL <- renderText(
    Lst_DL() %>% toString() %>% paste("%")
  )
  output$LastShoe <- renderText(
    Lst_Shoe() %>% toString() %>% paste("%")
  )
  output$targetA <- renderText(
    paste0(
      round(
        (Lst_A() - Curr_A())/Lst_A()*100, 2
      ),
      " %"
    )
  )
  
  output$targetL <- renderText(
    paste0(
      round(
        (Lst_L() - Curr_L())/Lst_L()*100, 2
      ),
      " %"
    )
  )
  
  output$targetDL <- renderText(
    paste0(
      round(
        (Lst_DL() - Curr_Dl())/Lst_DL()*100, 2
      ),
      " %"
    )
  )
  
  output$targetShoe <- renderText(
    paste0(
      round(
        (Lst_Shoe() - Curr_Shoes())/Lst_Shoe()*100, 2
      ),
      " %"
    )
  )
  
  ## Time Compare
  observeEvent(
    input$time, {
      timeC <- reactive(
        dates %>% filter(Date == input$date) %>% filter(!timeRound == input$time)
      )
      updateSelectizeInput(session, 
                           'timeCompare', 
                           choices = timeC()$timeRound,
                           selected = unlist(timeC()$timeRound)[-1],
                           server = TRUE)
    }
  )
  
  }

shinyApp(ui = ui, server = server)


