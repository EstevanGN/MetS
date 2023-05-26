# Libraries --------------------------------------------------------------------
library(shiny)      # basic for shiny apps
library(DT)         # Output tables
library(readr)      # Read big size files
library(tidyverse)  # %>% function
# / ----------------------------------------------------------------------------



# Shiny options ----------------------------------------------------------------
options(shiny.maxRequestSize=3000*1024^2)
# / ----------------------------------------------------------------------------



# App functions ----------------------------------------------------------------

## Possible RTs from a specific mzValue ----------------------------------------
possibleRT <- function(dataSamples, mz, interval){
  mzTarget <- dataSamples %>%
    filter(`m/z (positive ionization)` >= mz-interval, 
           `m/z (positive ionization)` <= mz+interval )
  return(data.frame(mzTarget)[,"RT..min."])
}
## / ---------------------------------------------------------------------------

## With a specific RT searching all mz near (with correlation) -----------------
mzNear <- function(dataSamples, rt, interval, mz, cor){
  rtValue <- rt
  rtTarget <- dataSamples %>%
    filter(`RT (min)` >= rtValue-interval, 
           `RT (min)` <= rtValue+interval )
  MatrixRT <- as.matrix(t(rtTarget[,5:dim(dataSamples)[2]]))
  
  id_mzValue <- which.min(
    abs(rtTarget$`m/z (positive ionization)` - mz)
  )
  
  idCorrelation <- cor(MatrixRT, method="pearson")[id_mzValue,] > cor
  
  result <- cbind(
    data.frame(
      rtTarget[idCorrelation,]
    )[,c(1,3,4)],
    "Corr"=cor(MatrixRT, method="pearson")[id_mzValue,][idCorrelation]
  )
  
  return(result)
}
## / ---------------------------------------------------------------------------

## With a set of RTs, searching for the closer to mz fragments -----------------
closerFragmentation <- 
  function(dataSamples, rtSet, interval, mz, cor, fragment){
    distanceToFragments <- NULL
    distance <- NULL
    distanceTable <- matrix(0, length(rtSet), 3)
    
    for(i in 1:length(rtSet)){
      
      result <- mzNear(dataSamples, rtSet[i], interval, mz, cor)
      for(j in 1:length(fragment)){
        for(k in 1:dim(result)[1]){
          distance[k] <- 
            abs(result$m.z..positive.ionization.[k]-fragment[j])
        }
        distanceToFragments[j] <- min(distance)
        distance <- NULL
      }
      totalSum <- sum(distanceToFragments)
      
      distanceTable[i,] <- c(i, rtSet[i], totalSum)
    }
    
    return(
      data.frame("RT_id"=distanceTable[which.min(distanceTable[,3]),1],
                 "RT"=distanceTable[which.min(distanceTable[,3]),2],
                 "distance"=distanceTable[which.min(distanceTable[,3]),3])
    )
  }
## / ---------------------------------------------------------------------------

## print all possible tables with Rt near --------------------------------------
printResults <- function(dataSamples, rtSet, interval, mz, cor){
  
  for(i in 1:length(rtSet)){
    
    result <- mzNear(dataSamples, rtSet[i], interval, mz, cor)
    if(dim(result)[1]>1){
      cat("rt_id:", i, " rtValue:", rtSet[i], "\n")
      print(result)
      cat("\n\n")
    }
    
  }
  
}
## / ---------------------------------------------------------------------------




# User interface ---------------------------------------------------------------
ui <- navbarPage("Metabolomic Search",
                 
  ## Upload data section -------------------------------------------------------
  tabPanel("Upload Data",
    sidebarLayout(
      sidebarPanel(
        
        h2("Metabolomic Data"),
        p("Choose your metabolomic data from a file with 
          CSV format (only). Press ", 
          strong("'Browse...'"), 
          " button to upload data. Soon more formats."),
        
        
        ### Browse data button -------------------------------------------------
        fileInput("file1", 
          h4("Choose CSV File"),
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        ### / ------------------------------------------------------------------
        
        
        fluidRow(
          ### Header option ----------------------------------------------------
          column(5,
            h4("Header"),
            checkboxInput(
              "header", 
              "It has header", 
              TRUE
            )
          ),
          ### / ----------------------------------------------------------------
          
          
          ### Row start option -------------------------------------------------
          column(5,
            numericInput(
              "rowStart", 
              h4("From row:"), 
              value = 1,
              min = 1)
          )
          ### / -------------------------------------------------
        ),
        
        
        fluidRow(
          ### Quote option -----------------------------------------------------
          column(5,
            radioButtons("quote", 
              h4("Quote"),
              c(None="",
                "Double Quote"='"',
                "Single Quote"="'"),
              '"'),
          ),
          ### / ----------------------------------------------------------------
                 
                 
          ### Separator option -------------------------------------------------
          column(5,
            radioButtons("sep", 
              h4("Separator"),
              c(Comma=",",
                Semicolon=";",
                Tab="\t"),
              ","),
          )
          ### / ----------------------------------------------------------------
        ),
        
        
        br(),
        br(),
        
        ### UNAL Logo ----------------------------------------------------------
        img(src = "logo_unal.png", height = 80, width = 200)
        ### / ------------------------------------------------------------------
      ),
      
      
      ### Present data uploaded ------------------------------------------------
      mainPanel(
        h1("Data Uploaded"),
        
        br(),
        br(),
        
        DTOutput(
          "dataSamples"
        )
      )
      ### / --------------------------------------------------------------------
    )
  ),
  
  
  ## Search for molecules section ----------------------------------------------
  navbarMenu("Search",
    
    ### Only with m/z value ----------------------------------------------------
    tabPanel("Only with m/z value",
      sidebarLayout(
        sidebarPanel(
          h1("Only with m/z value"),
          h2("Parameters"),
          p("Choose the values of every parameter to obtain 
            a metabolomic search. Maximum precision allowed 10^-5."),
                          
          
          fluidRow(
            #### M/z value option ----------------------------------------------
            column(5,
              numericInput(
                "mzValue",
                h4("Mz Value:"),
                min = NA,
                max = NA,
                step = 0.00001,
                value = 304.15
              ),
            ),
            #### / -------------------------------------------------------------
                            
                            
            #### M/z interval option -------------------------------------------
            column(5,
              numericInput(
                "mzInterval",
                h4("Mz Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
                          
          fluidRow(
            p("", em("Mz Interval"),": is the maximum distance allowed to 
              mz value for create an interval of search. I.e.", 
              strong("[mzValue-mzInterval, mzValue+mzInterval]"), " is 
              the range of search for near values of targed mz.")
          ),
          
          
          fluidRow(
            #### Fragments option ----------------------------------------------
            textInput(
              "fragments", 
              h4("Fragments"), 
              value = "138,09; 156,10"
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            p("Write down every possible fragment separated by ';'")
          ),
          
          
          fluidRow(
            #### Correlation option --------------------------------------------
            column(5,
              numericInput(
                "correlationLevel",
                h4("Correlation"),
                min = 0,
                max = 1,
                step = 0.00001,
                value = 0.95
              ),
            ),
            #### / -------------------------------------------------------------
            
            
            #### RT Interval option --------------------------------------------
            column(5,
              numericInput(
                "rtInterval",
                h4("Rt Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            p("", em("Rt Interval"),": is the maximum distance allowed to 
              Rt value for create an interval of search.")
          ),
          
          
          fluidRow(
            #### Search button -------------------------------------------------
            actionButton(
              "search", 
              "Search", 
              icon("search"), 
              style="color: #DEEDCF; 
                    background-color: #56B870; 
                    border-color: #0A2F51"
            )
            #### / -------------------------------------------------------------
          )
        ),
        
        
        mainPanel(
          #### Best result table -----------------------------------------------
          h1("Best result:"),
          DTOutput(
            "bestResult"
          ),
          #### / ---------------------------------------------------------------
          
          
          #### Printing other results ------------------------------------------
          h2("Other results:"),
          verbatimTextOutput("summary")
          #### / ---------------------------------------------------------------
        )
      )
    ),
    
    
    ### With m/z and RT values -------------------------------------------------
    tabPanel("With m/z and RT values",
      sidebarLayout(
        sidebarPanel(
          h1("With m/z and RT values"),
          h2("Parameters"),
          p("Choose the values of every parameter to obtain 
            a metabolomic search. Maximum precision allowed 10^-5."),
          
          
          fluidRow(
            #### M/z value option ----------------------------------------------
            column(5,
              numericInput(
                "mzValue_MZRT",
                h4("Mz Value:"),
                min = NA,
                max = NA,
                step = 0.00001,
                value = 304.15
              ),
            ),
            #### / -------------------------------------------------------------
            
            
            #### Correlation option -------------------------------------------
            column(5,
              numericInput(
                "correlationLevel_MZRT",
                h4("Correlation"),
                min = 0,
                max = 1,
                step = 0.00001,
                value = 0.95
              ),
            ),
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            #### RT value option -----------------------------------------------
            column(5,
              numericInput(
                "rtValue_MZRT",
                h4("Rt Value:"),
                min = 0,
                step = 0.00001,
                value = 4.97
              ),
            ),
            #### / -------------------------------------------------------------
            
            
            #### RT Interval option --------------------------------------------
            column(5,
              numericInput(
                "rtInterval_MZRT",
                h4("Rt Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            #### Second search button -------------------------------------------------
            actionButton(
              "search_MZRT", 
              "Search", 
              icon("search"), 
              style="color: #DEEDCF; 
                    background-color: #56B870; 
                    border-color: #0A2F51"
            )
            #### / -------------------------------------------------------------
          )
          
        ),
        
        
        mainPanel(
          #### Best result table -----------------------------------------------
          h1("Best result:"),
          DTOutput(
            "bestResult_MZRT"
          ),
          #### / ---------------------------------------------------------------
        )
      )
    )
    
    
  ),
  
  
  ## More info section ---------------------------------------------------------
  tabPanel("Information",
    h1("Authors"),
    p("",
      strong("Daniel Estevan Garcia Niño"), br(),
      "dgarcian@unal.edu.co", br(),
      "Statistician.", br(),
      "Student, Computer Science.", br(),
      "Universidad Nacional De Colombia.", br(),
      "Sede Bogotá."),
    
    br(),
           
    p("",
      strong("Federico Roda, PhD"), br(),
      "Max Planck Tandem Group Leader", br(),
      "Universidad Nacional de Colombia, Sede Bogotá", br(),
      "e-mail: frodaf@unal.edu.co"),
           
    br(),
           
    p("",
      strong("Liliana López Kleine"), br(),
      "Profesora titular", br(),
      "llopezk@unal.edu.co", br(),
      "Departamento de estadística", br(),
      "Facultad de Ciencias", br(),
      "Universidad Nacional de Colombia - sede Bogotá"),
           
    h1("Help"),
    p("For more information:", 
      a(href="https://github.com/EstevanGN/Metabolomic-search",
        "GitHub repository"))
  )
  ## / -------------------------------------------------------------------------

)




# Server (functions) -----------------------------------------------------------
server <- shinyServer(function(input, output) {
  
  ## Read sample data ------------------------------------------------------------------
  data <- reactive({
    req(input$file1,
        input$header,
        input$sep,
        input$quote,
        input$rowStart)
    inFile <- input$file1
    df <- read_delim(inFile$datapath, 
                     col_names = input$header, 
                     delim = input$sep,
                     quote = input$quote,
                     skip = input$rowStart-1)
    return(df)
  })
  ## / -------------------------------------------------------------------------
  
  
  ## Print sample data ---------------------------------------------------------
  output$dataSamples <- renderDT(
    data()
  )
  ## / -------------------------------------------------------------------------
  

  ## First button search -------------------------------------------------------
  # initialization of search parameters:
  searchMol <- reactiveValues(
    frag = NULL,
    RT = NULL,
    rtNear = NULL
  )
  
  # action:
  observeEvent(input$search, {
    # Obtain intervals:
    searchMol$frag <- as.numeric(gsub(",", ".", 
                                      strsplit(input$fragments, "; ")[[1]]
                                      ))
    # Obtain possible RTs:
    searchMol$RT <- 
      possibleRT(data(), input$mzValue, input$mzInterval)
    
    # Obtain best RT:
    searchMol$rtNear <-
      closerFragmentation(data(), searchMol$RT, input$rtInterval, 
                          input$mzValue, input$correlationLevel, 
                          searchMol$frag)
    
    
    ### Print Best table -------------------------------------------------------
    output$bestResult  <- DT::renderDataTable(
      DT::datatable(
        { mzNear(
          data(),
          searchMol$rtNear$RT, 
          input$rtInterval, 
          input$mzValue, 
          input$correlationLevel
        ) },
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       lengthMenu = list(c(10,25,50,-1),
                                         c(10,25,50,"All"))),
        
        class = "display"
      )
    )
    ### / ----------------------------------------------------------------------
    
    
    ### Print possible tables --------------------------------------------------
    output$summary <- renderPrint({
      printResults(data(), searchMol$RT, input$rtInterval, 
                   input$mzValue, input$correlationLevel)
    })
    ### / ----------------------------------------------------------------------
    
  })
  
  
  ## Second button search ------------------------------------------------------
  
  # action:
  observeEvent(input$search_MZRT, {
    
    ### Print Best table -------------------------------------------------------
    output$bestResult_MZRT  <- DT::renderDataTable(
      DT::datatable(
        { mzNear(
          data(),
          input$rtValue_MZRT, 
          input$rtInterval_MZRT, 
          input$mzValue_MZRT, 
          input$correlationLevel_MZRT
        ) },
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       lengthMenu = list(c(10,25,50,-1),
                                         c(10,25,50,"All"))),
        
        class = "display"
      )
    )
    ### / ----------------------------------------------------------------------
    
  })
  
  
})




# Execute app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)