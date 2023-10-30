# Libraries --------------------------------------------------------------------
library(shiny)          # basic for shiny apps
library(DT)             # Output tables
library(readr)          # Read big size files
library(tidyverse)      # %>% function
library(shinyvalidate)  # Input validation 
library(shinybusy)      # App indicators
library(MSbox)          # Calculate mz
# / ----------------------------------------------------------------------------



# Shiny options ----------------------------------------------------------------
options(shiny.maxRequestSize=3000*1024^2)
# / ----------------------------------------------------------------------------



# App functions ----------------------------------------------------------------

## Possible RTs from a specific mzValue ----------------------------------------
possibleRT <- function(dataSamples, mzColumn, rtColumn, mz, interval){
  mzTarget <- dataSamples %>%
    filter(dataSamples[,mzColumn] >= mz-interval, 
           dataSamples[,mzColumn] <= mz+interval)
  return(data.frame(mzTarget)[,rtColumn])
}
## / ---------------------------------------------------------------------------

## With a specific RT searching all mz near (with correlation) -----------------
mzNear <- function(
    dataSamples, 
    rowColumn, 
    rt, 
    rtColumn, 
    interval, 
    mz, 
    mzColumn, 
    cor, 
    sampColumn
){
  
  samColumns <- as.numeric(
    strsplit(sampColumn, "-")[[1]]
  )
  
  rtTarget <- dataSamples %>%
    filter(dataSamples[,rtColumn] >= rt-interval, 
           dataSamples[,rtColumn] <= rt+interval )
  
  indColumns <- samColumns[1]:samColumns[2]
  matrixRT <- as.matrix(t(rtTarget[,indColumns]))
  
  idMzValue <- which.min(
    abs(rtTarget[,mzColumn] - mz)
  )
  
  idCorrelation <- cor(matrixRT, method="pearson")[idMzValue,] > cor
  columnsNames <- names(dataSamples)[indColumns]
  sampMaxIntensity <- NULL
  
  focusTable <- rtTarget[idCorrelation,]
  
  for(i in 1:nrow(focusTable)){
    sampMaxIntensity[i] <- paste0(
      columnsNames[which.max(focusTable[i,indColumns])],
      " (",
      max(focusTable[i,indColumns]),
      ")"
    )
  }
  
  result <- cbind(
    data.frame(
      focusTable
    )[,c(rowColumn, mzColumn, rtColumn)],
    "Corr"=cor(matrixRT, method="pearson")[idMzValue,][idCorrelation],
    "Samp.max.int"=sampMaxIntensity
  )
  
  return(result)
}
## / ---------------------------------------------------------------------------

## With a table from mzNear calculate distance to fragments --------------------
fragDistance <- function(result, fragment){
  distanceToFragments <- NULL
  distance <- NULL
  
  for(j in 1:length(fragment)){
    for(k in 1:dim(result)[1]){
      distance[k] <- 
        abs(result[k,2]-fragment[j])
    }
    distanceToFragments[j] <- min(distance)
    distance <- NULL
  }
  return( sum(distanceToFragments) )
}
## / ---------------------------------------------------------------------------

## With a set of RTs, searching for the closer to mz fragments -----------------
closerFragmentation <- function(
    dataSamples,
    rowColumn,
    rtSet,
    rtColumn,
    interval, 
    mz, 
    mzColumn,
    cor, 
    sampColumn,
    fragment
){
  distanceTable <- matrix(0, length(rtSet), 3)
  
  if("" %in% fragment){
    # If there are no fragments
    
    for(i in 1:length(rtSet)){
      result <- mzNear(
        dataSamples,
        rowColumn,
        rtSet[i],
        rtColumn,
        interval, 
        mz,
        mzColumn,
        cor,
        sampColumn
      )
      
      totalSum <- min(abs(result[,2] - mz))
      distanceTable[i,] <- c(i, rtSet[i], totalSum)
    }
    
  }else{
    # If there are fragments
    
    for(i in 1:length(rtSet)){
      result <- mzNear(
        dataSamples,
        rowColumn,
        rtSet[i],
        rtColumn,
        interval, 
        mz,
        mzColumn,
        cor,
        sampColumn
      )
      
      totalSum <- fragDistance(result, fragment)
      
      distanceTable[i,] <- c(i, rtSet[i], totalSum)
    }
  }
  
  return(
    data.frame("RT_id"=distanceTable[which.min(distanceTable[,3]),1],
               "RT"=distanceTable[which.min(distanceTable[,3]),2],
               "distance"=distanceTable[which.min(distanceTable[,3]),3])
  )
}
## / ---------------------------------------------------------------------------

## Print all possible tables with Rt near --------------------------------------
printResults <- function(
    dataSamples,
    rowColumn,
    rtSet,
    rtColumn,
    interval, 
    mz, 
    mzColumn,
    cor,
    sampColumn,
    fragment
){
  
  completeResults <- data.frame(
    "Group_id" = character(0),
    "Score" = numeric(0),
    "Parental_RT" = numeric(0),
    "Row_names" = character(0),
    "mz" = numeric(0),
    "RT" = numeric(0),
    "Correlation" = numeric(0)
  )
  
  for(i in 1:length(rtSet)){
    partialResult <- mzNear(dataSamples, rowColumn, rtSet[i], rtColumn, 
                            interval, mz, mzColumn, cor, sampColumn)
    group <- rep(i, nrow(partialResult))
    if("" %in% fragment){
      sco <- rep( 
        min(abs(partialResult[,2] - mz)), 
        nrow(partialResult)
      )
    }else{
      sco <- rep( 
        fragDistance(partialResult, fragment), 
        nrow(partialResult)
      )
    }
    parental <- rep(rtSet[i], nrow(partialResult))
    
    completeResults <- rbind(
      completeResults,
      cbind(group, sco, parental, partialResult)
    )
  }
  
  return(completeResults)
}
## / ---------------------------------------------------------------------------




# User interface ---------------------------------------------------------------
ui <- navbarPage("Metabolomic Search",
  
  ## Upload data section -------------------------------------------------------
  navbarMenu("Upload Data",
             
    ### Upload main data section -----------------------------------------------
    tabPanel("Metabolomic Data",
      sidebarLayout(
        sidebarPanel(
                          
          h1("Metabolomic Data"),
          
          br(),
          
          h2("General configuration "),
          
          p("Choose your metabolomic data from a file with 
          CSV format (only). Press ", 
          strong("'Browse...'"), 
          " button to upload data."),
                          
          #### Browse data button ----------------------------------------------
          fileInput("mainFile", 
            h4("Choose CSV File"),
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #### / ---------------------------------------------------------------
                          
          fluidRow(
            #### Header option -------------------------------------------------
            column(5,
              h4("Header:"),
              checkboxInput(
                "header", 
                "It has header", 
                TRUE
              )
            ),
            #### / -------------------------------------------------------------
            
            #### Decimal option ------------------------------------------------
            column(
              5,
              radioButtons(
                "dec", 
                h4("Decimal style:"),
                c(Point=".",
                  Comma=","),
                "."
              ),
            )
            #### / -------------------------------------------------------------
          ),
                          
          fluidRow(
            #### Quote option --------------------------------------------------
            column(5,
              radioButtons("quote", 
                h4("Quote:"),
                c("Double Quote"="\"",
                  "None"="",
                  "Single Quote"="'"),
                "\""
              ),
            ),
            #### / -------------------------------------------------------------
            
            #### Separator option ----------------------------------------------
            column(
              5,
              radioButtons(
                "sep", 
                h4("Separator character:"),
                c("Semicolon"=";",
                  "Space"=" ",
                  "Comma"=",",
                  "Tab"="\t",
                  "Vertical Line |"="|"),
                ";"
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
          fluidRow(
            #### Row start option ----------------------------------------------
            tags$div(
              id = "inline",
              numericInput(
                "rowStart", 
                h4("Row to start:"), 
                value = 1,
                min = 1
              )
            )
            #### / -------------------------------------------------------------
          ),
          
          h2("Columns configuration"),
          p("Edit the following parameters to configure the columns of 
            your data frame. For each title give a column number"),
          
          fluidRow(
            #### Columns configuration -----------------------------------------
            tags$head(
              tags$style(
                type="text/css", 
                "#inline label{ display: table-cell; 
                text-align: center; vertical-align: middle; } 
                #inline .form-group { display: table-row;}")
            ),
            
            tags$div(
              id = "inline", 
              numericInput(
                "rowNamesColumn", 
                "Row names:",
                value = 1,
                min = 1,
                step = 1
              ),
              numericInput(
                "mzValueColumn", 
                "m/z value:",
                value = 2,
                min = 1,
                step = 1
              ),
              numericInput(
                "rtValueColumn", 
                "RT value:",
                value = 3,
                min = 1,
                step = 1
              ),
              textInput(
                "samplesColumn",
                "Samples:",
                value = "4-187"
              )
            )
            #### / -------------------------------------------------------------
          ),
                          
          br(),
          br(),
                          
          #### UNAL Logo -------------------------------------------------------
          img(src = "logo_unal.png", height=80, width=200)
          #### / ---------------------------------------------------------------
        ),
                        
        mainPanel(
          #### Present example of data format ---------------------------------- 
          h3("Example of a data table format"),
          img(src = "Samples data example.png", height="100%", width="100%"),
          
          #### Present data uploaded -------------------------------------------
          h1("Data Uploaded"),
          br(),
          br(),
          DTOutput(
            "dataSamples"
          )
        )
        #### / -----------------------------------------------------------------
      )
    ),
    
    ### Upload compound data ---------------------------------------------------
    tabPanel("Compound Data",
      sidebarLayout(
        sidebarPanel(
                 
          h1("Compound Data"),
          p("Choose your compound data from a file with 
          CSV format (only). Press ", 
          strong("'Browse...'"), 
          " button to upload data."),
                 
          #### Browse compound button ------------------------------------------
          fileInput("compoundDataFile", 
            h4("Choose CSV File:"),
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"
            )
          ),
          #### / ---------------------------------------------------------------
                 
          fluidRow(
            #### Header option -------------------------------------------------
            column(5,
              h4("Header:"),
              checkboxInput(
                "compoundHeader", 
                "It has header", 
                TRUE
              )
            ),
            #### / -------------------------------------------------------------
            
            #### Separator option ----------------------------------------------
            column(5,
              radioButtons("compoundSeparator", 
                h4("Separator character:"),
                c("Semicolon"=";",
                  "Space"=" ",
                  "Comma"=",",
                  "Tab"="\t",
                  "Vertical Line |"="|"),
                ";"
              )
            )
            #### / -------------------------------------------------------------
          ),
                 
          fluidRow(
            #### Quote option --------------------------------------------------
            column(5,
              radioButtons("compoundQuote", 
                h4("Quote:"),
                c(None="",
                  "Double Quote"="\"",
                  "Single Quote"="'"),
                "\""
              ),
            ),
            #### / -------------------------------------------------------------
            
            #### Decimal option ------------------------------------------------
            column(5,
              radioButtons("compoundDec", 
                h4("Decimal style:"),
                c(Comma=",",
                  Point="."),
                ","
              ),
            )
            #### / -------------------------------------------------------------
          ),
                 
          fluidRow(
            #### Row start option ----------------------------------------------
            numericInput("compoundRowStart", 
              h4("Row to start:"), 
              value = 1,
              min = 1
            )
            #### / -------------------------------------------------------------
          ),
          
          br(),
          br(),
          
          #### UNAL Logo -------------------------------------------------------
          img(src = "logo_unal.png", height = 80, width = 200)
          #### / ---------------------------------------------------------------
        ),
               
        #### Present data uploaded --------------------------------------------- 
        mainPanel(
          h1("Data Uploaded"),
          br(),
          br(),
          DTOutput(
            "compoundData"
          )
        )
        #### / -----------------------------------------------------------------
      )
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
                min = 0.00001,
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
              h4("Fragments:"), 
              value = "138,09-156,10"
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            p("Write down every possible fragment separated by '-'. Use ',' 
            as decimal style")
          ),
          
          
          fluidRow(
            #### Correlation option --------------------------------------------
            column(5,
              numericInput(
                "correlationLevel",
                h4("Correlation:"),
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
                h4("RT Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            p("", em("RT Interval"),": is the maximum distance allowed to 
              RT value for create an interval of search.")
          ),
          
          
          fluidRow(
            #### Search button -------------------------------------------------
            actionButton(
              "search", 
              "Search", 
              icon("search"), 
              style="color: #FFFFFF;
                    background-color: #4372AA;
                    border-color: #000000"
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
          
          
          #### Print complete results ------------------------------------------
          h2("Complete results:"),
          DTOutput(
            "summary"
          )
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
                min = 0.00001,
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
                h4("RT Value:"),
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
                h4("RT Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
            #### / -------------------------------------------------------------
          ),
          
          
          fluidRow(
            #### Second search button ------------------------------------------
            actionButton(
              "search_MZRT", 
              "Search", 
              icon("search"), 
              style="color: #FFFFFF; 
                    background-color: #4372AA; 
                    border-color: #000000"
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
  
  
  ## More tools section --------------------------------------------------------
  navbarMenu("More tools",
    
    ### Calculate mz section ---------------------------------------------------
    tabPanel("Calculate mz",
      sidebarLayout(
        sidebarPanel(
          h3("MSbox::mz"),
          p("This tool calculate accurate mass-to-charge ratio (m/z)."),
          h3("Arguments:"),
          tags$div(
            tags$ul(
              tags$li(strong("m:")," chemical formula of an ion, 
                      case insensitive."),
              tags$li(strong("z:")," charge."),
              tags$li(strong("caseSensitive:")," if case sensitive is 'FALSE', 
                      the elements are seperated by numbers. 
                      For instance, Carbon dioxyde can be written as 'c1o2' 
                      or any combination of the two elements in lower or 
                      upper cases. However, the number of elements should 
                      be clearly stated in the chemical formula. If case 
                      sensitive is 'TRUE', the elements are seperated by 
                      upper case letters. For instance, Carbon dioxyde must 
                      be written as 'C1O2' or 'CO2'. You don't need to write 
                      the number of the element if it is 1."),
            )
          ),
          
          fluidRow(
            #### m option ------------------------------------------------------
            textInput(
              "m", 
              h4("m:"), 
              value = "C7H7O"
            )
            #### / -------------------------------------------------------------
          ),
          
          fluidRow(
            #### z option ------------------------------------------------------
            column(5,
                   radioButtons("z", 
                                h4("z:"),
                                c("1"=1,
                                  "-1"=-1),
                                1
                   )
            ),
            #### / -------------------------------------------------------------
            #### caseSensitive option ------------------------------------------
            column(5,
                   checkboxInput(
                     "caseSensitive", 
                     h4("caseSensitive"),
                     TRUE
                   )
            ),
            #### / -------------------------------------------------------------
          ),
          
          fluidRow(
            #### Search button -------------------------------------------------
            actionButton(
              "searchMZ", 
              "Search", 
              icon("search"), 
              style="color: #FFFFFF;
                    background-color: #4372AA;
                    border-color: #000000"
            )
            #### / -------------------------------------------------------------
          )
          
        ),
        mainPanel(
          h1("m/z Result:"),
          textOutput(
            "accurateMZ"
          )
        )
      )
    ),
    ### / ----------------------------------------------------------------------
    
    ### Information section ----------------------------------------------------
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
    ### / ----------------------------------------------------------------------
  )
  ## / -------------------------------------------------------------------------

)




# Server (functions) -----------------------------------------------------------
server <- shinyServer(function(input, output) {
  
  ## Read sample data ----------------------------------------------------------
  data <- reactive({
    req(input$mainFile,
        input$header,
        input$sep,
        input$quote,
        input$dec,
        input$rowStart,
        input$rowNamesColumn,
        input$mzValueColumn,
        input$rtValueColumn,
        input$samplesColumn)
    
    inFile <- input$mainFile
    
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Reading data..."
    )
      df <- read.table(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        dec = input$dec,
        skip = input$rowStart-1
      )
    remove_modal_spinner()
      
    return(df)
  })
  ## / -------------------------------------------------------------------------
  
  
  ## Print sample data ---------------------------------------------------------
  output$dataSamples <- renderDT({
    dt <- datatable(
      data(),
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        columnDefs = list(
          list(orderable=TRUE, targets=0)
        )
      ),
      class = "display"
    )
    dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
    dt
  })
  ## / -------------------------------------------------------------------------
  
  
  ## Read compound data --------------------------------------------------------
  compound_data <- reactive({
    req(input$compoundDataFile,
        input$compoundHeader,
        input$compoundSeparator,
        input$compoundQuote,
        input$compoundDec,
        input$compoundRowStart)
    
    compoundInFile <- input$compoundDataFile
    
    compoundDF <- read.table(
      compoundInFile$datapath,
      header = input$compoundHeader,
      sep = input$compoundSeparator,
      quote = input$compoundQuote,
      dec = input$compoundDec,
      skip = input$compoundRowStart-1
    )
    
    return(compoundDF)
  })
  ## / -------------------------------------------------------------------------
  
  
  ## Print compound data -------------------------------------------------------
  output$compoundData <- renderDT(
    compound_data()
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
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Please wait..."
    )
    
    # Obtain intervals:
    if(input$fragments == ""){
      searchMol$frag <- c("")
    }else{
      searchMol$frag <- as.numeric(gsub(",", ".", 
                                        strsplit(input$fragments, "-")[[1]]
      ))
    }
    
    # Obtain possible RTs:
    searchMol$RT <- 
      possibleRT(
        data(), 
        input$mzValueColumn, 
        input$rtValueColumn, 
        input$mzValue, 
        input$mzInterval
      )
    
    if(length(searchMol$RT) == 0){
      ### No data found. Please modify the parameters. -------------------------
      
      #### Print Best table ----------------------------------------------------
      output$bestResult <- renderDT({
        dt <- datatable(
          data.frame(
            "Row.names"   = c("No data found. Please modify the parameters."),
            "mz"         = c(NA),
            "RT"          = c(NA),
            "Correlation" = c(NA),
            "Sample.max.intensity" = c(NA)
          ),
          extensions = 'Buttons',
          options = list(
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            columnDefs = list(
              list(orderable=TRUE, targets=0)
            ),
            lengthMenu = list(c(10,25,50,-1),
                              c(10,25,50,"All"))
          ),
          class = "display"
        )
        dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
        dt
      })
      #### / -------------------------------------------------------------------
      
      #### Print complete results ----------------------------------------------
      output$summary <- renderDT({
        dt <- datatable(
          data.frame(
            "Group.id"    = c("No data found. Please modify the parameters."),
            "Score"       = c(NA),
            "Parental.RT" = c(NA),
            "Row.names"   = c(NA),
            "mz"         = c(NA),
            "RT"          = c(NA),
            "Correlation" = c(NA),
            "Sample.max.intensity" = c(NA)
          ),
          extensions = 'Buttons',
          options = list(
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            columnDefs = list(
              list(orderable=TRUE, targets=0)
            ),
            lengthMenu = list(c(10,25,50,-1),
                              c(10,25,50,"All"))
          ),
          class = "display"
        )
        dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
        dt
      })
      #### / -------------------------------------------------------------------
    }else{
      ### Data found -----------------------------------------------------------
      
      # Obtain best RT:
      searchMol$rtNear <-
        closerFragmentation(
          data(),
          input$rowNamesColumn,
          searchMol$RT, 
          input$rtValueColumn,
          input$rtInterval, 
          input$mzValue,
          input$mzValueColumn,
          input$correlationLevel,
          input$samplesColumn,
          searchMol$frag
        )
      
      #### Print Best table ----------------------------------------------------
      bestResult <- mzNear(
        data(),
        input$rowNamesColumn,
        searchMol$rtNear$RT,
        input$rtValueColumn,
        input$rtInterval, 
        input$mzValue,
        input$mzValueColumn,
        input$correlationLevel,
        input$samplesColumn
      )
      rownames(bestResult) <- NULL
      colnames(bestResult) <- c("Row.names", "mz", "RT", 
                                "Correlation", "Sample.max.intensity")
      numeric_cols <- sapply(bestResult, is.numeric)
      bestResult[numeric_cols] <- lapply(
        bestResult[numeric_cols], function(x) round(x, 5)
      )
      
      output$bestResult  <- renderDT({
        dt <- datatable(
          bestResult,
          extensions = 'Buttons',
          options = list(
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            columnDefs = list(
              list(orderable=TRUE, targets=0)
            ),
            lengthMenu = list(c(10,25,50,-1),
                              c(10,25,50,"All"))
          ),
          class = "display"
        )
        dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
        dt
      })
      #### / -------------------------------------------------------------------
      
      
      #### Print complete results ----------------------------------------------
      allResults <- printResults(
        data(),
        input$rowNamesColumn,
        searchMol$RT,
        input$rtValueColumn,
        input$rtInterval, 
        input$mzValue, 
        input$mzValueColumn,
        input$correlationLevel,
        input$samplesColumn,
        searchMol$frag
      )
      rownames(allResults) <- NULL
      colnames(allResults) <- c("Group.id", "Score", "Parental.RT", 
                                "Row.names", "mz", "RT", "Correlation",
                                "Sample.max.intensity")
      numeric_cols <- sapply(allResults, is.numeric)
      allResults[numeric_cols] <- lapply(
        allResults[numeric_cols], function(x) round(x, 5)
      )
      
      output$summary <- renderDT({
        dt <- datatable(
          allResults,
          extensions = 'Buttons',
          options = list(
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            columnDefs = list(
              list(orderable=TRUE, targets=0)
            ),
            lengthMenu = list(c(10,25,50,-1),
                              c(10,25,50,"All"))
          ),
          class = "display"
        )
        dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
        dt
      })
      #### / -------------------------------------------------------------------
    }

  remove_modal_spinner()
  })
  
  
  ## MZ and RT button search ----------------------------------------------
  # initialization of search parameters:
  # action:
  observeEvent(input$search_MZRT, {
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Please wait..."
    )
    
    ### Print Best table -------------------------------------------------------
    bestResult_MZRT <- mzNear(
      data(),
      input$rowNamesColumn,
      input$rtValue_MZRT,
      input$rtValueColumn,
      input$rtInterval_MZRT, 
      input$mzValue_MZRT,
      input$mzValueColumn,
      input$correlationLevel_MZRT,
      input$samplesColumn
    )
    rownames(bestResult_MZRT) <- NULL
    colnames(bestResult_MZRT) <- c("Row.names", "mz", "RT", 
                              "Correlation", "Sample.max.intensity")
    numeric_cols <- sapply(bestResult_MZRT, is.numeric)
    bestResult_MZRT[numeric_cols] <- lapply(
      bestResult_MZRT[numeric_cols], function(x) round(x, 5)
    )
    
    output$bestResult_MZRT  <- renderDT({
      dt <- datatable(
        bestResult_MZRT,
        extensions = 'Buttons',
        options = list(
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(
            list(orderable=TRUE, targets=0)
          ),
          lengthMenu = list(c(10,25,50,-1),
                            c(10,25,50,"All"))
        ),
        class = "display"
      )
      dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
      dt
    })
    ### / ----------------------------------------------------------------------
    
    remove_modal_spinner()
  })
  
  
  ## MZ button search ----------------------------------------------------------
  #action:
  m_reactive <- eventReactive( input$searchMZ, {
    input$m
  })
  z_reactive <- eventReactive( input$searchMZ, {
    as.numeric(input$z)
  })
  caseSensitive_reactive <- eventReactive( input$searchMZ, {
    input$caseSensitive
  })
    ### Calculate function and return result -----------------------------------
    output$accurateMZ  <- renderText({
      mz(
        m_reactive(),
        z = z_reactive(),
        caseSensitive = caseSensitive_reactive()
      )
    })
    ### / ----------------------------------------------------------------------
  
})




# Execute app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)