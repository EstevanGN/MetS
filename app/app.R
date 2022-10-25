# ___________________________________________________________________
# Libraries ---------------------------------------------------------
# ___________________________________________________________________
library(shiny)      # basic for shiny apps
library(DT)         # Output tables
library(readr)      # Read big size files
library(tidyverse)  # %>% function



# ___________________________________________________________________
# Shiny options------------------------------------------------------
# ___________________________________________________________________
options(shiny.maxRequestSize=170*1024^2)



# ___________________________________________________________________
# App functions -----------------------------------------------------
# ___________________________________________________________________

# Possible RTs from a specific mzValue:
possibleRT <- function(dataSamples, mz, interval){
  mzTarget <- dataSamples %>%
    filter(`m/z (positive ionization)` >= mz-interval, 
           `m/z (positive ionization)` <= mz+interval )
  return(data.frame(mzTarget)[,"RT..min."])
}

# With a specific RT searching all mz near (with correlation):
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

# With a set of RTs, searching for the closer to mz fragments:
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

# print all possible tables with Rt near:
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


# ___________________________________________________________________
# User interface ----------------------------------------------------
# ___________________________________________________________________
ui <- navbarPage("Metabolomics Search",
  
  ## ________________________________________________________________
  ## Upload data section --------------------------------------------
  ## ________________________________________________________________
  tabPanel("Upload Data",
    sidebarLayout(
      sidebarPanel(
        h2("Metabolomic Data"),
        p("Choose your metabolomic data from a file with 
        CSV format (only). Press ", 
          strong("'Browse...'"), 
          " button to upload data. Soon more formats."),
        
        ### _________________________________________________________
        ### Collect file button -------------------------------------
        ### _________________________________________________________
        fileInput(
          "file1", 
          h4("Choose CSV File"),
          accept = c("text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        
        
        fluidRow(
          ### _______________________________________________________
          ### Header option -----------------------------------------
          ### _______________________________________________________
          column(
            5,
            h4("Header"),
            checkboxInput(
              "header", 
              "It has header", 
              TRUE
            )
          ),
          
          ### _______________________________________________________
          ### Row start option --------------------------------------
          ### _______________________________________________________
          column(
            5,
            numericInput("rowStart", 
                         h4("From row:"), 
                         value = 1,
                         min = 1)
          )
        ),
        
        fluidRow(
          ### _______________________________________________________
          ### Quote option ------------------------------------------
          ### _______________________________________________________
          column(
            5,
            radioButtons("quote", h4("Quote"),
                         c(None="",
                           "Double Quote"='"',
                           "Single Quote"="'"),
                         '"'),
          ),
          
          ### _______________________________________________________
          ### Separator option --------------------------------------
          ### _______________________________________________________
          column(
            5,
            radioButtons("sep", h4("Separator"),
                         c(Comma=",",
                           Semicolon=";",
                           Tab="\t"),
                         ","),
          )
        ),
        
        br(),
        br(),
        
        ### _________________________________________________________
        ### UNAL Logo -----------------------------------------------
        ### _________________________________________________________
        img(src = "logo_unal.png", height = 80, width = 200)
      ),
      
      ### ___________________________________________________________
      ### Present data uploaded -------------------------------------
      ### ___________________________________________________________
      mainPanel(
        h1("Data Uploaded"),
        
        br(),
        br(),
        
        DTOutput(
          "dataSamples"
        )
      )
    )
  ),

  ## ________________________________________________________________
  ## Searching molecules section ------------------------------------
  ## ________________________________________________________________
  navbarMenu("Execute",
             
    tabPanel("Searching molecules",
      sidebarLayout(
        ## ____________________________________________________________
        ## Selection parameters ---------------------------------------
        ## ____________________________________________________________
        sidebarPanel(
          h2("Parameters"),
          p("Choose the values of every parameter to obtain 
            a metabolomic search. Maximum precision allowed 10^-5."),
          
          fluidRow(
            ### _______________________________________________________
            ### Mz value option ------------------------------------
            ### _______________________________________________________
            column(
              5,
              numericInput(
                "mzValue",
                h4("Mz Value:"),
                min = NA,
                max = NA,
                step = 0.00001,
                value = 304.15
              ),
            ),
            
            ### _______________________________________________________
            ### Mz Interval option ------------------------------------
            ### _______________________________________________________
            column(
              5,
              numericInput(
                "mzInterval",
                h4("Mz Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
          ),
          
          fluidRow(
            p("", em("Mz Interval"),": is the maximum distance allowed to 
              mz value for create an interval of search. I.e.", 
              strong("[mzValue-mzInterval, mzValue+mzInterval]"), " is 
              the range of search for near values of targed mz.")
          ),
          
          fluidRow(
            ### _______________________________________________________
            ### Fragments option --------------------------------------
            ### _______________________________________________________
            textInput(
              "fragments", 
              h4("Fragments"), 
              value = "138.09; 156.10"
            )
          ),
          fluidRow(
            p("Write down every possible fragment separated by ';'")
          ),
          
          fluidRow(
            ### _______________________________________________________
            ### Correlation option ------------------------------------
            ### _______________________________________________________
            column(
              5,
              numericInput(
                "correlationLevel",
                h4("Correlation"),
                min = 0,
                max = 1,
                step = 0.00001,
                value = 0.95
              ),
            ),
            
            ### _______________________________________________________
            ### Rt Interval option ------------------------------------
            ### _______________________________________________________
            column(
              5,
              numericInput(
                "rtInterval",
                h4("Rt Interval:"),
                min = 0.00001,
                max = NA,
                step = 0.00001,
                value = 0.01
              ),
            )
          ),
          
          fluidRow(
            p("", em("Rt Interval"),": is the maximum distance allowed to 
              Rt value for create an interval of search.")
          ),
          
          ### _________________________________________________________
          ### Search button -------------------------------------------
          ### _________________________________________________________
          fluidRow(
              actionButton(
                "search", 
                "Search", 
                icon("search"), 
                style="color: #DEEDCF; background-color: #56B870; border-color: #0A2F51"
              )
          )
        ),
        mainPanel(
          ### _________________________________________________________
          ### Best result table ---------------------------------------
          ### _________________________________________________________
          h1("Best result:"),
          DTOutput(
            "bestResult"
          ),
          
          ### _________________________________________________________
          ### Printing other results ----------------------------------
          ### _________________________________________________________
          h2("Other results:"),
          verbatimTextOutput("summary")
        )
      )
    ),
  
    tabPanel("Print specific RT",
      sidebarLayout(
        sidebarPanel(
          h2("RT"),
          p("choose between specific RT or an RT_id from the 
            'Other Results' tab"),
          
          ### _________________________________________________________
          ### RT value or RT_id ---------------------------------------
          ### _________________________________________________________
          radioButtons(
            "RTorRT_id",
            "Search by:",
            c("RT" = "RT", 
              "RT_id" = "RT_id"),
            selected = NULL,
            inline = TRUE
          ),
          
          ### _________________________________________________________
          ### RT value or RT_id value ---------------------------------
          ### _________________________________________________________
          numericInput(
            "RTchoosedValue",
            "Give a value",
            value = NA,
            min = 0,
            step = 0.00001
          ),
          
          ### _________________________________________________________
          ### Print button --------------------------------------------
          ### _________________________________________________________
          actionButton(
            "searchRT", 
            "Print", 
            icon("print"), 
            style="color: #DEEDCF; background-color: #56B870; border-color: #0A2F51"
          )
          
        ),
        mainPanel(
          h1("For print:"),
          DTOutput(
            "RTchoosed"
          )
        )
      )
    )
  
  ),

  ## ________________________________________________________________
  ## More info section ----------------------------------------------
  ## ________________________________________________________________                 
  navbarMenu("Information",
                            
    tabPanel("Authors"),
    
    tabPanel("Help")
  )
)



# ___________________________________________________________________
# Server (functions) ------------------------------------------------
# ___________________________________________________________________
server <- shinyServer(function(input, output) {
  # _________________________________________________________________
  # Read data -------------------------------------------------------
  # _________________________________________________________________
  data <- reactive({
    req(input$file1,
        input$rowStart)
    inFile <- input$file1
    df <- read_delim(inFile$datapath, 
                   col_names = input$header, 
                   delim = input$sep,
                   quote = input$quote,
                   skip = input$rowStart-1)
    return(df)
  })
  
  # _________________________________________________________________
  # Print data ------------------------------------------------------
  # _________________________________________________________________
  output$dataSamples <- renderDT(
    data()
  )
  
  # _________________________________________________________________
  # Button search ---------------------------------------------------
  # _________________________________________________________________
  searchMol <- reactiveValues(
    frag = NULL,
    RT = NULL,
    rtNear = NULL
  )
  
  observeEvent(input$search, {
    searchMol$frag <- as.numeric(strsplit(input$fragments, "; ")[[1]])
    
    searchMol$RT <- 
      possibleRT(data(), input$mzValue, input$mzInterval)
    
    searchMol$rtNear <-
      closerFragmentation(data(), searchMol$RT, input$rtInterval, 
                          input$mzValue, input$correlationLevel, 
                          searchMol$frag)
    
    # _______________________________________________________________
    # Print Best table ----------------------------------------------
    # _______________________________________________________________
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
      ))
    
    # _______________________________________________________________
    # Print possible tables -----------------------------------------
    # _______________________________________________________________
    output$summary <- renderPrint({
      printResults(data(), searchMol$RT, input$rtInterval, 
                   input$mzValue, input$correlationLevel)
    })
    
  })
  
  # _________________________________________________________________
  # Button RT print -------------------------------------------------
  # _________________________________________________________________
  specificRT <- reactiveValues(RT = NULL)
  
  observeEvent(input$searchRT, {
    if(input$RTorRT_id == "RT"){
      specificRT$RT <- input$RTchoosedValue
    }else{
      specificRT$RT <- searchMol$RT[input$RTchoosedValue]
    }
    
    output$RTchoosed  <- DT::renderDataTable(
      DT::datatable(
        { 
          mzNear(
            data(),
            specificRT$RT,
            input$rtInterval, 
            input$mzValue, 
            input$correlationLevel
          ) 
        },
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       lengthMenu = list(c(10,25,50,-1),
                                         c(10,25,50,"All"))),
        
        class = "display"
      ))
  })
  
})



# ___________________________________________________________________
# Execute app -------------------------------------------------------
# ___________________________________________________________________
shinyApp(ui = ui, server = server)