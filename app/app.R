# ___________________________________________________________________
# Libraries ---------------------------------------------------------
# ___________________________________________________________________
library(shiny)    # basic for shiny apps
library(DT)       # Output tables
library(readr)    # Read big size files



# ___________________________________________________________________
# Shiny options------------------------------------------------------
# ___________________________________________________________________
options(shiny.maxRequestSize=170*1024^2)



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
        
        fileInput(
          "file1", 
          h4("Choose CSV File"),
          accept = c("text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        
        fluidRow(
          column(
            5,
            h4("Header"),
            checkboxInput(
              "header", 
              "It has header", 
              TRUE
            )
          ),
          
          column(
            5,
            numericInput("rowStart", 
                         h4("From row:"), 
                         value = 1,
                         min = 1)
          )
        ),
        
        fluidRow(
          column(
            5,
            radioButtons("quote", h4("Quote"),
                         c(None="",
                           "Double Quote"='"',
                           "Single Quote"="'"),
                         '"'),
          ),
          
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
        
        img(src = "logo_unal.png", height = 80, width = 200)
      ),
      
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
  tabPanel("Searching molecules"),

  ## ________________________________________________________________
  ## More info section ----------------------------------------------
  ## ________________________________________________________________                 
  navbarMenu("Information",
                            
    tabPanel("Authors"),
    
    tabPanel("Help"))
)



# ___________________________________________________________________
# Server (functions) ------------------------------------------------
# ___________________________________________________________________
server <- shinyServer(function(input, output) {
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
  
  output$dataSamples <- renderDT(
    data()
  )
})



# ___________________________________________________________________
# Execute app -------------------------------------------------------
# ___________________________________________________________________
shinyApp(ui = ui, server = server)