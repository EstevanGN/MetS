tab_compoundData <- tabPanel(
  "Compound Data",
  sidebarLayout(
    sidebarPanel(
      
      
      
      h1("Compound Data"),
      br(),
      h2("General configuration "),
      p("Choose your compound data from a file with CSV format (only). Press ", 
      strong("'Browse...'"), " button to upload data."),
             
      
      
      # Browse data button -----------------------------------------------------
      fileInput(
        "compoundFile", 
        h4("Choose CSV File"),
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      
      
             
      fluidRow(
        # Header option --------------------------------------------------------
        column(
          5,
          h4("Header:"),
          checkboxInput(
            "compoundHeader", 
            "It has header", 
            TRUE
          )
        ),
        
        # Decimal option -------------------------------------------------------
        column(
          5,
          radioButtons(
            "compoundDec", 
            h4("Decimal style:"),
            c(Point=".",
              Comma=","),
            "."
          ),
        )
      ),
      
      
             
      fluidRow(
        # Quote option ---------------------------------------------------------
        column(
          5,
          radioButtons(
            "compoundQuote", 
            h4("Quote:"),
            c("Double Quote"="\"",
              "None"="",
              "Single Quote"="'"),
            "\""
          ),
        ),
        
        # Separator option -----------------------------------------------------
        column(
          5,
          radioButtons(
            "compoundSep", 
            h4("Separator character:"),
            c("Semicolon"=";",
              "Space"=" ",
              "Comma"=",",
              "Tab"="\t",
              "Vertical Line |"="|"),
            ";"
          ),
        )
      ),
      
      
             
      fluidRow(
        # Row start option -----------------------------------------------------
        tags$div(
          id = "inline",
          numericInput(
            "compoundRowStart", 
            h4("Row to start:"), 
            value = 1,
            min = 1
          )
        )
      ),
      
      
             
      h2("Columns configuration"),
      p("Edit the following parameters to configure the columns of 
      your data frame. For each title give a column number"),
      
      
             
      fluidRow(
        # Columns configuration ------------------------------------------------
        tags$head(
          tags$style(
            type="text/css", 
            "#inline label{ display: table-cell; 
               text-align: center; vertical-align: middle; } 
               #inline .form-group { display: table-row;}"
          )
        ),
        
        tags$div(
          id = "inline", 
          
          numericInput(
            "compoundRowNamesColumn", 
            "Row names:",
            value = 1,
            min = 1,
            step = 1
          ),
          
          numericInput(
            "compoundMzValueColumn",
            "m/z value:",
            value = 3,
            min = 1,
            step = 1
          ),
          
          numericInput(
            "compoundRtValueColumn", 
            "RT value:",
            value = 4,
            min = 1,
            step = 1
          ),
          
          numericInput(
            "compoundFragmentsColumn", 
            "Fragments:",
            value = 5,
            min = 1,
            step = 1
          )
        )
      ),
      
      
      
      br(),
      br(),
      
      
      
      # UNAL Logo --------------------------------------------------------------
      img(src = "logo_unal.png", height="70%", width="70%")
    ),
           
    mainPanel(
      # Present example of data format -----------------------------------------
      h3("Example of a data table format"),
      img(src = "Compound data example.png", height="100%", width="100%"),
      
      # Present data uploaded --------------------------------------------------
      h1("Data Uploaded"),
      br(),
      br(),
      DTOutput(
        "compoundReaded"
      )
    )
  )
)