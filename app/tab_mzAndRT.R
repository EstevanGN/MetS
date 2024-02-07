tab_mzAndRT <- tabPanel(
  "With m/z and RT values",
  sidebarLayout(
    sidebarPanel(
             
      
      
      h1("With m/z and RT values"),
      h2("Parameters"),
      p("Choose the values of every parameter to obtain 
      a metabolomic search. Maximum precision allowed 10^-5."),
             
      
             
      fluidRow(
        # M/z value option -----------------------------------------------------
        column(
          5,
          numericInput(
            "mzValue_MZRT",
            h4("Mz Value:"),
            min = 0.00001,
            max = NA,
            step = 0.00001,
            value = 304.15
          ),
        ),
        
        # M/z interval option --------------------------------------------------
        column(
          5,
          numericInput(
            "mzInterval_MZRT",
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
        # RT value option ------------------------------------------------------
        column(
          5,
          numericInput(
            "rtValue_MZRT",
            h4("RT Value:"),
            min = 0,
            step = 0.00001,
            value = 4.97
          ),
        ),
      ),
      
      
             
      fluidRow(
        # Correlation option ---------------------------------------------------
        column(
          5,
          numericInput(
            "correlationLevel_MZRT",
            h4("Correlation"),
            min = 0,
            max = 1,
            step = 0.00001,
            value = 0.95
          ),
        ),
        
        # RT Interval option ---------------------------------------------------
        column(
          5,
          numericInput(
            "rtInterval_MZRT",
            h4("RT Interval:"),
            min = 0.00001,
            max = NA,
            step = 0.00001,
            value = 0.01
          ),
        )
      ),
      
      
      
      fluidRow(
        p("", em("RT Interval"),": is the maximum distance allowed to 
        RT value for create an interval of search.")
      ),
           
      
             
      fluidRow(
        # Second search button -------------------------------------------------
        actionButton(
          "search_MZRT", 
          "Search", 
          icon("search"), 
          style="color: #FFFFFF; 
             background-color: #4372AA; 
             border-color: #000000"
        )
      )
    ),
           
    
           
    mainPanel(
      # Best result table ------------------------------------------------------
      h1("Best result:"),
      DTOutput(
        "bestResult_MZRT"
      ),
      br(),
      br(),
      uiOutput("MZRT_graph_possibility"),
      br(),
      plotlyOutput("MZRTSelectedPlot")
    )
  )
)