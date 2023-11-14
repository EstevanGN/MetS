tab_compoundSearch <- tabPanel(
  "Search compound list",
  sidebarLayout(
    sidebarPanel(
      
      
      
      h1("Search compound list"),
      h2("Parameters"),
      p("Choose the values of every parameter to obtain 
      a metabolomic search. Maximum precision allowed 10^-5."),
      
      
      
      fluidRow(
        # M/z interval option --------------------------------------------------
        column(
          5,
          numericInput(
            "mzIntervalCompound",
            h4("Mz Interval:"),
            min = 0.00001,
            max = NA,
            step = 0.00001,
            value = 0.01
          ),
        ),
        
        # RT Interval option ---------------------------------------------------
        column(
          5,
          numericInput(
            "rtIntervalCompound",
            h4("RT Interval:"),
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
        p("", em("RT Interval"),": is the maximum distance allowed to 
        RT value for create an interval of search.")
      ),
      
      
      
      fluidRow(
        # Correlation option ---------------------------------------------------
        column(
          5,
          numericInput(
            "correlationCompound",
            h4("Correlation:"),
            min = 0,
            max = 1,
            step = 0.00001,
            value = 0.95
          ),
        ),
      ),
      
      
      
      fluidRow(
        # Third search button --------------------------------------------------
        actionButton(
          "searchCompound", 
          "Search", 
          icon("search"), 
          style="color: #FFFFFF; 
             background-color: #4372AA; 
             border-color: #000000"
        )
      )
    ),
    
    
    
    mainPanel(
      # Print complete results -------------------------------------------------
      h1("Complete results:"),
      DTOutput(
        "summaryCompound"
      )
    )
  )
)