tab_onlyMZ <- tabPanel(
  "Only with m/z value",
  sidebarLayout(
    sidebarPanel(
             
      
      
      h1("Only with m/z value"),
      h2("Parameters"),
      p("Choose the values of every parameter to obtain 
      a metabolomic search. Maximum precision allowed 10^-5."),
      
      
      
      fluidRow(
        # M/z value option -----------------------------------------------------
        column(
          5,
          numericInput(
            "mzValue",
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
        # Fragments option -----------------------------------------------------
        textInput(
          "fragments", 
          h4("Fragments:"), 
          value = "138,09-156,10"
        )
      ),
             
      
             
      fluidRow(
        p("Write down every possible fragment separated by '-'. Use ',' 
        as decimal style")
      ),
      
             
             
      fluidRow(
        # Correlation option ---------------------------------------------------
        column(
          5,
          numericInput(
            "correlationLevel",
            h4("Correlation:"),
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
            "rtInterval",
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
        # Search button --------------------------------------------------------
        actionButton(
          "search", 
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
        "bestResult"
      ),
      br(),
      br(),
      uiOutput("graph_possibility"),
      br(),
      plotlyOutput("selectedPlot"),
      
      # Print complete results -------------------------------------------------
      h2("Complete results:"),
      DTOutput(
        "summary"
      ),
      br(),
      br(),
      uiOutput("complete_graph_possibility"),
      br(),
      plotlyOutput("completeSelectedPlot")
    )
  )
)