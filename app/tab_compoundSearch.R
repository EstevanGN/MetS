tab_compoundSearch <- tabPanel(
  "Search compound list",
  sidebarLayout(
    sidebarPanel(
      
      
      
      h1("Search compound list"),
      
      h2("What mz to use?"),
      
      fluidRow(
        # Charge option -------------------------------------------------------------
        column(
          5,
          radioButtons(
            "chargeListMZ", 
            h4("Charge:"),
            c("1"=1,
              "-1"=-1),
            1
          )
        ),
        
        # caseSensitive option -------------------------------------------------
        column(
          5,
          checkboxInput(
            "caseSensitiveListMZ", 
            h4("CaseSensitive"),
            TRUE
          )
        ),
      ),
      
      fluidRow(
        p("", em("Charge"),": charge of the experiment.")
      ),
      
      fluidRow(
        p("", em("CaseSensitive"),": the elements of the compound formula 
          are separated by upper case letters.")
      ),
      
      fluidRow(
        p("For more information, go to ", em("More tools/Calculate mz"), ".")
      ),
      
      fluidRow(
        # Mz of register or mz by formula --------------------------------------
        column(
          5,
          checkboxInput(
            "mzByFormula", 
            p("Use mz by formula when possible."),
            TRUE
          )
        )
      ),
      
      
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
      # Print mz vs formula mz -------------------------------------------------
      h2("Choose mz calculated by formula or mz by file:"),
      br(),
      p("We present a scatter diagram for you to make an informed decision. 
        Decide whether you want to search with the mz of the file or with 
        the mz calculated by formula (when possible)."),
      br(),
      plotlyOutput("mzCalculatedPlot"),
      
      # Print complete results -------------------------------------------------
      h1("Complete results:"),
      DTOutput(
        "summaryCompound"
      ),
      br(),
      br(),
      uiOutput("compound_graph_possibility"),
      br(),
      plotlyOutput("compoundSelectedPlot")
    )
  )
)