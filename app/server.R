server <- shinyServer(function(input, output) {
  
  
  
  # Read sample data -----------------------------------------------------------
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
  
  
  
  # Print sample data ----------------------------------------------------------
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
  
  
  
  # Read compound data ---------------------------------------------------------
  compoundData <- reactive({
    req(input$compoundFile,
        input$compoundHeader,
        input$compoundSep,
        input$compoundQuote,
        input$compoundDec,
        input$compoundRowStart,
        input$compoundRowNamesColumn,
        input$compoundMzValueColumn,
        input$compoundRtValueColumn,
        input$compoundFragmentsColumn)
    
    inFile <- input$compoundFile
    
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Reading data..."
    )
    
    df <- read.table(
      inFile$datapath,
      header = input$compoundHeader,
      sep = input$compoundSep,
      quote = input$compoundQuote,
      dec = input$compoundDec,
      skip = input$compoundRowStart-1
    )
    
    remove_modal_spinner()
    
    return(df)
  })
  
  
  
  # Print compound data --------------------------------------------------------
  output$compoundReaded <- renderDT({
    dt <- datatable(
      compoundData(),
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
  
  
  
  # Button search for "Only mz" section ----------------------------------------
  # initialization of search parameters:
  searchMol <- reactiveValues(
    frag = NULL,
    RT = NULL,
    rtNear = NULL
  )
  
  # save parameters to no update until press button:
  mzValue_reactive <- eventReactive( input$search, {input$mzValue})
  mzInterval_reactive <- eventReactive( input$search, {input$mzInterval})
  fragments_reactive <- eventReactive( input$search, {input$fragments})
  correlation_reactive <- eventReactive( input$search, {input$correlationLevel})
  rtInterval_reactive <- eventReactive( input$search, {input$rtInterval})
  
  # action:
  observeEvent(input$search, {
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Please wait..."
    )
    
    # Obtain intervals:
    if(fragments_reactive() == ""){
      searchMol$frag <- c("")
    }else{
      searchMol$frag <- as.numeric(gsub(",", ".", 
                                        strsplit(fragments_reactive(), "-")[[1]]
      ))
    }
    
    # Obtain possible RTs:
    searchMol$RT <- 
      possibleRT(
        data(), 
        input$mzValueColumn, 
        input$rtValueColumn, 
        mzValue_reactive(), 
        mzInterval_reactive()
      )
    
    if(length(searchMol$RT) == 0){
      ## No data found. Please modify the parameters. --------------------------
      
      ### Print Best table -----------------------------------------------------
      output$bestResult <- renderDT({
        dt <- datatable(
          empty_bestResult(),
          
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
      
      ### Print complete results -----------------------------------------------
      output$summary <- renderDT({
        dt <- datatable(
          empty_completeResult(),
          
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
      
    }else{
      ## Data found ------------------------------------------------------------
      
      # Obtain best RT:
      searchMol$rtNear <-
        closerFragmentation(
          data(),
          input$rowNamesColumn,
          searchMol$RT, 
          input$rtValueColumn,
          rtInterval_reactive(), 
          mzValue_reactive(),
          input$mzValueColumn,
          correlation_reactive(),
          input$samplesColumn,
          searchMol$frag
        )
      
      ### Print Best table -----------------------------------------------------
      output$bestResult  <- renderDT({
        dt <- datatable(
          bestResultTable(
            data(),
            input$rowNamesColumn,
            searchMol$rtNear$RT,
            input$rtValueColumn,
            rtInterval_reactive(), 
            mzValue_reactive(),
            input$mzValueColumn,
            correlation_reactive(),
            input$samplesColumn
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
      
      
      ### Print complete results -----------------------------------------------
      output$summary <- renderDT({
        dt <- datatable(
          completeResultTable(
            data(),
            input$rowNamesColumn,
            searchMol$RT,
            input$rtValueColumn,
            rtInterval_reactive(), 
            mzValue_reactive(), 
            input$mzValueColumn,
            correlation_reactive(),
            input$samplesColumn,
            searchMol$frag
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
    }
    
    remove_modal_spinner()
  })
  
  
  
  # Button search for "MZ and RT" section --------------------------------------
  # save parameters to no update until press button:
  mzMZRT_reactive <- eventReactive( input$search_MZRT, {
    input$mzValue_MZRT
  })
  mzIntervalMZRT_reactive <- eventReactive( input$search_MZRT, {
    input$mzInterval_MZRT
  })
  rtMZRT_reactive <- eventReactive( input$search_MZRT, {
    input$rtValue_MZRT
  })
  correlationMZRT_reactive <- eventReactive( input$search_MZRT, {
    input$correlationLevel_MZRT
  })
  rtIntervalMZRT_reactive <- eventReactive( input$search_MZRT, {
    input$rtInterval_MZRT
  })
  
  # action:
  observeEvent(input$search_MZRT, {
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Please wait..."
    )
    
    ## Print Best table --------------------------------------------------------
    if(
      rtMZRT_reactive() >= min(data()[,input$rtValueColumn])
      &
      rtMZRT_reactive() <= max(data()[,input$rtValueColumn])
    ){
      bestResult_MZRT <- bestResultTable(
        data(),
        input$rowNamesColumn,
        rtMZRT_reactive(),
        input$rtValueColumn,
        rtIntervalMZRT_reactive(), 
        mzMZRT_reactive(),
        input$mzValueColumn,
        correlationMZRT_reactive(),
        input$samplesColumn
      )
      
      
      if(
        min(
          abs(bestResult_MZRT[,"mz"]-mzMZRT_reactive())
        ) > mzIntervalMZRT_reactive()
      ){
        output$bestResult_MZRT <- renderDT({
          dt <- datatable(
            empty_bestResult(),
            
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
        
      }else{
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
      }
      
    }else{
      output$bestResult_MZRT <- renderDT({
        dt <- datatable(
          empty_bestResult(),
          
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
    }
    
    remove_modal_spinner()
  })
  
  
  
  # Button search for compound list --------------------------------------------
  # initialization of search parameters:
  searchMolCompound <- reactiveValues(
    frag = NULL,
    RT = NULL,
    rtNear = NULL
  )
  
  # save parameters to no update until press button:
  mzIntComp_reactive <- eventReactive( input$searchCompound, {
    input$mzIntervalCompound
  })
  rtIntComp_reactive <- eventReactive( input$searchCompound, {
    input$rtIntervalCompound
  })
  corrComp_reactive <- eventReactive( input$searchCompound, {
    input$correlationCompound
  })
  
  # action:
  observeEvent(input$searchCompound, {
    show_modal_spinner(
      spin = "cube-grid",
      color = "#4372AA",
      text = "Please wait..."
    )
    
    compoundResult <- data.frame(
      "Compound"    = c(NA),
      "Group.id"    = c(NA),
      "Score"       = c(NA),
      "Parental.RT" = c(NA),
      "Row.names"   = c(NA),
      "mz"          = c(NA),
      "RT"          = c(NA),
      "Correlation" = c(NA),
      "Sample.max.intensity" = c(NA)
    )
    
    for( i in 1:nrow(compoundData()) ){
      nameCompound <- compoundData()[i,input$compoundRowNamesColumn]
      mzCompound <- as.numeric(
        gsub(",", ".",compoundData()[i,input$compoundMzValueColumn])
      )
      rtCompound <- compoundData()[i,input$compoundRtValueColumn]
      fragCompound <- compoundData()[i,input$compoundFragmentsColumn]
      
      if( rtCompound!="" & fragCompound=="" ){
        ## There is only one RT ------------------------------------------------
        rtCompound <- as.numeric(gsub(",", ".",rtCompound))
        
        if(
          rtCompound >= min(data()[,input$rtValueColumn])
          &
          rtCompound <= max(data()[,input$rtValueColumn])
        ){
          bestResultCompound <- bestResultTable(
            data(),
            input$rowNamesColumn,
            rtCompound,
            input$rtValueColumn,
            rtIntComp_reactive(), 
            mzCompound,
            input$mzValueColumn,
            corrComp_reactive(),
            input$samplesColumn
          )
          
          if(
            min(
              abs(bestResultCompound[,"mz"]-mzCompound)
            ) > mzIntComp_reactive()
          ){
            dfTemp <- data.frame(
              "Compound" = nameCompound,
              empty_completeResult()
            )
            #compoundResult <- rbind(compoundResult, dfTemp)
          }else{
            completeResultCompound <- completeResultTable(
              data(),
              input$rowNamesColumn,
              c(rtCompound),
              input$rtValueColumn,
              rtIntComp_reactive(), 
              mzCompound, 
              input$mzValueColumn,
              corrComp_reactive(),
              input$samplesColumn,
              fragCompound
            )
            dfTemp <- data.frame(
              "Compound" = rep(nameCompound,nrow(completeResultCompound)),
              completeResultCompound
            )
          }
          
        }else{
          dfTemp <- data.frame(
            "Compound" = nameCompound,
            empty_completeResult()
          )
        }
        
      }else{
        ## In other case -------------------------------------------------------
        # ( Without RT and without fragments,
        # With RT and with fragments,
        # Only with fragments)
        
        # Obtain intervals:
        if(fragCompound == ""){
          searchMolCompound$frag <- c("")
        }else{
          searchMolCompound$frag <- as.numeric(gsub(",", ".", 
                                            strsplit(fragCompound, "-")[[1]]
          ))
        }
        
        # Obtain possible RTs:
        searchMolCompound$RT <- 
          possibleRT(
            data(), 
            input$mzValueColumn, 
            input$rtValueColumn, 
            mzCompound, 
            mzIntComp_reactive()
          )
        
        if(length(searchMolCompound$RT) == 0){
          ### No data found. Please modify the parameters. ---------------------
          
          dfTemp <- data.frame(
            "Compound" = nameCompound,
            empty_completeResult()
          )
          
        }else{
          ### Data found -------------------------------------------------------
          completeResultCompound <- completeResultTable(
            data(),
            input$rowNamesColumn,
            searchMolCompound$RT,
            input$rtValueColumn,
            rtIntComp_reactive(), 
            mzCompound, 
            input$mzValueColumn,
            corrComp_reactive(),
            input$samplesColumn,
            searchMolCompound$frag
          )
          dfTemp <- data.frame(
            "Compound" = rep(nameCompound,nrow(completeResultCompound)),
            completeResultCompound
          )
            
        }
      }
      
      compoundResult <- rbind(compoundResult, dfTemp)
    }
    
    compoundResult <- compoundResult[-1,]
    rownames(compoundResult) <- 1:nrow(compoundResult)
    
    output$summaryCompound <- renderDT({
      dt <- datatable(
        compoundResult,
        
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
    
    remove_modal_spinner()
  })
  
  
  
  # Button search for calculate mz with formula --------------------------------
  # save parameters to no update until press button:
  m_reactive <- eventReactive( input$searchMZ, {
    input$m
  })
  z_reactive <- eventReactive( input$searchMZ, {
    as.numeric(input$z)
  })
  caseSensitive_reactive <- eventReactive( input$searchMZ, {
    input$caseSensitive
  })
  ## Calculate function and return result --------------------------------------
  output$accurateMZ  <- renderText({
    mz(
      m_reactive(),
      z = z_reactive(),
      caseSensitive = caseSensitive_reactive()
    )
  })
  
})