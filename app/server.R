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
      
      output$graph_possibility <- renderUI({
        HTML('<span style="color: #4372AA; font-weight: bold;">Graphic not 
             available.</span>')
      })
      
      output$selectedPlot <- renderPlotly({NULL})
      
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
      
      output$complete_graph_possibility <- renderUI({
        HTML('<span style="color: #4372AA; font-weight: bold;">Graphic not 
             available.</span>')
      })
      
      output$completeSelectedPlot <- renderPlotly({NULL})
      
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
      dtBestReult <- bestResultTable(
        data(),
        input$rowNamesColumn,
        searchMol$rtNear$RT,
        input$rtValueColumn,
        rtInterval_reactive(), 
        mzValue_reactive(),
        input$mzValueColumn,
        correlation_reactive(),
        input$samplesColumn
      )
      output$bestResult  <- renderDT({
        dt <- datatable(
          dtBestReult,
          
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
          
          class = "display",
        )
        
        dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
        dt
      })
      
      output$graph_possibility <- renderUI({
        HTML('<span style="color: #4372AA; font-weight: bold;">Select rows 
        from the table to display the intensity graph by samples.</span>')
      })
      
      # Plot selected rows
      output$selectedPlot <- renderPlotly({
        selected <- input$bestResult_rows_selected
        if (length(selected) == 0) return(NULL)
        
        names <- dtBestReult[selected, 'Row.names']
        
        data_filtered <- data() %>%
          filter(pull(., input$rowNamesColumn) %in% names)
        samColumns <- as.numeric(
          strsplit(input$samplesColumn, "-")[[1]]
        )
        data_filtered <- data_filtered[,c(input$rowNamesColumn,
                               samColumns[1]:samColumns[2])]
        
        transposed_data <- data_filtered %>%
          pivot_longer(cols = -1, names_to = "Variable", values_to = "Valor")
        colnames(transposed_data) <- c("Names", "Samples", "Value")
        
        plot_ly(transposed_data, x = ~Samples, y = ~Value, 
                color = ~factor(Names), type = 'bar') %>%
          layout(barmode = 'group',
                 xaxis = list(title = "Samples"),
                 yaxis = list(title = "Intensity"),
                 title = "Intensity of samples in selected rows")
      })
      
      
      ### Print complete results -----------------------------------------------
      dtSummary <- completeResultTable(
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
      output$summary <- renderDT({
        dt <- datatable(
          dtSummary,
          
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
      
      output$complete_graph_possibility <- renderUI({
        HTML('<span style="color: #4372AA; font-weight: bold;">Select rows 
        from the table to display the intensity graph by samples.</span>')
      })
      
      # Plot selected rows
      output$completeSelectedPlot <- renderPlotly({
        selected <- input$summary_rows_selected
        if (length(selected) == 0) return(NULL)
        
        names <- dtSummary[selected, 'Row.names']
        
        data_filtered <- data() %>%
          filter(pull(., input$rowNamesColumn) %in% names)
        samColumns <- as.numeric(
          strsplit(input$samplesColumn, "-")[[1]]
        )
        data_filtered <- data_filtered[,c(input$rowNamesColumn,
                                          samColumns[1]:samColumns[2])]
        
        transposed_data <- data_filtered %>%
          pivot_longer(cols = -1, names_to = "Variable", values_to = "Valor")
        colnames(transposed_data) <- c("Names", "Samples", "Value")
        
        plot_ly(transposed_data, x = ~Samples, y = ~Value, 
                color = ~factor(Names), type = 'bar') %>%
          layout(barmode = 'group',
                 xaxis = list(title = "Samples"),
                 yaxis = list(title = "Intensity"),
                 title = "Intensity of samples in selected rows")
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
        
        output$MZRT_graph_possibility <- renderUI({
          HTML('<span style="color: #4372AA; font-weight: bold;">Graphic not 
             available.</span>')
        })
        
        output$MZRTSelectedPlot <- renderPlotly({NULL})
        
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
        
        output$MZRT_graph_possibility <- renderUI({
          HTML('<span style="color: #4372AA; font-weight: bold;">Select rows 
        from the table to display the intensity graph by samples.</span>')
        })
        
        # Plot selected rows
        output$MZRTSelectedPlot <- renderPlotly({
          selected <- input$bestResult_MZRT_rows_selected
          if (length(selected) == 0) return(NULL)
          
          names <- bestResult_MZRT[selected, 'Row.names']
          
          data_filtered <- data() %>%
            filter(pull(., input$rowNamesColumn) %in% names)
          samColumns <- as.numeric(
            strsplit(input$samplesColumn, "-")[[1]]
          )
          data_filtered <- data_filtered[,c(input$rowNamesColumn,
                                            samColumns[1]:samColumns[2])]
          
          transposed_data <- data_filtered %>%
            pivot_longer(cols = -1, names_to = "Variable", values_to = "Valor")
          colnames(transposed_data) <- c("Names", "Samples", "Value")
          
          plot_ly(transposed_data, x = ~Samples, y = ~Value, 
                  color = ~factor(Names), type = 'bar') %>%
            layout(barmode = 'group',
                   xaxis = list(title = "Samples"),
                   yaxis = list(title = "Intensity"),
                   title = "Intensity of samples in selected rows")
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
      
      output$MZRT_graph_possibility <- renderUI({
        HTML('<span style="color: #4372AA; font-weight: bold;">Graphic not 
             available.</span>')
      })
      
      output$MZRTSelectedPlot <- renderPlotly({NULL})
    }
    
    remove_modal_spinner()
  })
  
  
  
  # Button search for compound list --------------------------------------------
  # mz vs mz by formula:
  dtCalculated <- NULL # To save the mz by formula.
  
  mz_plot <- reactive({
    #mz_old = as.numeric(compoundData()[, input$compoundMzValueColumn])
    charge <- as.numeric(input$chargeListMZ)
    case_sensitive <- input$caseSensitiveListMZ
    calculated_mz <- NULL
    
    for ( i in 1:length(compoundData()[,input$compoundFormulaColumn])){
      calculated_mz[i] <- tryCatch(
       {
         result <- suppressWarnings(mz(
           compoundData()[i, input$compoundFormulaColumn],
           z = charge,
           caseSensitive = case_sensitive
         ))
         if (is.na(result)) {
           compoundData()[i, input$compoundMzValueColumn]
         } else {
           result
         }
       },
       error = function(e) {
         compoundData()[i, input$compoundMzValueColumn]
       }
     )
    }
    
    dtCalculated <<- data.frame(
     "ID" = compoundData()[, input$compoundRowNamesColumn],
     "mz_old" = as.numeric(
       gsub(",", ".", compoundData()[, input$compoundMzValueColumn])
     ),
     "mz_new" = as.numeric(
       gsub(",", ".", calculated_mz)
     )
    )
    
    # Plot ----------------------
    # Calculate linear regression
    lm_model <- lm(mz_old ~ mz_new, data = dtCalculated)
    
    # Calculate R squared
    r_squared <- summary(lm_model)$r.squared
    
    # Get intercept and slope
    intercept <- coef(lm_model)[1]
    slope <- coef(lm_model)[2]
    
    # Create the interactive chart with plotly
    MZplot <- plot_ly(
      data = dtCalculated, 
      x = ~mz_new, 
      y = ~mz_old, 
      text = ~ID, 
      name = "(mz_F, mz) ID",
      type = 'scatter', 
      mode = 'markers'
    )
    
    # Add the regression line to the graph
    MZplot <- add_trace(
      MZplot, 
      type = "scatter", 
      mode = "lines",
      x = dtCalculated$mz_new,
      y = fitted(lm_model),
      line = list(color = 'red'),
      name = paste(
        "R^2: ", 
        format(r_squared, digits = 4), 
        "<br>Intercept: ", 
        format(intercept, digits = 4), 
        "<br>Slope: ", 
        format(slope, digits = 4))
    )
    
    # Customize the chart layout
    MZplot <- layout(MZplot, title = "Records of mz vs mz by formula",
                     xaxis = list(title = "Mz by formula (mz_F)"),
                     yaxis = list(title = "Records of mz (mz)"))
    
    MZplot
  })
  
  output$mzCalculatedPlot <- renderPlotly({
    mz_plot()  # Llamamos a la función reactivo dentro del renderPlotly
  })
  
  
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
      if (input$mzByFormula){
        mzCompound <- dtCalculated$mz_new[i]
      }else{
        mzCompound <- dtCalculated$mz_old[i]
      }
      rtCompound <- compoundData()[i,input$compoundRtValueColumn]
      if ( is.na(rtCompound) ){
        rtCompound <- ""
      }
      fragCompound <- compoundData()[i,input$compoundFragmentsColumn]
      if ( is.na(fragCompound) ){
        fragCompound <- ""
      }
      
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
        
        class = "display",
        filter = 'top'
      )
      
      dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) 
      dt
    })
    
    output$compound_graph_possibility <- renderUI({
      HTML('<span style="color: #4372AA; font-weight: bold;">Select rows 
        from the table to display the intensity graph by samples.</span>')
    })
    
    # Plot selected rows
    output$compoundSelectedPlot <- renderPlotly({
      selected <- input$summaryCompound_rows_selected
      if (length(selected) == 0) return(NULL)
      
      names <- compoundResult[selected, 'Row.names']
      
      data_filtered <- data() %>%
        filter(pull(., input$rowNamesColumn) %in% names)
      samColumns <- as.numeric(
        strsplit(input$samplesColumn, "-")[[1]]
      )
      data_filtered <- data_filtered[,c(input$rowNamesColumn,
                                        samColumns[1]:samColumns[2])]
      
      transposed_data <- data_filtered %>%
        pivot_longer(cols = -1, names_to = "Variable", values_to = "Valor")
      colnames(transposed_data) <- c("Names", "Samples", "Value")
      
      plot_ly(transposed_data, x = ~Samples, y = ~Value, 
              color = ~factor(Names), type = 'bar') %>%
        layout(barmode = 'group',
               xaxis = list(title = "Samples"),
               yaxis = list(title = "Intensity"),
               title = "Intensity of samples in selected rows")
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