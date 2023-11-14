tab_calculateMZ <- tabPanel(
  "Calculate mz",
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
        # m option -------------------------------------------------------------
        textInput(
          "m", 
          h4("m:"), 
          value = "C7H7O"
        )
      ),
             
      
      
      fluidRow(
        # z option -------------------------------------------------------------
        column(
          5,
          radioButtons(
            "z", 
            h4("z:"),
            c("1"=1,
              "-1"=-1),
            1
          )
        ),
        
        # caseSensitive option -------------------------------------------------
        column(
          5,
          checkboxInput(
            "caseSensitive", 
            h4("caseSensitive"),
            TRUE
          )
        ),
      ),
             
      
      
      fluidRow(
        # Search button --------------------------------------------------------
        actionButton(
          "searchMZ", 
          "Search", 
          icon("search"), 
          style="color: #FFFFFF;
             background-color: #4372AA;
             border-color: #000000"
        )
      )
    ),
           
    
    
    mainPanel(
      h1("m/z Result:"),
      textOutput(
        "accurateMZ"
      )
    )
  )
)