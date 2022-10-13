
ui <- fluidPage(

  tags$head(
    tags$style(HTML("

     .multicol {

       -webkit-column-count: 2; /* Chrome, Safari, Opera */

       -moz-column-count: 2; /* Firefox */

       column-count: 2;

     }

   "))

  ),

  sidebarLayout(
    
    sidebarPanel(
      width = 3,

      # selectInput(
      #   "response_input",
      #   "Please select response variable",
      #   c("Visceral adipose tissue (sq. cm)" = "vat", "Fat mass (%)" = "fmp"),
      #   selected = "vat"
      # ),
      # 
      # br(),
      
      p("Graphical parameters of points", style = "font-size: 8pt;"),
      
      wellPanel(
        
        selectInput(
          "col_pal_input",
          "Colour palette",
          plotly_col_pals,
          selected = "Bluered"
        ),
        
        br(),
        
        sliderInput(
          "alpha_input", 
          "Alpha transparency of points", 
          min = 0, 
          max = 1,
          value = 0.4, 
          step = 0.1, 
          round = FALSE, 
          ticks = FALSE
        )
        
      ),
      
      
      br(),
      
      p("Graphical parameters of compositional mean", style = "font-size: 8pt;"),
      
      wellPanel(
      
        sliderInput(
          "mean_size_input", 
          "Size of sample compositional mean", 
          min = 0, 
          max = 30,
          value = 20, 
          step = 2, 
          round = FALSE, 
          ticks = FALSE
        ),
        
        radioButtons(
          "mean_shape_input", 
          "Shape of sample compositional mean",
          c("circle", "circle-open", "cross", "diamond", "diamond-open", "square", "square-open"), 
          "cross", 
          inline = FALSE
        )
      
      ),
      
      br(),
      
      p("Graphical parameters of axes", style = "font-size: 8pt;"),
      
      wellPanel(
      
        checkboxInput(
          "axis_inc_input", 
          "Include composition axes", 
          value = TRUE
        )
      
      )
      

    ),


    mainPanel(

      fluidRow(
        column(
          width = 6,
          
          wellPanel(
            tags$ul(
              tags$li("The plots are rendered using a single 1 Ghz processor, so may take up to 10 sec to appear"),
              tags$li("Best graphical display requires window width >1000px"),
              tags$li("Please click and drag to rotate plot or scroll to zoom in/out"),
                tags$li("Hover mouse over points to get compositional values and prediciton")
            ), 
            style = "font-size: 9pt;"
          )
        ),
        
        column(
          width = 6,
          
          wellPanel(
            # tags$p(
            #   "Note:",
            #   style = "font-size: 10pt;"
            # ), 
            tags$ul(
              tags$li(strong("sleep + SB + LPA + MVPA = 24 hours")),
              tags$li(strong("sleep:"), "hours spent in sleep"),
              tags$li(strong("SB:"), "hours spent in sedentary behaviour"),
              tags$li(strong("LPA:"), "hours spent in light physical activity"),
              tags$li(strong("MVPA:"), "hours spent in moderate-to-vigorous physical activity")
            ),
            style = "font-size: 9pt;"
          )
        )
        
      ),
      
      tabsetPanel(
        type = "tabs",

        

        tabPanel(
          HTML("Visceral adipose tissue (cm&sup2;)"),
          
          br(),
          
          plotlyOutput("plot1", height = px_h_plot, width = px_w_plot),
          
          wellPanel(
            p(
              strong("Figure 1:"),
              paste(
                "Quaternary diagram (tetrahedron) showing all observed compositions and",
                "predicted VAT among children as points within the tetrahedron (points", 
                "coloured by predicted VAT). Note that all 24-hour time-use compositions", 
                "are contained within the tetrahedron. Time-use compositional variables can",
                "be observed as the proportional distance of points between their respective", 
                "labelled vertex and opposing face (for example, the points in the sample are",
                "only a fraction of the distance towards the MVPA vertex showing the relatively", 
                "small amount of time spent in MVPA."
              )
            ),
            p(
              "CL Rasmussen, A Gába, T Stanford, J Dygrýn, D Dumuid, D Janda, and K Hron.", 
              em("The Goldilocks Day for healthy adiposity measures among children and adolescents."),
              "Manuscript to be submitted."
              # a(
              #   href = "https://linktobecreated.com", 
              #   "The Goldilocks Day for preventing excess adiposity among children and adolescents"
              # )
            ),
            p("Code freely available at:", 
              a(
                href = "https://github.com/tystan/czech_goldilocks", 
                "github.com/tystan/czech_goldilocks"
              )
            ),
            style = "font-size: 9pt;"
          )
          
        ),        
        
        
        tabPanel(
          "Fat mass (%)",
          
          br(),
          
          
          plotlyOutput("plot2", height = px_h_plot, width = px_w_plot),
          
          wellPanel(
            p(
              strong("Figure 2:"),
              paste(
                "Quaternary diagram (tetrahedron) showing all observed compositions and",
                "predicted FM% among adolescents as points within the tetrahedron (points", 
                "coloured by predicted FM%). Note that all 24-hour time-use compositions", 
                "are contained within the tetrahedron. Time-use compositional variables can",
                "be observed as the proportional distance of points between their respective", 
                "labelled vertex and opposing face (for example, the points in the sample are",
                "only a fraction of the distance towards the MVPA vertex showing the relatively", 
                "small amount of time spent in MVPA."
              )
            ),
            p(
              "CL Rasmussen, A Gába, T Stanford, J Dygrýn, D Dumuid, D Janda, and K Hron.", 
              em("The Goldilocks Day for healthy adiposity measures among children and adolescents."),
              "Manuscript to be submitted."
              # a(
              #   href = "https://linktobecreated.com", 
              #   "The Goldilocks Day for preventing excess adiposity among children and adolescents"
              # )
            ),
            p("Code freely available at:", 
              a(
                href = "https://github.com/tystan/czech_goldilocks", 
                "github.com/tystan/czech_goldilocks"
              )
            ),
            style = "font-size: 9pt;"
          )
          
        )
        

      )


    )


  )


)
