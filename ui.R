
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
      ),
      
      
      br(),
      
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
      ),
      
      br(),
      
      checkboxInput(
        "axis_inc_input", 
        "Include composition axes", 
        value = TRUE
      ),
      
      # checkboxInput(
      #   "geo_mean_input", 
      #   "Include sample mean", 
      #   value = TRUE
      # ),
      

      br(),

      tags$fieldset(
        tags$p(
          "Note:",
          style = "font-size: 10pt;"
        ), 
        tags$ul(
          tags$li(strong("sleep:"), "hours spent in sleep"),
          tags$li(strong("sb:"), "hours spent in sedentary behaviour"),
          tags$li(strong("lpa:"), "hours spent in light physical activity"),
          tags$li(strong("mvpa:"), "hours spent in moderate-to-vigorous physical activity"),
          tags$li(strong("sleep + sb + lpa + mvpa = 24 hours"))
        ),
        style = "font-size: 9pt;"
      ),
      
      br(),
      br(),
      
      p(
        "Best graphical display requires window width >1000px",
        style = "font-size: 9pt;"
      )



    ),


    mainPanel(


      tabsetPanel(
        type = "tabs",

        

        tabPanel(
          "Fat mass (%)",
          br(),
          p(
            "Please click and drag to rotate and scroll to zoom", 
            style = "font-size: 9pt;"
          ),
          plotlyOutput("plot1", height = px_h_plot, width = px_w_plot)
        ),
        
        tabPanel(
          "Visceral adipose tissue (sq. cm)",
          br(),
          br(),
          p("under construction")
          # plotlyOutput("plot2", height = px_h_plot, width = px_w_plot)
        ),
        

      )


    )


  )


)
