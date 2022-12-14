

# server <- function(input, output, session) {
shinyServer(function(input, output, session) {



  pal_select <- reactive({
    input$col_pal_input
  })
  
  axis_inc_select <- reactive({
    input$axis_inc_input
  })
  
  mean_size_select <- reactive({
    input$mean_size_input
  })
  
  mean_shape_select <- reactive({
    input$mean_shape_input
  })
  
  alpha_select <- reactive({
    input$alpha_input
  })
  
  
  output$plot1 <- renderPlotly({

    
    vfa_plot <- 
      plot_4_comp(
        vfa %>%
          mutate(
            Sleep = 24 * Sleep,
            SB = 24 * SB,
            LPA = 24 * LPA,
            MVPA = 24 * MVPA
          ), 
        "Sleep", "SB", "LPA", "MVPA", 
        col = "VAT", 
        alpha = alpha_select(),
        pal = pal_select()
      )
    
    

    if (axis_inc_select()) {
      vfa_plot <- 
        vfa_plot %>%
        add_axis(., tetra_axis_lst_hrs[[1]]) %>%
        add_axis(., tetra_axis_lst_hrs[[2]]) %>%
        add_axis(., tetra_axis_lst_hrs[[3]]) %>%
        add_axis(., tetra_axis_lst_hrs[[4]]) 
    }
    
    if (mean_size_select() > 0) {
      vfa_plot <- 
        vfa_plot %>%
        add_trace(
          type = "scatter3d",
          mode = "markers",
          data = vfa_mean_tetra_coord,
          x = ~x, 
          y = ~y, 
          z = ~z, 
          hovertext = ~obs_labs,
          hoverinfo = "text",
          color = I("black"),
          showlegend = FALSE,
          opacity = 1,
          marker = 
            list(
              size = mean_size_select(), 
              symbol = mean_shape_select()
            ) # "circle-open"
        ) 
    }
    

    # print(vfa_plot$x$cur_data)
    # print(vfa_plot$x$visdat)
    # print(vfa_plot$x$layoutAttrs[[vfa_plot$x$cur_data]])
    
    # style(
    #   vfa_plot,
    #   marker = list(
    #     # coloraxis = "coloraxis",
    #     color = ~col,
    #     showscale = TRUE,
    #     colorscale = pal_select(),
    #     colorbar = list(
    #       len = 0.5,
    #       title = list(
    #         text = col,
    #         font = list(
    #           size = 20
    #         )
    #       )
    #     )
    #   ),
    #   traces = 0
    # )
    
    
    vfa_plot
    

  })


  output$plot2 <- renderPlotly({
    
    
    fmp_plot <- 
      plot_4_comp(
        fmp %>%
          mutate(
            Sleep = 24 * Sleep,
            SB = 24 * SB,
            LPA = 24 * LPA,
            MVPA = 24 * MVPA
          ), 
        "Sleep", "SB", "LPA", "MVPA", 
        col = "FM%", 
        alpha = alpha_select(),
        pal = pal_select()
      )
    
    
    
    if (axis_inc_select()) {
      fmp_plot <- 
        fmp_plot %>%
        add_axis(., tetra_axis_lst_hrs[[1]]) %>%
        add_axis(., tetra_axis_lst_hrs[[2]]) %>%
        add_axis(., tetra_axis_lst_hrs[[3]]) %>%
        add_axis(., tetra_axis_lst_hrs[[4]]) 
    }
    
    if (mean_size_select() > 0) {
      fmp_plot <- 
        fmp_plot %>%
        add_trace(
          type = "scatter3d",
          mode = "markers",
          data = fmp_mean_tetra_coord,
          x = ~x, 
          y = ~y, 
          z = ~z, 
          hovertext = ~obs_labs,
          hoverinfo = "text",
          color = I("black"),
          showlegend = FALSE,
          opacity = 1,
          marker = 
            list(
              size = mean_size_select(), 
              symbol = mean_shape_select()
            ) # "circle-open"
        ) 
    }
    
    
    # print(fmp_plot$x$cur_data)
    # print(fmp_plot$x$visdat)
    # print(fmp_plot$x$layoutAttrs[[fmp_plot$x$cur_data]])
    
    # style(
    #   fmp_plot,
    #   marker = list(
    #     # coloraxis = "coloraxis",
    #     color = ~col,
    #     showscale = TRUE,
    #     colorscale = pal_select(),
    #     colorbar = list(
    #       len = 0.5,
    #       title = list(
    #         text = col,
    #         font = list(
    #           size = 20
    #         )
    #       )
    #     )
    #   ),
    #   traces = 0
    # )
    
    
    fmp_plot
    
    
  })
  
  # outputOptions(output, "audit", suspendWhenHidden = FALSE)


  # ---- close_app_action ----

  # Close the R session when browser closes
  # session$onSessionEnded(function() {
  #   stopApp()
  # })



})

