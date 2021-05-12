function(input, output, session) {

  rv <- reactiveValues()



  # Add plot layer
  # observeEvent(input$add_layer, {
  #
  #   if (input$remove_layer[[1]] == 0 & is.null(rv$counter$no_of_layers)){
  #     rv$counter$add_layer <- input$add_layer[[1]]
  #     layer_id <- rv$counter$add_layer
  #     rv$counter$no_of_layers <- rv$counter$add_layer
  #   } else {
  #     layer_id <- rv$counter$no_of_layers + 1
  #     rv$counter$no_of_layers <- layer_id
  #   }
  #
  #
  #   # Console print layer sequence
  #   if(isTRUE(getOption("SANOFI_APP_DEBUG"))) {
  #     cat("\nLayer: ", layer_id, "\n")
  #   }
  #
  #   # UI for Adding type of plot or geom layer
  #   insertUI(
  #     selector = "#add_layer",
  #     where = "beforeBegin",
  #     immediate = TRUE,
  #     ui = tags$div(
  #       id = paste0("add_layer_",layer_id),
  #       fluidRow(
  #         column(
  #           width = 3, style = 'padding-bottom: 7px;', align = 'left',
  #           tags$b(paste('Layer', layer_id))
  #         )
  #       ),
  #       fluidRow(
  #         annotateLayerUI(id = paste0("layer_", layer_id))
  #       ),
  #       ## using geomLayer UI module
  #       hr()
  #     )
  #   )
  #
  #
  # }, ignoreInit = TRUE)
  #
  # # Remove Layer
  # observeEvent(input$remove_layer,{
  #
  #   # if (input$remove_layer[[1]] == 1){
  #     rv$counter$rm_layer <- rv$counter$no_of_layers
  #     layer_id <- rv$counter$rm_layer
  #     rv$counter$no_of_layers <- layer_id - 1
  #   # } else {
  #   #   rv$counter$rm_layer <- rv$counter$no_of_layers
  #   #   layer_id <- rv$counter$rm_layer
  #   #   rv$counter$no_of_layers <- layer_id - 1
  #   # }
  #
  #   # fix for more than 1 input clicks
  #   removeUI(
  #     selector = paste0("#add_layer_",layer_id),
  #     immediate = TRUE
  #   )
  #
  # })


  # capture plot mouse events
  # observe({
  #   # tab1 plot capture
  #   rv$plot_click <- list(x = input[["plot_click_t1"]]$x, y = input[["plot_click_t1"]]$y)
  #   rv$plot_dblclick <- list(x = input[["plot_dblclick_t1"]]$x, y = input[["plot_dblclick_t1"]]$y)
  #   rv$plot_hover <- list(x = input[["plot_hover_t1"]]$x, y = input[["plot_hover_t1"]]$y)
  #   rv$plot_brush <- list(xmin = input[["plot_brush_t1"]]$xmin, xmax = input[["plot_brush_t1"]]$xmax,
  #                         ymin = input[["plot_brush_t1"]]$ymin, ymax = input[["plot_brush_t1"]]$ymax)
  # })


  # observeEvent(rv$counter$no_of_layers,{
  #
  #   lapply(seq_along(1:rv$counter$no_of_layers), function(layer_id){
  #
  #     rv$layerCapture[[paste0("result", layer_id)]] <- annotateLayerServer(
  #                                                       id = paste0("layer_", layer_id),
  #                                                       plotClick = reactive(rv$plot_click),
  #                                                       plotDblClick = reactive(rv$plot_dblclick),
  #                                                       plotHover = reactive(rv$plot_hover),
  #                                                       plotBrush = reactive(rv$plot_brush),
  #                                                       plotName = "plot_tab2"
  #                                                     )
  #   })
  #
  # }, ignoreInit = TRUE)


  # plot read/load
  # gg1 <- reactive({ggplot(mtcars, aes(wt, mpg)) + geom_point()})


  # observe({
  # # observeEvent(rv$layerCapture,{
  #
  #   if(isFALSE(is.null(rv$layerCapture))){
  #     rv$annotateResult1 <- rv$result1()
  #   }
  #
  #   # rv$annotateResult1 <- rv$result1()[setdiff(names(rv$result1()), grep("^plot.*", names(rv$result1()), value = T))]
  # })

  #################################################################################################################
  # Tab 1 - Upload Graph ----

  observeEvent(eventExpr = input$upload_demo_selector,
               handlerExpr = {
                 message('demo - updating gg_base')
                 gg <- readRDS(file = file.path('data',paste0('ex_', input$upload_demo_selector,'.Rds')))
                 rv$gg_base <- gg
               })

  output$download_demo_rds <- downloadHandler(
    filename = function() {
      'ex.Rds'
    },
    content = function(file) {
      file.copy(from = file.path('data',paste0('ex_', input$upload_demo_selector,'.Rds')),
                to = file)
    }
  )

  observeEvent(eventExpr = input$upload_rds,
               handlerExpr = {
                 message('rds - updating gg_base')
                 gg <- readRDS(file = input$upload_rds$datapath)
                 rv$gg_base <- gg
               })

  observeEvent(eventExpr = rv$gg_base,
               handlerExpr = {
                 output$plot_tab1 <- renderPlot({
                   try_plot <- rv$gg_base
                   try_plot <- try(try_plot, silent = TRUE)
                   try_plot_print <- try(print(try_plot), silent = TRUE)
                   if(inherits(x = try_plot_print, what = 'try-error')) {
                     try_plot <- ggplot() + theme_bw() + xlim(0,1) + ylim(0,1) + annotate(geom="text",label="Are all required input values populated?", x = 0.5, y = 0.5, size = 6) + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
                   }
                   try_plot
                 })
               })

  #################################################################################################################
  # Tab 2 - Visualzie -----
  observe({
    # tab1 plot capture
    rv$plot_click <- list(x = input[["plot_click_t1"]]$x, y = input[["plot_click_t1"]]$y)
    rv$plot_dblclick <- list(x = input[["plot_dblclick_t1"]]$x, y = input[["plot_dblclick_t1"]]$y)
    rv$plot_hover <- list(x = input[["plot_hover_t1"]]$x, y = input[["plot_hover_t1"]]$y)
    rv$plot_brush <- list(xmin = input[["plot_brush_t1"]]$xmin, xmax = input[["plot_brush_t1"]]$xmax,
                          ymin = input[["plot_brush_t1"]]$ymin, ymax = input[["plot_brush_t1"]]$ymax)
  })

  rv$result1 <- annotateLayerServer(id = "layer1",
                                    plotClick = reactive(rv$plot_click),
                                    plotDblClick = reactive(rv$plot_dblclick),
                                    plotHover = reactive(rv$plot_hover),
                                    plotBrush = reactive(rv$plot_brush),
                                    plotName = "plot_tab2")
  #
  observe({
    # str(rv$result1())

    rv$annotateResult1 <- rv$result1()
    # rv$annotateResult1 <- rv$result1()[setdiff(names(rv$result1()), grep("^plot.*", names(rv$result1()), value = T))]
  })

  # plot read/load
  # gg1 <- reactive({ggplot(mtcars, aes(wt, mpg)) + geom_point()})
  # gg1 <- reactive({readRDS(file = 'data/test.Rds')})

  observe({
    if(length(rv$annotateResult1) > 2){
      rv$ggView <- rv$gg_base + base::eval(r_call3(rv$annotateResult1))
    } else {
      rv$ggView <- rv$gg_base
    }
    str(r_call3(rv$annotateResult1))
  })

  #################################################################################################################

  output$plot_tab2 <- renderPlot({
    # gg1()
    # rv$ggView

    try_plot <- rv$ggView
    try_plot <- try(try_plot, silent = TRUE)
    try_plot_print <- try(print(try_plot), silent = TRUE)
    if(inherits(x = try_plot_print, what = 'try-error')) {
      try_plot <- ggplot() + theme_bw() + xlim(0,1) + ylim(0,1) + annotate(geom="text",label="Are all required input values populated?", x = 0.5, y = 0.5, size = 6) + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
    }
    try_plot
  })


} # close of server function


