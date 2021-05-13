function(input, output, session) {

  rv <- reactiveValues()
  gi <- reactiveValues()

  # initialization (runs once)
  observe({
    isolate({
      # app specific initialization
      rv$counter <- list()
      rv$annotateLayers <- list()
    })
  })

  #################################################################################################################
  # Tab 1 - Upload Graph / Choose Demo graph ----
  #################################################################################################################

  observeEvent(eventExpr = input$upload_demo_selector, handlerExpr = {
    cat('demo - updating gg_base\n')
    gg_file <- file.path('data',paste0('ex_', input$upload_demo_selector,'.Rds'))
    gg <- readRDS(file = file.path('data',paste0('ex_', input$upload_demo_selector,'.Rds')))

    rv$gg_base_file <- gg_file
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

  observeEvent(eventExpr = input$upload_rds, handlerExpr = {
    cat('rds - updating gg_base\n')
    gg_file <- input$upload_rds$datapath
    gg <- readRDS(file = gg_file)

    rv$gg_base_file <- gg_file
    rv$gg_base <- gg
  })

  observeEvent(eventExpr = rv$gg_base, handlerExpr = {
    # plot tab 1 Demo
    output$plot_tab1 <- renderPlot({
      try_plot <- rv$gg_base
      try_plot <- try(try_plot, silent = TRUE)
      try_plot_print <- try(print(try_plot), silent = TRUE)
      if(inherits(x = try_plot_print, what = 'try-error')) {
        try_plot <- ggplot() + theme_bw() + xlim(0,1) + ylim(0,1) + annotate(geom="text",label="Are all required input values populated?", x = 0.5, y = 0.5, size = 6) + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
      }
      try_plot
    })
  }) # close observeEvent

  #################################################################################################################
  # Tab 2 - Visualize ----
  #################################################################################################################

  # show plot before `add layer` button is clicked
  observe({
    if (isTRUE(is.null(rv$counter$no_of_layers)) || rv$counter$no_of_layers == 0) {
      rv$ggView <- rv$gg_base
      rv$ggPublish <- rv$gg_base
    }
  })

  # code handler when RDS plot upload/select is changed in middle of workflow
  observeEvent(rv$gg_base,{

    if(is.null(rv$counter$no_of_layers)) rv$counter$no_of_layers <- 0

    rv$ggView <- rv$gg_base

    # remove all layers and captures
    if(rv$counter$no_of_layers > 0){
      lapply(rev(seq_along(1:rv$counter$no_of_layers)), function(layer_id){

        removeUI(
          selector = paste0("#add_layer_",layer_id),
          immediate = TRUE
        )

        # remove layer capture
        if(isFALSE(is.null(rv$capture[[paste0("annotateResult",layer_id)]]))){
          rv$capture[[paste0("annotateResult",layer_id)]] <- NULL
          # gi[[paste0("result",layer_id)]] <- NULL
        }
      })
    }

    # reset layer counter
    rv$counter$no_of_layers <- 0

  }, ignoreInit = TRUE)

  # enable/disable remove layer action button
  observe({
    if (isTRUE(is.null(rv$counter$no_of_layers)) || rv$counter$no_of_layers == 0) {
      sanofiJS$disable(id = "remove_layer")
    } else
      sanofiJS$enable(id = "remove_layer")
  })

  # capture plot mouse events - tab 2 plot
  observe({
    # tab2 plot capture
    rv$plot_click <- list(x = input[["plot_click_t1"]]$x, y = input[["plot_click_t1"]]$y)
    rv$plot_dblclick <- list(x = input[["plot_dblclick_t1"]]$x, y = input[["plot_dblclick_t1"]]$y)
    rv$plot_hover <- list(x = input[["plot_hover_t1"]]$x, y = input[["plot_hover_t1"]]$y)
    rv$plot_brush <- list(xmin = input[["plot_brush_t1"]]$xmin, xmax = input[["plot_brush_t1"]]$xmax,
                          ymin = input[["plot_brush_t1"]]$ymin, ymax = input[["plot_brush_t1"]]$ymax)
  })

  # Add plot layer
  observeEvent(input$add_layer, {

    if (input$remove_layer[[1]] == 0 & is.null(rv$counter$no_of_layers)){
      rv$counter$add_layer <- input$add_layer[[1]]
      layer_id <- rv$counter$add_layer
      rv$counter$no_of_layers <- rv$counter$add_layer
    } else {
      layer_id <- rv$counter$no_of_layers + 1
      rv$counter$no_of_layers <- layer_id
    }


    # Console print layer sequence
    if(isTRUE(getOption("SANOFI_APP_DEBUG"))) {
      cat("\nLayer: ", layer_id, "\n")
    }

    # UI for Adding type of plot or geom layer
    insertUI(
      selector = "#add_layer",
      where = "beforeBegin",
      immediate = TRUE,
      ui = tags$div(
        id = paste0("add_layer_",layer_id),
        fluidRow(
          column(
            width = 12,
            style = 'padding-bottom: 7px;',
            tags$b(paste('Layer', layer_id))
          )
        ),
        fluidRow(
          column(width = 12,
                 style = 'margin-left: 10px;',
                 annotateLayerUI(id = paste0("layer_", layer_id))
          )),
        hr()
      )
    )


  }, ignoreInit = TRUE)

  ## Remove Layer button
  observeEvent(input$remove_layer,{

    rv$counter$rm_layer <- rv$counter$no_of_layers
    layer_id <- rv$counter$rm_layer
    rv$counter$no_of_layers <- layer_id - 1

    # remove last annotate layer
    removeUI(
      selector = paste0("#add_layer_",layer_id),
      immediate = TRUE
    )

    # remove last layer capture
    if(isFALSE(is.null(rv$capture[[paste0("annotateResult",layer_id)]]))){
      rv$capture[[paste0("annotateResult",layer_id)]] <- NULL
      # gi[[paste0("result",layer_id)]] <- NULL
    }

  })

  ## add layer - module servers
  observe({

    if (isFALSE(is.null(rv$counter$no_of_layers))) {
      if (rv$counter$no_of_layers > 0) {

        lapply(seq_along(1:rv$counter$no_of_layers), function(layer_id){

          if(layer_id == rv$counter$no_of_layers)
            if(isTRUE(is.null(gi[[paste0("result",layer_id)]]))){
              if(isFALSE(is.null(input[[paste0("layer_", layer_id,"-geom_type")]]))) {

                gi[[paste0("result",layer_id)]] <- annotateLayerServer(
                  id = paste0("layer_", layer_id),
                  plotClick = reactive(rv$plot_click),
                  plotDblClick = reactive(rv$plot_dblclick),
                  plotHover = reactive(rv$plot_hover),
                  plotBrush = reactive(rv$plot_brush),
                  plotName = "plot_tab2"
                )
              }
            }

        }) # close lapply
      }
    }
  }) # close observe

  observe({
    if (isFALSE(is.null(rv$counter$no_of_layers))) {
      if (rv$counter$no_of_layers > 0) {

        lapply(seq_along(1:rv$counter$no_of_layers), function(layer_id){
          if(isFALSE(is.null(gi[[paste0("result",layer_id)]]))){
            rv$capture[[paste0("annotateResult",layer_id)]] <- gi[[paste0("result",layer_id)]]()
          }
        })

      }
    }
  })

  # observe({
  observeEvent(rv$capture,{

    if (isFALSE(is.null(rv$counter$no_of_layers))) {
      # if (rv$counter$no_of_layers > 0) {
      if (rv$counter$no_of_layers >= 0) {

        annotateLayers <- lapply(rv$capture, function(ele_i) {
          if (isFALSE(is.null(ele_i))) {
            if (length(ele_i) > 2) {
              base::eval(r_call3(ele_i))
            }
          }
        })

        rv$codeRaw <- lapply(rv$capture, function(ele_i) {
          if (isFALSE(is.null(ele_i))) {
            if (length(ele_i) > 2) {
              ele_i
            }
          }
        })

        if (length(annotateLayers) > 0)
          rv$ggView <- rv$gg_base + annotateLayers
        else
          rv$ggView <- rv$gg_base
      }
    }

  }) # close observe

  ## output plot tab 2
  output$plot_tab2 <- renderPlot({
    # rv$ggView
    try_plot <- rv$ggView
    try_plot <- try(try_plot, silent = TRUE)
    try_plot_print <- try(print(try_plot), silent = TRUE)
    isolate({
      if(inherits(x = try_plot_print, what = 'try-error')) {
        cat('plot error\n')
        try_plot <- rv$gg_prev
      } else {
        rv$gg_prev <- rv$ggView
        rv$ggPublish <- rv$ggView
      }
    })
    try_plot
  })


  #################################################################################################################
  # Tab 3 - Publish ----
  #################################################################################################################

  ## show stable ggplot2::last_plot(ggPublish) under Tab 3 Publish section
  output$plot_modal_dialog_box <- renderPlot({
    rv$ggPublish
  })

  ## code handler - generate rv$code
  # observeEvent(rv$codeRaw,{
  observeEvent(input$tabs_top_header,{

    if(length(rv$codeRaw) >= 1 && isFALSE(all(sapply(rv$codeRaw, is.null)))){
      codeRaw <- r_call3(collapse(.dots = c(list(list(.fn = 'as.name', 'gg')),rv$codeRaw),
                                  .fn = '+'))
    } else codeRaw <- r_call3(list(.fn = 'as.name', 'gg'))

    packages_string <- "library(ggplot2)"
    base_string <- paste0("gg <- readRDS(file = '",basename(rv$gg_base_file),"')")
    annotate_string <- deparse(codeRaw)

    ## code output
    rv$code <- c('# packages',
                 packages_string,
                 '',
                 '# load base graph',
                 base_string,
                 '',
                 '# append annotations(s) to base graph, if any',
                 annotate_string)

  }, ignoreInit = TRUE) # close of observeEvent


  ## Code View tab under Tab 3 Publish section - Code
  # observeEvent(rv$code,{
  observeEvent(input$tab3_Publish_id,{

    if(input$tabs_top_header == "tab3_Publish"){
      if(input$tab3_Publish_id == "tab3_PublishCode"){

        output$code_display_publish <- renderUI({

          shiny::showNotification("Rendering Code...", id = "renderCodeNotification",
                                  duration = 2, type = "message", closeButton = TRUE)

          aceEditor(outputId = paste0("gg_plot_code_modal"),
                    height = "500px",
                    value = rv$code,
                    mode = "r",
                    theme = "github", #getAceThemes()
                    readOnly = TRUE,
                    showLineNumbers = TRUE, highlightActiveLine = TRUE
          )
        })
      }
    }

  }, ignoreInit = TRUE)

  # Download Code button - Tab 3
  output$download_plot_code <- downloadHandler(

    filename = function() {
      paste0('ggplot_plot_code','.R')
    },
    content = function(file) {
      writeLines(rv$code, con = file)
    }
  )

} # close of server function


