sanofiNavbarPage(

  title = "Graph Annotator",
  altTitle = '',
  id = "tabs_top_header",

  header = tags$head(
    # css
    tags$link(rel = 'stylesheet',
              type = 'text/css',
              href = 'code/custom.css'),
    # javascript
    tags$script(type = 'text/javascript',
                src = 'code/custom.js')),

  # Tab 0 - Introduction ---
  tabPanel(title = tagList(tags$span(class = 'tab-title-number', "0"), tags$span('Introduction')),
           value = "tab0_intro",
           includeMarkdown('code/intro.Rmd'),
           br(),
           br()),

  # Tab 1 - Upload Graph -----
  tabPanel(title = tagList(tags$span(class = 'tab-title-number', "1"), tags$span('Upload Graph')),
           value = "tab1_upload_data",
           column(width = 12,
                  fluidRow(
                    tags$h2('How do you want to upload your graph?'),
                    column(width = 4,
                           tags$br(),

                           tabsetPanel(
                             id = "tabs_upload_data",
                             tabPanel(title = 'Demo', value = 'upload_demo',
                                      tags$br(),
                                      tags$h3('Demo Examples'),
                                      tags$br(),
                                      column(width = 12,
                                             fluidRow(tags$p('If you just want to try Graph Annotator, select from the list of example graphs on the right.'),
                                                      selectInput(inputId = 'upload_demo_selector',
                                                                  label = 'Select a demo',
                                                                  choices = c('Example 1 - Scatter Plot' = 1,
                                                                              'Example 2 - Bar Plot' = 2,
                                                                              'Example 3 - Box Plot' = 3),
                                                                  selectize = FALSE,
                                                                  size = 3),
                                                      downloadButton(outputId = "download_demo_rds", label = "Example (.Rds)")
                                             ))),
                             tabPanel(title = 'RDS Upload', value = 'upload_rds', icon = icon(name = 'r-project'),
                                      tags$br(),
                                      tags$h3('RDS Upload'),
                                      tags$br(),
                                      column(width = 12,
                                             fluidRow(
                                               tags$p('Upload your own custom ', tags$code('ggplot2'),'object!'),
                                               tags$p(' See below for an example on HOW to create the Rds file, a stored single R object, which is used to create Example 1 from the Demo tab.'),
                                               aceEditor(
                                                 outputId = "upload_rds_example",
                                                 mode = 'r',
                                                 wordWrap = TRUE,
                                                 height = "75px",
                                                 readOnly = TRUE,
                                                 value = "library(ggplot2)\ngg <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_classic()\nsaveRDS(gg, file = 'ex.Rds')"
                                               ),
                                               tags$br(),
                                               fileInput(inputId = "upload_rds",
                                                         label = "Choose Rds File",
                                                         multiple = FALSE,
                                                         accept = c(".rds",
                                                                    ".RDS",
                                                                    ".Rds")),
                                               helpText('Note, only native ', tags$code('ggplot2'), ' code is currently supported.'),
                                             )
                                      )
                             )
                           )
                    ),
                    column(width = 8,
                           plotOutput(outputId = 'plot_tab1'))
                  )
           )
  ),

  # Tab 2 - Visualize -----
  tabPanel(title = tagList(tags$span(class = 'tab-title-number', "2"), tags$span('Visualize')),
           value = "tab2_Visualize",
           column(width = 4,
                  fluidRow(includeMarkdown('code/visualize.Rmd'),
                           br(),
                           h4('Annotate Layer(s) Mapping')),
                  fluidRow(
                    style = 'padding-bottom: 3px; padding-top: 0px; padding-left: 5px; padding-right: 5px;',
                    tags$div( #class = 'glassmorphism',
                      style = 'padding:0px; padding-bottom: 10px;', align = 'left',
                      actionButton(inputId = "add_layer",
                                   label = "Add Layer",
                                   icon = icon("plus")),
                      actionButton(inputId = "remove_layer",
                                   label = "Remove Layer",
                                   icon = icon("minus"))
                    )
                  )
           ),

           column(
             width = 8,
             br(),
             br(),
             plotOutput(
               "plot_tab2",
               click = "plot_click_t1",
               dblclick = dblclickOpts(id = "plot_dblclick_t1",
                                       delay = 300),
               hover = hoverOpts(id = "plot_hover_t1",
                                 delay = 100,
                                 delayType = "debounce"),
               brush = brushOpts(id = "plot_brush_t1",
                                 delay = 200,
                                 delayType = "debounce",
                                 # fill = "white",
                                 fill = "rgb(255 255 255 / 15%)",
                                 stroke = "black",
                                 opacity = 0.25)
             )
           )
  ),

  # Tab 3 - Publish -----
  tabPanel(title = tagList(tags$span(class = 'tab-title-number', "3"), tags$span('Publish')),
           value = "tab3_Publish",
           column(width = 4,
                  fluidRow(includeMarkdown('code/publish.Rmd'),
                           br(),
                           downloadButton(outputId = 'download_plot_code', label = "R Code"))),
           column(width = 8,
                  br(),
                  br(),
                  fluidRow(
                    tabsetPanel(
                      # type = "pills",
                      id = "tab3_Publish_id",
                      tabPanel(
                        title = 'Plot',
                        value = "tab3_PublishPlot",
                        tags$br(),
                        fluidRow(
                          column(width = 12,
                                 plotOutput(outputId = 'plot_modal_dialog_box'))
                        )
                      ), # tabPanel 1

                      tabPanel(
                        title = 'Code',
                        value = "tab3_PublishCode",
                        tags$br(),

                        fluidRow(
                          column(width = 12,
                                 style = 'padding-top: 5px;',
                                 uiOutput("code_display_publish")
                          )
                        )
                      ) # tabPanel 2
                    ) # tabsetPanel
                  ))# fluidRow above tabsetPanel

  ),

  # Tab Widgets ----
  tabPanel(title = span(a(title = 'Rstudio Cloud', href="https://rstudio.cloud/spaces/143406/project/2544541",target="_blank",icon('cloud')),
                        a(title = 'GitHub', href="https://github.com/m-montero/GraphAnnotator",target="_blank",icon('github'))),
           value = "sanofiRightTabPanelLinks")


) # close sanofiNavbarPage

