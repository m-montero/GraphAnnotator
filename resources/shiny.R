# boiler plate

if(dir.exists("code")) shiny::addResourcePath(prefix = "code", directoryPath = "code")
if(dir.exists("data")) shiny::addResourcePath(prefix = "data", directoryPath = "data")

shiny::addResourcePath(prefix = "resources", directoryPath = "resources")

# wrappers

sanofiNavbarPage <- function(title, ..., id = NULL, altTitle = title, navTitle = NULL, windowTitle = title,
                             header = NULL, footer = NULL, theme = NULL, position = 'fixed-top'){
  # theme
  themeLogo <- NULL
  if(isTRUE(theme == 'art')){
    themeLogo <- tagList(tags$img(id = 'sanofi-header-logo-bp', src = 'resources/b&p_logo.svg'),
                         tags$span('Powered by'),
                         tags$img(id = 'sanofi-header-logo-art', src = 'resources/art_logo.svg'))
  }

  # position: only "static-top" and "fixed-top" are supported
  positionCSS <- NULL
  if(position == 'fixed-top'){
    positionCSS <- tags$link(rel = 'stylesheet',
                             type = 'text/css',
                             href = 'resources/shiny_navFixedTop.css')
  }

  # styled navigation page
  fluidPage(

    # enable MathJax
    withMathJax(),

    # css
    tags$link(rel = 'stylesheet',
              type = 'text/css',
              href = 'resources/shiny.css'),

    positionCSS,

    # javascript
    tags$script(type = 'text/javascript',
                src = 'resources/shiny_top.js'),

    # custom header
    tags$div(id = 'sanofi-header',
             tags$div(id = 'sanofi-header-left',
                      tags$div(id = 'sanofi-header-left-logo',
                               tags$img(src = 'resources/hexStickers_annotateLogoPencil.png')),
                      tags$div(id = 'sanofi-header-title',
                               title),
                      tags$div(id = 'sanofi-header-title-alt',
                               altTitle)),
             tags$div(id = 'sanofi-header-right',
                      tags$div(id = 'sanofi-header-right-logo',
                               themeLogo))
    ),

    # shiny
    navbarPage(title = navTitle,
               ...,
               id = id,
               position = position,
               windowTitle = windowTitle,
               header = header,
               footer = footer,
               inverse = TRUE,
               collapsible = TRUE),

    # javascript bottom
    tags$script(type = 'text/javascript',
                src = 'resources/shiny_bottom.js')

  )
}

sanofiAccordionPanel <- function(title, ..., collapse = FALSE, id = NULL){
  tags$div(id = id,
           class = 'sanofi-accordion',
           tags$button(
             id = paste0(id,'_button'),
             type = 'button',
             class = ifelse(collapse, 'sanofi-accordion-button sanofi-accordion-collapse', 'sanofi-accordion-button'),
             title),
           tags$div(class = ifelse(collapse, 'sanofi-accordion-panel sanofi-accordion-panel-collapse', 'sanofi-accordion-panel'),
                    ...)
  )
}

sanofiJS <- list()

# alert
sanofiJS$alert <- function(message, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage(type = 'sanofi_alert', message = list(message = message))
}

# disable/enable
sanofiJS$disable <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage(type = 'sanofi_disable_id', message = list(id = id))
}

sanofiJS$enable <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage(type = 'sanofi_enable_id', message = list(id = id))
}

# show/hide
sanofiJS$show <- function(id = NULL, class = NULL, display = 'inherit', session = shiny::getDefaultReactiveDomain()){
  if(!is.null(id)) session$sendCustomMessage(type = 'sanofi_display_id', message = list(id = id, display = display))
  if(!is.null(class)) session$sendCustomMessage(type = 'sanofi_display_class', message = list(class = class, display = display))
}
sanofiJS$hide <- function(id = NULL, class = NULL, session = shiny::getDefaultReactiveDomain()){
  if(!is.null(id)) session$sendCustomMessage(type = 'sanofi_display_id', message = list(id = id, display = 'none'))
  if(!is.null(class)) session$sendCustomMessage(type = 'sanofi_display_class', message = list(class = class, display = 'none'))
}

# visible/invisible
sanofiJS$visible <- function(id = NULL, class = NULL, visibility = 'inherit', session = shiny::getDefaultReactiveDomain()){
  if(!is.null(id)) session$sendCustomMessage(type = 'sanofi_visibility_id', message = list(id = id, visibility = visibility))
  if(!is.null(class)) session$sendCustomMessage(type = 'sanofi_visibility_class', message = list(class = class, visibility = visibility))
}
sanofiJS$invisible <- function(id = NULL, class = NULL, session = shiny::getDefaultReactiveDomain()){
  if(!is.null(id)) session$sendCustomMessage(type = 'sanofi_visibility_id', message = list(id = id, visibility = 'hidden'))
  if(!is.null(class)) session$sendCustomMessage(type = 'sanofi_visibility_class', message = list(class = class, visibility = 'hidden'))
}

# click
sanofiJS$click <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage(type = 'sanofi_click_id', message = list(id = id))
}

# collapse/un-collapse
sanofiJS$collapse <- function(id, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage(type = 'sanofi_collapse_id', message = list(id = id))
}

sanofiJS$uncollapse <- function(id, maxHeight = NULL, session = shiny::getDefaultReactiveDomain()){
  if(!is.null(maxHeight)){
    session$sendCustomMessage(type = 'sanofi_uncollapse_id_maxHeight', message = list(id = id, maxHeight = shiny::validateCssUnit(maxHeight)))
  } else {
    session$sendCustomMessage(type = 'sanofi_uncollapse_id', message = list(id = id))
  }
}

