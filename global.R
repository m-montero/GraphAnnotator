

# project template
library(shiny)
library(ggplot2)
# library(yaml)
# library(jsonlite)

## Project Specific - from module
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# library(dplyr)
library(grDevices)

#project specific
library(colourpicker)
library(shinyAce)
# library(datasets)

# save ggplot images svg
library(svglite)
library(gdtools)
library(systemfonts)


# common Sanofi Shiny template
if(dir.exists("resources")) source("resources/shiny.R")

# project/app specific global variable options
options(
  SANOFI_APP_DEBUG = FALSE
)


#### project specific ----------------------------------------------------------
# boiler plate - Custom Functions
collapse <- function(..., .fn = '+', .dots = list()){

  # dots
  dots <- list(...)
  dots <- c(dots, .dots)

  # checks
  if(isTRUE(length(dots) <= 1)) return(dots)

  # collapse
  out <- dots[[1]]
  for(i in seq_along(dots)[-1]){
    out <- list(.fn = .fn,
                out,
                dots[[i]])
  }

  # return
  return(out)
}

call3 <- function(.fn, ..., .ns = NULL){
  # call
  out <- rlang::call2(.fn = .fn, ..., .ns = .ns)
  # handle symbols
  if(isTRUE(.fn == 'as.name')) out <- eval(expr = out)
  # return
  return(out)
}

r_call3 <- function(x){
  # `x` is a list
  if(isTRUE(is.list(x)) & isFALSE(is.data.frame(x))){
    # at least one element of `x` is a list
    if(any(sapply(X = x, FUN = function(y) is.list(y)))){
      # recursive
      x <- lapply(x, FUN = function(y) r_call3(y))
      # do.call forces evaluation
      .fn <- x[['.fn']]
      x[['.fn']] <- NULL
      return(call3(.fn = .fn, !!!x))
    } else {
      # x is a list, but has no elements that are also a list
      return(do.call('call3', x))
    }
  }
  # non-list objects (e.g. character, numeric)
  return(x)
}


