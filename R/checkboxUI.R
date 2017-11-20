#' @title checkboxUI
#' @description Wrapper around shiny::checkboxInput
#' @param id A unique string id for the checkbox. If your code is modularised,
#'     remember to wrap the id with the relevant session namespace function, e.g.
#'     checkboxUI(id = session$ns("someinput")).
#' @param label A label for the checkbox. 
#' @param width The width of the entire shiny::checkbox UI element. Must be 
#'     valid css (i.e. '10\%' or '150px' will work).
#' @param custom_css If provided, the checkbox is wrapped in a div styled 
#'     by the custom_css input. For example, one could provide 
#'     custom_css = "display: inline-block;" to display the checkbox inline.
#' @export
checkboxUI <- function(id,
                       label = paste("Please provide a label argument to the",
                                         "checkbox with id = ", id, ".", 
                                     sep = ""),
                      icon = NULL,
                      width = NULL,
                      custom_css = NULL) {
  # Create a namespace that we encapsulate around IDs in the UI function.
  # This ensures that no UI elements of this type are duplicated, provided
  # we assign unique IDs to our UI elements.
  ns <- NS(id)
  
  # Create a shiny::checkboxInput with inputId ns("checkbox") wrapped in
  # a div (styled with the custom_css argument if provided)
  div(
    id = paste(ns("checkbox"), "-div", sep = ""),
    style = ifelse(!is.null(custom_css), custom_css, ""), 
    shiny::checkboxInput(
      inputId = ns("checkbox"), 
      label = label,
      width = width
    )
  )
}

#' @title checkboxUIs
#' @description Create a tagList of checkboxes using the checkboxUI 
#'     wrapper for shiny::checkboxInput. 
#' @param ids String id for the checkboxes. If your code is modularised,
#'     remember to wrap the id with the relevant session namespace function, e.g.
#'     checkboxUIs(id = sapply(c("someinput", "anotherinput", session$ns))).
#' @param labels Labels for the checkboxes. 
#' @param widths The widths of the checkboxes. Must be valid css (i.e. 
#'     "10\%" or "150px" will work; see shiny::validateCssUnit). To use the same 
#'     width on all checkboxes, provide a single-text string or 
#'     a vector with one text string. To use different widths for each checkbox,
#'     provide a vector of custom css text strings with one element per 
#'     action button.
#' @param custom_css If provided, the  checkboxes are wrapped in divs each
#'     styled by the custom_css input. For example, when creating multiple 
#'     checkboxes one could provide custom_css = "display: inline-block;" to 
#'     display them inline. To use the same style on all checkboxes, 
#'     provide a single-text string or single-element vector. To use different
#'     styles for each checkbox, provide a vector of custom css text strings
#'     with one element per checkbox.
#' @export
checkboxUIs <- function(ids,
                        labels = NULL,
                        icons = NULL,
                        widths = NULL,
                        custom_css = NULL) {
  
  # Create a div 
  ui.elements = list()
  
  for (i in 1:length(ids)) {
    # Create a custom actionButton using the actionButtonUI wrapper that 
    # ensures proper namespace handling.
    
    if (is.null(widths)) {
      width = NULL
    } else if (length(widths) == 1) {
      width = widths
    } else if (length(widths) > 1) {
      width = widths[i]
    }
    
    if (is.null(custom_css)) {
      custom_css = NULL
    } else if (length(custom_css) == 1) {
      custom_css = custom_css
    } else if (length(custom_css) > 1) {
      custom_css = custom_css[i]
    }
    
    ui.elements[[i]] = checkboxUI(
      id = ids[i],
      label = labels[i],
      width = width,
      custom_css = custom_css
    )
  }
  
  tagList(ui.elements)
}