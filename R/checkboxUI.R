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
      icon = icon,
      width = width
    )
  )
}