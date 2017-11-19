#' Wrapper around shiny::numericInput.
#' @param id A unique string id for the numericInput. If your code is modularised,
#' remember to wrap the id with the relevant session namespace function, e.g.
#' numericValueUI(id = session$ns("someinput")).
#' @param width The width of the shiny::numericValue UI element. Must be valid
#' css (i.e. "10%" or "150px" will work).
#' @param icon An icon for the action button (see shiny::icon).
#' @param width The widths of the action buttons. Must be valid css.
#' @param custom_css If provided, the action button is wrapped in a div styled 
#' by the custom_css input. For example, one could provide 
#' custom_css = "display: inline-block;" to display the action button inline.
#' @export
actionButtonUI <- function(id,
                           label = paste("Please provide a label argument to the actionButton",
                                         " with id = ", id, ".", sep = ""),
                           icon = NULL,
                           width = NULL,
                           custom_css = NULL) {
  # Create a namespace that we encapsulate around IDs in the UI function.
  # This ensures that no UI elements of this type are duplicated, provided
  # we assign unique IDs to our UI elements.
  ns <- NS(id)
  
  # Create a shiny::actionButton with inputId ns("actionbutton") wrapped in
  # a div (styled with the custom_css argument if provided)
  div(style = ifelse(!is.null(custom_css), custom_css, ""), 
    shiny::actionButton(
      inputId = ns("actionbutton"), 
      label = label,
      icon = icon,
      width = width
    )
  )
}

#' Creates a tagList of action buttons wrapped in a (potentially styled) div.
#' @param ids A unique string id for the numericInput. If your code is modularised,
#' remember to wrap the id with the relevant session namespace function, e.g.
#' numericValueUI(id = session$ns("someinput")).
#' @param labels A label for the input.
#' @param icons Icons for the action buttons.
#' @param widths The widths of the action buttons. Must be valid
#' css (i.e. "10%" or "150px" will work).
#' @param custom_css If provided, the action buttons are wrapped in divs each
#' styled by the custom_css input. For example, when creating multiple action
#' buttons, one could provide custom_css = "display: inline-block;" to display 
#' the action buttons inline. To use the same style on all action buttons, 
#' provide a single-text string or single-element vector. To use different
#' styles for each action button, provide a vector of custom css text strings.
#' @examples 
#' actionButtonUIs(ids = c("trigger_calculation", "trigger_plot"))
#' actionButtonUIs(ids = c("trigger_calculation", "trigger_plot"), 
#' custom_css = "display: inline-block")
#' actionButtonUIs(ids = c("trigger_calculation", "trigger_plot"), 
#' custom_css = c("display: inline-block", "display: inline-flex")
#' @export
actionButtonUIs <- function(ids,
                            labels = NULL,
                            icons = NULL,
                            widths = NULL,
                            custom_css = NULL) {
  
  # Create a div 
  ui.elements = list()
  
  for (i in 1:length(ids)) {
    # Create a custom actionButton using the actionButtonUI wrapper that 
    # ensures proper namespace handling.
    ui.elements[[i]] = actionButtonUI(
      id = ids[i],
      label = labels[i],
      icon = icons[i],
      width = widths[i],
      custom_css = ifelse(length(custom_css) == 1, custom_css, custom_css[i])
      )
  }
  
  tagList(ui.elements)
}
