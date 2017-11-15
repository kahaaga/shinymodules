#' Wrapper around shiny::numericInput.
#' @param id A unique string id for the numericInput. If your code is modularised,
#' remember to wrap the id with the relevant session namespace function, e.g.
#' numericValueUI(id = session$ns("someinput")).
#' @param label A label for the input.
#' @param value The default value.
#' @param min The smallest allowed value.
#' @param max The largest allowed value.
#' @param step The step size when incrementing.
#' @param width The width of the shiny::numericValue UI element. Must be valid
#' css (i.e. "10%" or "150px" will work).
#'@param custom_css The numericInput will be wrapped in a div() styled with
#' the css code provided as a string with the custom_css input. For example,
#' when creating multiple instances of the same type of UI element, one could
#' provide custom_css = "display: inline-block;" to display the different
#' elements inline.
#' @export
numericValueUI <- function(id,
        label = paste("Please provide a label argument to the numericInputUI",
                    " with id = ", id, ".", sep = ""),
        value = 0,
        min = value - 3*value,
        max = value + 3*value,
        step = (max-min)/100,
        width = "10%",
        custom_css = "") {
    # Create a namespace that we encapsulate around IDs in the UI function.
    # This ensures that no UI elements of this type are duplicated, provided
    # we assign unique IDs to our UI elements.
    ns <- NS(id)

    # Create a shiny::numericInput with inputId ns("slider"), so the inputId of
    # this numericInputUI is "id-slider". For example, if id = "cost", then
    # ns("numeric") gives a numericInputUI with inputId = "cost-numeric".
    shiny::numericInput(inputId = ns("numericinput"), label = label,
                        value = value,  min = min, max = max,
                        step = step, width = width)
}

#' Creates a tagList of numericInputUI instances.
#' @param ids A unique string id for the numericInput. If your code is modularised,
#' remember to wrap the id with the relevant session namespace function, e.g.
#' numericValueUI(id = session$ns("someinput")).
#' @param labels A label for the input.
#' @param values The default values.
#' @param minvals The smallest allowed values.
#' @param maxvals The largest allowed values.
#' @param steps The step sizes when incrementing.
#' @param widths The widths of the shiny::numericValue UI element. Must be valid
#' css (i.e. "10%" or "150px" will work).
#'@param custom_css The numericInput will each be wrapped in da iv() styled with
#' the css code provided as a string with the custom_css input. For example,
#' when creating multiple instances of the same type of UI element, one could
#' provide custom_css = "display: inline-block;" to display the different
#' elements inline.
#' @export
numericValueUIs <- function(ids,
                            labels = NULL,
                            values = NULL,
                            minvals = NULL,
                            maxvals = NULL,
                            steps = NULL,
                            widths = NULL,
                            custom_css = NULL) {
    ui.elements = list()

    for (i in 1:length(ids)) {
      ui.elements[[i]] = div(
        style = ifelse(!is.null(custom_css), custom_css, ""),
        numericValueUI(id = ids[i],
                      label = labels[i],
                      value = values[i],
                      min = minvals[i],
                      max = maxvals[i],
                      step = steps[i],
                      width = widths[i])
        )
    }

    tagList(ui.elements)
}
