#'@export
sliderInputUI <- function(
        id, 
        value = 0,
        min = 0, 
        max = 100,
        step = 1,
        label = paste("Please provide a label argument to the sliderInputUI",
                    " with id = ", id, "."),
        width = "100%",
        custom_css = "",
        pre = "",
        post = ""
        ) {
    # Create a namespace that we encapsulate around IDs in the UI function.
    # This ensures that no UI elements of this type are duplicated, provided 
    # we assign unique IDs to our UI elements. 
    #
    # For example, if we created a sliderInput with inputId "slider" and 
    # id = "cost", then ns("slider") would return "cost-slider".
    ns <- NS(id) 
    
    # If we want to return more than one UI element, we need to place them in
    # a shiny::tagList. Otherwise, only the last element would be returned 
    # (the default behaviour of R functions).
    shiny::sliderInput(inputId = ns("slider"), 
                    label = label, 
                    value = value, 
                    min = min, 
                    max = max,
                    step = step,
                    width = width,
                    pre = pre,
                    post = post)
}


#' Create tagList of sliderInputUI instances.
#' @export
sliderInputUIs <- function(ids, 
                            labels = NULL, 
                            values = NULL,
                            minvals = NULL,
                            maxvals = NULL,
                            steps = NULL,
                            widths = NULL,
                            prefixes = NULL,
                            postfixes = NULL,
                            custom_css = NULL) {
    ui.elements = list()
    
    for (i in 1:length(ids)) {
        ui.elements[[i]] = div(
          style = ifelse(is.null(custom_css), "", custom_css[[i]]),
          sliderInputUI(id = ids[i], 
                        label = labels[i],
                        value = values[i],
                        min = minvals[i],
                        max = maxvals[i],
                        step = steps[i],
                        width = widths[i],
                        pre = prefixes[i],
                        post = postfixes[i]))
    }
    
    tagList(ui.elements)
}
