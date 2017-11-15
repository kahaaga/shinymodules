selectInputUI <- function(id,
                          label = paste("Please provide a label argument to the numericInputUI",
                                         " with id = ", id, ".", sep = ""), 
                          choices = c("Choice 1", "Choice 2"),
                          selectize = F,
                          width = "100%",
                          custom_css = NULL) {
    # Create a namespace that we encapsulate around IDs in the UI function.
    # This ensures that no UI elements of this type are duplicated, provided 
    # we assign unique IDs to our UI elements. 
    ns <- NS(id) 
    
    # Create a shiny::selectInput with inputId ns("selectinput"), so the inputId of 
    # this numericInputUI is "id-selectinput". For example, if id = "category", then
    # ns("category") gives a numericInputUI with inputId = "category-selectinput".
    shiny::selectInput(inputId = ns("selectinput"), 
                       label = label, 
                        choices = choices, 
                       selectize = selectize,
                        width = width)
}

#' Create tagList of selectInputUI instances.
#' @param ids Vector of ids 
#' @param labels Vector of labels for the dropdown menus
#' @param choices List of choice vectors for the dropdown menus. List elements
#' can be named; if so, they are grouped in the dropdown menu.
#' @param selectize Vector of booleans indiciating if dropdown menus should be
#' selectized.
#' @param widths Vector of valid css widths.
selectInputUIs <- function(ids, 
                            labels = NULL, 
                            choices = NULL,
                            selectize = NULL,
                            widths = NULL,
                           custom_css = NULL) {
    ui.elements = list()
    
    for (i in 1:length(ids)) {
      
        ui.elements[[i]] = selectInputUI(id = ids[i], 
                                          label = labels[i],
                                          choices = choices[[i]],
                                          selectize = selectize[i],
                                          width = widths[i])
    }
    
    tagList(ui.elements)
}