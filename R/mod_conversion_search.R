#' conversion_search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_conversion_search_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinyWidgets::panel(selectInput(inputId = ns("in_infiletype"), label = "Input Filetype", choices = interchange::supported_conversions()[[1]], width = "100%"), heading = "Input Filetype", status = "success") %>% col_6(),
      shinyWidgets::panel(selectInput(inputId = ns("in_outfiletype"), label = "Output Filetype", choices =interchange::supported_conversions()[[2]], width = "100%"), heading = "Output Filetype", status = "danger") %>% col_6()
    ),
    icon_down_arrow(break_after = TRUE),
    shinyWidgets::panel(actionButton(inputId = ns("in_bttn_convert"),label = "Convert", width = "100%"))
  )
}

#' conversion_search Server Functions
#'
#' @noRd
mod_conversion_search_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input[["in_bttn_convert"]], {
      target <- paste0(input[["in_infiletype"]], "_to_", input[["in_outfiletype"]])

      tryCatch(
        expr = {
          shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = target)
        },
        error = function(err){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Conversion not Supported",
            text = "This conversion is not yet supported by interchangeGUI.
            If you'd like it added, please create a github issue https://github.com/CCICB/interchangeGUI/issues",
            type = "warning"
          )
        }
      )

      })
  })
}

## To be copied in the UI
# mod_conversion_search_ui("conversion_search_1")

## To be copied in the server
# mod_conversion_search_server("conversion_search_1")
