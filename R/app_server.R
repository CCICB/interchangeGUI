

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_conversion_search_server("mod_conversion_search", parent_session = session)
  mod_convert_gistic_to_crux_server("mod_convert_gistic_to_crux")
}
