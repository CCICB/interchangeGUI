#' convert_gistic_to_crux UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_convert_gistic_to_crux_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' convert_gistic_to_crux Server Functions
#'
#' @noRd 
mod_convert_gistic_to_crux_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_convert_gistic_to_crux_ui("convert_gistic_to_crux_1")
    
## To be copied in the server
# mod_convert_gistic_to_crux_server("convert_gistic_to_crux_1")
