

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Increase supported input size to 900MB
  options(shiny.maxRequestSize=900*1024^2)

  # COnversions
  mod_conversion_search_server("mod_conversion_search", parent_session = session)
  mod_convert_gistic_to_crux_server("mod_convert_gistic_to_crux")
  mod_convert_vcf_to_maf_server("mod_convert_vcf_to_maf")
}
