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
    shinyWidgets::panel(
      heading = "Step 1: Import GISTIC2 zip",
      fileInput(ns("in_file_gistic_zip"), label = "Import Gistic ZIP", width = "100%")
      ),
    icon_down_arrow(break_after = TRUE),
    shinyWidgets::panel(
      heading = "Configure",
      shinyWidgets::radioGroupButtons(
        ns("in_radio_cnlevel"),
        label = "Select Copynumber Level",
        choices = c("all", "deep", "shallow"),
        selected = "all"
      ) %>% col_2(),
      textInput(ns("in_text_cohort_name"), label = "Name of Cohort", width = "100%", value = 'My Cohort',placeholder = "My Cohort") %>% col_4(),
      shinyWidgets::prettyCheckbox(ns("in_check_is_tcga"), label = "Is TCGA?", value = FALSE, animation = "tada", inline = TRUE) %>% col_2()
      ),
    icon_down_arrow(break_after = TRUE),
    shinyWidgets::panel(downloadButton(ns("out_download_bttn"), label = "Download ", width = "100%"))
  )
}

#' convert_gistic_to_crux Server Functions
#'
#' @noRd
mod_convert_gistic_to_crux_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    input
    output$out_download_bttn <- downloadHandler(
      filename = function(){
        paste0(tools::file_path_sans_ext(input$in_text_cohort_name, compression = TRUE), ".gistic.Rds")
      },
      content = function(file){
        interchange::convert_gistic_tar_to_crux(
          input$in_file_gistic_zip$datapath,
          cnLevel = input$in_radio_cnlevel,
          isTCGA = input$in_check_is_tcga,
          outfile = file
        )
      }
      )
  })
}

## To be copied in the UI
# mod_convert_gistic_to_crux_ui("convert_gistic_to_crux_1")

## To be copied in the server
# mod_convert_gistic_to_crux_server("convert_gistic_to_crux_1")
