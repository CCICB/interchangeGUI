#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyWidgets::setShadow(class = "panel"),
    shinyjs::useShinyjs(),
    #shinyWidgets::setBackgroundColor("#393939"),
    # Your application UI logic

    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = div("Interchange", style = "font-weight: bold;")),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "tabs",
          shinydashboard::menuItem(
            text = "Convert Files", tabName = "convert_files", icon = icon("file-export"),
            shinydashboard::menuSubItem(text = "Conversion Search", tabName =  "search"),
            shinydashboard::menuSubItem(text = "GISTIC to CRUX", tabName =  "GISTIC_to_CRUX"),
            shinydashboard::menuSubItem(text = "VCF to MAF", tabName =  "VCF_to_MAF")
          ),
          shinydashboard::menuItem(text = "Identify Genome & Coord Style", icon = icon("dna"))
        )
      ),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "search", mod_conversion_search_ui("mod_conversion_search")),
          shinydashboard::tabItem(tabName = "GISTIC_to_CRUX", mod_convert_gistic_to_crux_ui("mod_convert_gistic_to_crux")),
          shinydashboard::tabItem(tabName = "VCF_to_MAF", mod_convert_vcf_to_maf_ui("mod_convert_vcf_to_maf"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "interchangeGUI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
