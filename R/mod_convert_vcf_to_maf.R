#' convert_vcf_to_maf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_convert_vcf_to_maf_ui <- function(id){

  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
      heading = "Step 1: Select VCF Files",
      fluidRow(
        fileInput(inputId = ns('in_file_vcf'), label = "Import VCFs", multiple = TRUE, width = '80%', accept = c(".vcf", ".gz")) |> column(width = 3),
        shinydashboard::box(
          title = 'Converting VCF files to a MAF',
          'To convert VCF files to MAFs they must first be annotated with VEP, using specific settings.
          If your analysis pipeline does not perform this annotation automatically,
          see [add link] for instructions on how to manually annotate VCFs using the online VEP tool',
          tags$br(),tags$br(),
          shiny::downloadLink(outputId = ns("out_download_vcf"), label = "Download Demo VCF files"),
          width = "100%"
        ) |> column(width = 9),
      )
    ),
    icon_down_arrow(break_after = TRUE),

    shinyWidgets::panel(
      heading = "Step 2: Configure",


      fluidRow(
        textInput(inputId = ns('in_text_cohort_name'), label = 'Cohort Name', placeholder = 'Lung Cancer / Breast Cancer / etc.', value = "My Cohort") |> col_3(),
        textInput(inputId = ns('in_text_reference_genome'), label = 'Reference Genome', placeholder = 'hg19 / hg38 / CHM13 / etc.') |> col_3(),
        checkboxInput(inputId = ns('in_check_parse_filenames'), label = "Parse Sample Identifiers from Filenames?", value = TRUE) |> col_3(style="margin-top: 24px"),
        conditionalPanel(
          condition = "input.in_check_parse_filenames",
          ns = ns,
          radioButtons(inputId = ns('in_radio_extraction_method'),inline = TRUE, label = "Part of the filename describing tumor sample name", choices =  c('before dot' ='before_dot', 'before underscore'='before_underscore'), selected = 'before_dot') |> col_3(style="margin-top: 20px"),
        ),
        ),

      tags$hr(),

      fluidRow(
        checkboxInput(inputId = ns("in_check_vcf_tumor_equals_tumor_id"), label = "Assume IDs in VCF match Tumor Sample Barcodes", value = FALSE) |> col_3(),
        conditionalPanel(
          condition = "!input.in_check_vcf_tumor_equals_tumor_id", ns = ns,
          textInput(inputId = ns("in_text_vcf_tumor_id"), label = "ID of tumour sample in VCF", value = "TUMOR") |> col_3()
        ),
        textInput(inputId = ns("in_text_vcf_normal_id"), label = "ID of normal sample in VCF", value = "NORMAL") |> col_3()
        )
      ),

    icon_down_arrow(break_after = TRUE),
    shinyWidgets::panel(
      heading = "Step 3: Check VCF File -> Tumour Name Mappings",
      p(
        'Give each VCF file you want to convert an appropriate tumour sample identifier.
        This value will appear under "Tumor_Sample_Barode" in the MAF file produced'
      ),
      wellPanel(
        uiOutput(
          outputId = ns('out_ui_tumour_ids'),
        )
      )
      # Add either data.table output here OR maybe even configurable values
    ),

    icon_down_arrow(break_after = TRUE),

    shinyWidgets::panel(
      actionButton(inputId = ns("in_button_convert"), label = 'Convert', width = "100%"),
    ),

    # Invisible downloadbutton
    downloadButton(ns('download_maf'), label = 'Download', style = "visibility: hidden;")
  )
}

##' convert_vcf_to_maf Server Functions
#'
#' @noRd
mod_convert_vcf_to_maf_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    maf <- reactiveVal()

    paths_to_tempfiles <- reactive({input[["in_file_vcf"]][["datapath"]]})
    filenames <- reactive({input[["in_file_vcf"]][["name"]]})

    nfiles <- reactive({ length(paths_to_tempfiles()) })

    default_tumor_ids <- reactive({
      if(input[["in_check_parse_filenames"]])
        return(vcf2mafR::paths_to_sample_names(filenames(), extract = input[["in_radio_extraction_method"]]))
      else
        return(paste0("Tumor", seq_len(nfiles())))
    })

    input_ids_for_tumor_id_textinputs <- reactive({
      vapply(
        X = seq_len(nfiles()),
        FUN = \(n){
          paste0("in_vcf_id_", n)
          },
        FUN.VALUE = character(1)
      )
    })

    # Define what each VCFs tumor (sample) id is
    tumor_ids <- reactive({
      validate(need(!is.null(input_ids_for_tumor_id_textinputs()), message = "Please Wait ... "))
      #browser()
      vapply(
        X = input_ids_for_tumor_id_textinputs(),
        FUN = \(id) { input[[id]] },
        FUN.VALUE = character(1)
      )
    })

    # Add an observer to display a message when the "Convert" button is pressed
    observeEvent(input$in_button_convert, {
      if(is.null(paths_to_tempfiles())){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = 'Missing Files',
          text = "Please select files to convert",
          type = 'error'
          )
        return(invisible(NULL))
      }

      if(is.null(input[["in_text_reference_genome"]]) || nchar(input[["in_text_reference_genome"]]) == 0 ){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = 'Missing Reference Genome',
          text = "Go to 'Step 2: Configure' and indicate which reference genome your VCF variants were called against",
          type = 'error'
        )
        return(invisible(NULL))
      }

      if(!input[["in_check_vcf_tumor_equals_tumor_id"]] && (is.null(input[["in_text_vcf_tumor_id"]]) || nchar(input[["in_text_vcf_tumor_id"]]) == 0 )){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = 'Missing Tumor ID in VCF',
          text = "Go to 'Step 2: Configure' and indicate the ID of your tumor samples in your VCFs (or use checkbox to assume Tumor IDs in VCF match Tumor Sample Barcodes",
          type = 'error'
        )
        return(invisible(NULL))
      }
      if(is.null(input[["in_text_vcf_normal_id"]]) || nchar(input[["in_text_vcf_normal_id"]]) == 0){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = 'Missing Normal ID in VCF',
          text = "Go to 'Step 2: Configure' and indicate the ID of your Normal / Germline samples in your VCFs (or use checkbox to assume Tumor IDs in VCF match Tumor Sample Barcodes",
          type = 'error'
        )
        return(invisible(NULL))
      }


      # Add equivalent for normal

      vcf_tumor_ids <- reactive({
        if(input[['in_check_vcf_tumor_equals_tumor_id']])
          return(tumor_ids())
        else
          return(input[["in_text_vcf_tumor_id"]])
        })



      df_maf = tryCatch(
        expr = {
          shinybusy::show_modal_spinner(text = "Converting VCF to a MAF", session = session)
          vcf2mafR::vcfs2maf(
            vcfs = paths_to_tempfiles(),
            ref_genome = input[["in_text_reference_genome"]],
            tumor_id = tumor_ids(),
            # Instead of vcf ids we should have usre specify them.
            vcf_tumor_id = vcf_tumor_ids(),
            vcf_normal_id = input[["in_text_vcf_normal_id"]],
            parse_tumor_id_from_filename = FALSE # We've already handled this logic further up: tumor_id will be updated
          )
        },
        error = function(err){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = 'VCF to MAF Conversion Failed',
            text = tags$span(HTML(cli::ansi_html(as.character(err)))),
            type = 'error')
            shinybusy::remove_modal_spinner(session = session)
            return(invisible(NULL))
        },
        warning = function(warn){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = 'VCF to MAF Conversion Failed',
            text = tags$span(HTML(cli::ansi_html(as.character(warn)))),
            type = 'error')
            shinybusy::remove_modal_spinner(session = session)
            return(invisible(NULL))
        }
      )

      # Ensure we only try and download the MAF if our conversion succeeds
      if(is.null(df_maf))
       return(invisible(NULL))

      # Update maf reactive so our download works
      maf(df_maf)
      shinybusy::update_modal_spinner(text = "preparing MAF for dowload", session = session)

      # Download the MAF file by using javascript to click on our invisible download button
      modded_download_id <- ns('download_maf')
      shinyjs::runjs(paste0("$('#", modded_download_id, "')[0].click();"))

      # Remove the modal
      shinybusy::remove_modal_spinner(session = session)
      #shinyjs::runjs("$('#mod_convert_vcf_to_maf-download_maf')[0].click();")
    })

    # Dynamically render UI
    output[["out_ui_tumour_ids"]] <- renderUI({
      validate(need(!is.null(nfiles()) & nfiles() > 0, message = "Please Import Your VCF files"))
        div(style = "display: flex; flex-wrap: wrap; gap: 10px",
          lapply(seq_len(nfiles()), function(i) {
            textInput(
              inputId = ns(input_ids_for_tumor_id_textinputs()[i]),
              label = paste0("File: [",filenames()[i], ']'),
              value = default_tumor_ids()[i], width = "200px",
            )
          })
        )
      })


    # Validator (Indicate Required Input
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("in_text_reference_genome", shinyvalidate::sv_required())
    iv$add_rule("in_text_vcf_tumor_id", shinyvalidate::sv_required())
    iv$add_rule("in_text_vcf_normal_id", shinyvalidate::sv_required())
    iv$add_rule("in_text_cohort_name", shinyvalidate::sv_required())


    for (i in 1:500){
      iv$add_rule(paste0("in_vcf_id_", i), shinyvalidate::sv_required())
    }
    iv$enable()

    # Download Handlers
    output$download_maf <- downloadHandler(filename = function(){
        return(paste0(input[["in_text_cohort_name"]], '.maf'))
      },
      content = function(file){
      data.table::fwrite(x = maf(), file = file, sep="\t")
      })

    output$out_download_vcf <- downloadHandler(filename = "demo.vep_annotated_vcfs.zip", content = function(file) {
      path = system.file("example_data/vcf2maf_demo.zip", package = "interchangeGUI")
      file.copy(path, file)
    })

  })
}
## To be copied in the UI
# mod_convert_vcf_to_maf_ui("convert_vcf_to_maf_1")

## To be copied in the server
# mod_convert_vcf_to_maf_server("convert_vcf_to_maf_1")
