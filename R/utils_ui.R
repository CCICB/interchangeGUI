#' icon_down_arrow
#'
#' @param alignment One of 'left', 'right', 'center' or 'inherit'(string)
#' @param fontsize a valid css fontsize, e.g. 60px (string)
#'
#' @return centered down arrow fluidrow()
#'
#' @noRd
#' @family arrows
icon_down_arrow <- function(fontsize="40px", alignment = "center", break_after = FALSE){
  utilitybeltassertions::assert_non_empty_string(fontsize)
  utilitybeltassertions::assert_non_empty_string(alignment)

  style_ = paste0("font-size: ",fontsize,"; text-align: center;")
  f <- fluidRow(style="display: grid",
                tags$i(class = "glyphicon glyphicon-arrow-down", style = style_)
  )

  if(break_after){
    return(tagList(f, br()))
  }
  else
    return(f)
}
