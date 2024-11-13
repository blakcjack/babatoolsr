#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom methods hasArg
#' @importFrom dplyr select
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(forcats)
})


# First, let's build a pallete
#' A pallete to create bababos brand and visualization scheme
#' @return Vector with pallete options
#' @author Suberlin Sinaga
#' @title create_pallete
#' @param pallete Named vector of pallete to use
#' @export
create_pallete <- function(pallete) {
  pallete <- if (hasArg(pallete)) pallete else bababos_pallete
  return(pallete)
}

bababos_pallete <- c(light = "#010e24", white = "#ffffff", dark = "#efb509",
                     bg = "#e9f1f7", primary = "#010e24", support1 = "#112A46",
                     base = "#72D185", support2 = "#741919", support3 = "#803C0C",
                     area = "#e0de5c")

# now, let's build our theme that match our company profile
#' This is official theme for bababos publication.
#' @export
#' @author Suberlin Sinaga
#' @return ggplot2 theme
#' @param base_theme Theme used as a base for the ggplot theme
#' @param base_size The size of the various text
bababos_theme <- function(base_theme = theme_gray, base_size = 8) {
  theme <- base_theme(base_size = 8) +
    theme(
      plot.background = element_rect(fill = bababos_pallete[['bg']]),
      panel.background = element_rect(fill = bababos_pallete[['bg']]),
      title = element_text(colour = bababos_pallete[['primary']], face = "bold", size = 15),
      axis.title = element_text(colour = bababos_pallete[['primary']], face = "bold", size = 12),
      text = element_text(colour = bababos_pallete[['primary']]),
      axis.text = element_text(size = 9, face = "bold"),
      strip.text = element_text(colour = bababos_pallete[['primary']], size = 9),
      #   legend.box.background = element_rect(fill = bababos_pallete[['bg']]),
      #   legend.key = element_rect(fill = bababos_pallete['dark']),
      #   legend.background = element_rect(fill = bababos_pallete['bg']),
      legend.position = "bottom",
      legend.text = element_text(color = bababos_pallete[['primary']], size = 9),
      plot.caption = element_text(color = bababos_pallete[['support2']]),
      panel.grid = element_blank(),
      legend.title = element_text(color = bababos_pallete[['primary']]),
      # panel.grid.major = element_line(color = basic_palette['dark'], size = 0.1)
    )
    return(theme)
}
