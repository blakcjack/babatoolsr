# script used to preprocess the data from ICIS
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_linerange
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom dplyr rename

#' @title price_range_vis
#' @param df_summarized data.frame: Summarized dataframe to importer level, that has min_price and max_price varilable
#' @param commodity str: The name of the entity that is being plotted
#' @param min_price str: The column name stored the minimum price
#' @param max_price str: The column name stored the minimum price
#' @param importer str: The column name stored the importer/exporter name
#' @export
price_range_vis <- function(df_summarized, commodity, importer = "importer", min_price = "min_price", max_price = 'max_price'){
  df_ <- df_summarized |>
    rename(
      name = !!importer,
      min_price = !!min_price,
      max_price = !!max_price
    )
  min_range <- min
  p1 <- df_ |>
    ggplot(aes(x = fct_reorder(name, min_price), y = max_price)) +
    geom_errorbar(aes(ymin = min_price, ymax = max_price), col = "red") +
    geom_linerange(aes(ymin = 0.92, ymax = min_price), col = "grey20", linetype = 2, linewidth = 0.4) +
    scale_y_continuous(breaks = seq(0.6, 1.5, 0.01))

  return(p1)
}
