#'@title Correlation analysis of the municipal spends
#'
#'@name muniCorr
#'
#'@description
#'Performs a correlation analysis for the total municipal spend over the time period by providing a correlation table and scatterplot.
#'All variables are included in the printout.
#'
#'@param df The dataset which includes variables to be analyzed
#'
#'@returns A table and a plot showing correlations between variables.
#'
#'
#'@export
#'
#'@import tidyr graphics
#'

muniCorr <-
function(df){
  stopifnot(nrow(df)==440, ncol(df)==4,
            colnames(df) == c('kpi','municipality','period','values'))
  df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values)
  
  graphics::pairs(df1[,-c(1:4)], gap = .2, col = "darkgreen", pch = 19, lower.panel = NULL)
  base::print(stats::cor(df1[,-c(1:4)]))
}
