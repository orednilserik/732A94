#'@title Compare municipalities with a plot
#'
#'@name muniCompare
#'
#'@param muni_vec A character vector of municipalities in the data frame
#'
#'
#'@param df The data frame downloaded with data_dl()
#'
#'@param var A string of a KPI variable in the data frame
#'
#'
#'@returns A time serie plot of the chosen municipalities(muni_vec) for a variable
#'from the data frame
#'
#'
#'@export
#'
#'@import ggplot2
#'
#'
#'@examples
#'# This is en example of how to use the function
#'df <- data_dl() # download the data first
#'muniCompare(c('uppsala','stockholm'), df,'residents')
#'

muniCompare <-
function(muni_vec,df, var){
  stopifnot(tolower(muni_vec) %in% df$municipality,colnames(df) == c('kpi','municipality','period','values'),
            tolower(var) %in% df$kpi)
  
  # picking out data for the choosen municipality
  indice <- df[df$municipality %in% muni_vec,] 
  indice <- indice[indice$kpi == var,]
  Cost <- 'Spent'
  cost_title <- 'municipal spend on '
  if(var == 'residents' | var == "net_migration"){
    cost_title <- 'number of '
    Cost <- 'Count'}
  var_title <- strsplit(var, '_')[[1]][1]
  
  # Time serie graph for the municipality
  base::print(ggplot2::ggplot(indice, ggplot2::aes(x=period, y=values, color=municipality)) +
                ggplot2::geom_line(size=1.5) +
                ggplot2::scale_x_continuous(breaks = seq(min(indice$period),
                                                         max(indice$period), by=2)) +
                ggplot2::scale_color_discrete("Municipality") +
                ggplot2::theme_bw()+ ggplot2::ylab(paste0(Cost)) + ggplot2::xlab('Year') +
                ggplot2::ggtitle(paste0('Comparison of ',cost_title,var_title,' in ',
                                        paste0(stringr::str_to_title(muni_vec),collapse = " & "))) + 
                ggplot2::theme(axis.title.y=ggplot2::element_text(angle=0, vjust=0.5),
                               plot.title = ggplot2::element_text(hjust = 0.5)))
  
}
