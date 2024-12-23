#'@title Line plot showing magnitude of all municipal spend
#'
#'@name muniLines
#'
#'@description
#'Visualizes municipal spend over the last 10 years for a chosen municipality via lineplot.
#'Different type of spends are colored accordingly.
#'
#'@param muni A character vector denoting what municipality is to be analyzed
#'
#'@param df A dataset containing the municipality of interest
#'
#'@returns A line plot showing the spending for the time period
#'
#'@import ggplot2
#'
#'@export
#'

muniLines <-
function(muni,df){
  
  # cheking the input which can give an error message back to the user
  if((!tolower(muni) %in% df$municipality)==TRUE || !is.data.frame(df)||
     nrow(df) != 440 && ncol(df) != 4){
    print('Check the spelling or choose another municipality, or use data_dl() to get
          the correct dataset')
    stop()
  }
  # Changing to lowercase 
  muni <- tolower(muni)
  # picking out data for the choosen municipalities
  indice <- df[df$municipality == muni,] 
  indice <- indice[indice$kpi != 'residents',]
  indice <- indice[indice$kpi != 'net_migration',]
  # Time serie graph for the municipality
  base::print(ggplot2::ggplot(indice, ggplot2::aes(x=period, y=values, color=kpi)) +
                ggplot2::geom_line(size=1.5) +
                ggplot2::scale_x_continuous(breaks= seq(min(indice$period), 
                                                        max(indice$period), by=2)) +
                ggplot2::scale_color_discrete("KPI") +
                ggplot2::theme_bw()+ ggplot2::ylab('Spend per resident') + ggplot2::xlab('Year') +
                ggplot2::ggtitle(paste0('Time series of municipal spend for ', stringr::str_to_title(muni))) + 
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
}
