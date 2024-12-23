data_dl <- function(){
  
  res <- httr::GET("http://api.kolada.se/v2/data/kpi/N40011,N40010,N09018,N30005,N07037,N09022,N10038,N05002,N20011,N01951,N01800/municipality/0180,1280,1480,0380/year/2013,2014,2015,2016,2017,2018,2019,2020,2021,2022")
  
  # Konvertera fr?n unicode till character
  # rawToChar(res$content)
  
  # Konvertera till list fr?n json struktur
  data <- jsonlite::fromJSON(rawToChar(res$content))
  df <- data$values
  
  for(i in 1:nrow(df)){
    #df[i,4] <- df[[4]][[i]] %>% dplyr::filter(gender == "T") %>% dplyr::select(value)
    df[i,4] <- df$values[[i]][4][df$values[[i]][2] == "T"]
  }
  
  df$values <- as.numeric(df$values)
  
  df$municipality[df$municipality == "0180"] <- "stockholm"
  df$municipality[df$municipality == "1280"] <- "malmö"
  df$municipality[df$municipality == "1480"] <- "göteborg"
  df$municipality[df$municipality == "0380"] <- "uppsala"
  
  df$kpi[df$kpi == "N40011"] <- "labor_spend"
  df$kpi[df$kpi == "N40010"] <- "refugee_spend"
  df$kpi[df$kpi == "N09018"] <- "leisure_spend"
  df$kpi[df$kpi == "N30005"] <- "care_spend"
  df$kpi[df$kpi == "N07037"] <- "infrastructure_spend"
  df$kpi[df$kpi == "N09022"] <- "culture_spend"
  df$kpi[df$kpi == "N10038"] <- "education_spend"
  df$kpi[df$kpi == "N05002"] <- "political_spend"
  df$kpi[df$kpi == "N20011"] <- "elder_disabled_spend"
  df$kpi[df$kpi == "N01951"] <- "residents"
  df$kpi[df$kpi == "N01800"] <- "net_migration"
  
  return(df)
}



# A function which indice the choosen municipality and plots data for it 
muniLines <-function(muni,df){
  
  # cheking the input which can give an error message back to the user
  if((!tolower(muni) %in% df$municipality)==TRUE || !is.data.frame(df)){
    print('Check the spelling or choose another municipality')
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



muniCompare<- function(muni_vec,df, var){
  
  
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




muniModel <- function(y,form,df){
  if(!(y %in% df$kpi || form %in% df$kpi)){
    print("Only the defined for the given municipalities. Also check spelling!")
    stop()
  }
  # Changing the data to wide format 
  df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values) 
  
  formula <- formula(paste0(y,'~',paste0(form,collapse = ' + ')))
  
  mod <- lm(formula, data=df1)
  
  base::print(ggplot2::ggplot(data.frame('res'=mod$residuals),ggplot2::aes(x=res) ) + 
                ggplot2::geom_density(fill=8, alpha=0.7) +
                ggplot2::theme_bw()+ ggplot2::ylab('Density') + 
                ggplot2::xlab(paste0('Residuals \n',deparse(formula)))+
                ggplot2::ggtitle('Density plot for the residuals')+
                ggplot2::theme(axis.title.y=ggplot2::element_text(angle=0, vjust=0.5),
                               plot.title = ggplot2::element_text(hjust = 0.5)))
  
  s <- summary(mod) # summary of the model
  
  return(s)
}



muniCorr <- function(df){
  df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values)
  
  graphics::pairs(df1[,-c(1:4)], gap = .2, col = "darkgreen", pch = 19, lower.panel = NULL)
  base::print(stats::cor(df1[,-c(1:4)]))
}

package.skeleton(name="BaDaAn")
