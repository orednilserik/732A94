#'@title Data downloader
#'
#'@name data_dl
#'
#'@description 
#' Downloading data with an api to kolada, variables and years are 
#' chosen and translated to English
#'
#'@return The function returns a data frame with 4 columns and 440 rows
#'
#'
#'@export
#'
#'@import httr jsonlite
#'
#'@source \url{https://www.kolada.se/?focus=16585}
#'
#'@examples
#' # example of how to use the function
#'df <- data_dl()
#'


data_dl <-
function(){
  
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
  df$municipality[df$municipality == "1280"] <- "malmoe"
  df$municipality[df$municipality == "1480"] <- "goeteborg"
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
