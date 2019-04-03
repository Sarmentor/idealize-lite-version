library(gtrendsR)


gtrend <- function(keys, local){
  
  nkeys <- length(keys)
  
  sub_code <- as.character(countries[which(countries[,"country_code"]==mycountry & countries[,"name"]==paste(toupper(local))),"sub_code"])
  
  trends <- data.frame()
  
  geo.vec <- c(strsplit(sub_code[1], split="-")[[1]][2],strsplit(sub_code[1], split="-")[[1]][1])
  
  #browser()
  
  for(key in 1:length(keys)){
    
   
    #function from package
     trends <- rbind(trends,as.data.frame(row.names = paste(keys[key]),t(as.numeric(
        gtrends(keyword = keys[key], geo= geo.vec, time = "today 12-m",gprop = c("web"), category = 0, hl = "en", low_search_volume = FALSE,cookie_url = "http://trends.google.com/Cookies/NID")$interest_over_time$hits
        ))))
  }
  
  data <- list(nkeys=nkeys,keys=keys,trends=trends)
  return(data)
}