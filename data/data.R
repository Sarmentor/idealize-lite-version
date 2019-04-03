library(gtrendsR)

countries <- c("GB","US","ES","PT")

df.cities <- data.frame()

for (i in 1:length(countries)){
  
  #browser()
  
  cities <- gtrends(keyword="Ronaldo",geo=paste(countries[i]))$interest_by_city$location
  enc <- Encoding(cities)
  Encoding(cities) <- "UTF-8"
  country <- rep(countries[i], length(cities))
  df.cities <- rbind(df.cities,data.frame(cities=cities,country=country))
  
}

df.cities

save("df.cities",file="Cities&Countries.RData")