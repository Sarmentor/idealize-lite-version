library("tm")
library("lsa")
library("TTR")
library("sigmoid")
library("jug")


IndexCalc <- function(stream){
  
  
  #retrieves ID
  id <- stream[[1]]$id
  
  #retrieves DATA
  data <- stream[[1]]$data
  
  google <- stream[[1]]$google
  
  nkeys <- data[[1]]$nkeys
  keys <- data[[1]]$keys
  keysweight <- data[[1]]$keysweight
  
  ind.vec <- c()  
  na.keys.google.index <- c()
  #browser()
  
  #Calculates Index per Keyword
  for(i in 1:nkeys){
    
    ntrend <- google[[i]]$"ntrend"
    trend <- google[[i]]$"trend"
    avgcomp  <- google[[i]]$"avgcomp"
    
    if( is.numeric(ntrend) & is.numeric(avgcomp) ){
      
      #browser()
      #calls "Indicator" function
      trending.value <- Indicator(trend,mode="RSI")
      
      #calls "Sigmoid" function
      weight <- Sigmoid(trending.value)
      
      #REVER ISTO
      #normalize average comp
      #max.comp <- 3
      #avgcomp.norm <- avgcomp/3
      
      #Index
      ind <- (trending.value/100)*weight + (1/avgcomp)*(1-weight)
      if ( ind > 1) { ind = 1}
      #adds to index per keyword vector
      ind.vec <- c(ind.vec, ind*10)
      
      
    }else{
      
      if( !(is.numeric(ntrend) || is.numeric(avgcomp))){
        
        na.keys.google.index <- c(na.keys.google.index, i) 
        next
        
      }
      
      
      if((ntrend == "NA" || ntrend == "NaN") & is.numeric(avgcomp)){
        
        ind <- (1/avgcomp)*(0.5)
        if ( ind >= 10) {
          ind = 10
        }else{
          
        }
        #adds to index per keyword vector
        ind.vec <- c(ind.vec, ind)
        
      }
      if((avgcomp == "NA" || avgcomp == "NaN") & is.numeric(ntrend)){
        
        #calls "Indicator" function
        trending.value <- Indicator(trend,mode="RSI")
        ind <- (trending.value/100)*0.5
        #adds to index per keyword vector
        ind.vec <- c(ind.vec, ind*10)
      }
      
    }
    
  }
  #Calculates Global Index
  sum.keysweight <- sum(keysweight)
  
  index <- 0
  weighted.sum <- 0
  
  sapply(ind.vec, FUN=function(x){
    #browser()
    index <<- index + 1
    norm.keysweight <<- keysweight[index]/sum.keysweight
    weighted.sum <<- weighted.sum + x*norm.keysweight
  })
  
  #global.ind <- weighted.sum/length(ind.vec)
  global.ind <- weighted.sum
  
  #Insert NAs in response
  #browser()
  if(length(na.keys.google.index)!=0){
    for(index in 0:(length(na.keys.google.index)-1)){
      
      if(index == 0 & na.keys.google.index[index+1] == 2){
        ind.vec <- append(ind.vec, "NA", after=(na.keys.google.index[index+1]+index)-1)
      }else{
        ind.vec <- append(ind.vec, "NA", after=(na.keys.google.index[index+1]+index))
      }
      
    }
  }
  #returns
  return(list(id, keys, ind.vec, global.ind))
  
}


Indicator <- function(trend, mode=c("RSI", "MACD", "OBV")){
  
  #Choose indicator from mode
  
  
  #Calculates indicator
  ind <- RSI(trend, n=5)
  ind.length <- length(ind)
  
  #returns
  return(ind[ind.length])		
  
}

Sigmoid <- function(trending.value){
  
  x <- seq(-50,50)
  
  sig <- sigmoid(x, SoftMax=TRUE)
  
  #Calculates weight of trend parcel
  trend.weight <- sig[trending.value+1]
  
  #returns trend.weight (competition.weight = 1 - trend.weight)
  return(trend.weight)
  
}


#por keyword ver peso NGD por local e em comparação com a capital do país
#talvez alterar NGD para apenas entrada de uma keyword, local e capital

#Permitir expansão de indice com trends tiradas do Google e para um determinado período
#permitir escolher indicador financeiro para ver força da trend por local e relativo a capital


