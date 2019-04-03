library(NLP)
library(tm)
library(openNLP)
library(graph)
library(microbenchmark)


# --- Pré-processing (PP) FUNCTIONS
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
###### illustrate usage of tagPOS
#str <- "this is a the first sentence."
#tagged_str <-  tagPOS(str)
#tagged_str

## $POStagged
## [1] "this/DT is/VBZ a/DT the/DT first/JJ sentence/NN ./."
## 
## $POStags
## [1] "DT"  "VBZ" "DT"  "DT"  "JJ"  "NN"  "."

###### Other utility functions - PP #####
SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

IsPunctuated <- function(Phrase) {
  length(grep("\\.|,|!|\\?|;|:|\\)|]|}\\Z",Phrase,perl=TRUE))>0 # punctuation: . , ! ? ; : ) ] }
}

SelectTaggedWords <- function(Words,tagID) {
  Words[ grep(tagID,Words) ]
}

RemoveTags <- function(Words) {
  sub("/[A-Z]{2,3}","",Words)
}


### Stream Functions ####

IsSelectedWord <- function(Word, selected_words) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}

GetWordLinks <- function(position,scope,words,selected_words) {
  scope <- ifelse(position+scope>length(words),length(words),position+scope)
  links <- ""
  for (i in (position+1):scope) {
    if ( IsSelectedWord(words[i], selected_words) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}

ConstructTextGraph <- function(n, words, selected_words) { 
  word_graph <- new("graphNEL")
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i], selected_words) ) {                                   
      links <- GetWordLinks(i,n,words,selected_words)                                
      if (links[1] != "") {                                     
        #cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
        if ( length(which(nodes(word_graph)==words[i]))==0  ) {     
          word_graph <- addNode(words[i],word_graph)
        }                                               
        
        for (j in 1:length(links)) {
          if ( length(which(nodes(word_graph)==links[j]))==0 ) {
            word_graph <- addNode(links[j],word_graph)
            word_graph <- addEdge(words[i],links[j],word_graph,1)
          } 
          else {
            if ( length(which(edges(word_graph,links[j])[[1]]==words[i]))>0 ) { 
              prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
              edgeData(word_graph,words[i],links[j],"weight") <- prev_edge_weight+1
            }
            else {
              word_graph <- addEdge(words[i],links[j],word_graph,1)
            }
          } 
        }
      }
    }
    i <- i+1
  }
  
  number.of.nodes <<- numNodes(word_graph)
  word_graph
  
}

## Visualize obtained text graph
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

#library("Rgraphviz")
#plot(text_graph, attrs = list(node = list(fillcolor = "lightblue", fontsize = 20),edge = list(arrowsize=0.5)))


PR <- function(d,threshold,text_graph, text_nodes,nodes_num,nodes_rank){
  k <- 0                                  # iterations
  convergence_reached <- FALSE
  repeat {
    for (i in 1:nodes_num) {
      incoming_link <- adj(text_graph,text_nodes[i])[[1]]
      incoming_num <- length(incoming_link)
      
      tmp <- 0
      for (j in 1:incoming_num) {
        link_num <- which(text_nodes==incoming_link[j])
        outgoing_num <- length(adj(text_graph,text_nodes[link_num])[[1]])
        tmp <- tmp + nodes_rank[link_num,1] / outgoing_num
      }
      nodes_rank[i,1] <- (1-d)+d*tmp
    }
    k <- k+1
    for (i in 1:nodes_num) {
      if (abs(nodes_rank[i,1]-nodes_rank[i,2])<threshold) convergence_reached <- TRUE
    }
    if (convergence_reached) break
    nodes_rank[,2] <- nodes_rank[,1]
  }
  
}

inc.post.proc <- function(words, text_nodes, nodes_num, nodes_rank, selected_words,words_with_punctuation){
  
  # ---Incremental POST-PROCESSING
  
  keywords_num <- round(nodes_num/3) # a third of the number of vertices in the graph.
  ranked_words <- data.frame(text_nodes,nodes_rank[,1])
  names(ranked_words) <- c("word","rank")
  strong_words <- ranked_words[order(ranked_words$rank,decreasing=TRUE),]
  strong_words <- as.character(strong_words$word[1:keywords_num])
  keywords <- ""
  keywords_scores <- 0
  for (i in 1:keywords_num) {
    keyword_positions <- which(words==strong_words[i])
    for (j in 1:length(keyword_positions)) {
      keyword <- ""
      keyword_score <- 0
      k <- keyword_positions[j]                                       
      repeat {
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- trim(paste(c(keyword,words[k]),collapse=" "))
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break                                                    
        
        if (IsPunctuated(words_with_punctuation[k])) break
        if (k==length(words)) break                               
        k <- k+1
      }
      k <- keyword_positions[j]-1                                 
      repeat {
        if (k<1) break
        
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- paste(c(words[k],trim(keyword)),collapse=" ")
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break
        
        if (k>1) {            
          if (IsPunctuated(words_with_punctuation[k-1])) break
        } 
        k <- k-1
      }
      if (keyword!=strong_words[i]) { 
        keywords <- c(keywords,keyword)
        keywords_scores <- c(keywords_scores,keyword_score)
      }   
    }
  }
  keywords_df <- data.frame(keywords,keywords_scores)
  keywords_list <- keywords_df[order(keywords_df$keywords_scores,decreasing=TRUE),] 
  
  #cleaning data.frame
  scores <- c()
  keywords <- unique(as.character(keywords_list$keywords[1:nrow(keywords_list)]))
  keywords <- keywords[-which(keywords=="")]
  
  sapply(keywords, function(x){
    scores <<- c(scores,as.numeric(unique(keywords_list[which(keywords_list$keywords==x),"keywords_scores"])))
  })
  
  weighted.scores <- c()
  sum.scores <- sum(scores)
  
  sapply(keywords, function(x){
    weighted.scores <<- c(weighted.scores,as.numeric(unique(keywords_list[which(keywords_list$keywords==x),"keywords_scores"]))/sum.scores)
  })
  
  keywords_list <- cbind(keywords,scores,weighted.scores)
  
  keywords_list
}

number.of.nodes <- 0
number.of.words <- 0

main <- function(doc){
  
  
  ### Pre-process Text ###
  ### Pré-processing Stream Functions ### 
  
  corp <- Corpus(VectorSource(doc))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, tolower)
  words_with_punctuation <- SplitText(as.character(corp[[1]]))
  corp <- tm_map(corp, removePunctuation)
  
  ### Stream functions ###
  #--- Initial and Stream GRAPH CONSTRUCTION
  
  words <- SplitText(as.character(corp[[1]]))
  number.of.words <<- length(words)
  tagged_text <- tagPOS(corp[[1]])
  tagged_words_aux <- SplitText(as.character(tagged_text))
  tagged_words_aux <- c(SelectTaggedWords(tagged_words_aux,"/NN"),SelectTaggedWords(tagged_words_aux,"/JJ")) # keep only NN & JJ tagged words 
  tagged_words_aux <- RemoveTags(tagged_words_aux)                                                      # remove un-used tag POS
  selected_words <- unique(tagged_words_aux)                                                         
  time.gen.graph <- microbenchmark(
    text_graph <- ConstructTextGraph(2, words, selected_words)  # co-occurrence of window size 2
    ,list = NULL, times = 1, check = NULL,
    control = list())
  
  ### ---  Incremental PAGE RANK Implementation
  d <- 0.85                               # damping factor
  threshold <- 1e-4               # convergence threshold 
  text_nodes <- nodes(text_graph)
  nodes_num <- length(text_nodes)
  nodes_rank <- matrix(1,nodes_num,2)
  
  #browser()
  
  time.pr <- microbenchmark(PR(d,threshold,text_graph, text_nodes,nodes_num,nodes_rank)
                            ,list = NULL, times = 1, check = NULL,
                            control = list())
  
  
  ### Post-Processing ###
  new.key <- inc.post.proc(words, text_nodes, nodes_num, nodes_rank, selected_words,words_with_punctuation)
  
  ### Output Keywords ###
  return(new.key)
}
