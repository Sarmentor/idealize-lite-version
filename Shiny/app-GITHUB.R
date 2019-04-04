library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(maps)
library(mapdata)    # Contains the hi-resolution points that mark out the countries.
library(mapproj)
library(sp)
library(leaflet)
library(graph)
library(grDevices)
library(ggplot2)
library(lettercase)
library(viridis)
library(pals)
library(scico)
library(ggrepel)
library(tidyverse)
library(data.table)
library(gtrendsR)


load("../data/Cities&Countries.RData")

my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}




my_theme2 = function() {
  my_theme() +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
}


# User interface ----
ui <- fluidPage(
  titlePanel("Idealize"),
    sidebarPanel(useShinyjs(),
      helpText("Idealize Idea Strength by Region and Country"),
      selectInput("Country", 
                  label = "Choose a country to display",
                  #choices = c("USA","New Zealand","France","Italy","World"),
                  choices = c("USA","World"),
                  selected = NULL)
      ,
      selectInput("search", 
                  label = "Choose a type of search",
                  choices =c("web", "news", "images", "froogle", "youtube"),
                  selected = "web"),
      selectInput("time",
                  label="Choose time period",
                  choices=c("Last hour",
                            "Last four hours",
                            "Last day",
                            "Last seven days",
                            "Past 30 days",
                            "Past 90 days",
                            "Past 12 months",
                            "Last five years",
                            "Since the beginning of Google Trends (2004)"),
                  selected = "Last five years"),
      actionButton("start", "GO!"),
      helpText("Idealize Extracted Keywords"),
      textOutput("keys")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          plotOutput("keysplot"), 
          title = "Interest over time (keywords)"
        ),
        tabPanel(
          plotOutput("ideaplot"), 
          title = "IDEA Interest over Region (keywords weighted)"
        )
        
      )
      
    )
)

# Server logic ----
server <- function(input, output){
  
  
  observeEvent(input$start,{
    
    #browser()
    
    
    showModal(modalDialog(textAreaInput("idea", "Write a summary of your idea in English language",
                                        placeholder = 'Please write a minimum of 150-200 words for better results',width="100%"
    ),footer=tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK") 
    )))
    
    
    if(input$Country=="World"){
      mycountry <<- ""
      mygeo <<- "world"
    }
    
    #if(input$Country=="France"){
    #  mycountry <<- "FR"
    #  mygeo <<- "france"
    #}
    
    #if(input$Country=="Italy"){
    #  mycountry <<- "IT"
    #  mygeo <<- "italy"
    #}
    
    #if(input$Country=="New Zealand"){
    #  mycountry <<- "NZ"
    #  mygeo <<- "nz"
    #}
    
    if(input$Country=="USA"){
      mycountry <<- "US"
      mygeo <<- "state"
    }
    
    if(input$search == "web"){
      mygprop <<- "web"
    }
    if(input$search == "news"){
      mygprop <<- "news"
    }
    if(input$search == "images"){
      mygprop <<- "images"
    }
    if(input$search == "froogle"){
      mygprop <<- "froogle"
    }
    if(input$search == "youtube"){
      mygprop <<- "youtube"
    }
    

    
    if(input$time== "Last hour" ){
      mytime <<- "now 1-H"
    }
    if(input$time== "Last four hours"){
      mytime <<- "now 4-H"
    }
    if(input$time== "Last day"){
      mytime <<- "now 1-d"
    }
    if(input$time== "Last seven days"){
      mytime <<- "now 7-d"
    }
    if(input$time== "Past 30 days"){
      mytime <<- "today 1-m"
    }
    if(input$time== "Past 90 days"){
      mytime <<- "today 3-m"
    }
    if(input$time== "Past 12 months"){
      mytime <<- "today 12-m"
    }
    if(input$time== "Last five years"){
      mytime <<- "today+5-y"
    }
    if(input$time== "Since the beginning of Google Trends (2004)"){
      mytime <<- "all"
    }

       
  })
  
  observeEvent(input$ok,{
    
    source("../TextRank/TextRank.R")
    
    #Call text Rank
    keys.list <<- main(input$idea)
    
    removeModal(session = getDefaultReactiveDomain())
    
    output$keys <- renderText({
      paste(keys.list[,"keywords"], collapse =" | ")
    })
    
    #it only accepts 5 keywords maximum
    keys.interest <- gtrends(keys.list[,"keywords"][1:5], time = mytime, gprop = mygprop, geo = mycountry)
    keys.interest %>% glimpse()
    
    
    output$keysplot <- renderPlot({plot(keys.interest) + my_theme() + geom_line(size = 0.5)})
    
   
    if(mycountry==""){
      InterestByRegion <- as_tibble(keys.interest$interest_by_country)
      InterestByRegion <- InterestByRegion %>% dplyr::mutate(region = location)
    }else{
      InterestByRegion <- as_tibble(keys.interest$interest_by_region)
      InterestByRegion <- InterestByRegion %>% dplyr::mutate(region = stringr::str_to_lower(location))
    }
    
    statesMap = ggplot2::map_data(mygeo)
    
    datDT <- as.data.frame(InterestByRegion)
    
    idea.interest <- data.frame(location = c(), hits = c(), geo = c(), gprop= c(),region=c())
    
    for(local in unique(datDT$location)){
        y <- datDT[which(datDT[,"location"]==local),"hits"]
        y[which(is.na(y))]<-0
        y[which(y=="<1")]<-0
        idea.interest.region <- mean(as.numeric(y)*as.numeric(keys.list[,"weighted.scores"][1:5]))
        if(mycountry==""){
          idea.interest <- rbind(idea.interest,data.frame(location = local, hits = idea.interest.region, geo = mycountry, gprop=mygprop,region=local))
        }else{
          idea.interest <- rbind(idea.interest,data.frame(location = local, hits = idea.interest.region, geo = mycountry, gprop=mygprop,region=tolower(local)))
        }
     }
    
    Merged <- merge(statesMap, idea.interest, by = "region")

    output$ideaplot <- renderPlot({Merged %>% ggplot(aes(x = long, y = lat)) +
                       geom_polygon(aes(group = group,fill = hits),colour = "white") +
                       scale_fill_gradientn(colours = rev(scico(15,palette = "tokyo")[2:7])) +
                       my_theme2() +
                       ggtitle("Google search interest for your idea \nin each state")})
   
  })

}
# Run app ----
shinyApp(ui, server)
