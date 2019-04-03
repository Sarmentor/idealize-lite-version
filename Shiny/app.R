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
# User interface ----
ui <- fluidPage(
  titlePanel("Idealize"),
    sidebarPanel(useShinyjs(),
      helpText("Idealize Idea Strength by Region and Country"),
      selectInput("Country", 
                  label = "Choose a country to display",
                  choices = c("ENGLAND","USA"),
                  selected = NULL)
      ,
      actionButton("start", "GO!"),
      helpText("Idealize Extracted Keywords"),
      textOutput("keys")
    ),
    mainPanel(
      tableOutput("ideatable"),
      dashboardBody(
      fluidRow(
        column(width = 12,
      box(id="box1",
        title = "Local Keyword(s) Trends", width = NULL, status = "primary",
        div(style = 'overflow-x: scroll', tableOutput('trendslocal'))
      ))),
      fluidRow(
        column(width = 12,
      box(id="box2",
        title = "Capital Keyword(s) Trends", width = NULL, status = "primary",
        div(style = 'overflow-x: scroll', tableOutput('trendscapital'))
      )))
    ))
)

# Server logic ----
server <- function(input, output){
  
  shinyjs::hide("box1")
  shinyjs::hide("box2")
  
  
  observeEvent(input$start,{
    
    #browser()
    
    
    showModal(modalDialog(textAreaInput("idea", "Write a summary of your idea in English language",
                                        placeholder = 'Please write a minimum of 150-200 words for better results',width="100%"
    ),footer=tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK") 
    )))
    
    
    if(input$Country=="ENGLAND"){
      gadm <<- readRDS(paste("../maps/gadm36_GBR_2_sp.rds",sep=""))
      regions <<- gadm$NAME_2
      mycountry <<- "GB"
      capital <<- "LONDON"
     

    }
    if(input$Country=="USA"){
      gadm <<- readRDS(paste("../maps/gadm36_USA_2_sp.rds",sep=""))
      regions <<- gadm$NAME_1
      mycountry <<- "US"
      capital <<- "WASHINGTON"
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
    
    
    
    showModal(modalDialog(selectInput("Region", 
                                      label = "Region of Search",
                                      choices= regions,
                                      selected= NULL)
                          ,footer=tagList(
                            modalButton("Cancel"),
                            actionButton("ok.region", "OK") 
                          )))
    
   
  })
  
  
  observeEvent(input$ok.region,{
    
    source("../Trends/Trends.R")
    
    trends <- gtrend(keys.list[,"keywords"], local = input$Region)
    trends.table <<- trends$trends
    
    idea.strength <- sum(as.numeric(keys.list[which(keys.list[,"keywords"]%in%rownames(trends.table)),"weighted.scores"])*trends.table[,ncol(trends.table)])/nrow(trends.table)
    
    trends.capital <- gtrend(keys.list[,"keywords"], local = capital)
    trends.table.capital <<- trends.capital$trends
    idea.strength.capital <- sum(as.numeric(keys.list[which(keys.list[,"keywords"]%in%rownames(trends.table.capital)),"weighted.scores"])*trends.table.capital[,ncol(trends.table.capital)])/nrow(trends.table.capital)
    
    #idea strenght in capital and chosen local
    #INSERT CODE HERE
    df.results <- cbind(idea.strength, idea.strength.capital)
    colnames(df.results) <- c(paste(input$Region, "local Strength",sep=" "), "Capital Region Strength") 
    output$ideatable <- renderTable(df.results)
    
    shinyjs::show("box1")
    shinyjs::show("box2")
    
    #Output retrieved trends
    output$trendscapital <- renderTable(trends.table.capital)
    output$trendslocal <- renderTable(trends.table)
    
    
    
    removeModal(session = getDefaultReactiveDomain())
    
  })

}
# Run app ----
shinyApp(ui, server)
