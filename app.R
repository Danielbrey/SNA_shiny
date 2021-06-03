library(shiny)
library(tidyverse)
library(rvest)
library(shinydashboard)
library(DT)
library(networkD3)
library(igraph)


data(MisLinks)
data(MisNodes)

#View(Links)
#View(Nodes)

nodelist <- c()

#MisLinks$source <- MisLinks$source
#MisLinks$target <- MisLinks$target

for(i in 1:nrow(MisLinks)){
    nodelist <- append(nodelist, MisLinks$source[i] + 1)
    nodelist <- append(nodelist, MisLinks$target[i] + 1)
}
nodeCloseness <- closeness(graph(nodelist), mode = "all", weights = NA)
nodeBetweenness <- betweenness(graph(nodelist), directed=T, weights=NA)
for(i in 1:nrow(MisLinks)){
    nodeSource <- MisLinks$source[i]
    MisLinks$sourceGroup[i] <- MisNodes$group[nodeSource]
    nodeTarget <- MisLinks$target[i]
    MisLinks$targetGroup[i] <- MisNodes$group[nodeTarget+1]
}


for(i in 1:nrow(MisNodes)){
    MisNodes$close[i] <- nodeCloseness[i] * 1000
    MisNodes$between[i] <- nodeBetweenness[i]
    MisNodes$closeGroup[i] <- floor(MisNodes$close[i])
    MisNodes$closeSquared[i] <- (MisNodes$close[i]-1.1)^2.5 #Made it more complicated so there would be a more significant difference.
    MisNodes$betweenGroup[i] <- if(MisNodes$between[i] == 0) 1 else 2
} 



ui <- fluidPage(
    
    titlePanel("SNA Graph on Shiny"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            inputPanel(
                #sliderInput("opacity", label = "Opacity",
                #min = 0.2, max = 0.9, value = 0.5, step = 0.1),
                selectInput("dataAnalysis", label = "Data Analysis",
                            choices = c("Closeness", "Betweenness")),
                selectInput("grouping", label = "Select Way of Grouping", choices = c("group", "closeGroup", "betweenGroup")),
                selectInput("chooseGroup", label = "Focus on one group", choices = c("full", 1,2,3,4,5,6,7,8,9,10))
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            forceNetworkOutput("force"),
            tableOutput("nodes"),
            tableOutput("links"),
            verbatimTextOutput("nodelist")
            
        )
    )
    
    

)


server <- function(input, output) {
    output$force <- renderForceNetwork({
        if(input$chooseGroup == "full"){
            filteredLinks <- MisLinks
            filteredNodes <- MisNodes
        }
        if(input$chooseGroup != "full"){
            filteredNodes <- MisNodes %>% 
                filter(group == input$chooseGroup)
            filteredLinks <- MisLinks %>% 
                filter((sourceGroup == input$chooseGroup) & (targetGroup == input$chooseGroup))
        }
        if(input$dataAnalysis == "Closeness"){
            forceNetwork(Links = filteredLinks, Nodes = filteredNodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name",
                         Group = input$grouping, opacity = 0.8,
                         Nodesize = "closeSquared")
            
        } else if(input$dataAnalysis == "Betweenness"){
            forceNetwork(Links = filteredLinks, Nodes = filteredNodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name",
                         Group = input$grouping, opacity = 0.8,
                         Nodesize = "between")
        }
        
    })
    
    output$nodes <- renderTable(MisNodes)
    output$links <- renderTable(MisLinks)
    output$nodelist <- renderPrint(typeof(MisNodes))
    
}


shinyApp(ui = ui, server = server)
