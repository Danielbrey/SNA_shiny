#The first time you use this program on a computer, you will need to install the packages instead of just calling them, so uncomment out the install.packages lines below.
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("networkD3")
#install.packages("igraph")

library(shiny)
library(tidyverse)
library(networkD3)
library(igraph)

#MisNodes and Mislinks are two data sets that contain data about characters from a book
#They can be used together to create an SNA graph and collect data
#MisNodes gives data about each node (character) and MisLinks describes the relationship between nodes
data(MisLinks)
data(MisNodes)

#You can use the command "View()" (see below) to look at the datasets (They are commented out at the moment) 
#View(MisLinks)
#View(MisNodes)

#We first create nodelist and add each node to the list (for each link, add the two nodes involved)
nodelist <- c()
for(i in 1:nrow(MisLinks)){
    nodelist <- append(nodelist, MisLinks$source[i] + 1)
    nodelist <- append(nodelist, MisLinks$target[i] + 1)
}

#Use SNA measurements to make two vectors (1D arrays) that have values for each node. Will be used for size of nodes in SNA graph
nodeCloseness <- closeness(graph(nodelist), mode = "all", weights = NA)
nodeBetweenness <- betweenness(graph(nodelist), directed=T, weights=NA)

#For MisLinks, we created columns to store the groups of each node related to the link
for(i in 1:nrow(MisLinks)){
    nodeSource <- MisLinks$source[i]
    MisLinks$sourceGroup[i] <- MisNodes$group[nodeSource]
    nodeTarget <- MisLinks$target[i]
    MisLinks$targetGroup[i] <- MisNodes$group[nodeTarget+1]
}

#For each node, we are storing values for different SNA measurements
for(i in 1:nrow(MisNodes)){
    MisNodes$close[i] <- nodeCloseness[i] * 1000
    MisNodes$between[i] <- nodeBetweenness[i]
    MisNodes$closeGroup[i] <- floor(MisNodes$close[i])
    MisNodes$closeSquared[i] <- (MisNodes$close[i]-1.1)^2.5 #Made it more complicated so there would be a more significant difference.
    MisNodes$betweenGroup[i] <- if(MisNodes$between[i] == 0) 1 else 2
} 


#ui is the frontend part of the app where inputs and outputs are initialized and placed
ui <- fluidPage(
    
    titlePanel("SNA Graph on Shiny"),
    
    # Sidebar with select inputs to change things about the graph (chooseGroup only works with "full" and group 1) 
    sidebarLayout(
        sidebarPanel(
            inputPanel(
                selectInput("dataAnalysis", label = "Data Analysis",
                            choices = c("Closeness", "Betweenness")),
                selectInput("grouping", label = "Select Way of Grouping", choices = c("group", "closeGroup", "betweenGroup")),
                selectInput("chooseGroup", label = "Focus on one group", choices = c("full", 1,2,3,4,5,6,7,8,9,10))
            )
        ),
        
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
