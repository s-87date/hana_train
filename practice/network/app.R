#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("PubMedNetwork.R", local = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("WordCloud"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("max",
                     "max.words:",
                     min = 100,
                     max = 500,
                     value = 250),
         sliderInput("freq",
                     "minimum.freq:",
                     min = 10,
                     max = 50,
                     value = 25)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
         
      )
   ),
  
   # Application title
   titlePanel("Co-Author Network"),   
   visNetworkOutput("network")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  wordcloud_rep <- repeatable(wordcloud)
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[,2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      # v <- pubmed_data$Title
      # wordcloud_rep(names(v), v, scale=c(4,0.5),
      #               min.freq = 25, max.words=input$bins,
      #               colors=brewer.pal(8, "Dark2"))
      wordcloud(pubmed_data$Title,
                scale=c(4,0.5),
                max.words=input$max,
                min.freq = input$freq,
                colors=brewer.pal(8, "Dark2"))
   })
   output$network <- renderVisNetwork({
     visNetwork(nodes, edges) %>%
       visLayout(randomSeed = "1") %>%
       visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                  collapse = list(enabled = TRUE, clusterOptions = list(shape = "square")),
                  nodesIdSelection = list(enabled = TRUE,
                                          selected = nodes[which(nodes$group=="main"), "id"] %>%
                                            as.numeric),
                  clickToUse = FALSE
       ) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

