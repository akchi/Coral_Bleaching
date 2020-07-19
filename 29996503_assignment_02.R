# importing the required libraries
library(shiny)
library(leaflet)
library(ggplot2)

# reading the dataset into a data frame
coral_data <- read.csv("assignment-02-data-formated.csv")

# re-formatting the variable 'value' to numeric data type
coral_data$value <- gsub("%", "", coral_data$value)
coral_data$value <- as.numeric(coral_data$value)/100

# ordering site locations by increasing value of latitudes (lower to upper)
lat_order <- unique(coral_data[c("location", "latitude")])
lat_order <- lat_order[order(lat_order$latitude),]
coral_data$location <- factor(coral_data$location, levels = lat_order$location)

# shiny application
# server script for the web application
server <- function(input, output) {
  
  # genrating the reactive tabular plot
  output$coralPlot <- renderPlot({
  
    coral = input$type
    
    # extracting a section of the data frame belonging to the selected coral type
    if (coral == 'all')
      df <- coral_data
    else
      df <- coral_data[coral_data$coralType == coral,]
    
    p <- ggplot(df, aes(x=year, y=value)) + 
      geom_point() +
      facet_grid(coralType~location) +
      geom_smooth(method = input$model)
    p <- p + labs(y = "rate")
    print(p)
  })
  
  # generating a map through leaflet
  output$mymap <- renderLeaflet({
    leaflet(data = coral_data) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(location), 
                 label=~as.character(location), 
                 labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
                                             direction = "left", textsize = "10px"))
  })
}

# defining the UI application
ui <- fluidPage(
  
  # application title
  titlePanel("Coral Bleaching"),
  
  # drop-down menu for selcting type of coral and smoother 
  mainPanel(
    fluidRow(
      column(5, selectInput("type", "Coral Type:",
                            list("All" = "all",
                                 "Blue Corals" = "blue corals",
                                 "Hard Corals" = "hard corals",
                                 "Sea Fans" = "sea fans",
                                 "Sea Pens" = "sea pens",
                                 "Soft Corals" = "soft corals"), width='200px')),
      
      column(5, selectInput("model", "Smoothers:",
                            list("Auto" = "auto",
                                 "Linear Model" = "lm",
                                 "Generalised Linear Model" = "glm",
                                 "Generalised Additive Model" = "gam",
                                 "Loess Model" = "loess"), width='200px'))
      
    ),
    # showing the reactive tabluar plot
    plotOutput("coralPlot")
  ),
  
  # creating a sidebar to display the map
  sidebarPanel(
    br(), br(),
    # creating a canvas map on the application
    leafletOutput("mymap")
  )
)

# rendering the web application
shinyApp(ui, server)
