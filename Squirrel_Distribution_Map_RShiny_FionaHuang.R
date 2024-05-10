
# load required packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(plotly)
library(gridExtra)


# read in datasets
squirrel_df <- read.csv("cleaned_squirrel.csv")
hectare_df <- read.csv("cleaned_hectare.csv")
hectare_grid_df <- read.csv("Hectare_Grid.csv")



# divide the hectares into groups, group by two rows (e.g. 01A-I & 02A-I)
letter <- LETTERS[seq(from = 1, to = 9)]
hectare_number <- c(paste0("0", 1:9), 10:42)

hecatre_number_group_1 <- hectare_number[seq(1, length(hectare_number), 2)]
hecatre_number_group_2 <- hectare_number[seq(2, length(hectare_number), 2)]

new_hectare_label <- c(paste0("0", 1:9), 10:42)

new_squirrel_df <- squirrel_df
new_squirrel_df$New.Hectare <- rep(NA, nrow(squirrel_df))
new_hectare_df <- hectare_df

for (i in 1:21) {
  hectare_label_1 <- paste0(hecatre_number_group_1[i], letter)
  hectare_label_2 <- paste0(hecatre_number_group_2[i], letter)
  hectare_label <- c(hectare_label_1, hectare_label_2)
  
  for (j in 1:18) {
    new_squirrel_df$New.Hectare[(squirrel_df$Hectare == hectare_label[j])] <- new_hectare_label[i]
    new_hectare_df$New.Hectare[(new_hectare_df$Hectare == hectare_label[j])] <- new_hectare_label[i]
  }
}




# create a squirrel description for each squirrel id
squirrel_info <- subset(new_squirrel_df, select=c("Unique.Squirrel.ID", "Age", "Primary.Fur.Color", 
                                                  "Hectare", "New.Hectare"))
squirrel_id <- as.matrix(NA, length(squirrel_info))

for (i in 1:nrow(squirrel_info)) {
  id_info <- squirrel_info[i,]
  
  id <- paste("ID:", id_info[1])
  color <- paste("Color:", id_info[3])
  age <- paste("Age:", id_info[2])
  hectare <- paste("Hectare:", id_info[4])
  new.hectare <- paste("New Hectare:", id_info[5])
  
  info_template <- paste(id, color, age, hectare, new.hectare, sep="<br>")
  squirrel_id[i] <- info_template
}




# create a function that generates squirrels distribution map
grid <- sf::st_as_sf(hectare_grid_df, wkt = "the_geom")
color.palette <- colorFactor(c("brown", "orange", "grey40"), domain=c("Black", "Cinnamon", "Gray"))
dates <- c("Overall", "10062018", "10072018", "10082018", "10102018", "10122018", 
           "10132018", "10142018", "10172018", "10182018", "10192018", "10202018")

createMap = function(date_input) {
  i <- which(dates == date_input)
  sub <- squirrel_df
  if (i != 1) {
    sub <- squirrel_df[(squirrel_df$Date == date_input),]
  }
  map <- leaflet(data = sub) %>% 
    addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)  %>% 
    setView(lng = -73.965, lat = 40.783, zoom = 13.4) %>%
    addPolygons(data = grid, 
                stroke = TRUE, weight = 1, 
                color = ~"salmon") %>% 
    addCircles(~X, ~Y, popup = ~squirrel_id, 
               radius = 20, stroke = FALSE, 
               fillColor = ~color.palette(squirrel_df$Primary.Fur.Color), 
               fillOpacity = 0.8) %>% 
    addLegend("bottomright", colors = c("brown", "orange", "grey"), 
              labels = c("Black", "Cinnamon", "Grey"), 
              title ="Squirrel Fur Color")
  map
}



# create a fucntiont that generates a stacked barplot for each date
createPlot = function(date_input) {
  
  i <- which(dates == date_input)
  sub <- new_squirrel_df
  if (i != 1) {
    sub <- new_squirrel_df[(new_squirrel_df$Date == date_input),]
  }
  
  sub <- sub[order(sub$New.Hectare),]
  
  ggplot(sub, aes(x=New.Hectare, fill=Primary.Fur.Color)) + 
    geom_bar(stat="count") +
    labs(x = "Hectare", y = "Number of Squirrels", fill = "Fur Color") + 
    ggtitle("Number of Squirrels in each Hectare") + 
    scale_fill_manual(values = c("brown", "orange", "grey40")) + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, hjust=1))
}




# create a shiny app
# use plotlyOutput and renderPlotly to make the plots interactive in rshiny
library(plotly)

ui <- navbarPage("Squirrel Data Analysis",
                 tabPanel("Squirrel Distribution Map", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = 'datechoice',
                                label = 'Choose a date:',
                                choices = dates
                              )
                            ),
                            mainPanel(
                              leafletOutput(outputId = "leafletMap"), 
                              plotlyOutput(outputId = "barPlot")
                            )
                          )
                 ),
                 fluid = T)


server <- function(input, output) { 
  output$barPlot <- renderPlotly(
    createPlot(input$datechoice)
  )
  output$leafletMap <- renderLeaflet(
    createMap(input$datechoice)
  )
}

shinyApp(ui = ui, server = server)

