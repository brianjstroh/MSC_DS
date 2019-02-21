library(leaflet)
library(reshape2)
library(colorRamps)
library(dplyr)
library(shiny)

source("line_graph.R")

flat.temps<-melt(GISTEMP.data, id.vars = names(GISTEMP.data)[1], variable.name = "Splits", value.name = "Temp.Diffs")
flat.temps$Temp.Diffs<-pmax(rep(-max(flat.temps$Temp.Diffs),nrow(flat.temps)),flat.temps$Temp.Diffs)+max(flat.temps$Temp.Diffs)

ui<-fluidPage(
      theme = "bootstrap.css",
      h4("Author: ",em("Brian Stroh")),
      h4("Date: ",em("February 21st, 2019")),
      br(),
      pageWithSidebar(
            headerPanel(title = "Global Temperature Anomalies from 1980-2014"),
            sidebarPanel(
                  sliderInput("curr.year", "Year of Study:", value = 1950, min = 1880, max = 2014, step = 1),
                  radioButtons(inputId = "temp.layers", "Temperature Layer:", choices = c("Global", "Hemispheric", "3-way Split", "8-way Split"), selected = "8-way Split"),
                  br(),
                  h4("Temperature Scale"),
                  imageOutput("myScale"),
                  h5("Central colors indicate that the temperature anomalies in this zone for this year are close to the average temperatures from the period 1951-1980.")
            ),
            mainPanel(
                  leafletOutput("my.map"),
                  plotlyOutput("myPlot"),
                  p("Temperatures shown are relative to the 1951-1980 average")
            )
      )
)


server <-function(input, output, session) {
      
      my.colors <- blue2red(422)
      output$myScale <- renderImage({
            list(src = "colorRampsblue2red.PNG",
                 width = 300,
                 height = 30)
      }, deleteFile = FALSE)
      
      
      output$myPlot <- renderPlotly({
            print(my_plot)
      })
      
      
      map.init <- reactive({
            leaflet() %>% 
                  addTiles() %>%
                  fitBounds(-180, -90, 180, 90) 
      })
      
      output$my.map <- renderLeaflet({
            user.map()
      })
      
      user.map <- reactive({
            if(input$temp.layers == "Global"){
                  user.map <- map.init() %>%
                        addRectangles(
                              lng1=-1500, lat1=-90,
                              lng2=1500, lat2=90,
                              group = "Global",
                              layerId = "Globe",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "Glob")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        )
                  return (user.map)
            } else if(input$temp.layers == "Hemispheric"){
                  user.map <- map.init() %>%
                        addRectangles(
                              lng1=-1500, lat1=0,
                              lng2=1500, lat2=90,
                              group = "Hemisphere.Splits",
                              layerId = "NHemi",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "NHem")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=-90,
                              lng2=1500, lat2=0,
                              group = "Hemisphere.Splits",
                              layerId = "SHemi",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "SHem")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        )
                  return (user.map)
            } else if(input$temp.layers == "3-way Split"){
                  user.map <- map.init() %>%
                        addRectangles(
                              lng1=-1500, lat1=-90,
                              lng2=1500, lat2=-24,
                              group = "3.Splits",
                              layerId = "3.1",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X90S.24S")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=-24,
                              lng2=1500, lat2=24,
                              group = "3.Splits",
                              layerId = "3.2",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X24S.24N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=24,
                              lng2=1500, lat2=90,
                              group = "3.Splits",
                              layerId = "3.3",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X24N.90N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        )
                  return (user.map)
            }  else if(input$temp.layers == "8-way Split"){
                  user.map <- map.init() %>%
                        addRectangles(
                              lng1=-1500, lat1=-90,
                              lng2=1500, lat2=-64,
                              group = "8.Splits",
                              layerId = "8.1",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X90S.64S")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=-64,
                              lng2=1500, lat2=-44,
                              group = "8.Splits",
                              layerId = "8.2",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X64S.44S")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=-44,
                              lng2=1500, lat2=-24,
                              group = "8.Splits",
                              layerId = "8.3",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X44S.24S")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=-24,
                              lng2=1500, lat2=0,
                              group = "8.Splits",
                              layerId = "8.4",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X24S.EQU")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=0,
                              lng2=1500, lat2=24,
                              group = "8.Splits",
                              layerId = "8.5",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "EQU.24N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=24,
                              lng2=1500, lat2=44,
                              group = "8.Splits",
                              layerId = "8.6",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X24N.44N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=44,
                              lng2=1500, lat2=64,
                              group = "8.Splits",
                              layerId = "8.7",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X44N.64N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) %>%
                        addRectangles(
                              lng1=-1500, lat1=64,
                              lng2=1500, lat2=90,
                              group = "8.Splits",
                              layerId = "8.8",
                              color = my.colors[filter(flat.temps, Year == input$curr.year, Splits == "X64N.90N")$Temp.Diffs],
                              opacity = 0,
                              fillOpacity = .35
                        ) 
                  return (user.map)
            }
      })
      
      
}


shinyApp(ui, server)