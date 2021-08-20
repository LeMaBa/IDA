#Installs / Imports all needed Packages

if(!require(install.load)){
  install.packages("install.load")
  library(install.load)
}

install_load("tidyverse", "readr", "ggplot2", "shiny", "leaflet","leafpop","leaflet.extras", "shinydashboard","DT", "dplyr")

#def. function 

#load final Dataframe
load("Final_Data_Group_03.RData")
data_explorer <- Anzahl_Fahrzeuge_pro_Gemeinde_mit_Geodaten

# Variables
  # Coordinats for Map Center
  lng_map_center <- 10.1831
  lat_map_center <- 48.9603
  
  #Select Input 
  
  #Read Parts from data
  #choices <- data_explorer %>% group_by(part) %>% select[[1]]
  choices <- c("T1","T2","T3")
  
  #Read Location
  #location <- data_explorer %>%  group_by(location) %>% select[[1]]
  location <- c(
    "Augsburg", "Ingolstadt", "Regensburg", "Würzburg", "Bamberg", "Bayreuth", "Aschaffenburg", "Erlangen",
    "Rosenheim", "Landshut"
  )
  
  
  
# Def. Header with Caption and Picture.
# Added ID for CSS
header <- dashboardHeader(
  
  #disable = TRUE
  title = div(img(src = "/www/LogoFQW.png", width = 40, height = 40), "Group 5", id = "logo"),
  titleWidth = 200
)

# Def. Pagebody
body <- dashboardBody(
  
  fluidPage(
    includeCSS("Additional_Files_Group_05/style2.css"),
    
    navbarPage(title = "Case Study",
               
      # Panel 1 - Plot of Failure for each Part for each Month
      navbarMenu(title = "Outage History",
        tabPanel(title = "Plot",
               fluidRow(
                 box(title = "Plot", status = "primary",solidHeader = FALSE, width =12, 
                     plotOutput(outputId = "plotFail", height = 750),
                     absolutePanel(top = 20, right = 40, width = 300, height = 100, draggable = TRUE,
                                   box(title = "Filter", status = "primary", solidHeader = TRUE, width = 12,
                                       selectInput(inputId = "locationHistory", label = "Choose Location", choices = location, multiple = FALSE))
                     )
                )
               )
        ),
        tabPanel(title = "Data",
                 fluidRow( box(title = "Data", status = "primary", solidHeader = TRUE, height = "800", width = 12,
                     DTOutput("tableHistory")
                 )
               )
             )
        
      ),
      
      #Panel 2 - History
      
      navbarMenu(title = "Analyse",
                 tabPanel(title = "Analyse",
                          fluidRow(
                            box(title = "Plot", status = "primary", solidHeader = FALSE, width = 12,
                                plotOutput(outputId = "plotForcast", width = 8, height = "750"),
                                absolutePanel(top = 20, right = 40, width = 300, height = 170, draggable = TRUE,
                                              box(title = "Filter", status = "primary", solidHeader = TRUE, height = "250", width = 12,
                                                  selectInput(inputId = "locationAnalyse", label = "Choose Location", choices = location),
                                                  selectInput(inputId = "part", label = "Choose Part", choices = choices)
                                              )
                                              
                                )
                            ) 
                          )
                        ),
                 tabPanel(title = "Data",
                        fluidRow(
                          box(title = "Data", status = "primary", solidHeader = TRUE, height = "800", width =12,
                          DTOutput("tableAnalyse")  
                   
                 )
      
               
                 )
               )
              ),
      
      #Panel 3 - Map      
      
      navbarMenu(title = "Forcast",
        tabPanel(title = "Map",
                 fluidRow(
                   leafletOutput(outputId = "map", width = "100%", height = "800"),
                   absolutePanel(top = 150, right = 40,
                                 selectInput(inputId = "locationMap", label = "Choose Location", choices = location)
                                 )
                 )  
      ),
        tabPanel(title = "Data",
                 fluidRow(
                   box(title = "Filter", status = "primary", solidHeader = TRUE, width = 4,
                       selectInput(inputId = "locationMap", label = "Choose Location", choices = location)
                   ),
                   box(title = "Data",status = "primary", solidHeader = TRUE, height = "800", width = 8, DTOutput("tableMap")
                   )
                 )           
                )

                      
                         
      )
    )
    
  )
 )

  

#Sidebar with 3 numerical Inputs
sidebar <- dashboardSidebar(
  disable = TRUE
)


#Loading of UI Elements
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)


# Server -> Definition of Functions of UI Elements (BACKEND)
server <- function(input, output, session){
  

  #1. A bar chart stacking the number of failures for each part for each month. 
  #It should be possible to filter by location.
  
  #output$plotFail <- renderPlot({
    
    
    #ggplot( data = data_explorer %>% filter(location, input$location) %>% filter(month, < "01.01.2017"), aes(x = month))+
      #geom_bar(fill = part)+
       #    ggtitle("Part Failure at "+input$locationHistory)+
      #     labs(x = "Month / Year", y ="Number of Faulty Parts")+
      #     theme_light()+
      #     geom_label()+
      #     theme(
      #       panel.grid.major = element_blank(),
      #       panel.grid.minor = element_blank(),
      #       axis.text = element_text(size = 14),
      #       axis.title = element_text(size = 14)
      #     )
  #})
  
  #2The outage history and a forecast for the outage of a selectable part for the 1st quarter of 2017. 
  #Both the total outage, and the outage per city mentioned above, should be apparent from the visualization.
  
  #output$plotForcast({
  #  ggplot(data = NULL)+
  #    geom_bar(data = data_explorer %>% filter(part, input$part), aes(x = month, y = failure))+
  #    geom_bar(data = data_explorer %>% filter(oart, input$part) %>% filter(location, input$location), aes(x = month, y = failure))+
  #    ggtitle("Part Failure")+
  #    labs(x = "Month / Year", y ="Number of Faulty Parts")+
  #    theme_light()+
  #    geom_label()+
  #    theme(
  #      panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank(),
  #      axis.text = element_text(size = 14),
  #      axis.title = element_text(size = 14)
  #    )
      
  #})
  

  #3. An interactive map highlighting all locations and integrate pop-ups showing your recommended 
  #numbers of units for the part selected from b. above
  
  output$map <- renderLeaflet({ 
   leaflet() %>%
      addTiles() %>%
      setView(lng_map_center, lat_map_center, zoom = 8) %>% #set the def. center & zoom level of the map
      addResetMapButton()
      
    #The Workshops will be marked and a popup Tabel ist generated with the current prediction for Q1/2017
      #addMarkers(
        
        #Koordinaten der Markierungen wird je nach UserInput definiert
        #lat = dataWorkshop$Breitengrad, 
        #lng = dataWorkshop$Laengengrad,
        
        #Es wird ein popup erstellt, das nach einem Mausklick auf eine markierte Gemeinde erscheint
        #popup = popupTable(
         #data_explore[-1,-2] %>% group_by(location)
        #)
     #)
  })
  
  #4 Your underlying data set as a table, so that you can prove visualized data. 
  #Again, remember to show only the necessary attributes.
  
  
  #4.1 - History
  #output$tableHistory <- 
  # renderDT(subset(data_explorer, filter(location, input$locationHistory) %>% filter(date, ...) %>% group_by(part)),options = list(
  #   autoWidth = TRUE 
  #         ), 
  #  filter = "top",
  #  rownames = FALSE
  #     )
  
  #4.2 - Forcast
  #output$tableAnalyse <- 
  #  renderDT(subset(data_explorer,  filter(location, input$locationAnalyse) %>% filter(part, input$part) %>% group_by(part)),options = list( 
  #    autoWidth = TRUE 
  #  ), 
  #  filter = "top", 
  #  rownames = FALSE
  #  )
  
  #4.3 - Map
  
  #output$tableMap <- 
  #  renderDT(subset(data_explorer, filter(location, input$locationMap) %>% filter(date, ....) %>% group_by(part)),options = list( 
  #    autoWidth = TRUE 
  #  ), 
  #  filter = "top",
  #  rownames = FALSE 
  #  )
}


addResourcePath('www',"Additional_Files_Group_05")
shinyApp(ui, server)