library(readr)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(leaflet)
library(DT)
library(gmt)
df <- read_csv("./state.csv" )
test <- read.csv("./ICMR.csv" )
ad<-test$address
ad<- as.character(ad)
df= df[-c(1),]
df= df[-c(37),]
states <- read_csv("./hospital.csv" )
zone <- read_csv("./Zone.csv" )
zone2 <- read_csv("./Zone2.csv" )
colnames(zone)[2:4] <-c("RZ","OZ","GZ")
colnames(df)[1:6] <-c("State_UN","RHos","RBed","UHos","UBed","Date")
df$RHos <- as.numeric(df$RHos)
df$RBed <- as.numeric(df$RBed)
df$UHos <- as.numeric(df$UHos)
df$UBed <- as.numeric(df$UBed)
df$THosp <- df$RHos+df$UHos
df$TBed <- df$RBed+df$UBed
df$Desp <- paste(df$State_UN,">>", 'Total Hospitals : ', df$THosp,'>> Total Beds : ', df$TBed)
zone$Desp <- paste(zone$State,">>", 'Red Zones : ', zone$RZ, ' ~ Orange Zones : ', zone$OZ, ' ~ Green Zones : ', zone$GZ)
ui <- dashboardPage(
  

  skin = "black",
  
  # header
  dashboardHeader(
    
    title = "COVID 19 : Hospital and Testing Spots",
    titleWidth = 450
  ),
    
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Helpline Numbers", icon = icon("list",lib='glyphicon'),href = "https://www.mohfw.gov.in/pdf/coronvavirushelplinenumber.pdf"),
      menuItem("FAQs", icon = icon("cog",lib='glyphicon'),href = "https://www.mohfw.gov.in/pdf/FAQ.pdf")  
      )
  ),
  
  # body
  dashboardBody(
    tags$head(tags$style(HTML('
        * {
        font-weight: bold;
        font-family: "HP Simplified";
        font-size: 18px;
        }
        .shiny-output-error-validation {
        font-size: 14px;
    '))),
    
    tabItems(
      tabItem(tabName = "dashboard",

        tags$head(tags$style(HTML('
        
        .main-header .logo {
        font-family: "HP Simplified";
        font-weight: bold;
        font-size: 24px;
        }
    '))),
        # row one
        frow <- fixedRow(
          valueBoxOutput("value1", width = 3),
          valueBoxOutput("value2", width = 3),
          valueBoxOutput("value3", width = 3),
          valueBoxOutput("value4", width = 3)
        ),
        
        frow1 <- fixedRow(
          valueBoxOutput("value5", width = 4),
          valueBoxOutput("value6", width = 4),
          valueBoxOutput("value7", width = 4)
        ),
                                         
        # row 2
        frow2 <- fluidRow( 
          box(
            title = "Hospitals"
            ,id = "box1"
            ,status = "primary"
            ,solidHeader = FALSE 
            ,collapsible = TRUE, 
              withSpinner(
              plotlyOutput("bar", height = "300px"))
            ,sliderInput("bin",
                         "Number of States:",
                         min = 3,
                         max = 37,
                         value = 10),
          )
          ,box(
            title = "Beds"
            ,id = "box2"
            ,status = "primary"
            ,solidHeader = FALSE 
            ,collapsible = TRUE
            ,withSpinner(
            plotlyOutput("bar2", height = "300px"))
            ,sliderInput("bins",
                         "Number of States:",
                         min = 3,
                         max = 37,
                         value = 10),
          )
        ),
        
        frow3 <- fluidRow(
          box(
            title = "Zones"
          ,id = "box3"
          ,status = "primary"
          ,solidHeader = FALSE 
          ,collapsible = TRUE
          ,width = 6
          ,withSpinner(
            leafletOutput("zmap", height = "700px")),
          ),
          box(
            title = "Statewise Zones",
            id='box4',
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 6,
            withSpinner(DTOutput("data",height = "700px"))
          )
        ),
      
        frow4 <- fluidRow(
          box(
            title = "Hospitals",
            id = "box5",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            collapsible = TRUE,
            withSpinner(
            leafletOutput("map", height = "700px"))
            
          ),
          box(
            title = "ICMR Testing Labs",
            id = "box6",
            status = "primary",
            solidHeader = FALSE,
            width = 6,
            collapsible = TRUE,
            withSpinner(
            leafletOutput("map1", height = "700px"))
          )
        ),
        
        frow5 <- fluidRow(
          column(width = 12,
            tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
        
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
       '),
            
            frow6<-   fluidRow(
                                id="myBox",
                               box(title="Testing Centers Near You(Within 20 Km)",
                                   status = "primary",
                                   id = "Box",
                                   collapsible = TRUE,
                                   width = 12,
                                   height = 500,
                                   withSpinner(
                                   DTOutput("centers",height = "100px"))
                           )
            )
          ),
          tags$footer("HATS by Graphic Era (Deemed to be University) ","|"," Developed By Kunal Aaryen Sinha & Srishti Patwal under the guidence of Ms.Garima Sharma", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;"),
        )
      )
    )
  )
)

# server function
server <- function(input, output, session) {
  output$lat <- renderText({
    input$lat
  })
  
  output$long <- renderText({
    input$long
  })
  output$centers <- renderDT({
    validate(
      need(input$lat != "", "Provide Location Access for viewing nearby Testing Centers")
    )
      t<-round(geodist( Nfrom=input$lat, Efrom=input$long, Nto=test$Lat, Eto=test$Long, units="km"),1)
      j<- 1
      tm=' '
      for(i in 1:length(t)){
      if(t[i]<20){
        tm[j]=ad[i]
        j=j+1
      }
    }
      DT::datatable(data.frame(Testing_Centers=tm), options = list(pageLength = 4))
  })
  
  output$value1 <- renderValueBox({
      dt <- sum(df$THosp)
      valueBox(
        dt
        ,paste('Hospital')
        ,icon = icon("heart",lib='glyphicon')
        ,color = "light-blue")  
    })
  output$value2 <- renderValueBox({ 
    dt1 <- sum(df$TBed)
    valueBox(
      dt1
      ,'Beds '
      ,icon = icon("bed",lib='glyphicon')
      ,color = "olive")  
  })
  output$value3 <- renderValueBox({ 
    valueBox(
      length(test$lab)
      ,'Testing Labs'
      ,icon = icon("plus",lib='glyphicon')
      ,color = "blue")  
  })
  output$value4 <- renderValueBox({ 
    dt1 <- Sys.Date()
    valueBox(
      dt1
      ,'Last Updated'
      ,icon = icon("time",lib='glyphicon')
      ,color = "aqua")  
  })
  output$value5 <- renderValueBox({
    valueBox(
      sum(zone$RZ)
      ,paste('Zones')
      ,color = "red")  
  })
  output$value6 <- renderValueBox({
    valueBox(
      sum(zone$OZ)
      ,paste('Zones')
      ,color = "orange")  
  })
  output$value7 <- renderValueBox({
    valueBox(
      sum(zone$GZ)
      ,paste('Zones')
      ,color = "green")  
  })
  output$bar <- renderPlotly({
    
    plot_ly(
      
      data = df,
      y = df$State_UN[1:input$bin],
      x = df$THosp[1:input$bin],
      type = "bar",
      orientation = "h"
      
    )
  })
    output$bar2 <- renderPlotly({
    
    plot_ly(
      data=df, 
      y = df$State_UN[1:input$bins],
      x = df$TBed[1:input$bins], 
      type='bar',
      orientation = "h"
    )
  })
    output$zmap <- renderLeaflet({
      
      greenLeafIcon <- makeIcon(
        iconUrl = "http://icon-park.com/imagefiles/location_map_pin_blue8.png",
        iconWidth = 34, iconHeight = 34
      )
      
      leaflet(zone) %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        ) %>%addMarkers(labelOptions = labelOptions(textsize = "15px",       
                                                    style = list(
                                                      "font-family" = "HP Simplified",
                                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "14px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    )),
                        lng =zone$Long, lat= zone$Lat,label = zone$Desp,icon = greenLeafIcon)%>%setView(lng =84, lat= 22, zoom = 5)
    })
    
    
    
    output$data <- renderDT({
      
      DT::datatable(data.frame(District=zone2$District,State=zone2$State,Zone=zone2$Zone), options = list(pageLength = 12))
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        ) %>%addCircleMarkers(lng =states$Lang, lat= states$Lat)%>%
              addMarkers(labelOptions = labelOptions(textsize = "15px",       
                style = list(
                "font-family" = "HP Simplified",
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )),
                lng =states$Lang, lat= states$Lat,label = df$Desp)%>%setView(lng =84, lat= 22, zoom = 5)
    })
    
    output$map1 <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        ) %>%addCircleMarkers(labelOptions = labelOptions(textsize = "15px",       
                                               style = list(
                                                 "font-family" = "HP Simplified",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "14px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                   lng =test$Long, lat= test$Lat,label = test$address)%>%setView(lng =84, lat= 22, zoom = 5)
    })
    
}

shinyApp(ui, server)
