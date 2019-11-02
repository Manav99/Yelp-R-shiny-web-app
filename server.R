# 
#  Server.R
#
# Load lots of libraries

library(jsonlite)
library(UsingR)
library(leaflet)
library(maps)
library(sf)
library(dplyr)
library(flextable)

# # download zip file
# download.file(
#   url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip"
#   , destfile = "TM_WORLD_BORDERS-0.3.zip"
# )
# 
# # unzip 
# unzip( zipfile = "TM_WORLD_BORDERS-0.3.zip" )

# # transfrom to sf
# world.borders <-
#   read_sf( dsn = getwd()
#            , layer = "TM_WORLD_BORDERS-0.3" )
# 
# # add the wikipedia page for each country
# world.borders$wiki <-
#   paste0( "https://en.wikipedia.org/wiki/", world.borders$NAME )


# Read pre-processed data

data<-fread("all_business.csv", stringsAsFactors = T)
# yelp_business$yelp_business_rest.Business_ID.1<-NULL
data1<- aggregate(yelp_business.Review_count~yelp_business.Categories, data=yelp_business,FUN = length)
datasorted <- head(data1[order(data1$yelp_business.Review_count,decreasing = T),],100)
newdata <- subset(yelp_business,yelp_business.Categories %in% unique(datasorted$yelp_business.Categories))


shinyServer(
  function(input, output) {
    
  
    output$Stars<- renderUI({
       if (input$Categories=="All") { data <- newdata
       
       }else {
        data <- subset(newdata,yelp_business.Categories==input$Categories) 
       }
      
      sliderInput("StarsSlider","Star rating",min=min(data$yelp_business.Stars),
                 max=max(data$yelp_business.Stars),step=0.50,value = c(min(data$yelp_business.Stars),max(data$yelp_business.Stars)))
                 })
    
    output$Reviews<- renderUI({
      if (input$Categories=="All") {data <- newdata
      }else {
        data <- subset(newdata,yelp_business.Categories==input$Categories) 
       }
      
      sliderInput("ReviewsSlider","No. of reviews recieved",min=min(0),
                  max=max(data$yelp_business.Review_count),step=400,value = c(0,max(data$yelp_business.Review_count)))
    })
    
    
    output$mymap <-renderLeaflet({  
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Making plot", value = 10)

      if (input$Categories=="All") {mapobject <- subset(data,
                                                        yelp_business.Stars >= input$StarsSlider[1] & yelp_business.Stars <= input$StarsSlider[2] 
                                                        & yelp_business.Review_count >= input$ReviewsSlider[1] & yelp_business.Review_count <= input$ReviewsSlider[2]
                                                        
      )}
      
      #else if (input$Categories=="Restaurants") {subset()}
      
      else {mapobject <- subset(data,
                                yelp_business.Stars >= input$StarsSlider[1] & yelp_business.Stars <= input$StarsSlider[2]
                                & yelp_business.Categories == input$Categories
                                & yelp_business.Review_count >= input$ReviewsSlider[1] & yelp_business.Review_count <= input$ReviewsSlider[2]
      )}
      
      
      
      
      # mapobject <- subset(yelp_business,
      #                     yelp_business.Stars >= 1 & yelp_business.Stars <= 5 
      #                     & yelp_business.Categories == 'Restaurants'
      # )

      mymap<-leaflet() %>% 
        addTiles() %>% 
        addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png")  %>%  
        mapOptions(zoomToLimits="always") %>%             
        addMarkers(lat=mapobject$yelp_business.latitude,lng=mapobject$yelp_business.longitude,
                   clusterOptions = markerClusterOptions(),
                   # popup=mapobject$yelp_business.Name,
                   popup = paste(strong("May the Food be with you!"), "<br>","<br>",
                                 "Name Business: ", strong(paste0(mapobject$yelp_business.Name," in the city of ",mapobject$yelp_business.City)), "<br>",
                                 "Yelp Rating on: ", strong(paste0(mapobject$yelp_business.Stars, " Star wars")), "<br>",
                                 "Yelp Reviews on: ",strong(mapobject$yelp_business.Review_count), "<br>",
                                 "Score Sentiment: ", strong(mapobject$EliteSentScore)),
                   
                   
                   icon = list(
                     iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
                     iconSize = c(75, 75)
                   ))
      # %>%  addPolygons( 
                   # data = world.borders
                   #                     , fill = "#D24618"
                   #                     , color = "#D24618"
                   #                     , opacity = 0.5
                   #                     , fillOpacity = 0.01
                   #                     , weight = 3
                   #                     , popup = paste0(
                   #                       "<b>Country: </b>"
                   #                       , world.borders$NAME
                   #                       , "<br>"
                   #                       , "<a href='"
                   #                       , world.borders$wiki
                   #                       , "' target='_blank'>"
                   #                       , "Click Here to View Wiki</a>"
                   #                     )
                   #                     , label = ~NAME
                   #                     , labelOptions = labelOptions(
                   #                       style = list("font-weight" = "normal"
                   #                                    , padding = "3px 8px"
                   #                                    , textsize = "15px"
                   #                                    , direction = "auto" ) )
                   #                     , highlightOptions = highlightOptions( 
                   #                       color = "#10539A"
                   #                       , weight = 3
                   #                       , fillColor = NA,
                   # stroke = F  ) 
    })
    
  }
)




