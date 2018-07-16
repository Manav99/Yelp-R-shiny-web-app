# 
#  Server.R
#
# Load lots of libraries

library(jsonlite)
library(UsingR)
library(leaflet)
library(maps)
library(dplyr)

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
                   clusterOptions = markerClusterOptions(),popup=mapobject$yelp_business.Name)
    })
    
  }
)




