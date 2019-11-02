
# ui.R


library(shiny) 
library(leaflet)
library(data.table)

yelp_business<-fread("all_business.csv", stringsAsFactors = T)
data1<- aggregate(yelp_business.Review_count~yelp_business.Categories, data=yelp_business,FUN = length)
datasorted <- head(data1[order(data1$yelp_business.Review_count,decreasing = T),],100)

newdata <- subset(yelp_business,yelp_business.Categories %in% unique(datasorted$yelp_business.Categories))

# attributes <- read.table("")
cf <- unique(factor(newdata$yelp_business.Categories))

shinyUI(pageWithSidebar( headerPanel("All Businesses on Yelp"), 
                         
                         sidebarPanel( h3(''),
                                       selectInput("Categories", "Pick a Business Category",
                                                   choices = c('All', as.character(cf)), selected = "Restaurants"),
                                   uiOutput("Stars"),
                                   uiOutput("Reviews"),
                              
                                   helpText("Help:
                                                   You may use this map to view top-business for any business category
                                                   or to see trending businesses by using above sliders. ")        
                         ),
                         
                         
                         mainPanel(
                           leafletOutput("mymap",width="100%",height=500) 
                         ) ))

