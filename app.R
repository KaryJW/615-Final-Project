library(knitr)
library(tm)
library(kableExtra)
library(devtools)
library(twitteR)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(ggmap)
library(DT)
library(shiny)





c1 <- c("Total","Different Platform")
c2 <- c("Rihanna","Taylor Swift")
c3 <- c("Plot","Table")
c4 <- c("Wordclouds","Top10")
ui <- fluidPage(
  titlePanel("Text Analysis-Between Rihanna and Taylor Swift"),
  navbarPage(
    "Type",
    theme = "bootstrap.min.css",
    
   tabPanel(
     "World Maps",
     sidebarLayout(
       sidebarPanel(
         selectInput("choosetbl",label = "Select", choices = c2)
       ),
       mainPanel(
         h4("Maps of the data points collected from twitter"),
         p("This shows the points in World map plot of topic Rihanna and Taylor Swift from twitter. It looks like most of the data points I collected are in the area of Europe and United States."),
         plotOutput("World_Map") 
         )
       )
     ),
   
   navbarMenu(
     "Data Source Platform",
     tabPanel("Plot",
        mainPanel(
        h4("This Plot shows the data counts from different platform."),
        p("Most of the tweets of #Rihanna are from Android, and then is Iphone. Most of the tweets contain topic #Taylor Swift are sent from IPhone"),
        plotOutput("Data_source")
        
                  )
   ),
              
    tabPanel("Table",
     mainPanel(
       tabsetPanel(
         id='dataset',
         tabPanel("Rihanna",DT::dataTableOutput("Table1")),
         tabPanel("Taylor Swift", DT::dataTableOutput("Table2"))
       ),
       h4("Table of the data counts from different platform.")
     ))),
   
   
   tabPanel(
     "Sentiment Anlysis",
     sidebarLayout(
       sidebarPanel(
         selectInput("choosetbl1",label = "Select", choices = c1)
       ),
       mainPanel(
         h4("This shows the Sentiment analysis. "),
         p("We can see in the total plot that most of the words are positive. " ),
         plotOutput("Sentiment")
       )
     )),
   
   navbarMenu(
     "WordClouds",
     tabPanel("Wordclouds",
       mainPanel(
         h4("The Plots show the Wordclouds of Rihanna and Taylor Swift."),
         img(src="wordcloud_riri.png", width=700,height=580),
         img(src="wordcloud_R.png", width=780,height=480)
                
              )
     ),
     tabPanel("TOP10",
              mainPanel(
                h4("Table of the data counts from different platform."),
                p("Top 1 word is rihanna, which is obviously. But it is suprise that the secon word is bieber. We can also see that fentybeauty-the cosmetics brand establised by Rihanna herself is also in top 10."),
                plotOutput("Top10")
              ))
  )
   
  
  
  )
  
)

  

      
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$World_Map <- renderPlot({
   
    if (input$choosetbl == "Rihanna") {
      ggplot() + geom_polygon(data = map.world, aes(x=long, y = lat, group = group),
                              fill="rosybrown",color="rosybrown") + coord_fixed(1.3)+
        geom_point(data= locationa11,aes(x = longitude, y = latitude),colour = "seagreen4",
                   alpha = 0.5,size = 5)+
        ggtitle("The Map of the Data Points-Rihanna")
      
    }
    else if (input$choosetbl == "Taylor Swift") {
      ggplot() + geom_polygon(data = map.world, aes(x=long, y = lat, group = group),
                              color="slategray",fill="slategray") + coord_fixed(1.3)+
        geom_point(data= locationaT1,aes(x = longitude, y = latitude),colour = "sienna1",
                    alpha = 0.5,size = 5)+
        ggtitle("The Map of the Data points--TS")
      
      
    
    }
  })
  output$Data_source <- renderPlot({
    
    gridExtra::grid.arrange(
      ggplot(a10_plat_5,aes(statusSource,y=n))+geom_bar(stat = "identity",width = 0.5,fill="lightpink")+
        coord_flip()+geom_text(aes(label=n),vjust=-.3,size=3.5)+
        theme_minimal()+
        ggtitle("The Counts of the Data From Different Platform-Rihanna"),
      ggplot(aT0_plat_5,aes(statusSource,y=n))+geom_bar(stat = "identity",width = 0.5,fill="khaki")+
        coord_flip()+geom_text(aes(label=n),vjust=-.3,size=3.5)+
        theme_minimal()+
        ggtitle("The Counts of the Data From Different Platform-TS"),
      ncol=1)
    
  })
  output$Table1 <- DT::renderDataTable({
   DT::datatable(a10_plat_5)
    })
  output$Table2 <- DT::renderDataTable({
    DT::datatable(aT0_plat_5)
  })
   
  
  output$Sentiment <- renderPlot({
    if (input$choosetbl1 == "Total") {
     gridExtra::grid.arrange(
       ggplot(a15_sent,aes(sentiment,y=n))+
        geom_bar(stat = "identity",fill="darksalmon")+geom_text(aes(label=n))+
        theme_minimal()+ggtitle("The Plot of Sentiment words--Total(Rihanna)")+coord_flip(),
       ggplot(aT5_sent,aes(sentiment,y=n))+geom_bar(stat = "identity",fill="khaki1")+geom_text(aes(label=n))+
         theme_minimal()+ggtitle("The Plot of Sentiment words--Total(TS)")+coord_flip(),
       ncol=1)
      
    }
    else if (input$choosetbl1 == "Different Platform") {
      gridExtra::grid.arrange(
      
      ggplot(a17, aes(x=statusSource, y=percent_of_tweets, fill = sentiment)) + 
        geom_bar(stat = "identity", position = "stack",width = 0.5) +
        scale_fill_brewer(palette = "RdBu") + xlab("Data Source Platform--Rihanna") +
        ylab("Percent of Tweets") + theme(axis.text.x =element_text(angle = 60,hjust = 1))+
        ggtitle("Sentiment of Different Source of Tweets"),
      ggplot(aT7, aes(x=statusSource, y=percent_of_tweets, fill = sentiment)) + 
        geom_bar(stat = "identity", position = "stack",width = 0.5) +
        scale_fill_brewer(palette = "RdBu") + xlab("Data Source Platform--TS") +
        ylab("Percent of Tweets") + theme(axis.text.x =element_text(angle = 60,hjust = 1))+
        ggtitle("Sentiment of Different Source of Tweets"),
      ncol=2)
    }
  
    
  })
    
  
 
    output$Top10 <- renderPlot({
      gridExtra::grid.arrange(
        ggplot(a15fd[1:10,],aes(x=reorder(factor(word), -freq),y=freq))+geom_bar(stat="identity",aes(fill=factor(word)))+
          theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle("Top 10--Rihanna")+
          xlab("Top10 Words") +
          ylab("Frequency"),
       ggplot(aT5fd[1:10,],aes(x=reorder(factor(word), -freq),y=freq))+geom_bar(stat="identity",aes(fill=factor(word)))+
         theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle("Top 10 --Taylor Swift")+
         xlab("Top10 Words") +
         ylab("Frequency"),
       ncol=2)
      
    })
  

}


# Run the application 
shinyApp(ui = ui, server = server)

