library(shiny) 
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr) 

Surveillance <- data.frame(
  Date = seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days"),
  Disease_1 = sample(1:100,365,replace=T), Disease_2 =sample(1:100,365,replace=T))
Surveillance <- Surveillance %>% mutate(
  Week = format(Date, "%Y-%m-%U"))

ui <- fluidPage(
  dateRangeInput("daterange", "Choice the date",
                 start = min(Surveillance$Date),
                 end = max(Surveillance$Date),
                 min = min(Surveillance$Date),
                 max = max(Surveillance$Date),
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'Week', language = 'fr', weekstart = 1),  
  selectInput(inputId = 'Diseases',
              label='Diseases',
              choices=c('Disease_1','Disease_2'),
              selected='Disease_1'),
  plotOutput("barplot"))


server <- function(input, output) {
  
  dateRangeInput<-reactive({
    dataset <- subset(Surveillance, Date >= input$daterange[1] & Date <= input$daterange[2]) 
    dataset <- dataset[,c(input$Diseases, "Week")]
    dataset
  })
  
  
  output$barplot <-renderPlot({
    ggplot(data=dateRangeInput(), aes_string(x="Week",y=input$Diseases))  +
      stat_summary(fun.y = sum, geom = "bar",colour="#56B4E9",fill="#56B4E9") +
      geom_bar(stat="identity") +
      labs(title=input$Diseases, y ="Number") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}
shinyApp (ui = ui, server = server)
