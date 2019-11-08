#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(ggplot2)
library(colourpicker)
source("function.R")
ui <- fluidPage(
    titlePanel("investing scenarios"),
    
    fluidRow(
        column(3, 
               sliderInput("Initial Amount",
                           "Initial Amount",
                           min = 0, max = 10000, 
                           step = 100,
                           pre = "$", sep = ",", 
                           value = 1000),
               sliderInput("Annual Contribution",
                           "Annual Contribution",
                           min = 0, max = 5000, 
                           step = 100,pre = "$", 
                           sep = ",", value = 200),
               sliderInput("Annual Growth Rate(in %)",
                           "Annual Growth Rate(in %)",
                           min = 0, max = 20, 
                           step = 0.1,value = 2)
               ),
        column(3,
               sliderInput("High Yield annual rate(in %)",
                           "High Yield annual rate(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 2),
               sliderInput("Fixed Income annual rate(in %)",
                           "Fixed Income annual rate(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 5),
               sliderInput("US Equality annual rate(in %)",
                           "US Equality annual rate(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 10)),
        column(3,
               sliderInput("High Yield volatility(in %)",
                           "High Yield volatility(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 0.1),
               sliderInput("Fixed Income volatility(in %)",
                           "Fixed Income volatility(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 4.5),
               sliderInput("US Equality volatility(in %)",
                           "US Equality volatility(in %)",
                           min = 0, max = 20, step = 0.1,
                           value = 15)),
        column(3,
               sliderInput("Year","Year",
                           min = 0, max = 50, step = 1,
                           value = 20),
               
               numericInput("Random seed",
                            "Random seed",value = 12345),
               
               selectInput("Facet?","Facet?",
                           choices = c("Yes","No"),
                           selected = "Yes")
        )
    ),
    fluidRow(
    column(12,plotOutput("rewards_plot"))
    )
)



server <- function(input,output){
    sliderValues <- reactive({
        data.frame(
            Name = c("Initial Amount",
                     "Annual Contribution",
                     "Annual Growth Rate(in %)",
                     "High Yield annual rate(in %)",
                     "Fixed Income annual rate(in %)",
                     "US Equality annual rate(in %)",
                     "High Yield volatility(in %)",
                     "Fixed Income volatility(in %)",
                     "US Equality volatility(in %)",
                     "Year",
                     "Random seed",
                     "Facet?"),
            Value = as.character(c(input$`Initial Amount`,
                      input$`Annual Contribution`,
                      input$`Annual Growth Rate(in %)`,
                      input$`High Yield annual rate(in %)`,
                      input$`Fixed Income annual rate(in %)`,
                      input$`US Equality annual rate(in %)`,
                      input$`High Yield volatility(in %)`,
                      input$`Fixed Income volatility(in %)`,
                      input$`US Equality volatility(in %)`,
                      input$`Year`,
                      input$`Random seed`,
                      input$`Facet?`)),
            stringsAsFactors = FALSE)
    
    })
    output$rewards_plot <- renderPlot({
        data <- sliderValues()
        high_yield <- as.numeric(data[c(1,2,3,4,7,10,11),2])
        high_yield <- rewards(high_yield[1],
                              high_yield[2],
                              high_yield[3]/100,
                              high_yield[4]/100,
                              high_yield[5]/100,
                              high_yield[6],
                              high_yield[7])
        fixed_income <- as.numeric(data[c(1,2,3,5,8,10,11),2])
        fixed_income <- rewards(fixed_income[1],
                                fixed_income[2],
                                fixed_income[3]/100,
                                fixed_income[4]/100,
                                fixed_income[5]/100,
                                fixed_income[6],
                                fixed_income[7])
        us_equality <- as.numeric(data[c(1,2,3,6,9,10,11),2])
        us_equality <- rewards(us_equality[1],
                               us_equality[2],
                               us_equality[3]/100,
                               us_equality[4]/100,
                               us_equality[5]/100,
                               us_equality[6],
                               us_equality[7])
        high_yield <- data.frame(high_yield,
                                 index=as.factor("high_yield"))
        fixed_income <- data.frame(fixed_income,
                                   index=as.factor("us_bonds"))
        us_equality <- data.frame(us_equality,
                                  index=as.factor("us_stocks"))
        rewards <- rbind(high_yield,fixed_income,us_equality)
        
        if(input$`Facet?`=="Yes"){
            rewards_plot <- 
                ggplot(data = data.frame(rewards),
                       aes(x=years,y=reward,fill=index,col=index))+
                geom_line(stat = "identity")+geom_point()+
                geom_area(position = "stack",alpha = 0.5)+
                facet_grid(.~index)+
                theme(
                    axis.text = element_text(size = 15),
                    plot.title = element_text(size = 20),
                    axis.title = element_text(size = 15),
                    legend.text = element_text(size = 15),
                    legend.title = element_text(size = 15),
                    strip.text.x = element_text(size = 15))
        }
        else{
            rewards_plot <-     
                ggplot(data = data.frame(rewards),
                       aes(x=years,y=reward,col=index))+
                geom_line(stat = "identity",size=1.3)+
                geom_point(size=2)+
                theme(
                    axis.text = element_text(size = 15),
                    plot.title = element_text(size = 20),
                    axis.title = element_text(size = 15),
                    legend.text = element_text(size = 15),
                    legend.title = element_text(size = 15))
        }
        rewards_plot
    })
}



shinyApp(ui, server)