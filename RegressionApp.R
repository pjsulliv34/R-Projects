library(tidyverse)
library(tidyquant)
library(dplyr)

stocks <-  c("AXP","AMGN","AAPL","BA","CAT","CSCO","CVX","GS","HD","HON","IBM","INTC","JNJ","KO","JPM","MCD","MMM","MRK","MSFT","NKE","PG","TRV","UNH","CRM","VZ","WBA","WMT","DIS")

Today = Sys.Date()
Year_4_date = Today - years(4)

month1_date = Today - months(1)
month2_date = Today - months(2)
month3_date = Today - months(3)
month4_date = Today - months(4)
month5_date = Today - months(5)
month6_date = Today - months(6)
month10_date = Today - months(10)
year1_date = Today- years(1)
Year_4_date = Today - years(4)

date_list <- c("1 Month" = month1_date, "2 Months" = month2_date, "3 Months" = month3_date, "4 months" = month4_date, "5 months" = month5_date,
               "6 Months"=month6_date, "10 Months"=month10_date, "1 Year"=year1_date , "4 Years"=Year_4_date)


library(shiny)
ui <- fluidPage(
    titlePanel("Stock Prices"),
    sidebarLayout(
        sidebarPanel(
            textInput("stock", "Enter in a Stock ID", value = "GE"),
            hr(),
            helpText("Pick a Stock from the Predetermined List"),
            
            
            
            selectInput("date", "Date Range",
                        choices = date_list),
           hr(),
            helpText("Pick a Date Range"),
            actionButton("gobutton","Submit")
            
        ),
        #create a spot for the plot
        mainPanel(tabsetPanel(
            tabPanel("Regression",plotOutput("StockPlot")),
            tabPanel("Prices",plotOutput("StockPlot2")),
            tabPanel("Volume",plotOutput("StockPlot3"))
            #,width = "auto", height= "40pc"
            #tabPanel("Combined",
            #        fluidRow(
            #           column(width = 5,plotOutput("StockPlot")),
            #          column(width = 5,plotOutput("SockPlot2")),
            #        column(width = 5,plotOutput("StockPlot3"))
            #))
        ))
    ))





server <- function(input, output){
    
                  
    output$StockPlot <- renderPlot({
        data <- tq_get(input$stock, get = "stock.prices", from = Year_4_date)
        data1<- data %>%
            filter(symbol == input$stock)%>%
            filter(date >= input$date )
        fit_new <- lm(data1$close~data1$date)
        sd1 <- sd(abs(fit_new$residuals))*2
        x <- plot(data1$date,data1$close, title(input$stock))+ abline(fit_new, col = "Red") +
            # points(x = data1$date, y = data1$close, col = "red")
            abline(fit_new$coefficients[1]+sd1,fit_new$coefficients[2],col = "blue")
        abline(fit_new$coefficients[1]-sd1,fit_new$coefficients[2], col = "Blue")
        abline(fit_new$coefficients[1]+2*sd1, fit_new$coefficients[2], col ="green")
        abline(fit_new$coefficients[1]-2*sd1, fit_new$coefficients[2], col = "green")
        print(x)
    })
    
    output$StockPlot2 <- renderPlot({
        data <- tq_get(input$stock, get = "stock.prices", from = Year_4_date)
        data1<- data %>%
            filter(symbol == input$stock)%>%
            filter(date >= input$date )
        
        x <- data1 %>%
            ggplot2::ggplot() +
            geom_line(aes(x = date, y = adjusted))+
            geom_line(aes(x = date, y = close))+
            geom_line(aes(x = date, y = open))
        print(x)
    })
    
    output$StockPlot3 <- renderPlot({
        data <- tq_get(input$stock, get = "stock.prices", from = Year_4_date)
        data1<- data %>%
            filter(symbol == input$stock)%>%
            filter(date >= input$date )
        
        x <- data1 %>%
            ggplot2::ggplot() +
            geom_line(aes(x = date, y = volume))
        
        print(x)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

