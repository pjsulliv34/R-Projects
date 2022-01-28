library(shiny)
library(rvest)
library(tidyverse)
library(knitr)
library(shinythemes)
library(tidyquant)
library(lubridate)
library(plotly)
library(ggplot2)


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


ui <- fluidPage(
    titlePanel("Stock Data Analysis"),
    #themeSelector(),
    theme = shinytheme('superhero'),
    sidebarLayout(
        sidebarPanel(
            textInput("stock", "Enter a stock Ticker: ","AAPL"),
            hr(),
            hr(),
            selectInput("date", "Date Range",
                        choices = date_list)),
            
       # actionButton("button","Update")),
            mainPanel(
                tabsetPanel(
                    tabPanel("Price Predictions",plotlyOutput("Pred_plot")),
                    tabPanel("Regression Standard Deviations", plotOutput("reg_plot")),
                    tabPanel("Close and Open", plotlyOutput("plot2") ),
           tabPanel("Yahoo Scrapped Data",tableOutput("table"))
          
                )
            )
        ))

server <- function(input, output, session){
    
  
    
  #observeEvent(input$button,{
       
        
        output$Pred_plot <- renderPlotly({
            
            
            
            
          #Stock =   reactive({input$stock})
         # Stock = Stock()
          Stock = input$stock
             
            
            start_date <- Sys.Date()
            
            retrieval_date <- start_date - years(4)
            
         Stock_data <- tq_get(Stock, get = "stock.prices", from = retrieval_date, to = start_date)
            Stock_data <- Stock_data %>%
                group_by(symbol) %>%
                tq_mutate(select = close,
                          mutate_fun = MACD,
                          col_rename = c("MACD", "Signal"))%>%
                tq_mutate(select = adjusted, mutate_fun = RSI) %>%
                tq_mutate(select = adjusted, mutate_fun = BBands, col_rename = "Bbands") %>%
                tq_mutate_xy(x = close, y =volume, mutate_fun = EVWMA, col_rename = "EVWMA") %>%
                mutate(Stock_movement = case_when(
                    lag(close)- close > 0 ~ "Decrease",
                    close > lag(close) ~ "Increase",
                    close == lag(close) ~"No Change"
                ) 
                ) %>%
                select(symbol:adjusted,Stock_movement,MACD,rsi,EVWMA)
            
            
            
            
            Stock_data <- Stock_data %>% mutate(forecast1day = lead(close,1),
                                                forecast1week = lead(close,7),
                                                forecast2week = lead(close,14),
                                                forecast4week = lead(close,31),
                                                forecast2month = lead(close,60),
                                                forecast4month = lead(close,120))
            
            Stock_data <- Stock_data[complete.cases(Stock_data),]
            
            
            current <- lm(close ~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecast1daymodel <- lm(forecast1day ~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecast1weekmodel <- lm(forecast1week ~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecast2weekmodel <- lm(forecast2week ~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecastmonthmodel <-lm(forecast4week~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecast2monthmodel <- lm(forecast2month~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            forecast4monthmodel <- lm(forecast4month~ volume + MACD+ rsi+ EVWMA , data = Stock_data)
            
            
            Stock_data$currentPrediction <- predict(current, Stock_data)
            Stock_data$day1prediction <- predict(forecast1daymodel, Stock_data)
            Stock_data$day7prediction <- predict(forecast1weekmodel, Stock_data)
            Stock_data$day14prediction <- predict(forecast2weekmodel, Stock_data)
            Stock_data$day31prediction <- predict(forecastmonthmodel, Stock_data)
            
            
            
            #pull new data
            
            
            
            start_date <- Sys.Date()
            start_date
            retrieval_date <- start_date - months(6)
            
            new_stock_data <- tq_get(Stock, get = "stock.prices", from = retrieval_date, to = start_date)
            
            
            new_stock_data <- new_stock_data %>%
                group_by(symbol) %>%
                tq_mutate(select = close,
                          mutate_fun = MACD,
                          col_rename = c("MACD", "Signal"))%>%
                tq_mutate(select = adjusted, mutate_fun = RSI) %>%
                tq_mutate(select = adjusted, mutate_fun = BBands, col_rename = "Bbands") %>%
                tq_mutate_xy(x = close, y =volume, mutate_fun = EVWMA, col_rename = "EVWMA") %>%
                mutate(Stock_movement = case_when(
                    lag(close)- close > 0 ~ "Decrease",
                    close > lag(close) ~ "Increase",
                    close == lag(close) ~"No Change"
                ) 
                ) %>%
                select(symbol:adjusted,Stock_movement,MACD,rsi,EVWMA)
            
            new_stock_data$monthforecast <- predict(forecastmonthmodel, new_stock_data)
            new_stock_data$week2forecast <- predict(forecast2weekmodel, new_stock_data)
            new_stock_data$month2forecast <- predict(forecast2monthmodel, new_stock_data)
            new_stock_data$month4forecast <- predict(forecast4monthmodel, new_stock_data)
            
            new_stock_data$monthforecastdate <- new_stock_data$date + days(31)
            new_stock_data$week2forecastdate <- new_stock_data$date +days(14)
            new_stock_data$month2forecastdate <- new_stock_data$date+ days(60)
            new_stock_data$month4forecastdate <- new_stock_data$date +days(120)
            
            
           new_stock_data %>%
                
                ggplot()+geom_line(aes(x = date, y = close, color = 'Actual'))+
             geom_line(data = new_stock_data %>% filter(monthforecastdate >Sys.Date()), aes(x = week2forecastdate,y = week2forecast, color = '2 Week Forecast'))+
             geom_line(data = new_stock_data %>% filter(monthforecastdate >Sys.Date()), aes(x = monthforecastdate,y = monthforecast, color= '1 Month Forecast'))+
             geom_line(data = new_stock_data   %>% filter(month2forecastdate > Sys.Date()),aes(x = month2forecastdate, y = month2forecast,color = '2 Month Forecast'))+
                geom_line(data = new_stock_data %>% filter(month4forecastdate > Sys.Date()),aes(x = month4forecastdate,y = month4forecast, color = '4 Month Forecast'))+
                
                
                labs(title = "Stock Price Predictions")+
                xlab("Date")+
                ylab("Price ($)")+
             scale_color_manual(name = "Predictions",
                                values = c("Actual" = 'black', '2 Month Forecast' = 'red', '4 Month Forecast' = 'green', '1 Month Forecast' = 'orange', '2 Week Forecast' = 'blue'))
            
            
        })
        
        
       
          
        
        output$reg_plot <- renderPlot({
            
            
            
            
            
            #stock =   reactive({input$stock})
            #stock = stock()
          
          stock = input$stock
            
            data <- tq_get(input$stock, get = "stock.prices", from = Year_4_date)
            data1<- data %>%
                filter(symbol == input$stock)%>%
                filter(date >= input$date )
            fit_new <- lm(data1$close~data1$date)
            sd1 <- sd(abs(fit_new$residuals))*2
            x <- plot(data1$date,data1$close, title("Regression Plot"),xlab = "Date", ylab = "Price ($)")+ abline(fit_new, col = "Red") +
                # points(x = data1$date, y = data1$close, col = "red")
                abline(fit_new$coefficients[1]+sd1,fit_new$coefficients[2],col = "blue")
            abline(fit_new$coefficients[1]-sd1,fit_new$coefficients[2], col = "Blue")
            abline(fit_new$coefficients[1]+2*sd1, fit_new$coefficients[2], col ="green")
            abline(fit_new$coefficients[1]-2*sd1, fit_new$coefficients[2], col = "green")
            print(x)
            
        })
        
       
    output$table <- 
        
        
        renderTable({
        
       url <- paste0("https://finance.yahoo.com/quote/",input$stock,"/?p=",input$stock)
        #indicator <- read_html(url) %>% html_nodes("td:nth-child(1)") %>% html_text()
        #value <- read_html(url) %>% html_nodes("td:nth-child(2)") %>% html_text()
        #stock_table <- tibble(indictor = indicator, value = value)
        #print(stock_table)
        
        
       x <- read_html(url) %>% html_nodes("div#quote-summary") %>% html_table() %>% data.frame() %>% rename("Indicators" = X1, "Values" = X2)
        
    })
    
   
    
    output$plot2 <- renderPlotly({
      
      
      
        
        
        data <- tq_get(input$stock, get = "stock.prices", from = Year_4_date)
        
        data  %>% filter(symbol == input$stock)%>%
          filter(date >= input$date )%>% select(date, open, close) %>% pivot_longer(!date)%>% rename("Indicator" = name) %>%
            ggplot() +
            geom_line(aes(x = date, y = value, color= Indicator))+
            ylab("Price")
    })
        
   # })
    
    
    
    #Add in Price Precitions
    #Add in something that scraps the top 10 companies using
    
}

    
shinyApp(ui = ui, server = server)








