library(shiny)
library(tidyverse)
library(ggplot2)
library(devtools)
library(httr)
library(jsonlite)
library(forecast)
library(markdown)
library(DT)

#user interface


ui <- navbarPage("COVID-19 Data",
                 tabPanel("Graph",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Type",label = "Type",
                                          choices = c("1Y","5Y","10Y","25Y","MAX"),
                                          selected = "1Y"
                              ),
                              selectInput(inputId = "Country",label = "Country",
                                          choices = c("Afghanistan"),
                                          selected = "Afghanistan"
                              ),
                              dateRangeInput(inputId = "Range",label = "Range",
                                             start = "2020-01-01",
                                             end = "2021-03-11",
                                             min = "2020-01-01",
                                             max = Sys.Date())
                              
                            ),
                            mainPanel(
                              h2(textOutput(outputId = "Total_Global")),
                              h2(textOutput(outputId = "Total_Country")),
                              plotOutput(outputId = "histplot")
                            )
                          )
                 ),
                 tabPanel("Forecast",
                          plotOutput(outputId = "forecastplot"),
                          dataTableOutput(outputId = "forecasttable")
                 ),
                 tabPanel("About",
                          h2("Information about COVID-19"),
                          br(),
                          h4("Coronavirus disease (COVID-19) is an infectious disease caused by a newly discovered coronavirus.
           Most people infected with the COVID-19 virus will experience mild to moderate respiratory
           illness and recover without requiring special treatment. Older people,
           and those with underlying medical problems like cardiovascular disease, diabetes,
           chronic respiratory disease, and cancer are more likely to develop serious illness."),
                          h3("Symptoms"),
                          uiOutput("myList"),
                          br(),
                          h2("More Info"),
                          uiOutput("tab")
                          
                 )
)

# Server function
server <- function(input, output, session) {
  
  #Get all the country and their Slug in URL
  country_code <- reactive({input$Country %>% tolower() %>% str_replace(" ","-")})
  
  # All the Available Country
  link <- GET("https://api.covid19api.com/summary")
  available <- fromJSON(content(link, as = "text", encoding = "UTF-8"))
  
  # Fill the Country
  observeEvent(
    input$Type,
    updateSelectInput(session, "Country",
                      choices = c(available$Countries %>% group_by(Country) %>% summarise())))
  
  
  #Text Title
  output$Total_Global <- renderText({
    str_glue("Global Total Confirmed Cases: {number}",number = available$Global$TotalConfirmed %>% format(., big.mark = ",", scientific = FALSE))
  })
  
  output$Total_Country <- renderText({
    
    str_glue("Global Total Deaths: {number}",number = available$Global$TotalDeaths %>% format(., big.mark = ",", scientific = FALSE))
  })
  
  
  # create the country plot
  output$histplot <- renderPlot({
    
    
    endpoint <- str_glue("https://api.covid19api.com/total/country/{country}/status/{type}?from={start}T00:00:00Z&to={end}T00:00:00Z",
                         type = tolower(input$Type),
                         country = country_code(),
                         start = input$Range[1],
                         end = input$Range[2])
    w <- GET(endpoint)
    
    case <- fromJSON(content(w, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    #need to change the date in english form since my computer is in Chinese
    Sys.setlocale("LC_TIME", "C")
    data <- data.frame(day = as.Date(case$Date),value = case$Cases)
    
    ggplot(data, aes(x=day, y=value)) + geom_area(fill = "lightblue") + geom_line(size=1,color = "Black") +
      ggtitle(paste("Total Number of",input$Type,"Cases in",input$Country , "from",input$Range[1] ,"to", input$Range[2])) + xlab("Days") + ylab("Cases") + scale_x_date(date_labels = "%Y %b %d") + 
      theme(axis.text = element_text(size = 15)) + theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  ## FORCAST TABS
  # create forecast plot
  output$forecastplot <- renderPlot({
    
    
    endpoint1 <- str_glue("https://api.covid19api.com/total/country/{country}/status/{type}?from={start}T00:00:00Z&to={end}T00:00:00Z",
                          type = tolower(input$Type),
                          country = country_code(),
                          start = input$Range[1],
                          end = input$Range[2])
    r <- GET(endpoint1)
    
    case1 <- fromJSON(content(r, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    data1 <- data.frame(day = as.Date(case1$Date),value = case1$Cases)
    
    fit <- auto.arima(data1$value)
    plot(forecast(fit, h=20),main = paste("20 Day Forecast of COVID-19",input$Type, "Cases for", input$Country),ylab="Cases",xlab = paste("Number of Date Since",input$Range[1]))
  })
  
  output$forecasttable <- renderDataTable({
    
    endpoint2 <- str_glue("https://api.covid19api.com/total/country/{country}/status/{type}?from={start}T00:00:00Z&to={end}T00:00:00Z",
                          type = tolower(input$Type),
                          country = country_code(),
                          start = input$Range[1],
                          end = input$Range[2])
    e <- GET(endpoint2)
    
    case1 <- fromJSON(content(e, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    data1 <- data.frame(day = as.Date(case1$Date),value = case1$Cases)
    
    fit <- auto.arima(data1$value)
    
    forecast(fit, h=20) %>% as.tibble() %>% rename(c("80% CI Lower Bound"="Lo 80","80% CI Upper Bound"="Hi 80","95% CI Lower Bound"="Lo 95","95% CI Upper Bound"="Hi 95"))
  })
  
  
  ## ABOUT TABS
  # Bullet Point
  output$myList <- renderUI(HTML("<ul><li>Fever or chills</li>
                                 <li>Cough</li>
                                 <li>Shortness of breath or difficulty breathing</li>
                                 <li>Fatigue</li>
                                 <li>Muscle or body aches</li>
                                 <li>Headache</li>
                                 <li>New loss of taste or smell</li>
                                 <li>Sore throat</li>
                                 <li>Congestion or runny nose</li>
                                 <li>Nausea or vomiting</li>
                                 <li>Diarrhea</li></ul>"))
  
  # hyperline in about section
  url <- a("Centers for Disease Control and Prevention", href="https://www.cdc.gov/coronavirus/2019-ncov/index.html")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  
}


shinyApp(ui, server)
