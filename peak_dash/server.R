# load libray

library(shiny)
library(shinyWidgets)
library(shiny.semantic)
library(stringi)
library(dplyr)
library(reshape2)
library(scales)

# Define server logic required to draw a histogram
df_hour <- read_csv("hourlydata.csv")

shinyServer(function(input, output) {
  
  kpi_data <- reactive({
    df_kpi <- df_hour %>%
      select(reporting_year,data_status,metric,value) %>%
      filter(data_status == "current") %>%
      select(-data_status) %>%
      group_by(reporting_year,metric) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      dcast(metric ~ reporting_year,value.var = "value") %>%
      mutate(
        change = (`2018` - `2017`) / `2017`,
        text = case_when(
          change > 0 ~ paste0(percent(change)," from same period last year"),
          change < 0 ~ paste0(percent(change)," from same period last year"),
          change == 0 ~ paste0("unchanged from same period last year")
        )
      )
  })
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$kpi <- renderUI({
    div(
      uicard(
        div(class = "six",
            div(class = "header","127.18K"),
            div(class = "description", "+5% from last year"),
            div(class = "meta", "Visits"))
      )
    )
  })
  
  output$kpi2 <- renderUI({
    uicards(class = "three",
            kpi_data() %>% 
            purrrlyr::by_row(
              ~{uicard(
                div(class = "content",
                div(class = "header",formatC(.$`2018`,format="d", big.mark=",")),
                div(class = "description",stri_trans_totitle(.$metric)),
                div(class = "meta",.$text)
                ))
              }) %>% {
                .$.out
              })
  })
  
})
