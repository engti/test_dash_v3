#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shiny.semantic)
library(readr)
library(purrr)
library(purrrlyr)

## read data
df1 <- read_csv("aggregate_kpi.csv")


# Define UI for application that draws a histogram
shinyUI(
    semanticPage(
      title = "JCP Peak Dashboard",
      suppressDependencies("bootstrap"),
      ## header
      h2(class = "ui header", "JCP Peak Dashboard"),
      tabset(list(
        list(
          menu = div("KPI Trends"),
          content = htmlOutput("kpi2")
        ), 
        list(
          menu = div("Product Report"), 
          content = div("Second content",
                        uicards(class = "six",
                                df1 %>% 
                                  purrrlyr::by_row(~{
                                    uicard(
                                        div(
                                            class = "content",
                                            div(class = "header",paste0(.$`2018_visits`," ",.$channel)), 
                                            div(class = "meta", paste("+/- from last year",.$change))
                                            )
                                          )
                                  }) %>% {
                                    .$.out
                                  })
                        )),
        list(
          menu = div("Marketing Channel"), 
          content = div("Third content")
          )
          )
        )
    )
  )

