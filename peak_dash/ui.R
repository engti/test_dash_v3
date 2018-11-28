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
# df1 <- read_csv("aggregate_kpi.csv")


# Define UI for application that draws a histogram
shinyUI(
    semanticPage(
      title = "JCP Peak Dashboard",
      suppressDependencies("bootstrap"),
      ## header
      h2(class = "ui header", "JCP Peak Dashboard"),
      tabset(
        list(
          list(
            menu = div("KPI Totals",id="kpi_totals"),
            content = list(
              htmlOutput("kpi_boxes"),
              # div(class = "ui horizontal divider",  uiicon("tag"), "Channel Share & Trend Data"),
              div(class = "ui divider"),
              div(class = "ui grid", 
                  div(class = "six wide column",
                      h4(class = "ui header", uiicon("mobile alternate icon"), div(class = "content","Visits by Channel")),
                      plotOutput("channel_share")),
                  div(class = "five wide column",
                      h4(class = "ui header", uiicon("mobile alternate icon"), div(class = "content","Visits by Channel")),
                      plotOutput("chart_brand")),
                  div(class = "five wide column",
                      h4(class = "ui header", uiicon("chart line icon"), div(class = "content","Key Metric Trend"))
                      )
                  )
              )
          ), 
          list(
            menu = div("Product Report"), 
            content = div("Second content")
            ),
          list(
            menu = div("Marketing Channel"), 
            content = div("Third content")
            )
          )
        )
    )
  )

