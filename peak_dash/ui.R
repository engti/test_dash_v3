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
            menu = div("KPI Trends",id="myDiv"),
            content = list(
              htmlOutput("kpi_boxes"),
              div(class = "ui horizontal divider",  uiicon("tag"), "Channel Share & Trend Data"),
              div(class = "ui grid", 
                  div(class = "six wide column", plotOutput("channel_share")),
                  div(class = "ten wide column","Column")
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

