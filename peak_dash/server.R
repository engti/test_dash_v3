# load libray

library(shiny)
library(shinyWidgets)
library(shiny.semantic)
library(stringi)
library(dplyr)
library(reshape2)
library(scales)

# read data
df_hour <- read_csv("hourlydata.csv")
# define factos
metric_levels <- c("Visits","Revenue","Orders","Orders per Visit","Average Order Value","$ per Visit")


shinyServer(function(input, output) {
  
  kpi_data <- reactive({
    df_kpi <- df_hour %>%
      select(reporting_year,data_status,metric,value) %>%
      filter(data_status == "current") %>%
      select(-data_status) %>%
      group_by(reporting_year,metric) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      dcast(reporting_year ~ metric,value.var = "value") %>%
      mutate(
        order_per_visit = orders / visits,
        avg_ord_value = revenue / orders,
        rev_per_visit = revenue / visits
      ) %>%
      melt(id.vars = "reporting_year") %>%
      dcast(variable ~ reporting_year,value.var = "value") %>%
      mutate(
        change = (`2018` - `2017`) / `2017`,
        text = case_when(
          change > 0 ~ paste0("+",percent(change)," from same period last year"),
          change < 0 ~ paste0(percent(change)," from same period last year"),
          change == 0 ~ paste0("unchanged from same period last year")
        )
      ) %>%
      mutate(
        variable = case_when(
          variable == "orders" ~ "Orders",
          variable == "revenue" ~ "Revenue",
          variable == "visits" ~ "Visits",
          variable == "order_per_visit" ~ "Orders per Visit",
          variable == "avg_ord_value" ~ "Average Order Value",
          variable == "rev_per_visit" ~ "$ per Visit"
        )
      ) %>%
      rename(metric = variable) %>%
      mutate(
        pretty_metric = case_when(
          metric == "Orders" ~ formatC(`2018`,format="d", big.mark=","),
          metric == "Revenue" ~ paste0("$",formatC(`2018`,format="d", big.mark=",")),
          metric == "Visits" ~ formatC(`2018`,format="d", big.mark=","),
          metric == "Orders per Visit" ~ paste0(sprintf(`2018`*100, fmt = '%#.2f'),"%"),
          metric == "Average Order Value" ~ paste0("$",formatC(`2018`,format="d", big.mark=",")),
          metric == "$ per Visit" ~ paste0("$",sprintf(`2018`, fmt = '%#.2f'))
        ),
        metric = factor(metric,levels = metric_levels)
      ) %>%
      arrange(metric)
  })
   
  channel_total <- reactive({
    df_hour %>%
      select(name,data_status,reporting_year,metric,value) %>%
      filter(data_status == "current",metric=="visits") %>%
      select(-data_status,-metric) %>%
      group_by(name,reporting_year) %>%
      summarise(visits = sum(value)) %>%
      dcast(name~reporting_year,value.var = "visits") %>%
      mutate(
        change = (`2018` - `2017`) / `2017`
      ) %>%
      arrange(desc(`2018`))
  })
  
  # outputs ----
  output$kpi_boxes <- renderUI({
    uicards(class = "six",
            kpi_data() %>% 
            purrrlyr::by_row(
              ~{uicard(
                div(class = "content",
                div(class = "header",.$pretty_metric),
                div(class = "description",stri_trans_totitle(.$metric)),
                div(class = "meta",.$text)
                ))
              }) %>% {
                .$.out
              })
  })
  
  output$channel_share <- renderPlot({
    channel_total() %>%
      ggplot(aes(reorder(name,`2018`),`2018`)) +
        geom_col() +
        geom_text(aes(label=scales::percent(change)), hjust=0, nudge_y=2000) +
        scale_y_comma(limits=c(0,max(tmp$`2018`)*1.2)) +
        coord_flip() +
        labs(x="",y="Visits",
             title="Channel Visits",
             caption="Data from Adobe Analytics") + 
        theme_ipsum_rc(grid="X")
  })
  
})
