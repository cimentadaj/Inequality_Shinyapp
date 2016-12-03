library(shiny)
library(tidyverse)

pisa <- read_csv("pisa.csv", col_names = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$input1 <- renderUI({
    selectInput("country", label = "Select your country",
                choices = sort(unique(pisa$country)))
  })
  
  output$input2 <- renderUI({
    selectInput("year", label = "Pick the year",
                choices = sort(unique(subset(pisa, pisa$country == input$country)$year)))
  })
  
  output$graph <- renderPlot({
    pisa %>%
    filter(country == input$country, year == input$year) %>%
    ggplot(aes(score, rank, colour = as.factor(ses2), alpha = 0.05)) +
    geom_point() +
    ylab("Student ranking") +
    xlab("Math test score") +
    ggtitle(paste("Inequality for", input$country, " in", input$year)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_color_discrete(name = "Parent's education",
                         labels = c("Low educated",
                                    "Middle educated",
                                    "High educated")) +
    scale_alpha(guide = F) +
    xlim(0, 1000) +
    theme_scientific() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.85, 0.2))
  })
  
})
