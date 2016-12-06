library(shiny)
library(tidyverse)
library(artyfarty)

pisa <- read_csv("pisa.csv", col_names = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$input1 <- renderUI({
    selectInput("survey", label = "Select the survey",
                choices = sort(unique(pisa$survey)))
  })
  
  output$input2 <- renderUI({
    selectInput("country", label = "Select the country",
                choices = sort(unique(subset(pisa, survey == input$survey)$country)))
  })
  
  output$input3 <- renderUI({
    selectInput("year", label = "Pick the year",
                choices = sort(unique(subset(pisa, survey == input$survey & country == input$country )$year)))
  })
  
  output$graph <- renderPlot({
    pisa %>%
    filter(survey == input$survey, country == input$country, year == input$year) %>%
    ggplot(aes(score, rank, colour = as.factor(ses2))) +
    geom_point(alpha = 0.7, size = 2) +
    ylab("Student ranking") +
    xlab("Math test score") +
    ggtitle(paste("Inequality for", input$country, "in", input$year)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_color_discrete(name = "Parent's education",
                         labels = c("Low educated",
                                    "Middle educated",
                                    "High educated")) +
    scale_alpha(guide = F) +
    xlim(0, 1000) +
    theme_scientific() +
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
          legend.position = c(0.85, 0.2),
          axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"),
          legend.text=element_text(size=14),
          legend.title = element_text(size=14))

  })
  
})
