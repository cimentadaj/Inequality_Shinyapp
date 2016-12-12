library(shiny)
library(tidyverse)
library(artyfarty)

pisa <- read_csv("pisa.csv", col_names = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observe({
    if (input$graph_tab > 0)
      updateTabsetPanel(session, "tabs", selected = "Graphics")
  })

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
  
  output$input4 <- renderUI({
    selectInput("survey2", label = "Select the survey",
                choices = sort(unique(pisa$survey)))
  })
  
  output$input5 <- renderUI({
    selectInput("country2", label = "Select the country",
    choices = sort(unique(subset(pisa, survey == input$survey2)$country)))
  })
  
  output$input6 <- renderUI({
    selectInput("year2", label = "Pick the year",
    choices = sort(unique(subset(pisa, survey == input$survey2 & country == input$country2 )$year)))
  })
  
  output$input7 <- renderUI({
    selectInput("survey3", label = "Select the survey",
                choices = sort(unique(pisa$survey)))
  })
  
  output$input8 <- renderUI({
    selectInput("country3", label = "Select the country",
                choices = sort(unique(subset(pisa, survey == input$survey3)$country)))
  })
  
  output$input9 <- renderUI({
    selectInput("year3", label = "Pick the year",
                choices = sort(unique(subset(pisa, survey == input$survey3 & country == input$country3 )$year)))
  })
  
  
  filtered <- reactive({
    if (is.null(input$survey) | is.null(input$country) | is.null(input$year)) {
      return(NULL)
    }
    
    pisa1 <- pisa %>%
      filter(survey == input$survey, country == input$country, year == input$year)
    pisa1
  })
  
  filtered2 <- reactive({
    if (is.null(input$survey2) | is.null(input$country2) | is.null(input$year2)) {
      return(NULL)
    }
    
    pisa2 <- pisa %>%
      filter(survey == input$survey2, country == input$country2, year == input$year2)
    pisa2
  })
  
  filtered3 <- reactive({
    if (is.null(input$survey3) | is.null(input$country3) | is.null(input$year3)) {
      return(NULL)
    }
    
    pisa3 <- pisa %>%
      filter(survey == input$survey3, country == input$country3, year == input$year3)
    pisa3
  })
  
  ineq_graph <- reactive({
    if (is.null(filtered())) {
      return(NULL)
    }
    
    ggplot(filtered(), aes(score, rank, colour = as.factor(ses2))) +
      geom_point(alpha = 0.7, size = 3) +
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
  
  ineq_graph2 <- reactive({
    if (is.null(filtered2())) {
      return(NULL)
    }
    
    ggplot(filtered2(), aes(score, rank, colour = as.factor(ses2))) +
      geom_point(alpha = 0.7, size = 2) +
      ylab("Student ranking") +
      xlab("Math test score") +
      ggtitle(paste("Inequality for", input$country2, "in", input$year2)) +
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
  
  ineq_graph3 <- reactive({
    if (is.null(filtered3())) {
      return(NULL)
    }
    
    ggplot(filtered3(), aes(score, rank, colour = as.factor(ses2))) +
      geom_point(alpha = 0.7, size = 2) +
      ylab("Student ranking") +
      xlab("Math test score") +
      ggtitle(paste("Inequality for", input$country3, "in", input$year3)) +
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

  output$graph <- renderPlot({
    ineq_graph()
  })
  
  output$graph2 <- renderPlot({
    ineq_graph2()
  })
  
  output$graph3 <- renderPlot({
    ineq_graph3()
  })
  
})
