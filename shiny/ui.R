library(shiny)
library(shinythemes)
## Add save plot button

align <- "justify"

shinyUI(
  tabsetPanel(
    tabPanel("Introduction",
    fluidPage(theme = shinytheme("readable"),
    titlePanel("Educational inequality around the world"),
    sidebarLayout(
      sidebarPanel = NULL,
      # "For comments, suggestions or collaborations, feel free to contact me at",
      # a("cimentadaj@gmail.com", href = "cimentadaj@gmail.com"), "or visit my website at",
      # a("www.jorgecimentada.com", href = "http://www.jorgecimentada.com"),
      # ".",
      # br(),
      # br(),
      # br(),
      # "Any commits are welcome at the apps",
      # a("Github repository", href = "https://github.com/cimentadaj/Inequality_Shinyapp")
      # )
      mainPanel(
        p("In the last couple of years, public and academic interest on educational inequality
          has been growing. The objective of this application is to help inform researchers
          and interested users in the evolution and actual level of educational
          inequality in their countries.", align = align),
        
        p("This app depicts the current level of inequality for any given country
           in some specific years. The database was constructed by merging all available",
          a("PISA", href = "http://www.oecd.org/pisa/aboutpisa/"), "waves,
           all available",
          a("PIRLS and TIMSS", href = "http://timssandpirls.bc.edu/about.html"),
          "waves, and all available",
          a("LLECE", href = "http://www.unesco.org/new/en/santiago/education/education-assessment-llece/"),
          "waves.", align = align),
        p("The theoretical argument behind this graph comes from the work of John Roemer while the artistic
          idea behind the graph comes from Bradbury, Corak, Waldfogel and Washbrook (2015)."),
        p("In any given country,
          suppose we give a national examiniation; all children took the same test on mathematics, for example.
          Suppose we separate all children into three rooms based on their parents education: low educated,
          middle educated and high educated. Now, within the 'low educated' room, suppose we create
          a ranking where the brightest student is assigned the 100th ranking, the second brightest
          the 99th ranking, and so on. So all children have a place in the ranking.
          Assume we repeated the same thing for the two remaining rooms.
          Let's ask the children to come out of the rooms and stand next to their corresponding
          ranking from the other two rooms, so that the 100th child of all three groups are together and so on.
          Finally, each child carries a sign that shows their score on the mathematics test.", align = align),
        p("If family background and parental education had",
          strong("nothing"), "to do with the child's performance, we should expect
          that, on average, similar rankings should have similar scores. The farther each ranking-pair
          is from each other, the stronger the family background effect is.", align = align),
        p("We can see a graphical example here:", align = align),
        img(src = "belgium.png", width = 900, height = 600, align = align),
        br(), br(), br(),
        p("This scatterplot exemplifies the above by highlighting the position of the 50th rank of the lower educated children.
          The 50th rank of the middle class (green line), for example, has 53 more points than the lower class.
          On top of that, the 50th rank from the high class (red line) has 55 more points than the middle class.
          This plot serves as an intuitive measurement of the degree of educational inequality in a country:
          the more separated the colored lines are, the higher the achievement inequality. Feel free to explore
          your countries' level of inequality and see how it's evolved over time.", align = align),
        br(),
        actionButton("graph_tab", "Click here to access your countries' plot.", icon = icon("flag")), align = align)))),

        tabPanel("Graphics",
                 fluidPage(
                   fluidRow(column(4, 
                           uiOutput('input1')),
                           column(5,
                           uiOutput('input2')),
                           column(3,
                           uiOutput('input3'))),
                           hr(),
                           mainPanel(plotOutput("graph", width = 1200, height = 600)))),
        tabPanel("Compare two countries",
                 fluidPage(
                  fluidRow(column(2, 
                           uiOutput('input4')),
                           column(2,
                           uiOutput('input5')),
                           column(2,
                           uiOutput('input6')),
                           column(2, 
                           uiOutput('input7')),
                           column(2,
                           uiOutput('input8')),
                           column(2,
                           uiOutput('input9'))),
                   hr(),
                   mainPanel(
                     fluidRow(
                     splitLayout(cellWidths = c("75%", "75%"),
                                 plotOutput("graph2"),
                                 plotOutput("graph3"))
                   )))),
    id = "tabs", selected = "Introduction"
  )
)

# splitLayout(
#   plotOutput("graph")
# )
