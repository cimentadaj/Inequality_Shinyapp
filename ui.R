library(shiny)

shinyUI(fluidPage(
    titlePanel("Educational inequality around the world"),
      mainPanel(
        p("For quite some time public and academic interest on educational inequality
          has been growing. The objective of this app is to help inform researchers
          and interested users in the actual status of inequality in their country
          and how it came to be there.", align = "justify"),
        
        p("The following app depicts the current level of inequality for any given country
           at some specific years. The database was constructed by merging all available",
          a("PISA", href = "http://www.oecd.org/pisa/aboutpisa/"), "waves,
           all available",
          a("PIRLS and TIMSS", href = "http://timssandpirls.bc.edu/about.html"),
          "waves, and all available",
          a("LLECE", href = "http://www.unesco.org/new/en/santiago/education/education-assessment-llece/"),
          "waves.", align = "justify"),
        p("The theoretical argument behind this graph comes from the work of John Roemer while the idea
          behind the graph
          comes from Bradbury, Corak, Waldfogel and Washbrook (2015)."),
        br(),
        p("In a country,
          suppose we give a national examiniation, so all children took the same test on mathematics, for example.
          Suppose we separate all children into three rooms based on their parents education: low educated,
          middle educated and high educated. Now, within the 'low educated' room, suppose we create
          a ranking where the brightest student is assigned the first number the first and so on.
          So all children now have a ranking.
          Assume we repeated that for the two remaining rooms. We would a ranking for each room.
          Let's ask the children to come out of the rooms and stand next to their corresponding
          ranking from the other rooms, so that the 1st child of all three groups are together and so on.
          Finally, each child carries a sign that shows their score on the test."),
          p("The interesting thing about this exercises is that if family background and parental education had",
          strong("nothing"), "to do with the child's performance, we should expect
          that, on average, similar rankings should have similar scores. The farther each ranking-pair
          is from each other, the stronger the family background-effect is. We can see a graphical example here:"),
        img(src = "belgium.png", width = 800, height = 600)
    )
  )
)


# Following the work of Bradbury, Corak, Waldfogel and Washbrook (2015) and
# John Roemer,"

