library(shiny)
data(iris)
# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(iris), "Species")
species <- unique(iris$Species)
pageWithSidebar(
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        selectInput('subset', 'Species', species),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1')
    )
)