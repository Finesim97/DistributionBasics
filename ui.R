#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

types = list("Normal" = 1, "LogNormal" = 2,"Binomial"=3,"Hypergeom."=4,
     "Eruptions" = 5)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Boxplots and Density Estimation" ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(position = "left",
        sidebarPanel(
            h2("Settings"),
            radioButtons("disttype", h3("Distribution"),
                         choices = types,selected = 1),
            helpText("Please select a distribution."),
            numericInput("seed",h3("Seed"),value = 42),
            helpText("Seed for sample generation"),
            numericInput("samples",h4("Samples"),value = 10, min = 1, max = 10**6),
            helpText("Number of samples to draw"),
            h3("Normal Dist. Parameters"),
            numericInput("mean",h4("Mean"),value = 0),
            helpText("Mean of the distribution"),
            numericInput("var",h4("Variance"),value = 1, min=0),
            helpText("Variance of the distribution"),
            h3("Discrete Distribution Parameters"),
            numericInput("draws",h4("Drawn balls"),value = 10, min=1),
            helpText("Number of balls to draw from the urn"),
            numericInput("hits",h4("Hit balls"),value = 10, min=1),
            helpText("Number of 'hit' balls in the urn"),
            numericInput("misses",h4("Miss balls"),value = 90, min=1),
            helpText("Number of 'miss' balls in the urn")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Boxplot Methods", plotOutput("boxplots",height = "600px"),h4("R Help"), verbatimTextOutput("helptext")),
                        tabPanel("Histogram", plotOutput("hist", height = "600px")),
                        tabPanel("Comparisons", plotOutput("combi", height = "750px"))
            )
        )
    )
))
