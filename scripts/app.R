#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            radioButtons(inputId = "decoupage",
                         label = "Paramètre",
                         choices = c("Altitude", "Surface BV", "Largeur", "Pente")),
            radioButtons(inputId = "decoupage3",
                         label = "Statistique",
                         choices = c("Moyenne", "Médiane")),
            radioButtons(inputId = "decoupage2",
                         label = "Guilde",
                         choices = c("Rhéophiles", "Lithophiles", "Amphihalins", "EEE")),
            radioButtons(inputId = "decoupage4",
                         label = "Réseau",
                         choices = c("RCS", "RHP", "RRP"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     
    #     pres_alt <- rnorm(n = 100, 500, 200)
    #     abs_alt <- pres_alt * rnorm(n = 100, 1.3, 0.8)
    #     alt <- c(pres_alt, abs_alt) %>% 
    #         abs()
    #     
    #     pres <- c(rep("Présence", length(pres_alt)), rep("Absence", length(pres_alt)))
    #     
    #     df <- cbind(pres, alt) %>% as.data.frame() %>% 
    #         mutate(alt = as.numeric(alt))
    #     
    #     annees <- 1995:2019
    #     
    #     for(mon_annee in annees) {
    #         
    #         data_annee <- df %>% 
    #             mutate(annee = mon_annee,
    #                    alt = ifelse(pres == "Présence", alt + mon_annee / 100 * rnorm(1,1,0.3))) %>% 
    #             
    #     }
    #     
    #     
    # 
    #     # draw the histogram with the specified number of bins
    #     ggplot(data = df, aes(x = alt, fill = pres)) + geom_density(alpha = 0.4)
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
