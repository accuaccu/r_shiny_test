#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random walk"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number observations:",
                        min = 1,
                        max = 100,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", click = "plot_click"),
           verbatimTextOutput("info")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Load data
    dt_data_set <- fread('simulated_data.csv')

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        par(pty = 's')
        plot(dt_data_set$V1,
             type = 'l',
             yaxt = 'n',
             ylim = c(
                 floor(min(dt_data_set)),
                 ceiling(max(dt_data_set))
             ),
             xlab = 'Index',
             ylab = 'Cumulative sum of x'
        )
        axis(side = 2,
             at = seq(
                 floor(min(dt_data_set)),
                 ceiling(max(dt_data_set)),
                 1
             ),
             las = 2)
    })
    
    output$info <- renderText(
        {
            paste0('x = ', input$plot_click$x,'\ny = ', input$plot_click$y)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
