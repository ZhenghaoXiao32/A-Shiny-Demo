library(shiny)


vars <- setdiff(names(iris), "Species")

ui <- pageWithSidebar(
    headerPanel('Iris K-means Clustering'),
    sidebarPanel(
        h4("Pick 2 variables and number of clusters to start k-means clustering: "),
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Plot of Clustering", plotOutput("plot1")),
            tabPanel("Centers of Clustering", tableOutput("table1"))
        )
    )
)


server <- function(input, output, session) {
    
    selectedData <- reactive({
        iris[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        text(clusters()$centers, cex = 4, lwd = 4)
    })
    output$table1 <- renderTable({
        cbind(data.frame(Clusters = as.integer(seq(1, nrow(kmeans(selectedData(), input$clusters)$centers), by = 1))),
              data.frame(kmeans(selectedData(), input$clusters)$centers))
    })
    
}
    
    
# Run the application 
shinyApp(ui = ui, server = server)
