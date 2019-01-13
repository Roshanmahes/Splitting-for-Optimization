### Renders a table using Shiny. ###
# Source: https://plot.ly/r/datatable/

library(shiny)
library(DT)
library(crosstalk)

m <- read.table("results.txt",header=T)

ui <- fluidPage(
  h1("SCO Results",align="center"),
  DT::dataTableOutput("x1"),
  fluidRow(p(class='text-center',downloadButton('SCOTable',
                                  'Download Filtered Data')))
)

server <- function(input, output) {
  
  d <- SharedData$new(m,~rowname)
  
  # highlight selected rows in the table
  output$x1 <- DT::renderDataTable({
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m)
    if (NROW(m2)) {
      DT::formatStyle(dt,"rowname",target="row",color=
                DT::styleEqual(m2$rowname,rep("white",length(m2$rowname))),
                backgroundColor=DT::styleEqual(m2$rowname,
                rep("black",length(m2$rowname))))
    } else {dt}
  })
  
  # download the filtered data
  output$SCOTable = downloadHandler('SCOTable.csv', content=function(file) {
    s <- input$x1_rows_selected
    if (length(s)) {
      write.csv(m[s,,drop=FALSE],file)
    } else {
      write.csv(m[d$selection(),],file)
    }
  })
}

shinyApp(ui, server)
