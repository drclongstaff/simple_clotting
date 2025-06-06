# This function plots data from all the wells
one_plotFun <- function(PLATE, WELLNUM, TABRES) {
  # Set plotting parameters
  TabRes <- TABRES

  k <- WELLNUM

  par(mar = c(4, 4, 1, 1)) # dimensions for figure

  # Only one plot in this case so no looping needed
  plotmake_fun(PLATE, Time, TabRes, mint, maxt, maxy, samples, k, axx="s", axy="s")
}


library(shiny)
# plot module ----
oneplot_ui <- function(id) {
  ns <- NS(id)

  card(
    height = 450,
    full_screen = TRUE,
    card_header(
      numericInput(ns("wellnum"), label = "Plot well number", value = 1, min = 1, step = 1),
      class = "bg-info"
    ),
    card_body(
      style = "background-color: #FAFBFB ;",
      tagList(
        plotOutput(ns("oneplot"))
        # plotly::plotlyOutput(ns("oneplot"))
      )
    )
  )
}


oneplot_server <- function(id, procfile, resfile) {
  moduleServer(id, function(input, output, session) {
    # thenames <- reactive({colnames(procfile()[,-1])})
    # aname <- reactive({thenames()[input$wellnum]})
    plot <- reactive({
      one_plotFun(procfile(), input$wellnum, resfile())
    })
    output$oneplot <- renderPlot({
      plot()
    })
    # output$oneplot <- plotly::renderPlotly({plot()})
  })
}
