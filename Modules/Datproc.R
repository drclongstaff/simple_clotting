# Initial data processing

# Some functions for data processing


# This function subtracts a background from all data defined by row in all data
BaselineNP <- function(n, NP) {
  n <- n - n[NP]
}

# Subtract the min abs of each column and include an offset
BaselineOff <- function(n, OF) {
  n <- n - min(n) - OF
}

library(shiny)
# plot module ----
DatProc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # fluidRow(),
    # tags$h5("Baseline options"),
    card(
      height = 300,
      # full_screen = TRUE,
      card_header(h5("Baseline and curve maximum options"), class = "bg-success"),
      card_body(
        style = "background-color: #F6FCF4;",
        fluidRow(
          column(7, radioButtons(ns("abini"),
            label = h5("Baseline options"), inline = TRUE,
            choices = c(
              "global zero", "nth absorbance",
              "min + offset"
            )
          )),
          column(5, radioButtons(ns("abfin"),
            label = h5("Max absorbance options"), inline = TRUE,
            choices = c(
              "each curve max",
              "global max"
            ),
            selected = "each curve max"
          )),
          fluidRow(
            column(2, numericInput(ns("back"),
              label = h6("global zero"), step = 0.005, value = 0.042
            )),
            column(2, numericInput(ns("arow"),
              label = h6("nth point"), value = 1
            )),
            column(2, numericInput(ns("off"), label = h6("offset"), value = 0, min = 0, step = 0.001)),
            column(3, NULL),
            column(3, numericInput(ns("finabs"),
              label = h6("global max"), step = 0.05, value = 0.4
            ))
          ),

          # tags$h6("Maximum absorbance options"),

          fluidRow(),

          # column(3, tags$h4("")),
        )
      )
    ),
    tableOutput(ns("procdat"))
  )
}

DatProc_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # data2 is for subtracting a fixed specified background
    data <- reactive({
      as_tibble(df())
      # as_tibble(cbind("Time"=df()[[1]], df()[,-1]-input$back))
    })
    # data2 is for subtracting a fixed specified background
    data2 <- reactive({
      as_tibble(cbind("Time" = df()[[1]], df()[, -1] - input$back))
    })

    # data3 is data with absorbance subtracted from a point number (row)
    data3 <- reactive({
      data3 <- map_df(df()[, -1], ~ BaselineNP(.x, input$arow)) %>% # BaselineNP is a function defined above
        add_column("Time" = df()[[1]], .before = TRUE) #
    })

    # data4 is data with min value and optimum offset subtracted
    data4 <- reactive({
      map_df(df()[, -1], ~ BaselineOff(.x, input$off)) %>%
        add_column("Time" = df()[[1]], .before = TRUE) #
    })

    procdat <- reactive({
      switch(input$abini,
        # "global zero"=data2(),
        "global zero" = data(),
        "nth absorbance" = data3(),
        "min + offset" = data4()
      )
    })
  })
}
