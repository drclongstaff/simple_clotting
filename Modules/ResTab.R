# myRes <- data.table::fread("clotRes.csv")
myRes <- vroom::vroom("clotRes.csv", delim = ",", show_col_types = FALSE)

# Get the column names

filenames_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # dataTableOutput(ns("obs"))
    DT::DTOutput(ns("obs"))
  )
}

filenames_server <- function(id, theobs) {
  moduleServer(id, function(input, output, session) {
    myobs <- reactive({
      AA <- matrix(theobs(), byrow = TRUE, nrow = input$numrows)
      # clipr::write_clip(AA)
      AA <- data.frame(AA)
      setNames(AA, rep(" ", length(AA)))
    })
  })
}
