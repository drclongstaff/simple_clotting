library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(shinythemes)
library(bslib)

source("./Modules/Datload.R")
source("./Modules/Datproc.R")
source("./Modules/Datcalc.R")
source("./Modules/Makeplot.R")
source("./Modules/Plotone2.R")
source("./Modules/Plotsall2.R")
source("./Modules/ResTab.R")

ThisVersion <- "version 0.3.3"
ThisApp <- "Simple clotting app"
# thematic::thematic_shiny()
options(shiny.maxRequestSize = 30 * 1024^2) # allows for multiple Excel sheets

ui <- fluidPage(
  navbarPage(title = ThisApp),
  thematic::thematic_shiny(),
  theme = bs_theme(bootswatch = "lumen"),
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:#FEFEFE;}"),
      csvFileUI("datafile", h5("Load user data")),
      TabRes_ui("x"),
      oneplot_ui("x"),
      card(
        card_header(h6("Citation for publications"), class = "bg-dark"),
        helpText(h6(
          tags$i("Longstaff C, Development of a Shiny app tool to simplify and standardize the analysis
              of hemostasis assay data: communication from the SSC of the ISTH, J Thromb Haemost, 15: 1044-6, 2017"),
          tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1111/jth.13656", "DOI 10.1111/jth.13656")
        )),
        card_header(h6("Contact details"), class = "bg-light"),
        # tags$a(href="mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"))),

        tags$a(href = "mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"),
        helpText(h6(ThisApp, ThisVersion, " last accessed", Sys.Date()), ),
        card_header(
          class = "bg-light",
          tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "Links to other apps and help notes"),
          tags$br(),
          tags$a(href = "https://www.youtube.com/@colinlongstaff7270", "Youtube channel of help videos")
        )
      ),
    ),
    mainPanel(
      tabsetPanel(
        type = "tab",
        tabPanel(
          "Plot", # main opening page

          DatProc_ui("x"),
          plot_ui("x"),
          filenames_ui("x"),
          card(
            height = 450,
            full_screen = TRUE, # is expandable
            card_header(h5("Results table"),
              class = "bg-warning"
            ),
            card_body(
              style = "background-color: #FFFCF4;",
              layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(
                  open = TRUE,
                  fluidRow(
                    radioButtons("atab",
                      label = h6("Display results"),
                      choices = c("all results", "selected"), selected = "selected", inline = TRUE
                    )
                  ),
                  fluidRow(
                    selectInput("obs1", label = h6("Selected"), choices = c(colnames(myRes)), selected = "clot.time")
                  )
                ),
                tableOutput("obs"),
                tableOutput("tabres"), # For results file

                tableOutput("whichtab")
              )
            )
          )
        ),
        tabPanel(
          "Data",
          fluidRow(
            column(9, radioButtons("dattab",
              label = "Display data",
              choices = c("raw data", "processed data"), selected = "raw data", inline = TRUE
            )),
            tableOutput("datatabs")
          )
        ),
        tabPanel(
          "Help",
          # shinythemes::themeSelector(),
          tags$blockquote(h6(
            "► The app loads, displays and anlyses a set of data with some typical settings ",
            tags$br(),
            tags$br(),
            " ► You can load your own data as csv, txt or xlsx and the format will be detected ",
            tags$br(),
            tags$br(),
            " ► Your data should be organised as in the example data (see the Data tab) with a column of Time followed by any number  >1 of columns of absorbance readings",
            tags$br(),
            tags$br(),
            "► Try to avoid spaces or empty wells in data files",
            tags$br(),
            tags$br(),
            "► The green boxes are where parameters that affect curve analysis are adjusted: % clotted, threshold for interpolation, baseline and fixed or variable maximum",
            tags$br(),
            tags$br(),
            "► The default setting of 0.5 'proportion change' calculates a time for 50% clotting but can be changed",
            tags$br(),
            tags$br(),
            "► The code traps errors but if the data is noisy and the program crashes try increasing the 'threshold' absorbance value",
            tags$br(),
            tags$br(),
            "► Increasing the 'threshold' gives more robust analysis but is less accurate because it avoids interpolation and gives results at the nearest time point ",
            tags$br(),
            tags$br(),
            "► 'Baseline options' allow the baseline to be set in 3 ways using the radio buttons and additional input text boxes",
            tags$br(),
            tags$br(),
            "► Set the baseline to a satisfactory start point before clotting ",
            tags$br(),
            tags$br(),
            "► You can set the maximum clotting to the maximum for each curve or enter a global value where you know clotting is complete ",
            tags$br(),
            tags$br(),
            "► The blue boxes allow adjustable settings that affect the way the clotting curves are plotted",
            tags$br(),
            tags$br(),
            "► The orange box contains the table of results, either selected parameter or the complete set of results",
            tags$br(),
            tags$br(),
            "► Plots and results can be expanded to full screen for detailed inspection",
            tags$br(),
            tags$br(),
            "►There is a web page with links to my other apps and some detailed help pages",
            tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "Links to other apps"),
          )),
          # tags$img(src="Clots1.png", width=500, height=300),
          # tags$img(src="Tables1.png", width=500, height=200)
        )
      )
    )
  )
)
server <- function(input, output, session) {
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  procfile <- DatProc_server("x", reactive({
    datafile()
  }))
  plotfile <- oneplot_server("x", reactive({
    procfile()
  }), resfile)
  mplotfile <- plot_server("x", reactive({
    procfile()
  }), resfile)
  resfile <- TabRes_server("x", reactive({
    procfile()
  }))

  theobs <- reactive({
    resfile() |>
      select(input$obs1) |>
      pull()
  })
  myobs <- filenames_server("x", theobs)

  output$whichtab <- renderTable({
    switch(input$atab,
      "selected" = myobs(),
      "all results" = resfile()
    )
  })

  output$datatabs <- renderTable({
    switch(input$dattab,
      "raw data" = datafile(),
      "processed data" = procfile()
    )
  })
}


shinyApp(ui, server)
