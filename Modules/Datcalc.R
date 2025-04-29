TabRes_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      height = 220,
      card_header(h5("Analysis options"), class = "bg-success"),
      card_body(
        style = "background-color: #F6FCF4;",
        fluidRow(
          column(7, numericInput(ns("ini"),
            label = h6("proportion change"), step = 0.05, value = 0.5
          )),
          column(5, numericInput(ns("thresh"),
            label = h6("threshold"), step = 0.01, value = 0.05
          ))
        ),
      )
    ),
    tableOutput(ns("tabres"))
  )
}


TabRes_server <- function(id, procfile) {
  moduleServer(id, function(input, output, session) {
    # Apply the min and max function defined above
    MaxandMin <- function(m, Time, thresh) {
      minA <- switch(input$abini,
        "global zero" = input$back,
        # "global zero"=min(m, na.rm=TRUE)-input$back,
        # "global zero"=min(m, na.rm=TRUE),
        "nth absorbance" = m[input$arow],
        # "nth absorbance"=min(m, na.rm=TRUE),
        "min + offset" = min(m, na.rm = TRUE) + input$off
        # "min + offset"=min(m, na.rm=TRUE)
      )

      firstA <- m[1]
      pointmax <- which.max(m)

      # maxA <- max(m, na.rm = TRUE)
      maxA <- switch(input$abfin,
        "each curve max" = max(m, na.rm = TRUE),
        #  "global max" = 1)
        "global max" = input$finabs
      )

      maxT <- Time[which.max(m)] # ,
      changeA <- maxA - minA
      # Here list the variables you want to collect
      Aminmax <- c(firstA, minA, maxA, changeA, maxT, pointmax)
    }

    uppity <- function(u, Time, ini, thresh) {
      minA <- switch(input$abini,
        "global zero" = input$back,
        "nth absorbance" = min(u, na.rm = TRUE),
        "min + offset" = min(u, na.rm = TRUE) + input$off
      )
      # Need to define how to get the max abs
      # maxAbs <- max(u, na.rm = TRUE)
      maxAbs <- switch(input$abfin,
        "each curve max" = max(u, na.rm = TRUE),
        #  "global max" = 1)
        "global max" = input$finabs
      )

      pointmax <- which.max(u)
      upTime <- Time[c(1:pointmax)] # vector of time to max
      upAbs <- u[c(1:pointmax)] # vector of absorbances to max
      pcChange <- ini * (maxAbs - minA) + minA # this minAbs is determined in shiny, may be set or calculated
      startPoint <- which(abs(upAbs - pcChange) == min(abs(upAbs - pcChange)))[1]
      # StartAbs is fitted if abs > threshold, otherwise is closest point
      # This prevents crashing if there are blank wells
      ifelse(max(u) - min(u) < thresh,
        startAbs <- upAbs[startPoint],
        startAbs <- round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x, 4)
      )

      # StartTime is fitted if abs > threshold, otherwise is closest point
      # This prevents crashing if there are blank wells
      ifelse(max(u) - min(u) < thresh,
        startTime <- upTime[startPoint],
        startTime <- round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y, 4)
      )

      # Collect the variables
      upcurve <- c(minA, startTime, startAbs, startAbs, startPoint, pointmax)
    }



    tabres <- reactive({
      thePlate <- procfile()
      # thePlate <- df() #using raw data
      # Time <- thePlate %>% select(1) %>% pull() #Time is a vector
      Time <- thePlate[[1]] # Time is the first column of the datafile
      ini <- input$ini
      thresh <- input$thresh
      mapDatDF <- thePlate[-1] %>%
        map_df(~ data.frame(
          firstAbs = MaxandMin(.x, Time, thresh)[1], min.abs = MaxandMin(.x, Time, thresh)[2],
          max.abs = MaxandMin(.x, Time, thresh)[3],
          delta.abs = MaxandMin(.x, Time, thresh)[4],
          max.time = MaxandMin(.x, Time, thresh)[5], pointmax = MaxandMin(.x, Time, thresh)[6],
          clot.time = uppity(.x, Time, ini, thresh)[2], clot.abs = uppity(.x, Time, ini, thresh)[4],
          startPoint = uppity(.x, Time, ini, thresh)[5]
        )) %>% # ,

        mutate(clotTomaxt = max.time - clot.time) %>%
        add_column(Wells = colnames(thePlate[-1]), .before = TRUE)

      TabRes <- mapDatDF %>%
        # select(Wells, min.abs, clot.time, clot.abs, max.abs, delta.abs, max.time, lys.time, lys.abs, clotTolys.time, startPoint, pointmax, decayPoint, endPoint, end.time) %>%
        select(Wells, min.abs, clot.time, clot.abs, max.abs, delta.abs, max.time, clotTomaxt, startPoint, pointmax) %>%
        # mutate(across(where(is.numeric), round, digits=4))
        mutate(across(where(is.numeric), \(x) round(x, digits = 4))) # use newer method for across
      # No clipboard for online app
      # clipr::write_clip(TabRes)
    })
  })
}
