library(shinyjs)

function(input, output, session) {

  values <- reactiveValues(
    data1 = NULL, data2 = NULL,  # the datasets
    twodatas = NULL  # whether or not the user is loading two datasets
  )

  # Store a dataset when a file is chosen
  observeEvent(input$dataset1Input, {
    datasetSelected(1)
  })
  observeEvent(input$dataset2Input, {
    datasetSelected(2)
  })
  datasetSelected <- function(num) {
    buttonId <- sprintf("dataset%sUploadArea", num)
    loadingEl <- sprintf("#%s .btn-loading-indicator", buttonId)
    doneEl <- sprintf("#%s .btn-done-indicator", buttonId)
    errElIcon <- sprintf("#%s .btn-error-indicator", buttonId)
    errEl <- sprintf("#%s .btn-err", buttonId)
    errMsgEl <- sprintf("#%s .btn-err-msg", buttonId)
    disable(buttonId)
    show(selector = loadingEl)
    hide(selector = doneEl)
    hide(selector = errEl)
    on.exit({
      enable(buttonId)
      hide(selector = loadingEl)
    })

    tryCatch({
      inputName <- sprintf("dataset%sInput", num)
      value <- read.csv(input[[inputName]]$datapath)
      show(selector = doneEl)
      delay(3000, hide(selector = doneEl, anim = TRUE, animType = "fade",
                       time = 0.5))
      values[[sprintf("data%s", num)]] <- value

      # show the next step
      show("datasetSelectToggle")
      show("matchParamsArea", anim = TRUE, animType = "fade")
      show("findMatchesBtn")
    }, error = function(err) {
      html(html = err$message, selector = errMsgEl)
      show(selector = errEl, anim = TRUE, animType = "fade")
    })
  }

  # Minimize/maximize UI sections
  observeEvent(input$datasetSelectToggle, ignoreNULL = FALSE, {
    if (input$datasetSelectToggle %% 2 == 0) {
      shinyjs::html("datasetSelectToggle", "[-]")
      shinyjs::show("datasetSelectInner")
    } else {
      shinyjs::html("datasetSelectToggle", "[+]")
      shinyjs::hide("datasetSelectInner")
    }
  })
  observeEvent(input$matchParamsToggle, ignoreNULL = FALSE, {
    if (input$matchParamsToggle %% 2 == 0) {
      shinyjs::html("matchParamsToggle", "[-]")
      shinyjs::show("matchParamsInner")
    } else {
      shinyjs::html("matchParamsToggle", "[+]")
      shinyjs::hide("matchParamsInner")
    }
  })

  # Show the "show dataset" links
  observe({
    toggle("dataset1Toggle", condition = !is.null(values$data1))
    toggle("dataset2Toggle", condition = !is.null(values$data2))
  })

  # Show the input tables
  output$dataset1Table <- DT::renderDataTable({
    DT::datatable(
      values$data1,
      selection = "none",
      class = 'stripe',
      options = list(
        dom = "iftlp"
      )
    )
  })
  output$dataset2Table <- DT::renderDataTable({
    DT::datatable(
      values$data2,
      selection = "none",
      class = 'stripe',
      options = list(
        dom = "iftlp"
      )
    )
  })


  output$matchingVarsOutput <- renderUI({
    tagList(
      fluidRow(
        column(
          3,
          selectInput("matchData1Vars", NULL, colnames(values$data1), selected = NULL,
                      multiple = TRUE)
        ),
        column(
          3,
          selectInput("matchData2Vars", NULL, colnames(values$data2), selected = NULL,
                      multiple = TRUE)
        ),
        column(
          3,
          selectInput("matchFx", NULL, getMatchingFxChoices(), selected = NULL)
        ),
        column(
          3,
          sliderInput("matchWeight", NULL, min = 0, max = 1, value = 1)
        )
      )
    )
  })

  # "Find matches" button is clicked
  observeEvent(input$findMatchesBtn, {
    shinyjs::info("f")
  })
}
