library(shinyjs)

mysort <- function(x) {
  if (is.null(x)) NULL
  else sort(x)
}

function(input, output, session) {

  values <- reactiveValues(
    debug = TRUE,      # debug mode on/off
    data1 = NULL, data2 = NULL,  # the datasets
    twodatas = FALSE,  # whether or not the user is loading two datasets
    numMatchRules = 0,
    results = NULL,    # the results from epimatch
    removedRows = NULL # row numbers of deleted rows
  )

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['debug']])) {
      values$debug <- TRUE
    }
  })

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
      if (num == 1) {
        show("datasetSelectToggle")
        show("matchParamsArea", anim = TRUE, animType = "fade")
        show("extraParamsArea", anim = TRUE, animType = "fade")
        show("findMatchesBtn")
      } else {
        values$twodatas <- TRUE
      }
      removeUI(selector = ".matchParamsRow", multiple = TRUE)
      values$numMatchRules <- 0
      values$removedRows <- c()
      addMatchRuleRow()
    }, error = function(err) {
      html(html = err$message, selector = errMsgEl)
      show(selector = errEl, anim = TRUE, animType = "fade")
    })
  }

  observe({
    toggle("dateFormat2", condition = values$twodatas)
  })

  # Minimize/maximize UI sections
  observeEvent(input$datasetSelectToggle, ignoreNULL = FALSE, {
    if (input$datasetSelectToggle %% 2 == 0) {
      shinyjs::html("datasetSelectToggle", "[-]")
    } else {
      shinyjs::html("datasetSelectToggle", "[+]")
    }
    shinyjs::toggle("datasetSelectInner", anim = TRUE, time = 0.25,
                    condition = input$datasetSelectToggle %% 2 == 0)
  })
  observeEvent(input$matchParamsToggle, ignoreNULL = FALSE, {
    if (input$matchParamsToggle %% 2 == 0) {
      shinyjs::html("matchParamsToggle", "[-]")
    } else {
      shinyjs::html("matchParamsToggle", "[+]")
    }
    shinyjs::toggle("matchParamsInner", anim = TRUE, time = 0.25,
                    condition = input$matchParamsToggle %% 2 == 0)
  })
  observeEvent(input$extraParamsToggle, ignoreNULL = FALSE, {
    if (input$extraParamsToggle %% 2 == 1) {
      shinyjs::html("extraParamsToggle", "[-]")
    } else {
      shinyjs::html("extraParamsToggle", "[+]")
    }
    shinyjs::toggle("extraParamsInner", anim = TRUE, time = 0.25,
                    condition = input$extraParamsToggle %% 2 == 1)
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
        dom = "iftlp",
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )

    )
  })
  output$dataset2Table <- DT::renderDataTable({
    DT::datatable(
      values$data2,
      selection = "none",
      class = 'stripe',
      options = list(
        dom = "iftlp",
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # Add UI row for another matching rule
  addMatchRuleRow <- function() {
    isolate({
      num <- values$numMatchRules + 1
      values$numMatchRules <- num
    })

    disableParams2 <- function(tag) {
      if (values$twodatas) {
        tag
      } else {
        disabled(tag)
      }
    }

    ui <- div(
      class = "matchParamsRow",
      `data-row-num` = num,
      span(class = "removeRowBtn", "x", title = "Remove row"),
      fluidRow(
        column(
          3,
          selectInput(paste0("matchData1Vars", num),
                      NULL, mysort(colnames(values$data1)), selected = NULL,
                      multiple = TRUE)
        ),
        column(
          3,
          disableParams2(
            selectInput(paste0("matchData2Vars", num),
                      NULL, mysort(colnames(values$data2)), selected = NULL,
                      multiple = TRUE)
          )
        ),
        column(
          3,
          selectInput(paste0("matchFx", num),
                      NULL, c("", mysort(epimatch::distFuns())), selected = "")
        ),
        column(
          3,
          sliderInput(paste0("matchWeight", num),
                      NULL, min = 0, max = 1, value = 1)
        )
      )
    )
    insertUI(selector = "#matchingVarsOutput", where = "beforeEnd", ui = ui)
  }

  # User clicked on the delete row button
  observeEvent(input$deleteRow, {
    row <- input$deleteRow[]
    removeUI(selector = sprintf(".matchParamsRow[data-row-num=%s]", row))
    values$removedRows <- c(values$removedRows, row)
  })

  # Add another row of matching parameters
  observeEvent(input$addMatchRowBtn, {
    addMatchRuleRow()
  })

  # "Find matches" button is clicked
  observeEvent(input$findMatchesBtn, {
    rowNums <- setdiff(seq(values$numMatchRules), values$removedRows)

    # build the functions list
    funlist <-
      lapply(rowNums, function(num) {
        fxn <- input[[paste0("matchFx", num)]]
        ret <-
          list(
            d1vars = input[[paste0("matchData1Vars", num)]],
            d2vars = input[[paste0("matchData2Vars", num)]],
            fun = fxn,
            weight = input[[paste0("matchWeight", num)]]
          )
        if (!values$twodatas) {
          ret[['d2vars']] <- NULL
        }
        if (fxn == "ageDists") {
          ret[['extraparams']] <- list(e = input$ageThreshold)
        }

        if (fxn == "dateDists") {
          extraparams <- list()
          extraparams[['threshold']] <- input$dateThreshold
          if (!is.null(input$dateFormat1)) {
            extraparams[['dat1Format']] <- paste0(input$dateFormat1, collapse = "")
          }
          if (!is.null(input$dateFormat2)) {
            extraparams[['dat2Format']] <- paste0(input$dateFormat2, collapse = "")
          }
          ret[['extraparams']] <- extraparams
        }
        ret
      })

    if (values$debug) {
      cat(str(funlist), "\n")
    }

    disable("findMatchesBtn")
    show("findMatchesLoading")
    hide(selector = ".findMatchesDone")
    hide("findMatchesError")
    on.exit({
      enable("findMatchesBtn")
      hide("findMatchesLoading")
    })
    tryCatch({
      values$results <- epimatch::matchEpiData(
        dat1 = values$data1, dat2 = values$data2,
        funlist = funlist,
        thresh = input$threshold
      )
      show(selector = ".findMatchesDone")
      delay(1000, hide(selector = ".findMatchesDone", anim = TRUE, animType = "fade",
                       time = 0.5))
      show("resultsSection")
    }, error = function(err) {
      html("findMatchesErrorMsg", html = err$message)
      show("findMatchesError", anim = TRUE, animType = "fade")
    })
  })

  # update number of results
  output$numResults <- renderText({
    length(values$results)
  })

  # build the results tables HTML
  output$results <- renderUI({
    values$results
    isolate({
      resultHtml <- ""

      for (result in values$results) {
        resultHtml <- paste0(resultHtml, "<div class='results-one-section'>")

        if (length(result$d1) > 0) {
          if (values$twodatas) {
            resultHtml <- paste0(resultHtml, "<h4><strong>Dataset 1</strong></h4>")
          }
          tableHtml <- capture.output(print(
            xtable::xtable(values$data1[result$d1, ]),
            type = "html",
            html.table.attributes = 'class="data table table-bordered table-striped table-condensed"'
          ))
          tableHtml <- paste(tableHtml, collapse = "")
          resultHtml <- paste0(resultHtml, tableHtml)
        }
        if (length(result$d2) > 0) {
          resultHtml <- paste0(resultHtml, "<h4><strong>Dataset 2</strong></h4>")
          tableHtml <- capture.output(print(
            xtable::xtable(values$data2[result$d2, ]),
            type = "html",

            html.table.attributes = 'class="data table table-bordered table-striped table-condensed"'
          ))
          tableHtml <- paste(tableHtml, collapse = "")
          resultHtml <- paste0(resultHtml, tableHtml)
        }
        resultHtml <- paste0(resultHtml, "</div>")
      }

      HTML(resultHtml)
    })
  })

  hide("loading-content", TRUE, "fade")
}
