library(shinyjs)

fixedPage(
  useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  h1(id = "appTitle", "Find matching records in your data"),

  div(id = "loading-content", "Loading...", icon("spinner", class = "fa-spin")),

  div(
    id = "datasetSelectArea",
    h3(id = "datasetSelectTitle", "1. Dataset selection"),
    hidden(actionLink("datasetSelectToggle", "")),
    div(
      id = "datasetSelectInner",
      fixedRow(
        column(
          6,
          div(
            id = "dataset1UploadArea",
            span("Dataset 1", class = "datasetUploadNumTitle"),
            hidden(
              img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
              icon("check", class = "btn-done-indicator")
            ),
            fileInput("dataset1Input", NULL, multiple = FALSE),
            hidden(div(class = "btn-err",
                       icon("exclamation-circle"),
                       strong("Error: "),
                       span(class = "btn-err-msg")
            )),
            actionLink("dataset1Toggle", "Show dataset 1")
          )
        ),
        column(
          6,
          div(
            id = "dataset2UploadArea",
            span("Dataset 2 (optional)", class = "datasetUploadNumTitle"),
            hidden(
              img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
              icon("check", class = "btn-done-indicator")
            ),
            fileInput("dataset2Input", NULL, multiple = FALSE),
            hidden(div(class = "btn-err",
                       icon("exclamation-circle"),
                       strong("Error: "),
                       span(class = "btn-err-msg")
            )),
            actionLink("dataset2Toggle", "Show dataset 2")
          )
        )
      ),

      conditionalPanel(
        "input.dataset1Toggle % 2 == 1",
        h3("Dataset 1", class = "datasetTableTitle"),
        fixedRow(column(12,
        DT::dataTableOutput("dataset1Table")))
      ),
      conditionalPanel(
        "input.dataset2Toggle % 2 == 1",
        h3("Dataset 2", class = "datasetTableTitle"),
        DT::dataTableOutput("dataset2Table")
      )
    )
  ),

  hidden(div(
    id = "matchParamsArea",
    h3(id = "matchParamsTitle", "2. Variables to match"),
    actionLink("matchParamsToggle", ""),
    div(
      id = "matchParamsInner",
      div(
        id = "matchParamsHeader",
        fixedRow(
          column(
            3,
            div("Dataset 1 variable")
          ),
          column(
            3,
            div("Dataset 2 variable")
          ),
          column(
            3,
            div("Matching type")
          ),
          column(
            3,
            div("Weight")
          )
        )
      ),
      div(id = "matchingVarsOutput"),
      actionButton("addMatchRowBtn", "Add another", icon = icon("plus"))
    )
  )),

  hidden(div(
    id = "extraParamsArea",
    h3(id = "extraParamsTitle", "3. Advanced options"),
    actionLink("extraParamsToggle", ""),
    div(
      id = "extraParamsInner",
      sliderInput("threshold", "Threshold", min = 0, max = 1, 0.5),
      sliderInput("ageThreshold", "Age fuzziness", min = 0, max = 50, 0),
      sliderInput("dateThreshold", "Date fuzziness", min = 0, max = 365, 0)
    )
  )),

  hidden(
    actionButton("findMatchesBtn", strong("4. Find matches"), class = "btn-primary btn-lg"),
    hidden(
      img(src = "ajax-loader-bar.gif", id = "findMatchesLoading"),
      icon("check", class = "btn-done-indicator findMatchesDone")
    ),
    hidden(div(id = "findMatchesError",
               icon("exclamation-circle"),
               strong("Error: "), span(id = "findMatchesErrorMsg")))
  ),

  hidden(div(id = "resultsSection",
      h2(id = "resultsTitle",
         textOutput("numResults",
                    inline = TRUE), "Results"),
      uiOutput("results")
  ))
)
