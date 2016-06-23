library(shinyjs)

fixedPage(
  useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  h1(id = "appTitle", "Find matching records in your data"),

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
        DT::dataTableOutput("dataset1Table")
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
      uiOutput("matchingVarsOutput")
    )
  )),

  hidden(
    actionButton("findMatchesBtn", strong("3. Find matches"), class = "btn-primary btn-lg")
  )
)
