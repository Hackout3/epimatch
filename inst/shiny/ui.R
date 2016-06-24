library(shinyjs)

# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "click",
    icon("question-circle")
  )
}

fixedPage(
  useShinyjs(),
  extendShinyjs(file.path("www", "shinyjs-funcs.js"), functions = c()),
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
      div(class = "sectionInstructions",
          "Upload either one dataset to be examined for internal record matches, or two dataset to compare for record matches."
          ),
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
            div("Dataset 1 variable(s)",
                helpPopup("Select the variable(s) you would like to match in your first dataset. You can select multiple variables."))
          ),
          column(
            3,
            div("Dataset 2 variable(s)",
                helpPopup("Select the variable(s) you would like to match in your second dataset, corresponding to the one selected in your first data set. You can select multiple variables."))
          ),
          column(
            3,
            div("Variable type",
                helpPopup("Designiate the type of variable you are comparing within dataset 1, or between datasets 1 and 2."))
          ),
          column(
            3,
            div("Weight",
                helpPopup("Designate the weight you would like to be applied to this variable match compared to the other variable (1 = highest weight)"))
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
      sliderInput("threshold",
                  div(
                    "Threshold of matching uncertainty",
                    helpPopup("Designate the amount of uncertainty for overall record matches (taking into account all variables) that you are willing to accept: 0 is a perfect match, 1 matches records with the most uncertainty.")
                  ),
                  min = 0, max = 1, 0.5),
      sliderInput("ageThreshold",
                  div(
                    "Age uncertainty (+/- years)",
                    helpPopup("Set the number of years +/- the age value being matched to that you would like to search for record matches within.")
                  ),
                  min = 0, max = 25, 0),
      sliderInput("dateThreshold",
                  div(
                    "Date uncertainty (+/- days)",
                    helpPopup("Set the number of days +/- the date being matched to that you would like to search for record matches within.")
                  ),
                  min = 0, max = 365, 0)
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
  )),

  div(id = "author", "Tool is publicly available online at",
      a("http://daattali.com/shiny/epimatch/", href = "http://daattali.com/shiny/epimatch/"))
)
