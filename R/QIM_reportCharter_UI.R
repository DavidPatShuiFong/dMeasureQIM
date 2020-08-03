# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_reportChart
#' @title dMeasure Quality Improvement Measures - charts for QIM UI
#'
#' need definitions including 'measure_names'
#'
#' @include QualityImprovementMeasures.R
NULL

data_categories <- c(
  "State", "Age", "Sex", "Indigenous",
  "Diabetes"
)
stack_categories <- data_categories[data_categories != "Age"]

#' @export
qim_reportCharter_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinydashboard::tabBox(
          id = ns("charter_leftpanel"),
          width = 12,
          shiny::tabPanel(
            title = "Filters",
            shiny::wellPanel(
              style = "height:35em",
              shinyWidgets::pickerInput(
                inputId = ns("qim_chosen"),
                label = "QIM",
                choices = measure_names,
                selected = measure_names[1]
              ),
              shinyWidgets::sliderTextInput(
                inputId = ns("age_range"),
                label = "Age range",
                choices = c(0, 5, 15, 25, 35, 45, 55, 65),
                selected = c(0, 65),
                # an age 'range'
              ),
              shinyWidgets::pickerInput(
                inputId = ns("sex_chosen"),
                label = "Sex",
                choices = sex_choices,
                selected = sex_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("ethnicity_chosen"),
                label = "Ethnicity",
                choices = ethnicity_choices,
                selected = ethnicity_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("diabetes_chosen"),
                label = "Diabetes",
                choices = diabetes_choices,
                selected = diabetes_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::tabPanel(
            title = "Charting",
            shiny::wellPanel(
              height = "35em",
              shinyWidgets::pickerInput(
                inputId = ns("series_chosen"),
                label = "Series",
                choices = data_categories,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("category_chosen"),
                label = "Category",
                choices = c("None"),
                # initially no choices, but will be expanded
                # when input$series_chosen has selections
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("stack_chosen"),
                label = "Stack",
                choices = "None",
                # initially no choices, but will be expanded
                # when input$series_chosen has selections
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )

              ),
              shiny::uiOutput(ns("charting_mirror")),
              shinyWidgets::switchInput(
                inputId = ns("proportion"),
                label = "Proportion",
                value = FALSE,
                labelWidth = "10 em"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 9
      )
    )

  )
}

#' Quality Improvement report charter - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'   access to appointments lists, results, correspondence and EMR database
#' @param report a list returned by qim_reportCreator
#'   should contain $report_values(), which is a dataframe
#'
#' @return none
#'
#' @export
qim_reportCharter <- function(input, output, session, dMQIM, report) {
  ns <- session$ns

  # modify choices for input$category_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$stack_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'stack' (except 'None'!)
      choices <- setdiff(choices, input$stack_chosen)
      choices <- c("None", choices)
      # retain previous choice
      chosen <- input$category_chosen

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "category_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  # modify choices for input$stack_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$category_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'stack' (except 'None'!)
      choices <- setdiff(choices, input$category_chosen)
      # cannot be 'age'
      choices <- setdiff(choices, intersect(choices, "Age"))
      # can still be 'None'
      choices <- c("None", choices)
      chosen <- input$stack_chosen

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "stack_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )


  output$charting_stack <- shiny::renderUI({
    # MUST be one of input$series_chosen
    choices <- input$series_chosen
    # cannot be chosen in 'category'
    choices <- setdiff(choices, input$category_chosen)

  })
  output$charting_mirror <- shiny::renderUI({
    shiny::tagList(

    )
  })
}
