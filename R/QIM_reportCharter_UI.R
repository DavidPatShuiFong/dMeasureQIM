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
  "Age10", "Sex", "Indigenous",
  "DiabetesType", "State"
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
            title = "Charting",
            shiny::wellPanel(
              style = "height:35em",
              shinyWidgets::pickerInput(
                inputId = ns("qim_chosen"),
                label = "QIM",
                choices = measure_names,
                selected = measure_names[1]
              ),
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
              shinyWidgets::pickerInput(
                # the category group of stream show on the negative (left)
                # side of a pyramid
                # e.g. if 'Female' is to be shown on the left,
                # then 'Sex' is the category
                inputId = ns("mirror_chosen"),
                label = "Mirror",
                choices = "None",
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("mirror_group"),
                label = "Mirror group",
                choices = NULL,
                # initially no choice
                # but if a category group e.g. 'Sex' is chosen
                # in 'mirror_chosen', then this is filled with
                # choices to be shown on the negative (left) side
                # of the pyramid e.g. 'Male/Female/X/Not available'
                selected = NULL,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::switchInput(
                inputId = ns("proportion"),
                label = "Proportion",
                value = FALSE,
                labelWidth = "10 em"
              ),
              shiny::hr(),
              shiny::actionButton(
                inputId = ns("show_grouped_values"),
                label = "Show grouped values"
              )
            )
          ),
          shiny::tabPanel(
            title = "Filters",
            shiny::wellPanel(
              height = "35em",
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
            title = "Restore/Load",
            shiny::wellPanel(
              style = "height:23em",
              shiny::tags$h5("Load report"),
              shiny::fileInput(
                ns("loadCSVFile"),
                "Choose GPstat! QIMReport .CSV file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values, text/plain",
                  ".csv"
                )
              ),
              shiny::br(),
              shiny::hr(),
              shiny::actionButton(
                inputId = ns("show_report_values"),
                label = "Show report"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 9,
        highcharter::highchartOutput(ns("chart"), height = "700px")
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

  empty_result <- data.frame(
    QIM = character(),
    Age10 = numeric(),
    Sex = character(),
    Indigenous = character(),
    DiabetesType = character(),
    Measure = character(),
    State = character(),
    n = numeric(),
    DateFrom = character(),
    DateTo = character()
  )
  report_values <- shiny::reactiveVal(empty_result)
  # where .CSV files are first stored,
  # or dataframes transferred from qim_reportCreator module
  # via 'report'
  report_filled <- shiny::reactiveVal(empty_result)
  # report_values filtered, and then 'filled' with
  # 'missing' demographic rows (where 'n' = 0)
  report_grouped <- shiny::reactiveVal(empty_result)
  # report_filled grouped, according to chart options
  rendered_chart <- shiny::reactiveVal(NULL)

  ##### data series choices #################################

  # modify choices for input$category_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$stack_chosen, input$mirror_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, priority = 5, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'stack' or 'mirror' (except 'None'!)
      choices <- choices[choices != input$stack_chosen]
      choices <- choices[choices != input$mirror_chosen]
      choices <- c("None", choices)
      # retain previous choice
      chosen <- intersect(choices, input$category_chosen)
      if (length(chosen) == 0) {chosen <- "None"} # i.e. 'character(0)'

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
    c(input$series_chosen, input$category_chosen,
      input$mirror_chosen), priority = 5,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'category' or 'mirror' (except 'None'!)
      choices <- choices[choices != input$category_chosen]
      choices <- choices[choices != input$mirror_chosen]
      # cannot be 'age'
      choices <- choices[choices != "Age"]
      # can still be 'None'
      choices <- c("None", choices)
      chosen <- intersect(choices, input$stack_chosen)
      if (length(chosen) == 0) {chosen <- "None"} # i.e. 'character(0)'

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "stack_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  shiny::observeEvent(
    c(input$series_chosen, input$category_chosen, input$stack_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, priority = 5, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'category' or stack' (except 'None'!)
      choices <- choices[choices != input$category_chosen]
      choices <- choices[choices != input$stack_chosen]
      # cannot be 'age'
      choices <- choices[choices != "Age"]
      # can still be 'None'
      choices <- c("None", choices)
      chosen <- intersect(choices, input$mirror_chosen) # previous choice
      if (length(chosen) == 0) {chosen <- "None"} # i.e. 'character(0)'

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "mirror_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  shiny::observeEvent(
    c(input$mirror_chosen), priority = 5,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      if (is.null(input$mirror_chosen) || input$mirror_chosen == "None") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = NULL,
          selected = character(0)
        )
      } else if (input$mirror_chosen == "Sex") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = sex_choices,
          selected = character(0)
        )
      } else if (input$mirror_chosen == "Indigenous") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = ethnicity_choices,
          selected = character(0)
        )
      } else if (input$mirror_chosen == "DiabetesType") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = diabetes_choices,
          selected = character(0)
        )
      } else if (input$mirror_chosen == "State") {
        # complex, this depends on the chosen QIM
        if (input$qim_chosen == measure_names[1]) {
          choices <- c("HbA1C not done", "HbA1C done")
        } else if (input$qim_chosen == measure_names[2]) {
          choices <- c("Not defined", "Non smoker", "Ex smoker", "Smoker")
        } else if (input$qim_chosen == measure_names[3]) {
          choices <- c("Not defined",
                       "Underweight", "Healthy", "Overweight", "Obese")
        } else if (input$qim_chosen == measure_names[4]) {
          choices <- c("Influenza not done", "Influenza done")
        } else if (input$qim_chosen == measure_names[5]) {
          choices <- c("Influenza not done", "Influenza done")
        } else if (input$qim_chosen == measure_names[6]) {
          choices <- c("Influenza not done", "Influenza done")
        } else if (input$qim_chosen == measure_names[7]) {
          choices <- c("Alcohol not done", "Alcohol done")
        } else if (input$qim_chosen == measure_names[8]) {
          choices <- c("CVD Risk not done", "CVD Risk done")
        } else if (input$qim_chosen == measure_names[9]) {
          choices <- c("CST not done", "CST done")
        } else if (input$qim_chosen == measure_names[10]) {
          choices <- c("BP not done", "BP done")
        } else {
          warning("Invalid QIM choice")
          choices <- NULL
        }
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = choices,
          selected = NULL # default is no choice
        )
      }
    }
  )

  ##### create summary table ####################################################

  shiny::observeEvent(
    c(report_filled(),
      input$series_chosen, input$category_chosen,
      input$stack_chosen, input$mirror_group), priority = -5,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      shiny::req(report_filled())

      ##### reduce the data to groups, and get a summary statistic 'n' ####
      report <- report_filled() %>>%
        dplyr::group_by(!!!dplyr::syms(input$series_chosen)) %>>%
        dplyr::summarise(n = sum(n)) %>>%
        dplyr::ungroup()
      ##### define series names ###########################################
      series_names <- input$series_chosen
      if (input$category_chosen != "None" && input$category_chosen %in% input$series_chosen) {
        # input$category_chosen should always be in input$series_chosen
        # but sometimes the state of choices for input$category_chosen might 'lag'
        #
        # 'category' is removed from the 'series' description
        # category will be on the 'x axis' (in a bar chart, 'x' is vertical!)
        # so does not not define a separate series
        series_names <- series_names[series_names != input$category_chosen]
        report$category <- unlist(
          report[, input$category_chosen], use.names = FALSE
        )
      } else {
        report$category <- "All"
      }
      report <- report %>>%
        dplyr::mutate(
          series_name = dMeasure::paste2(
            "", !!!dplyr::syms(series_names),
            # if input$series_chosen is NULL then will be ""
            # na.rm = TRUE removes "" if $series_chosen != NULL
            sep = "+", na.rm = TRUE)
        )

      ##### define stacks ####################
      if (input$stack_chosen != "None" && input$stack_chosen %in% input$series_chosen) {
        report$stack <- unlist(report[, input$stack_chosen], use.names = FALSE)
      } else {
        report$stack <- as.character(NA)
      }

      ##### mirror ###########################
      if (input$mirror_chosen == "Sex" && "Sex" %in% input$series_chosen) {
        # 'Sex' should not be an option in input$mirror_chosen if it is not
        # in $series_chosen, but unfortunately there may be a lag...
        for (x in sex_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(n = dplyr::if_else(Sex == x, -n, n))
          }
        }
      }

      if (input$mirror_chosen == "Indigenous" && "Indigenous" %in% input$series_chosen) {
        for (x in ethnicity_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(n = dplyr::if_else(Indigenous == x, -n, n))
          }
        }
      }

      if (input$mirror_chosen == "DiabetesType" && "DiabetesType" %in% input$series_chosen) {
        for (x in diabetes_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(n = dplyr::if_else(DiabetesType == x, -n, n))
          }
        }
      }

      if (input$mirror_chosen == "State" && "State" %in% input$series_chosen) {
        if (input$qim_chosen %in% measure_names[c(1,4,5,6,7,8,9,10)]) {
          # these are all 'not done' == FALSE and 'done' == TRUE options
          if (
            any(stringi::stri_sub(input$mirror_group, -8, -1) == "not done")
            # was a 'not done' == FALSE chosen to be 'mirrored'
          ) {
            report <- report %>>%
              dplyr::mutate(n = dplyr::if_else(State == "FALSE", -n, n))
            # if 'FALSE' then 'mirror' (turn negative)
          }
          if (
            any(
              stringi::stri_sub(input$mirror_group, -8, -1) != "not done" &
              stringi::stri_sub(input$mirror_group, -4, -1) == "done"
              # not a 'not done', but is a 'done' == TRUE
              # note that it is possible for *both* FALSE
              #  and TRUE to be 'mirrored'
            )
          ) {
            report <- report %>>%
              dplyr::mutate(n = dplyr::if_else(State == "TRUE", -n, n))
            # if TRUE then 'mirror' (turn negative)
          }
        }

        if (input$qim_chosen == measure_names[2]) {
          # 15+ smoker. 'not defined' is NA, should have already been converted
          lapply(
            c("Not defined", "Non smoker", "Ex smoker", "Smoker"),
            function(x) {
              if (x %in% input$mirror_group) {
                report <- report %>>%
                  dplyr::mutate(n = dplyr::if_else(State == x, -n, n))
              }
            }
          )
        }

        if (input$qim_chosen == measure_names[3]) {
          # 15+ BMI
          lapply(
            c("Not defined", "Underweight", "Healthy", "Overweight", "Obese"),
            function(x) {
              if (x %in% input$mirror_group) {
                report <- report %>>%
                  dplyr::mutate(n = dplyr::if_else(State == x, -n, n))
              }
            }
          )
        }
      }

      ##### calculate proportions #################################
      proportion_groups <- NULL
      if (input$category_chosen != "None") {
        proportion_groups <- c(proportion_groups, "category")
      }
      if (input$stack_chosen != "None") {
        proportion_groups <- c(proportion_groups, "stack")
      }
      report$mirrored <- FALSE
      if (input$mirror_chosen != "None") {
        proportion_groups <- c(proportion_groups, "mirrored")
        report <- report %>>%
          dplyr::mutate(mirrored = dplyr::if_else(n < 0, TRUE, FALSE))
        # any 'n' value less than zero has been mirrored
      }
      # categories, stacks and mirror all define separate groupings
      report <- report %>>%
        dplyr::group_by(!!!dplyr::syms(proportion_groups)) %>>%
        dplyr::mutate(proportion = n / sum(n) * sign(n)) %>>%
        # retain 'sign' of 'n'
        dplyr::ungroup()

      report <- report %>>%
        dplyr::select( # re-order the columns
          dplyr::one_of( # first the demographic/data categories
            data_categories[data_categories %in% input$series_chosen]
          ),
          "series_name", # the 'name'
          "n", # and the number
          "proportion",
          dplyr::everything() # everything else
        )

      report_grouped(report)
    }
  )

  shiny::observeEvent(
    c(report_grouped(), input$proportion),
    ignoreInit = TRUE, priority = -10, {
      shiny::req(report_grouped())
      shiny::req(nrow(report_grouped() > 0))

      if (input$proportion) {
        y_variable <- "proportion"
        decimal_points <- 2
        # will be a number between 0 and 1
      } else {
        y_variable <- "n"
        decimal_points <- 0
        # will be whole numbers (integers)
      }

      grouped_report <- report_grouped() %>>%
        dplyr::mutate(category = as.character(category))

      hc <- highcharter::hchart(
        grouped_report,
        type = "bar",
        highcharter::hcaes(
          x = category, y = !!dplyr::sym(y_variable),
          group = series_name
        ),
        stack = (dplyr::distinct(report_grouped(), series_name, stack) %>%
                   dplyr::arrange(series_name) %>%
                   dplyr::pull(stack))
      ) %>>%
        highcharter::hc_xAxis(
          reversed = FALSE # !!!
        ) %>>%
        highcharter::hc_yAxis(
          labels = list(
            formatter = highcharter::JS(
              "function(){return Math.abs(this.value);}"
            )
          ),
          plotLines = list(list(
            color = "#C0C0C0",
            width = 3,
            value = 0
          ))
        ) %>>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = highcharter::JS(
            paste0(
              "function() {
            return this.point.name + '<br/>' +
            '<b>' + this.series.name + ':</b> ' +
            Highcharts.numberFormat(Math.abs(this.point.y), ",
              decimal_points, ");}"
            )
          )
        ) %>>%
        highcharter::hc_plotOptions(
          bar = list(stacking = "normal")
        )
      rendered_chart(hc)
    }
  )

  output$chart <- highcharter::renderHighchart({
    rendered_chart()
  })

  shiny::observeEvent(
    input$show_grouped_values,
    ignoreInit = TRUE, {
      shiny::showModal(shiny::modalDialog(
        title = "Report",
        DT::renderDataTable({
          DT::datatable(
            data = report_grouped(),
            extensions = c("Buttons", "Responsive"),
            options = list(scrollX = TRUE,
                           dom = "frtiBp",
                           buttons = I("colvis"))
          ) %>>%
            DT::formatSignif(columns = "proportion", digits = 3)
        }),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    }
  )

  ##### Restore/Load CSV report #################################################
  # includes restoration from

  shiny::observeEvent(
    report$report_values(),
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(report$report_values())

      if (nrow(report$report_values()) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Detected report creation",
            "Use created report in 'Report Charter'?",
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("ok_copy_report"), "OK")
            )
          )
        )

        shiny::observeEvent(
          input$ok_copy_report,
          ignoreInit = TRUE, ignoreNULL = TRUE, {
            report_values(report$report_values())
            # copy the dataframe
            shiny::removeModal()
          }
        )
      }
    }
  )

  shiny::observeEvent(
    input$loadCSVFile,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(input$loadCSVFile)

      inFile <- input$loadCSVFile

      data <- read.csv(
        inFile$datapath,
        stringsAsFactors = FALSE,
        na.strings = "NA"
      )

      if (
        !all(
          c("QIM", "Age10", "Sex", "Indigenous",
            "DiabetesType", "Measure", "State", "n",
            "DateFrom", "DateTo")
          %in% names(data)
        )
      ) {
        # absolute minimum columns are not present
        shinytoastr::toastr_error(
          message = paste(
            "Not a valid GPstat QIM report"
          ),
          position = "bottom-left",
          closeButton = TRUE,
          timeOut = 0
        )
      } else {
        # all required columns are present
        data <- data %>>%
          dplyr::mutate(
            Age10 = as.numeric(Age10),
            n = as.numeric(n),
            DateFrom = as.Date(DateFrom),
            DateFrom = as.Date(DateTo),
          )
        report_values(data)
      }
    })

  shiny::observeEvent(
    c(report_values(), input$qim_chosen,
      input$sex_chosen, input$ethnicity_chosen,
      input$diabetes_chosen), priority = -3,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(report_values())
      shiny::req(nrow(report_values()) > 0)
      if (nrow(report_values()) > 0) {
        report <- report_values() %>>%
          # just the necessary columns
          dplyr::select(
            QIM, Age10, Sex, Indigenous, DiabetesType,
            Measure, State, n, DateFrom, DateTo
          ) %>>%
          dplyr::filter(
            # just the QIM that is chosen
            as.numeric(stringi::stri_sub(QIM, -2, -1)) ==
              which(measure_names == input$qim_chosen)
          ) %>>%
          # fill in the 'missing' demographic possiblities/rows ('n' = 0)
          dMeasureQIM::fill_demographics() %>>%
          tidyr::replace_na(
            # replace 'NA' not available with various
            # textual descriptions
            list(Sex = "Not stated", Indigenous = "Not stated",
                 DiabetesType = "Not stated",
                 State = "Not defined")
          ) %>>%
          dplyr::mutate( # also replace "" empty strings
            DiabetesType = dplyr::if_else(
              DiabetesType == "",
              "Not stated",
              DiabetesType
            )
          ) %>>%
          # then filter by age, sex, ethnicity and diabetestype
          # (if specified by the user)
          dplyr::filter(
            dplyr::between(Age10, input$age_range[1], input$age_range[2]),
            Sex %in% input$sex_chosen,
            Indigenous %in% input$ethnicity_chosen,
            DiabetesType %in% input$diabetes_chosen
          ) %>>%
          dplyr::select(
            QIM, Age10, Sex, Indigenous, DiabetesType,
            Measure, State, n, DateFrom, DateTo
          ) # may need to 're-order' columns

        report_filled(report)
      }
    }
  )

  shiny::observeEvent(
    input$show_report_values,
    ignoreInit = TRUE, {
      shiny::showModal(shiny::modalDialog(
        title = "Report",
        DT::renderDataTable({
          DT::datatable(
            data = report_values(),
            extensions = c("Buttons", "Responsive"),
            options = list(scrollX = TRUE,
                           dom = "frtiBp",
                           buttons = I("colvis"))
          )
        }),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    }
  )

}
