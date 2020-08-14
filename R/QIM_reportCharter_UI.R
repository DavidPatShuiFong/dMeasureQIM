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
              style = "height:50em",
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
                label = "Sub-category",
                # this is called the 'stack' in high-charts.
                # confusingly, the 'stacking' property doesn't really
                #  apply to stacks.
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
              shiny::uiOutput(ns("dateto_picker")),
              shiny::fluidRow(
                shiny::column(
                  width = 7,
                  shinyWidgets::switchInput(
                    inputId = ns("proportion"),
                    label = "Proportion",
                    value = FALSE,
                    labelWidth = "10 em"
                  )
                ),
                shiny::column(
                  width = 4,
                  shinyWidgets::pickerInput(
                    inputId = ns("chart_type"),
                    choices = c("bar", "column", "line", "area"),
                    selected = "bar",
                    multiple = FALSE
                  )
                )
              ),
              shiny::hr(),
              shiny::fluidRow(
                shiny::column(
                  width = 7,
                  shiny::actionButton(
                    inputId = ns("show_grouped_values"),
                    label = "Show grouped values"
                  )
                ),
                shiny::column(
                  width = 4,
                  shinyWidgets::pickerInput(
                    inputId = ns("chart_theme"),
                    choices = c(
                      "plain", "smpl", "538", "economist",
                      "elementary", "ffx", "flat", "ft",
                      "ggplot2", "google", "monokai", "tufte"
                    ),
                    selected = "plain",
                    multiple = FALSE
                  )
                )
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

  options(shiny.maxRequestSize = 30*1024^2)
  # the file limit size is normally 5 megabytes for upload
  # this increases to 30 megabytes

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

  ##### dateto picker ######################################

  dateto_picker_state <- shiny::reactiveVal("single")
  # single if only single choice
  # multiple if currently multiple choices possible
  #  multiple is the case if 'category_chosen' or 'stack_chosen' is "DateTo"
  dateto_picker_choices <- shiny::reactiveVal(NULL)
  dateto_picker <- shiny::reactiveVal(
    # by default a 'null' picker
    shinyWidgets::pickerInput(
      inputId = ns("dateto_chosen"),
      label = "End dates",
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      options = list(
        style = "btn-primary",
        `action-box` = TRUE
      )
    )
  )
  output$dateto_picker <- shiny::renderUI({dateto_picker()})
  shiny::observeEvent(
    c(report_values(),
      input$category_chosen, input$stack_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # change the dateto_picker if there is a change
      # in available choices, or if there is a change
      # from 'single' to 'multiple' mode
      #   - this happens if input$category_chosen or input$stack_chosen is
      #     changed from/to 'DateTo'

      dateto_choices <- unique(report_values()$DateTo)
      if (input$category_chosen == "DateTo" || input$stack_chosen == "DateTo") {
        # if the date periods are a category/stack then the user
        #  can choose more than one period
        if (is.null(dateto_picker_choices()) || # previous choices were NULL!
            # can't compare NULL with dateto_choices later on (causes an error)
            dateto_picker_state() == "single" || # switching from single to multiple
            dateto_picker_choices() != dateto_choices) { # change in choices
          # there has been a change in state
          # otherwise this event can be triggered by any change in
          #  input$category_choice or input$stack_chosen
          dateto_picker_state("multiple")
          dateto_picker_choices(dateto_choices)
          dateto_picker(shinyWidgets::pickerInput(
            inputId = ns("dateto_chosen"),
            label = "End dates",
            choices = dateto_choices,
            selected = max(dateto_choices),
            multiple = TRUE,
            options = list(
              style = "btn-primary",
              `action-box` = TRUE
            )
          ))
        }
      } else {
        # if the date periods are *NOT* chosen as a category/stack
        # then the use can only choose one period
        if (length(dateto_choices) > 0) {
          if (is.null(dateto_picker_choices()) || # previously no choices
              dateto_picker_state() == "multiple" || # switch from multiple to single
              dateto_picker_choices() != dateto_choices) { # different choices
            # only change if there is a change from
            #  1. no choices to some choices
            #  2. some choices to different choices
            #  3. multiple picker to single picker
            #     this happens if 'DateTo' is de-selected from
            #     category- stack- choice
            # otherwise this event could be triggered by any change
            #  in input$category_chosen or input$stack_chosen
            dateto_picker_state("single") # single choice state
            dateto_picker_choices(dateto_choices)
            dateto_picker(shinyWidgets::pickerInput(
              inputId = ns("dateto_chosen"),
              label = "End dates",
              choices = dateto_choices,
              selected = max(dateto_choices),
              multiple = FALSE,
              options = list(
                style = "btn-primary",
                `action-box` = TRUE
              )
            ))
          }
        } else {
          # in this case, there is no data available anyway!
          dateto_picker_state("single") # single choice state
          dateto_picker_choices(NULL)
          dateto_picker(shinyWidgets::pickerInput(
            inputId = ns("dateto_chosen"),
            label = "End dates",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              style = "btn-primary",
              `action-box` = TRUE
            )
          ))
        }
      }

    })

  ##### data series choices #################################

  # modify choices for input$category_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$stack_chosen, input$mirror_chosen,
      report_values()),
    ignoreInit = TRUE, ignoreNULL = FALSE, priority = 5, {
      shiny::req(report_values()) # report needs to be available!

      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      if (length(unique(report_values()$DateTo)) > 1) {
        # if more than one date period is available
        # then allow 'DateTo' to be a category
        choices <- c(choices, "DateTo")
      }
      # available choices cannot be already chosen in 'stack'
      # or 'mirror' (except 'None'!)
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

  # modify choices for input$stack_chosen ('sub-category')
  shiny::observeEvent(
    c(input$series_chosen, input$category_chosen, input$chart_type,
      input$mirror_chosen, report_values()),
    priority = 5,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      shiny::req(report_values()) # report needs to be available!

      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      if (length(unique(report_values()$DateTo)) > 1) {
        # if more than one date period is available
        # then allow to be a stack choice
        choices <- c(choices, "DateTo")
      }
      # available choice cannot be already be chosen in 'category'
      # or 'mirror' (except 'None'!)
      choices <- choices[choices != input$category_chosen]
      choices <- choices[choices != input$mirror_chosen]
      # cannot be 'age'
      choices <- choices[choices != "Age"]
      if (input$chart_type == "line" || input$chart_type == "area")
      {choices <- NULL}
      # stack/sub-category isn't normally relevant for  line or area
      # however, if 'area' has mirror mode, *then* the 'negative'
      #  values will be 'stacked' elsewhere
      # can always be 'None'
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
      # cannot already be chosen in 'category' or stack' (except 'None'!)
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
      input$stack_chosen, input$mirror_group),
    priority = -5,
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      shiny::req(report_filled())

      ##### reduce the data to groups, and get a summary statistic 'n' ####
      group_names <- input$series_chosen
      if (input$category_chosen == "DateTo" ||
          input$stack_chosen == "DateTo") {
        # can only choose 'DateTo' as a category/stack if more than
        #  one DateTo available. DateTo cannot be chosen as a series
        group_names <- c(group_names, "DateTo")
      }
      group_names <- intersect(names(report_filled()), group_names)
      # make sure that it is possible to group these names!
      #  occasionally the inputs (e.g. input$category_chosen) will
      #  'lag' the contents of report_filled()
      report <- report_filled() %>>%
        dplyr::group_by(!!!dplyr::syms(group_names)) %>>%
        dplyr::summarise(n = sum(n)) %>>%
        dplyr::ungroup()
      ##### define series names ###########################################
      series_names <- input$series_chosen
      if (input$category_chosen != "None" &&
          (input$category_chosen %in% input$series_chosen ||
           input$category_chosen == "DateTo")) {
        # input$category_chosen should always be in input$series_chosen
        # (unless it is 'DateTo')
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
        # add name for each of the series
        # a special case is when 'DateTo' is selected as a stack
        # in which case the DateTo name will be added when the stack
        # is defined
        dplyr::mutate(
          series_name = dMeasure::paste2(
            "", !!!dplyr::syms(series_names),
            # if input$series_chosen is NULL then will be ""
            # na.rm = TRUE removes "" if $series_chosen != NULL
            sep = "+", na.rm = TRUE)
        )

      ##### define stacks ####################
      if (input$stack_chosen != "None" &&
          (input$stack_chosen %in% input$series_chosen ||
           input$stack_chosen == "DateTo")) {
        report$stack <- unlist(report[, input$stack_chosen], use.names = FALSE)
        if (input$stack_chosen == "DateTo") {
          # special case, 'DateTo' is not a chosen series, but becomes
          # a series if it is chosen as a stack (but not a group!)
          report <- report %>>%
            dplyr::mutate(
              series_name = dMeasure::paste2(
                series_name, stack, sep = " ", na.rm = TRUE
              )
            )
        }
      } else {
        report$stack <- as.character(NA)
      }

      ##### mirror ###########################
      report$mirrored <- FALSE # default

      if (input$mirror_chosen == "Sex" && "Sex" %in% input$series_chosen) {
        # 'Sex' should not be an option in input$mirror_chosen if it is not
        # in $series_chosen, but unfortunately there may be a lag...
        for (x in sex_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(
                n = dplyr::if_else(Sex == x, -n, n),
                mirrored = dplyr::if_else(Sex == x, TRUE, mirrored)
              )
          }
        }
      }

      if (input$mirror_chosen == "Indigenous" && "Indigenous" %in% input$series_chosen) {
        for (x in ethnicity_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(
                n = dplyr::if_else(Indigenous == x, -n, n),
                mirrored = dplyr::if_else(Indigenous == x, TRUE, mirrored)
              )
          }
        }
      }

      if (input$mirror_chosen == "DiabetesType" && "DiabetesType" %in% input$series_chosen) {
        for (x in diabetes_choices) {
          if (x %in% input$mirror_group) {
            report <- report %>>%
              dplyr::mutate(
                n = dplyr::if_else(DiabetesType == x, -n, n),
                mirrored = dplyr::if_else(DiabetesType == x, TRUE, mirrored)
              )
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
              dplyr::mutate(
                n = dplyr::if_else(State == "FALSE", -n, n),
                mirrored = dplyr::if_else(State == "FALSE", TRUE, mirrored)
              )
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
              dplyr::mutate(
                n = dplyr::if_else(State == "TRUE", -n, n),
                mirrored = dplyr::if_else(State == "TRUE", TRUE, mirrored)
                # if TRUE then 'mirror' (turn negative)
              )
          }
        }

        if (input$qim_chosen == measure_names[2]) {
          # 15+ smoker. 'not defined' is NA, should have already been converted
          lapply(
            c("Not defined", "Non smoker", "Ex smoker", "Smoker"),
            function(x) {
              if (x %in% input$mirror_group) {
                report <- report %>>%
                  dplyr::mutate(
                    n = dplyr::if_else(State == x, -n, n),
                    mirrored = dplyr::if_else(State == x, TRUE, mirrored)
                  )
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
                  dplyr::mutate(
                    n = dplyr::if_else(State == x, -n, n),
                    mirrored = dplyr::if_else(State == x, TRUE, mirrored)
                  )
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
      if (input$mirror_chosen != "None") {
        proportion_groups <- c(proportion_groups, "mirrored")
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
    c(report_grouped(), input$proportion, input$chart_type, input$chart_theme),
    ignoreInit = TRUE, priority = -10, {
      shiny::req(report_grouped())
      shiny::req(nrow(report_grouped() > 0))
      shiny::req(input$chart_type)

      grouped_report <- report_grouped() %>>%
        dplyr::mutate(category = as.character(category))
      # change from factor to character

      if (input$proportion) {
        y_variable <- "proportion"
        decimal_points <- 2
        # will be a number between 0 and 1 (or potentially -1 to 0 if mirrored)
        y_max <- 1
        if (any(grouped_report[[y_variable]] < 0)) {
          y_min <- -1
        } else {
          y_min <- 0
        }
      } else {
        y_variable <- "n"
        decimal_points <- 0
        # will be whole numbers (integers)
        y_min <- NULL # 'flexible' y-axis limits
        y_max <- NULL
      }

      chart_type <- input$chart_type
      # bar or column or line or area chart

      if (chart_type == "area" && any(grouped_report[[y_variable]] < 0)) {
        # if this is an area chart
        #  *and* there is a 'mirror' (so there are negative values)
        #  then negative and positive values should be separate stacks
        # note that 'stack'/sub-category is disabled as a choice
        #  for 'area' charts
        grouped_report <- grouped_report %>>%
          dplyr::mutate(stack = mirrored)
      }

      stack_group <- dplyr::distinct(grouped_report, series_name, stack) %>%
        dplyr::arrange(series_name) %>%
        dplyr::pull(stack)

      hc <- highcharter::hchart(
        grouped_report,
        type = chart_type,
        highcharter::hcaes(
          x = category, y = !!dplyr::sym(y_variable),
          group = series_name
        ),
        stack = stack_group
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
          )),
          min = y_min,
          max = y_max
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
          bar = list(stacking = "normal"),
          column = list(stacking = "normal"),
          area = list(stacking = "normal")
        ) %>>%
        highcharter::hc_exporting(
          enabled = TRUE
        )

      if (input$chart_theme != "plain") {
        hc_theme <- switch(
          input$chart_theme,
          "smpl" = {highcharter::hc_theme_smpl()},
          "538" = {highcharter::hc_theme_538()},
          "economist" = {highcharter::hc_theme_economist()},
          "elementary" = {highcharter::hc_theme_elementary()},
          "ffx" = {highcharter::hc_theme_ffx()},
          "flat" = {highcharter::hc_theme_flat()},
          "ft" = {highcharter::hc_theme_ft()},
          "ggplot2" = {highcharter::hc_theme_ggplot2()},
          "google" = {highcharter::hc_theme_google()},
          "monokai" = {highcharter::hc_theme_monokai()},
          "tufte" = {highcharter::hc_theme_tufte()}
        )
        hc <- hc %>>% highcharter::hc_add_theme(hc_theme)
      }

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
  # includes restoration/load from file

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
            report_values(
              report$report_values() %>>%
                dplyr::mutate(DateTo = as.character(DateTo))
              # co-erce to character (instead of numeric)
            )
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
    c(report_values(), input$qim_chosen, input$age_range,
      input$sex_chosen, input$ethnicity_chosen,
      input$diabetes_chosen, input$dateto_chosen),
    priority = -3,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      # filter report_values() to create report_filled()
      # filter by QIM, DateTo, Age, Sex, Indigenous, DiabetesType
      # replace NA
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
          dplyr::filter(
            # restrict to chosen date period(s)
            DateTo %in% input$dateto_chosen
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
