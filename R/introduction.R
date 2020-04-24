# rintrojs introduction for QIM

#' rintrojs steps for CDM module
#'
#' @title steps_introduction_df
#' @param element_name the HTML datatable element, including datatable and helpers such as print/copy view
#' @param qim_tab the currently open tab
#' @param appointment_view appointment view
#' @description returns a dataframe of rintrojs steps
#'
#' requires pro-forma steps from DailyMeasure
#'
#' @export
steps_introduction_df <- function(element_name, qim_tab, appointment_view = FALSE) {
  steps_df <-
    data.frame(
      element = as.character(NA),
      intro = c(paste(
        shiny::tags$h4("GPstat! Quality Improvement Measures (QIM)"),
        shiny::br(),
        "View QIM reports, status and opportunities for chosen clinicians and dates.",
        shiny::br(), shiny::br(),
        "Use 'QIM Appointment' to view QIM opportunities",
        "for (usually future)",
        shiny::icon("calendar-alt"), "Appointments.",
        shiny::br(), shiny::br(),
        "Use 'PIP Quality Improvement' to produce QIM reports for (historical) contacts."
      )),
      position = "auto",
      stringsAsFactors = FALSE
    ) %>>%
    rbind(data.frame(
      element = as.character(NA),
      intro = c(paste(
        shiny::tags$h4("GPstat! Quality Improvement Measures (QIM)"),
        shiny::br(),
        "In", shiny::tags$em("QIM Appointment"), "the patients are drawn from",
        "the appointment book, as chosen by selected",
        shiny::icon("users"), shiny::strong("Clinicians"), "and",
        shiny::icon("calendar-alt"), shiny::strong("date range"), ".",
        shiny::br(), shiny::br(),
        "In", shiny::tags$em("PIP Quality Improvement"), "the patients are drawn",
        "from 'active' patients, as defined by",
        shiny::icon("handshake"), shiny::strong("Contact details"), ",",
        shiny::icon("users"), shiny::strong("Clinicians"), "and",
        shiny::icon("calendar-alt"), shiny::strong("date range")
      )),
      position = "auto"
    )) %>>%
    rbind(DailyMeasure::steps_choose_clinician_date_df())

  if (!appointment_view) {
    steps_df <- steps_df %>>%
      rbind(DailyMeasure::steps_choose_contact_details_df())
    # show how to choose 'active' contacts
  }

  if (appointment_view) {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Quality Improvement (QIM) Appointment view"),
          shiny::br(),
          "List of QIM opportunities",
          "according to currently selected clinicians and date range.",
          shiny::br(), shiny::br(),
          "By default shows : ", shiny::strong("Patient"), "(name),",
          shiny::strong("RecordNo"), ".",
          shiny::br(), shiny::br(),
          "In Appointments view also",
          "shows Appointment details e.g.", shiny::strong("AppointmentTime"),
          "and", shiny::strong("Provider"), " (clinician)."
        )),
        position = "auto"
      )) %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Quality Improvement (QIM) Appointment view"),
          shiny::br(),
          "By default does", shiny::tags$em("not"),
          "show demographic groupings (e.g.",
          shiny::strong("Age5"), ",",
          shiny::strong("Sex"), "and", shiny::strong("Ethnicity"), ")"
        )),
        position = "auto"
      ))
  } else {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Practice Incentive Program (PIP) Quality Improvement (QIM) view"),
          shiny::br(),
          "Generate reports of QIM measurements",
          "according to currently selected clinicians, dates",
          "and contact definitions.",
          shiny::br(), shiny::br(),
          "By default shows demographic groupings (e.g.",
          shiny::strong("Age5"), ",",
          shiny::strong("Sex"), "and", shiny::strong("Ethnicity"), ")",
          "and the number of patients in each grouping."
        )),
        position = "auto"
      ))
  }

  steps_df <- steps_df %>>%
    rbind(data.frame(
      element = element_name,
      intro = c(paste(
        shiny::tags$h4("Demographic groupings and Old Measurements"),
        shiny::br(),
        "Display (and grouping) by demographic groups (e.g.",
        shiny::strong("Age5"), ",",
        shiny::strong("Sex"), "and", shiny::strong("Ethnicity"),
        ") can be changed using the",
        shiny::icon("gears"), shiny::strong("Settings"), "dropdown",
        "to the top-right of the table, just to the left of the",
        "'Quality Improvement Measures' title.",
        shiny::br(), shiny::br(),
        "Note that the groupings can be", shiny::tags$em("hidden"),
        "using the 'Column visibility' button to the bottom-right",
        "of the table, but hiding the groupings does", shiny::tags$em("not"),
        "stop the grouping!",
        shiny::br(), shiny::br(),
        "By default measurements which are not recent/current,",
        "as defined by the",
        shiny::tags$html(
          shiny::tags$a("Department of Health PIP QI Incentive Guidance",
            href = "https://www1.health.gov.au/internet/main/publishing.nsf/Content/PIP-QI_Incentive_guidance",
            target = "_blank"
          )
        ),
        "are not included in the reports and listings.",
        "'Old' measurements can be included by toggling the",
        shiny::icon("calendar-times"), "/", shiny::icon("calendar-alt"),
        shiny::strong("Ignore old measurements"), "button in",
        "the", shiny::icon("gears"), "Settings dropdown."
      )),
      position = "auto"
    ))

  if (!appointment_view) {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Report/List view"),
          shiny::br(),
          shiny::icon("book-reader"), "Reports and",
          shiny::icon("clipboard-list"), "Lists.",
          shiny::br(),
          "Top-left of table",
          shiny::br(), shiny::br(),
          shiny::icon("book-reader"),
          "Reports show summary statistics, grouping patients by demographic,",
          "and measurement, groupings",
          shiny::br(),
          shiny::icon("clipboard-list"),
          "Lists show the individual patient details which are used to generate",
          "the reports.",
          shiny::br(), shiny::br(),
          "Note that some types of identifying patient information (e.g. names) are",
          shiny::tags$em("not"), "shown by default in the List view",
          "but can be shown by altering the 'Column visibility' settings,",
          "found on the bottom right."
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "Active") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Active"),
          shiny::br(),
          "Active view counts the number of patients in each category.",
          shiny::br(), shiny::br(),
          "In", shiny::tags$em("QIM Appointment"), "the patients are drawn from",
          "the appointment book, as chosen by selected",
          shiny::icon("users"), shiny::strong("Clinicians"), "and",
          shiny::icon("calendar-alt"), shiny::strong("date range"), ".",
          shiny::br(), shiny::br(),
          "In", shiny::tags$em("PIP Quality Improvement"), "the patients are drawn",
          "from 'active' patients, as defined by",
          shiny::icon("handshake"), shiny::strong("Contact details"), ",",
          shiny::icon("users"), shiny::strong("Clinicians"), "and",
          shiny::icon("calendar-alt"), shiny::strong("date range")
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "Diabetes") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Diabetes Measures"),
          shiny::br(),
          "Diabetes view also shows",
          shiny::strong("HbA1C"), "dates and values and",
          shiny::strong("influenza vaccination"), "dates."
        )),
        position = "auto"
      )) %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Diabetes Measures"),
          shiny::br(),
          shiny::icon("gear"), shiny::br(),
          "Top-right of the table view.",
          shiny::br(), shiny::br(),
          "Choose Diabetes measures to display.",
          shiny::br(), shiny::br(),
          "You can try it now",
          emo::ji("smile"), "!"
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "Cervical Screening") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Cervical Screening"),
          shiny::br(),
          "Cervical Screening view also shows",
          shiny::strong("CST"), "(cervical screening) dates."
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "15+") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Age 15+ Measures"),
          shiny::br(),
          "Age 15+ view also shows",
          shiny::strong("Smoking"), "status and recording dates,",
          shiny::strong("Alcohol"), "status and recording dates,",
          shiny::strong("Height"), ",", shiny::strong("Weight"), ",",
          shiny::strong("Waist"), "and", shiny::strong("BMI"), ".",
          shiny::br(), shiny::br(),
          "If you don't want to see all those measures you either",
          "hide some columns using the 'Column visibility' on
                                       the bottom-right, or use the",
          shiny::icon("gear"), "15+ Measures chooser on the",
          "top-right."
        )),
        position = "auto"
      )) %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("15+ Measures"),
          shiny::br(),
          shiny::icon("gear"), shiny::br(),
          "Top-right of the table view.",
          shiny::br(), shiny::br(),
          "Choose 15+ measures to display.",
          shiny::br(), shiny::br(),
          "You can try it now",
          emo::ji("smile"), "!"
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "65+") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Age 65+ Measures"),
          shiny::br(),
          "Age 65+ view also shows",
          shiny::strong("Fluvax"), "(influenza administration) date."
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "COPD (Lung Disease)") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("COPD (Lung Disease) Measures"),
          shiny::br(),
          "COPD (Lung Disease) view also shows",
          shiny::strong("Fluvax"), "(influenza administration) date."
        )),
        position = "auto"
      ))
  }

  if (qim_tab == "Cardiovascular risk") {
    steps_df <- steps_df %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Cardiovascular Risk"),
          shiny::br(),
          "Cardiovascular Risk view shows cardiovascular risk factors e.g.",
          shiny::strong("CardiovascularDisease"), "(history of),",
          shiny::strong("Diabetes"), "(history of),",
          shiny::strong("SmokingStatus"), ",",
          shiny::strong("Cholesterol"), "(including measurements and dates)",
          shiny::strong("BP"), "(including measurements and dates).",
          shiny::br(), shiny::br(),
          "There are a lot of cardiovascular risk factor measurements and",
          "recordings. If you wish to hide some of these measurements, use",
          "the 'Column visibility' button on the bottom-right of the table."
        )),
        position = "auto"
      )) %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Framingham Risk Equation"),
          shiny::br(),
          "If there is enough information to calculate cardiovascular risk",
          "according to the Framingham Risk Equation then the risk '",
          shiny::strong("frisk"), "' is calculated. In some situations '",
          shiny::strong("friskHI"), "' indicates the calculated risk is",
          "greater than 15%."
        )),
        position = "auto"
      )) %>>%
      rbind(data.frame(
        element = element_name,
        intro = c(paste(
          shiny::tags$h4("Cardiovascular Risk"),
          shiny::br(),
          shiny::icon("gear"), shiny::br(),
          "Top-right of the table view.",
          shiny::br(), shiny::br(),
          "The 'normal' Framingham Risk Equation does not include",
          "people over the age of 75+, those with known cardiovascular",
          "disease or people of Aboriginal and/or Torres Strait Islander",
          "background.",
          shiny::br(), shiny::br(),
          "GPstat! includes all these groups by default. You can include/exclude",
          "these groups using the 'Inclusions/Exclusions' drop-down chooser."
        )),
        position = "auto"
      ))
  }

  steps_df <- steps_df %>>%
    rbind(DailyMeasure::steps_datatable_helpers(element_name, print_copy_view = FALSE))

  return(steps_df)
}
