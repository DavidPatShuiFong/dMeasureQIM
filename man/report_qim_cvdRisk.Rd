% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QIM_cvdRisk.R
\name{report_qim_cvdRisk}
\alias{report_qim_cvdRisk}
\title{Cardiovascular disease risk Quality Improvement Measure report, in the contact list}
\usage{
report_qim_cvdRisk(
  dMeasureQIM_obj,
  contact = NA,
  date_from = NA,
  date_to = NA,
  clinicians = NA,
  min_contact = NA,
  contact_type = NA,
  min_date = NA,
  max_date = NA,
  demographic = NA,
  ignoreOld = NA,
  lazy = FALSE,
  store = TRUE
)
}
\arguments{
\item{dMeasureQIM_obj}{dMeasureQIM R6 object}

\item{contact}{patient list. default is $qim_contact.

    TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.

    FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.}

\item{date_from}{start date. default is $date_a}

\item{date_to}{end date (inclusive). default is $date_b}

\item{clinicians}{list of clinicians to view. default is $clinicians}

\item{min_contact}{minimum number of contacts. default is $contact_min, initially one (1)}

\item{contact_type}{contact types which are accepted. default is $contact_type}

\item{min_date}{most recent contact must be at least min_date. default is $contact_minDate, initially -Inf}

\item{max_date}{most recent contact at most max_date. default is $contact_maxDate}

\item{demographic}{demographic groupings for reporting.
if not supplied, reads $qim_demographicGroup
list of available demographic groups in $qim_demographicGroupings}

\item{ignoreOld}{ignore results/observatioins that don't qualify for quality improvement measures
if not supplied, reads $qim_ignoreOld}

\item{lazy}{recalculate the cvdRisk contact list?}

\item{store}{keep result in self$qim_cvdRisk_report}
}
\value{
dataframe of Patient (name), Demographic, Measure (done or not), Count, Proportion,
  Proportion_Demographic
}
\description{
Filtered by date, and chosen clinicians
}
\details{
QIM 08.Proportion of patients with the necessary risk factors assessed to enable CVD assessment

the reference date for 'most recent' measurement is 'date_to'
}
