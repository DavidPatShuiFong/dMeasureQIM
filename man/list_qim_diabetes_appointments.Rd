% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QIM_diabetes.R
\name{list_qim_diabetes_appointments}
\alias{list_qim_diabetes_appointments}
\title{List of diabetics, with Quality Improvement Measures, in contact list plus appointment details}
\usage{
list_qim_diabetes_appointments(
  dMeasureQIM_obj,
  contact = NA,
  date_from = NA,
  date_to = NA,
  clinicians = NA,
  min_contact = NA,
  min_date = NA,
  max_date = NA,
  type_diabetes = NA,
  contact_type = NA,
  ignoreOld = NA,
  lazy = FALSE,
  store = TRUE
)
}
\arguments{
\item{dMeasureQIM_obj}{dMeasureQIM R6 object}

\item{contact}{patient list. default is $qim_contact.
TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from $dMeasure object.
FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.}

\item{date_from}{start date. default is $date_a}

\item{date_to}{end date (inclusive). default is $date_b}

\item{clinicians}{list of clinicians to view. default is $clinicians}

\item{min_contact}{minimum number of contacts. default is $contact_min, initially one (1)}

\item{min_date}{most recent contact must be at least min_date. default is $contact_minDate, initially -Inf}

\item{max_date}{most recent contact at most max_date. default is $contact_maxDate}

\item{type_diabetes}{add type of diabetes?}

\item{contact_type}{contact types which are accepted. default is $contact_type}

\item{ignoreOld}{ignore results/observatioins that don't qualify for quality improvement measures
if not supplied, reads $qim_ignoreOld}

\item{lazy}{recalculate the diabetes contact list?}

\item{store}{keep result in self$qim_diabetes_list_appointments}
}
\value{
dataframe of Patient (name), InternalID, appointment details and measures
}
\description{
Filtered by date, and chosen clinicians.
 Note that a 'contact' could potentially be defined as something else other than
 an appointment! (e.g. billing, or record of visit)
}
\details{
QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months

QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months

QIM 10 - Blood pressure - most recent. the QIM measure is within the last six months

the reference date for 'most recent' measurement is 'date_to'
}
