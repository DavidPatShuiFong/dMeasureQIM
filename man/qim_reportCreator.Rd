% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QIM_report_UI.R
\name{qim_reportCreator}
\alias{qim_reportCreator}
\title{Quality Improvement report creation - server}
\usage{
qim_reportCreator(input, output, session, dMQIM, report)
}
\arguments{
\item{input}{as required by Shiny modules}

\item{output}{as required by Shiny modules}

\item{session}{as required by Shiny modules}

\item{dMQIM}{dMeasure QIM R6 object
access to appointments lists, results, correspondence and EMR database}

\item{report}{a list returned by qim_reportCharter
should contain $report_values(), which is a dataframe}
}
\value{
list with following components
\describe{
 \item{report_values}{dataframe of report}
}
}
\description{
Quality Improvement report creation - server
}
