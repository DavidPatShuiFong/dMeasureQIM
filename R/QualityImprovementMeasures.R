# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @md
#'
#' @name qim
#' @title dMeasure Quality Improvement Measures
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureIntegration
#'
#' @name dMeasureIntegration
#'
#' @description integration with dMeasure
#'   (especially DailyMeasure)
#'
#' @param information the information required
#'   `Provides` - modules provided (in this case, `dMeasureQIMRept` and `dMeasureQIMAppt`)
#'   `Requires` - the modules required (including `dMeasure`)
#'   `moduleID` - IDs of modules to create
#'
#' @return vector of required information
#'
#' @export
dMeasureIntegration <- function(information) {
  if (information == "Provides") {return(c("dMeasureQIMRept", "dMeasureQIMAppt"))}
  if (information == "Requires") {return(c("dMeasure"))}
  if (information == "moduleID") {
    return(
      list(
        list(ID = "qimRept", extraArgs = "contact = TRUE"),
        list(ID = "qimAppt", extraArgs = "contact = FALSE")
      )
    )
  }
}

#' dMeasureQIM class
#' @title dMeasureQIM class
#' @description Quality Improvement Measures reports and case-finding in EMR (Best Practice)
#'     a module for dMeasure
#' @export
dMeasureQIM <- R6::R6Class("dMeasureQIM",
  public = list(
    # dM is a dMeasure object
    dM = NULL,
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj
      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureQIM") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureQIM") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureQIM") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureQIM") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################


.private(dMeasureQIM, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureQIM, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

#####  ignore 'old' observations results? ############################################
# if TRUE, ignore results that are too old to be qualify for quality improvement measure

.private(dMeasureQIM, ".qim_ignoreOld", TRUE)
.active(dMeasureQIM, "qim_ignoreOld", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_ignoreOld)
  }
  if (is.logical(value)) {
    private$.qim_ignoreOld <- value
    private$set_reactive(self$qim_ignoreOldR, value)
  } else {
    warning("$qim_ignoreOld only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasureQIM, "qim_ignoreOldR", TRUE)

##### patient list to use ############################################
# if TRUE, then use 'contact' list. otherwise use appointment list
# this doesn't affect QIM_active, but affects other methods

.private(dMeasureQIM, ".qim_contact", TRUE)
.active(dMeasureQIM, "qim_contact", function(value) {
  # is the 'contact' patient list being used?
  #  this can be a combination (or even none of...) appointments, visits and billings
  #  with various criteria. dM$contact_count_list
  # if false, the 'appointment' list is being used from dM$appointments_filtered
  if (missing(value)) {
    return(private$.qim_contact)
  }
  if (is.logical(value)) {
    private$.qim_contact <- value
    private$set_reactive(self$qim_contactR, value)
  } else {
    warning("$qim_contact only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasureQIM, "qim_contactR", TRUE)

##### demographic groupings for reporting ############################################

.active(dMeasureQIM, "qim_demographicGroupings", function(value) {
  if (!missing(value)) {
    warning("$qim_demographicGroupings is read-only.")
  } else {
    return(c("Age10", "Sex", "Ethnicity", "MaritalStatus", "Sexuality"))
    # vector of valid demographic groups (for QIM reporting)
    # Age in 10 year categories
    # Ethnicity
    # MaritalStatus
    # Sexuality
  }
})

.private_init(dMeasureQIM, ".qim_demographicGroup", quote(self$qim_demographicGroupings))
.active(dMeasureQIM, "qim_demographicGroup", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_demographicGroup)
  }
  value <- intersect(value, self$qim_demographicGroupings)
  # make sure groups are valid
  private$.qim_demographicGroup <- value
  private$set_reactive(self$qim_demographicGroupR, value)
})
.reactive(dMeasureQIM, "qim_demographicGroupR", quote(self$qim_demographicGroupings))
