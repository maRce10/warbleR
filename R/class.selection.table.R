#' Class 'selection.table': double-checked frequency/time coordinates of selections
#'
#' Class for selections of signals in sound files
#' @param object object of class 'selection.table'
#' @param x object of class 'selection.table'
#' @export
#' @exportClass  selection.table
#' @details An object of class \code{selection.table} contains selection tables.
#'
#' The time series can be regular or irregular, indicated by the \code{regular} field
#'
#' In a regular \code{vpts} object the profiles are equally spaced in time.
#' In an irregular \code{vpts} object the time steps between profiles are of unequal length.
#'
#' Irregular time series can be projected onto a regular time grid using the \link{regularize} function.
#'
#' By contrast, in \link[=summary.vp]{vplist} objects the profiles have no time ordering, and can contain profiles of multiple radars.
#'
#' Data contained in this class object should be accessed with the \link{fetch} function.
#' Information stored under \code{attributes} (see below) can be accessed directly.
#'
#' An object of class \code{vpts} is a list containing
#' \describe{
#'  \item{\code{selections}}{data frame containing the frequency/time coordinates of the selections, sound file names, and any  additional information}
#'  \item{\code{check.resutls}}{results of the checks on data consistency using \link{checksels}}
#' }
