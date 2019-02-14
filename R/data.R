#' Shortcuts Table
#'
#' This data.frame contains the RStudio keyboard shortcuts for both Mac OSX and Windows.
#'
#' \itemize{
#'   \item category: one of ten categorizations of shortcuts based on behavior
#'   \item description: a brief description of what the shortcut does
#'   \item windows: the shortcut keys on Windows
#'   \item mac: the shortcut keys on Mac
#' }
#'
#' @docType data
#' @keywords datasets
#' @name shortcuts
#' @importFrom tibble tibble
#' @usage data(shortcuts)
#' @format a \code{tbl_df} with 140 rows and 4 variables
#' @examples
#' \dontrun{
#' data(shortcuts)
#'
#' head(shortcuts, 3)
#' #   category                      description windows           mac
#' # 1  Console           Move cursor to Console  Ctrl+2        Ctrl+2
#' # 2  Console                    Clear console  Ctrl+L        Ctrl+L
#' # 3  Console Move cursor to beginning of line    Home  Command+Left
#' }
NULL
