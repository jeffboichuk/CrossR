#' Find Keyboard Shortcut
#'
#' This function returns all keyboard shortcuts that match the keyword, category,
#' and operating system(s) specified.
#'
#' @importFrom dplyr select filter one_of
#' @param keyword character; a case-insensitive regex pattern identifying rows
#'   to filter. If nothing is specified then all shortcuts are returned via the "*"
#'   regex pattern
#' @param category character; one or more of ten categorizations of shortcuts
#'   based on behavior
#' @param os character; a string vector specifying \code{"windows"},
#'   \code{"mac"}, or both to return shortcuts that work on the user's operating
#'   system or the specified operating system(s)
#' @return A \code{tbl_df} of all shortcuts matching the keyword, category, and operating system(s)
#' @details
#' The ten different categories are as follows:
#' \itemize{
#'   \item Build
#'   \item Completions (Console and Source)
#'   \item Console
#'   \item Debug
#'   \item Editing (Console and Source)
#'   \item Git/SVN
#'   \item Plots
#'   \item Session
#'   \item Source
#'   \item Views
#' }
#' @examples
#' # return all shortcuts
#' all_shortcuts <- find_shortcut()
#'
#' # return all the shortcuts regarding "move"
#' move_shortcuts <- find_shortcut(keyword = "move")
#'
#' # return just the "move" shortcuts regarding
#' # the "Console" for Mac OSX
#' move_console_shortcuts <- find_shortcut(keyword = "move",
#'                                         category = "Console",
#'                                         os = "mac")
#' @export
find_shortcut <- function(keyword = "*",
                          category = c("Build", "Completions (Console and Source)",
                                     "Console", "Debug", "Editing (Console and Source)",
                                     "Git/SVN", "Plots", "Session", "Source", "Views"),
                          os = get_os()) {

   this_category <- match.arg(category, several.ok = TRUE)
   this_os <- match.arg(os, several.ok = TRUE, choices = c("mac", "windows"))

   result <- get('shortcuts') %>%
      filter(grepl(keyword, description, ignore.case = TRUE),
             category %in% this_category) %>%
      select(category, description, one_of(this_os))

   return(result)
}

get_os <- function(){
   sysinf <- Sys.info()
   if (!is.null(sysinf)) {
      os <- sysinf['sysname']
      if (os == 'Darwin') {
         os <- "mac"
      }
   } else {
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os)) {
         os <- "mac"
      }
      if (grepl("linux-gnu", R.version$os)) {
         os <- "mac" # show the mac shortcut on linux machines
      }
   }
   unname(tolower(os))
}
