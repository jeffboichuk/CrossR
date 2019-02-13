#' Find Keyboard Shortcut
#'
#' This function returns all keyboard shortcuts that match the keyword and
#' operating system(s) specified.
#'
#' @importFrom dplyr select filter one_of
#' @param keyword character; a case-insensitive regex pattern identifying rows
#'   to filter
#' @param os character; a string vector specifying \code{"windows"},
#'   \code{"mac"}, or both to return shortcuts that work on the user's operating
#'   system or the specified operating system(s)
#' @return A \code{tbl_df} of all shortcuts matching the keyword and operating
#'   system(s)
#' @examples
#' find_shortcut(keyword = "clear", os = "mac")
#' @export
find_shortcut <- function(keyword,
                          os = get_os()) {

   cols <- match.arg(os, several.ok = TRUE, choices = c("mac", "windows"))

   result <- shortcuts %>%
      filter(grepl(keyword, description,
                   ignore.case = TRUE)) %>%
      select(description, one_of(cols))
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
         os <- "windows"
      }
      if (grepl("linux-gnu", R.version$os)) {
         os <- "windows"
      }
   }
   unname(tolower(os))
}
