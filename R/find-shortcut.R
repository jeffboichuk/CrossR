find_shortcut <- function(description_includes, os = "windows and mac") {

   x <- shortcuts %>%
      dplyr::filter(
         stringr::str_detect(description, description_includes) == TRUE
      )

   return(x)
}

# note to add an `os` argument that takes "windows and mac" as the default and
# allows users to specify "windows" or "mac" if they want find_shortcut to
# return only the shortcut for their respective operating system.
# this code is not working:
#
# if (os = "windows") {
#    x <- select(x, -mac)
# } else if (os = "mac") {
#    x <- select(x, -windows)
# } else {
#    x
# }
