#' Get Encouragement
#'
#' This function returns one of many different messages that give the user positive
#' feedback letting them know that they are doing great and should keep going!
#'
#' @return A character string message of encouragment
#' @examples
#' get_encouragement()
#' @export
get_encouragement <- function() {

   encouragement <- c(
      "Encouragement!",
      "Look at you getting faster!",
      "More fluent with R than you were when you woke up, I see. :)",
      "There you go again, being excellent."
   )

   sample(encouragement, 1)
}
