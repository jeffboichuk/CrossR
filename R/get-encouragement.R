encouragement <- c(
   "Encouragement!",
   "Look at you getting faster!",
   "More fluent with R than you were when you woke up, I see. :)"
)

get_encouragement <- function() {
   sample(encouragement, 1)
}
