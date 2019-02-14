#' \code{CrossR} package\if{html}{\figure{logo.png}{options: width="25\%" align="right" alt="Figure: logo.png"}}
#'
#' HIIT Exercises for the R User
#'
#' Instructors can leverage CrossR to lead R users through high-intensity interval
#' training (HIIT) exercise sessions. These sessions guide R users through simple,
#' hands-on coding activities repetitiously for 20-90 seconds at a time. The goal
#' is to help R users become more fluent with the R programming language and
#' RStudio's shortcuts, while keeping things challenging and fun.
#'
#' @docType package
#' @name CrossR
#' @importFrom dplyr %>%
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
