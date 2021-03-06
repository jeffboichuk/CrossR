% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find-shortcuts.R
\name{find_shortcuts}
\alias{find_shortcuts}
\title{Find Keyboard Shortcuts}
\usage{
find_shortcuts(keyword = "*", category = c("Build", "Console", "Debug",
  "Execute", "Files", "Other", "Panes", "Profile", "Source Control",
  "Source Editor", "Source Navigation", "Tabs", "Terminal"),
  os = get_os())
}
\arguments{
\item{keyword}{character; a case-insensitive regex pattern identifying rows
to filter. If nothing is specified then all shortcuts are returned via the
"*" regex pattern}

\item{category}{character; one or more of ten categorizations of shortcuts
based on behavior}

\item{os}{character; a string vector specifying \code{"windows"},
\code{"mac"}, or both to return shortcuts that work on the user's operating
system or the specified operating system(s)}
}
\value{
A \code{tbl_df} of all shortcuts matching the keyword, category, and
  operating system(s)
}
\description{
This function returns all keyboard shortcuts that match the keyword,
category, and operating system(s) specified.
}
\details{
The ten different categories are as follows:
\itemize{
  \item Build
  \item Console
  \item Debug
  \item Execute
  \item Files
  \item Other
  \item Panes
  \item Profile
  \item Source Control
  \item Source Editor
  \item Source Navigation
  \item Tabs
  \item Terminal
}
}
\examples{
# return all shortcuts
all_shortcuts <- find_shortcuts()

# return all the shortcuts regarding "move"
move_shortcuts <- find_shortcuts(keyword = "move")

# return just the "move" shortcuts regarding
# the "Console" for Mac OSX
move_console_shortcuts <- find_shortcuts(keyword = "move",
                                        category = "Console",
                                        os = "mac")
}
