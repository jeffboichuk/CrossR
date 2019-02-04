library(rvest)
library(tidyverse)

x <- "https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts"

# read in the data
shortcuts <- read_html(x) %>%
   html_nodes("td") %>%
   html_text()

# convert to a tibble
shortcuts <- shortcuts %>%
   enframe(name = "row_number", value = "description")

# find which rows are empty in shortcuts_data and manually code the last row as
# empty. the empty rows always come before a title row, which always includes
# "\r\n"
empty_rows <- c(str_which(shortcuts$description, "\r\n") - 1, 440)

# removing empty_rows and title rows
shortcuts <- shortcuts %>%
   filter(
      !row_number %in% empty_rows,
      str_detect(description, "\r\n") == FALSE
   )

# reshaping the data
shortcuts <- shortcuts %>%
   # remove headings, such as Console, Source, etc.
   mutate(
      shortcut = rep(1:140, each = 3),
      type = rep(1:3, 140)
   ) %>%
   spread(type, description) %>%
   group_by(shortcut) %>%
   fill(everything(), .direction = "down") %>%
   fill(everything(), .direction = "up") %>%
   ungroup() %>%
   select(-row_number) %>%
   unique() %>%
   transmute(
      description = `1`,
      windows     = `2`,
      mac         = `3`
   )

shortcuts
