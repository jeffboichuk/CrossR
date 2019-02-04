library(rvest)
library(tidyverse)

# read in the raw data
shortcuts <- read_html(
   "https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts"
) %>%
   html_nodes("td") %>%
   html_text()

# convert to a tibble
shortcuts <- shortcuts %>%
   enframe(name = "row_number", value = "description")

# empty rows that come before title rows, which always include "\r\n", need to
# be dropped. we can find the indices for these rows with str_which. the last
# row of the raw data also needs to be dropped. dropping these rows will take us
# from 440 rows to 420 rows.

extraneous_rows <- (str_which(shortcuts$description, "\r\n") - 1) %>% c(., 440)

# dropping empty_rows and title rows
shortcuts <- shortcuts %>%
   filter(
      !row_number %in% extraneous_rows,
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

# saving the data
write_csv(shortcuts, "data-raw/shortcuts.csv")
save(shortcuts, file = "data/shortcuts.rda")
