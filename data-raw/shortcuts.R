library(rvest)
library(tidyverse)

# reading in the raw data
shortcuts <- read_html(
   "https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts"
) %>%
   html_nodes("td") %>%
   html_text()

# converting to a tibble
shortcuts <- shortcuts %>%
   enframe(name = "row_number", value = "description")

# adding a category variable
shortcuts <- shortcuts %>%
   mutate(
      category = ifelse(str_detect(description, "\r\n"), description, NA),
      category = str_remove_all(category, "\r\n")
   ) %>%
   fill(category, .direction = "down")

# empty rows that come before title rows, which always include "\r\n", need to
# be dropped. we can find the indices for these rows with str_which. the last
# row of the data needs to be dropped, too. dropping all of these rows will take
# us from 440 rows to 420 rows.

# identifying extraneous rows
extraneous_rows <- (str_which(shortcuts$description, "\r\n") - 1) %>% c(., 440)

# dropping extraneous rows
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
      category = category,
      description = `1`,
      windows     = `2`,
      mac         = `3`
   ) %>%
   mutate(
      description = ifelse(description == "Goto File/Function",
                           "Go to File/Function",
                           description)
   )

# saving the data
write_csv(shortcuts, "data-raw/shortcuts.csv")
save(shortcuts, file = "data/shortcuts.rda")
