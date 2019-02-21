
# This R script is for parsing the shortcuts from where the commands are defined
# in the IDE by XML. This is in contrast to parsing the HTML from a published
# cheatsheet of the IDE shortcuts, which we have found to be out-of-date
# compared to the shortcuts in the latest versions of RStudio

library(xml2)
library(rvest)
library(tidyverse)

# This commit hash (6979d6d339bdb036ac052f3de45ba0b3c0a79a96) refers to RStudio
# v1.1.463, the most recent version of RStudio, so participants who just
# installed RStudio will have the exact same keybindings.

# TODO: At some point, we should start using the SOURCE file to pin down the
# commit hash of the installed RStudio version.

# TODO: Also load the keybindings from the .R hidden directory so we can adapt
# to the user specified shortcuts if they have modified the defaults:
# ~/.R/rstudio/keybindings/rstudio_commands.json

parse_shortcutgroup <- function(x) {
  group_name <- xml_attr(x, "name")
  shortcuts <- x %>%
    html_nodes("shortcut") %>%
    map_df(.f = function(y) {
      as_tibble(t(unlist(xml_attrs(y))))
    })
  shortcuts$category <- group_name
  return(shortcuts)
}

raw_shortcuts <- read_xml(
   "https://raw.githubusercontent.com/rstudio/rstudio/6979d6d339bdb036ac052f3de45ba0b3c0a79a96/src/gwt/src/org/rstudio/studio/client/workbench/commands/Commands.cmd.xml"
   ) %>%
   html_nodes("shortcutgroup") %>%
   map_df(parse_shortcutgroup)

# Reformat the data to match our package spec (4 columns: Category, Description,
# Windows, Mac)
shortcuts <- raw_shortcuts %>%
  # drop any of the shortcuts from the group named "Not Displayed" since:
  # "shortcuts in this group won't be shown in the quick reference card."
  filter(category != "Not Displayed") %>%
  # remove "layoutEndZoom" because it doesn't appear in the HTML version or
  # modal shortcuts pane
  filter(refid != "layoutEndZoom") %>%
  # remove any shortcuts with a space because those are not going to work
  filter(!grepl("\\s+", value)) %>%
  # if no "title" attribute, then create title by converting the "refid"
  # attribute from camel case to proper case by inserting a space before every
  # capitalized letter But first, make a one-off change to VCS, PDF, and HTML
  mutate(
     refid       = gsub("VCS", "Vcs", refid),
     refid       = gsub("PDF", "Pdf", refid),
     refid       = gsub("HTML", "Html", refid)
  ) %>%
  mutate(
     description = ifelse(
        !is.na(title),
        title,
        str_to_title(gsub("([[:upper:]])+", " \\1", refid))
     )
  ) %>%
  # convert the case back to upper for Vcs, Pdf, and Html
  mutate(
     description = gsub("Vcs", "VCS", description),
     description = gsub("Pdf", "PDF", description),
     description = gsub("Html", "HTML", description)
  ) %>%
  # remove some weird triple dots that people descided should be part of the
  # description
  mutate(description = gsub("\\.\\.\\.", "", description)) %>%
  # infer the operating system based on the `if` column. if the `if` column
  # isn't specified, then assume it's the same across operating systems
  mutate(
     `if` = gsub(
        "org.rstudio.core.client.BrowseCap.is",
        "",
        `if`,
        fixed = TRUE
     ),
     `if` = gsub(
        "org.rstudio.studio.client.application.Desktop.is",
        "",
        `if`,
        fixed = TRUE
     ),
     `if` = gsub("()", "", `if`, fixed = TRUE)
  ) %>%
  # drop any that appear to solely target Chrome, Linux, Not Desktop, etc.
  # basically anything that is not going to work on RStudio Desktop running on
  # Windows or Mac
  # in addition, drop anything specifically saying !Linux because there are
  # other versions that will cover us so we don't need the info from these rows
  filter(
     !(`if` %in%
          c("Chrome", "Linux", "!Desktop", "Macintosh && Chrome", "!Linux"))
  ) %>%
  # recode the following into Windows
  mutate(
     os = ifelse(
        `if` %in% c("Windows", "!Macintosh", "WindowsDesktop"), "windows", NA
     )
  ) %>%
  # recode the following into Mac
  mutate(
     os = ifelse(
        `if` %in% c("Macintosh", "!Windows", "MacintoshDesktop"), "mac", os
     )
  ) %>%
  # recode the following into NA, which implies that a shortcut will work on
  # either system
  mutate(
     os = ifelse(
        `if` %in% c("!Chrome", "Desktop", "!(Macintosh && Chrome)"), NA, os
     )
  ) %>%
  # recode remaining NAs as mac if Meta (Cmd) is in the shortcut
  mutate(os = ifelse(grepl("Meta", value) & is.na(os), "mac", os))

# work from the bottom up
shortcuts <- shortcuts %>%
   mutate(dupe = duplicated(description, fromLast = TRUE)) %>%
   # drop duplicates and NA shortcuts, meaning that we have a suitable shortcut
   # for both operating systems
   filter(!(dupe & is.na(os))) %>%
   # remove anytime we have duplicate shortcuts for an os
   distinct(description, os, .keep_all = TRUE) %>%
   select(category, description, os, value) %>%
   spread(os, value) %>%
   # determine which are cross platform commands
   mutate(is_cross_platform = is.na(mac) & is.na(windows) & !is.na(`<NA>`)) %>%
   # apply Mac if the command is cross-platform
   mutate(mac = ifelse(is_cross_platform, `<NA>`, mac)) %>%
   # apply Windows if the command is cross-platform
   mutate(windows = ifelse(is_cross_platform, `<NA>`, windows)) %>%
   # drop this indicator column
   select(-is_cross_platform) %>%
   # if we have a Meta (implying Cmd) and NA for Windows, just sub in CTRL
   mutate(
      windows = ifelse(
         is.na(windows) & grepl("Meta", mac),
         gsub("Meta", "Ctrl", mac),
         windows
      )
   ) %>%
   # replace Meta with Cmd for Mac
   mutate(mac = gsub("Meta", "Cmd", mac)) %>%
   # replace Cmd with Ctrl for Windows
   # sometimes the spec says Command is good for both operating systems, but
   # then the shortcut uses "Cmd" (which on Windows is "Ctrl")
   mutate(windows = gsub("Cmd", "Ctrl", windows)) %>%
   select(-`<NA>`) %>%
   # add a space around the plus sign
   mutate(
      mac     = gsub("\\+", " + ", mac),
      windows = gsub("\\+", " + ", windows)
   ) %>%
   # swap the keystrokes to be from left to right
   # TODO (JEFF): swap the keystrokes from left to right for windows
   mutate(
      mac = gsub("Cmd \\+ Shift", "Shift + Cmd", mac),
      mac = gsub("Ctrl \\+ Shift", "Shift + Ctrl", mac),
      mac = gsub("Alt \\+ Shift", "Shift + Alt", mac),
      mac = gsub("Cmd \\+ Alt", "Alt + Cmd", mac)
   )

# saving the data
write_csv(shortcuts, "data-raw/shortcuts.csv")
save(shortcuts, file = "data/shortcuts.rda")
