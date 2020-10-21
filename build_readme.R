#### This script builds the README.md
#### for R/Pharma 2020 workshops and talks

library(tidyverse)
library(glue)

## Params

loc_data <- "talks_table.csv"

## Process

data <- read_csv(loc_data,  locale = locale(encoding = 'latin1'), na = character())

## Print
sink('README.md')

cat("## Building this document\n\n")
cat("To build this README, run `build_readme.R`. Talks data is in csv `talks_table.csv`\n\n")


cat("## Workshops\n\n")
data %>%
  filter(Type == 'workshop') %>%
  rowwise() %>%
  mutate(txt = paste0(paste0("<strong>", unlist(strsplit(Name, " // ")), "</strong> (<i>", unlist(strsplit(Affiliation, " // ")), "</i>)"), collapse = ", ")) %>%
  glue_data(
    "{txt}<br>{Title}",
    "<details><summary>Abstract</summary>",
    "</p>{Abstract}</p>",
    "</details>",
    "[Link to Workshop Material]({Slides})<br><br>"
  )

x <- lapply(1:3, function(i) {
  data %>%
    filter(Type == "talk") %>%
    filter(Day  == i) %>%
    rowwise() %>%
    mutate(txt = paste0(paste0("<strong>", unlist(strsplit(Name, " // ")), "</strong> (<i>", unlist(strsplit(Affiliation, " // ")), "</i>)"), collapse = ", ")) %>%
    glue_data(
      "{txt}<br>{Title}",
      "<details><summary>Abstract</summary>",
      "</p>{Abstract}</p>",
      "</details>",
      "[Slides]({Slides})<br><br>"
    )
})

for (i in 1:3) {
  cat("\n\n## Talks - Day ", i, "\n\n")
  cat(x[[i]])
}

sink()
