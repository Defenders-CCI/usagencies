# BSD_2_clause

library(dplyr)
library(stringr)

doc <- readLines("data-raw/usagencies.txt")
hits <- doc[grep(doc, pattern = "^\\*|==")]
branch <- hits[grep(hits, pattern = "^==[A-Za-z]")]
branch <- branch[1:5]
depts <- hits[grep(hits, pattern = "^===[A-Za-z]")]
depts <- depts[1:22]
first <- hits[grep(hits, pattern = "^\\*\\[")]
second <- hits[grep(hits, pattern = "^\\*{2}\\[")]
third <- hits[grep(hits, pattern = "^\\*{3}\\[")]

clean_misc <- function(s) {
  x <- str_replace_all(s, "=", replacement = "")
  x <- str_replace_all(x, "\\*", replacement = "")
  x <- str_replace_all(x, "\\[", replacement = "")
  x <- str_replace_all(x, "\\]", replacement = "")
  if(grepl(x, pattern = "\\|")) {
    x <- str_split(x, "\\|")[[1]][2]
  }
  return(x)
}

data <- data.frame(branch = c(""),
                   dept = c(""),
                   agency = c(""),
                   office = c(""),
                   entity = c(""),
                   stringsAsFactors = FALSE)
data

branch = NA
dept = NA
agency = NA
office = NA
entity = NA
for(i in 1:length(hits)) {
  if(str_detect(hits[i], "^==[A-Za-z]")) {
    branch <- clean_misc(hits[i])
  } else if(str_detect(hits[i], "^===[A-Za-z]")) {
    dept <- clean_misc(hits[i])
  } else if(str_detect(hits[i], "^\\*\\[")) {
    agency <- clean_misc(hits[i])
    office <- NA
    entity <- NA
    cur_row <- c(branch, dept, agency, office, entity)
    data <- rbind(data, cur_row)
  } else if(str_detect(hits[i], "^\\*{2}\\[")) {
    office <- clean_misc(hits[i])
    entity <- NA
    cur_row <- c(branch, dept, agency, office, entity)
    data <- rbind(data, cur_row)
  } else if(str_detect(line, "^\\*{3}\\[")) {
    c_entity <- clean_misc(hits[i])
    cur_row <- c(branch, dept, agency, office, entity)
    data <- rbind(data, cur_row)
  }
}
data <- data[-1,]
head(data, 20)
head(hits, 20)

usagencies <- data
usagencies$combo <- paste(usagencies$branch, usagencies$dept,
                          usagencies$agency, usagencies$office,
                          usagencies$entity, sep = " | ")
head(usagencies)

usagencies <- usagencies[-grep(usagencies$combo, pattern = "See Also"), ]

devtools::use_data(usagencies, overwrite = TRUE)
