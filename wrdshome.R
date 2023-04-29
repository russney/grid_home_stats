# application to visualize WRDS home directory usage
library(plyr)
library(stringr)
library(ggplot2)

# load in data 
setwd("/home/wrds/rney/diskusage/")
filenames <- list.files(path = ".", 
                        pattern = NULL, 
                        all.files = FALSE, 
                        full.names = F, 
                        recursive = F,
                        ignore.case = F
                        )

read_csv_filename <- function(filenames) {
  ret <- read.csv(filenames, sep = "", header = FALSE)
  ret$Source <- filenames
  ret
}

import.list <- ldply(filenames, read_csv_filename)
head(import.list)

# rename columns
names(import.list) <- c("size", "school", "date")

# clean up data
import.list$date <- str_replace_all(import.list$date, "disk.usage.", "")
import.list$school <- str_replace_all(import.list$school, "/home/", "")
import.list$date <- as.Date(import.list$date, "%d%m%Y" )

head(import.list)

# subset before graphing
tail(import.list, 12)
#graphthis <- subset(import.list, school %in% c("wrds", "umd"))
graphthis <- subset(import.list, date > "22-01-01")
graphthis <- subset(graphthis, school %in% head(tail(graphthis, n = 12), n = 11 )$school)
table(graphthis$school)

ggplot(data = graphthis, aes(x = date, y = size, group = school)) + 
  geom_line(aes(color = school)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) + 
  ylab("Bytes") + 
  xlab("Date (year)") +
  ggtitle("WRDS /home space usage by top schools") +
  theme(plot.title = element_text(hjust = 0.5))

