library(tidyverse)
library(lubridate)
library(stringi)
library(xml2)

args <- commandArgs(trailingOnly = TRUE)
print(args)

# read the HTML dataset
obj <- read_html(file.path("dev.html"))

# find the links and make open in new tab
links <- xml_find_all(obj, ".//a")
xml_set_attr(links, "target", "_blank")
xml_set_attr(links, "rel", "noreferrer noopener")

# find the links that have a class attribute
clses <- xml_attr(links, "class")
links <- links[!is.na(clses)]
dates <- ymd(clses[!is.na(clses)])
types <- stri_sub(clses[!is.na(clses)], 12L, -1L)

# When should each link become available?
avail <- dates
avail[types == 'reading'] <- dates[types == 'reading'] - 6L
avail[types == 'slides'] <- dates[types == 'slides']
avail[types == 'solutions'] <- dates[types == 'solutions'] + 1L
avail[types == 'exam'] <- dates[types == 'exam'] - 1L

# if a link text is [], these are slides that are not needed; remove them
index <- which(xml_text(links) == "[]")
xml_remove(links[index])

# take the dates after today; modify those links into spans, remove href; if
# after 13h15 (time of the last class) advance an extra day; also, remove
# links that do not exist
td <- as_date(now() + 60 * 60 * 13.25) + 0L
if (length(args) == 0 || (args[1] != "notime"))
{
  index <- which(td < avail) 
  if (length(index)) {
    links <- links[index]
    xml_set_name(links, "span")
    xml_set_attr(links, "href", NULL)
    xml_set_attr(links, "class", "futurelink")  
  }
}

#index <- which(!file.exists(xml_attr(links, "href")))
#if (length(index)) { links <- links[index] }
#xml_set_name(links, "span")
#xml_set_attr(links, "href", NULL)
#xml_set_attr(links, "class", "futurelink")

# write the file
write_html(obj, file.path("index.html"), options = "format")
