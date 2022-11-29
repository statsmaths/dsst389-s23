#############################################################################
# Settings and functions for working with the class notes and assignements
# Note that this file will be overwritten. Do not modify directly!
#
# Date: 24 November 2022

#############################################################################
# load a few required packages; others will be referenced with :: and :::
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(dsst))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(lubridate))

#############################################################################
# some standard settings
theme_set(theme_minimal())
if (.Platform$OS.type == "unix")
{
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
} else {
  Sys.setlocale("LC_ALL", "English")
}
Sys.setenv(LANG = "en")
options(width = 76L)
options(pillar.min_character_chars = 15)
options(dplyr.summarise.inform = FALSE)
options(readr.show_col_types = FALSE)
options(ggrepel.max.overlaps = Inf)
options(sparse.colnames = TRUE)
options(lubridate.week.start = 1)

null_to_na <- function(x) { ifelse(is.null(x), NA, x) }
