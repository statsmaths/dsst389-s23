#############################################################################
# Settings and functions for working with the class notes and assignements
# Note that this file will be overwritten. Do not modify directly!
#
# Date: 30 December 2022

#############################################################################
# load a few required packages.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(dsst))

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
