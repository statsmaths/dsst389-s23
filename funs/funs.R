#############################################################################
# Settings and functions for working with the class notes and assignements
# Note that this file will be overwritten. Do not modify directly!
#
# Date: 07 August 2022

#############################################################################
# load a few required packages; others will be referenced with :: and :::
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(hms))
suppressPackageStartupMessages(library(RcppRoll))
suppressPackageStartupMessages(library(broom))

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

#############################################################################
# spatial functions
sm_centroid <- function(data) {
  suppressWarnings({ z <- st_coordinates(st_centroid(data)) })
  return(tibble(lon = z[,1], lat = z[,2]))
}
spatial_join <- function(...) {
  return(st_as_sf(as_tibble(st_join(...))))
}

#############################################################################
# change default parameter in the arrange function
arrange <- function(.data, ...) { dplyr::arrange(.data, ..., .by_group = TRUE) }

#############################################################################
# custom theme; mimics Tufte theme
theme_tufte <- function()
{
    ret <- ggplot2::theme_bw(base_family = "sans", base_size = 11) +
        ggplot2::theme(
          legend.background = ggplot2::element_blank(),
          legend.key        = ggplot2::element_blank(),
          panel.background  = ggplot2::element_blank(),
          panel.border      = ggplot2::element_blank(),
          strip.background  = ggplot2::element_blank(),
          plot.background   = ggplot2::element_blank(),
          axis.line         = ggplot2::element_blank(),
          panel.grid        = ggplot2::element_blank(),
          axis.ticks        = ggplot2::element_blank()
        )
    ret
}
