#' Checks available materials from GitHub and downloads those that do not
#' currently exist locally.
.update_materials <- function()
{
  base <- "https://raw.githubusercontent.com/statsmaths/dsst389-s23/main"

  # what files are available to download?
  today <- as.character(Sys.Date())
  avail <- read.csv(
    file.path(base, "extra", "manifest.csv"),
    as.is = TRUE,
    colClasses = "character"
  )
  avail <- avail[nchar(avail$date) == 10L,]
  avail <- avail[avail$date <= today,]

  # download files that do not exist
  downloaded <- c()
  for (f in avail$path)
  {
    if (!file.exists(file.path("..", f)))
    {
      url <- file.path(base, f)
      download.file(url, file.path("..", f), quiet = TRUE)
      downloaded <- c(downloaded, f)
    }
  }

  # funs.R is special; make sure that it has not changed; if it has, then
  # replace it!
  f <- file.path("funs", "funs.R")
  local_funs <- readLines(file.path("..", f))
  remote_funs <- readLines(file.path(base, f))
  if (!identical(local_funs, remote_funs))
  {
    url <- file.path(base, f)
    download.file(url, file.path("..", f), quiet = TRUE)
    downloaded <- c(downloaded, f)
  }

  dt <- as.character(as.POSIXlt(Sys.time(), format = "%Y-%m-%dT%H:%M:%S"))
  if (length(downloaded))
  {
    message(sprintf("[%s] Downloaded file %s\n", dt, downloaded))
  } else {
    message(sprintf("[%s] Nothing new to download.", dt))
  }
}

.update_materials()
