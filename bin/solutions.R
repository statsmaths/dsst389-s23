library(tidyverse)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)

if (args[1] == "ALL") {
  files <- dir("src")
  files <- files[stri_sub(files, 1, 8) == "notebook"]
  files <- files[stri_sub(files, -14, -1) == "_solutions.Rmd"]
  files <- as.numeric(stri_sub(files, 9, 10))
  args <- files
}

for (ar in args)
{
  filename <- sprintf("notebook%02d_solutions.Rmd", as.numeric(ar))
  fout <- sprintf("notebook%02d_solutions.html", as.numeric(ar))

  x <- read_lines(file.path("src", filename))
  qstart <- which(stri_detect(x, regex = 'question-[0-9]+'))
  qnums <- stri_sub(stri_extract(x[qstart], regex = 'question-[0-9]+'), 10L, 11L)
  x[qstart] <- sprintf("%s\n# Question %s", x[qstart], qnums)
  x <- stri_replace_all(x, "options(dsst.traceit = FALSE)", fixed = 'source(\"../funs/funs_custom.R\")')
  write_lines(x, 'src/temp.Rmd')

  system("R -e \"rmarkdown::render('src/temp.Rmd')\"")

  file.rename(
    file.path("src", "temp.html"),
    file.path("ans", fout)
  )
  file.remove("src/temp.Rmd")  
}
