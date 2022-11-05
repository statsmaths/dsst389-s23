library(tidyverse)
library(stringi)


args <- commandArgs(trailingOnly = TRUE)
filename <- sprintf("notebook%02d_solutions.Rmd", as.numeric(args))
fout <- sprintf("notebook%02d_solutions.html", as.numeric(args))

x <- read_lines(file.path("src", filename))
qstart <- which(stri_detect(x, regex = 'question-[0-9]+'))
qnums <- stri_sub(stri_extract(x[qstart], regex = 'question-[0-9]+'), 10L, 11L)
x[qstart] <- sprintf("%s\n# Question %s", x[qstart], qnums)
write_lines(x, 'src/temp.Rmd')

system("R -e \"rmarkdown::render('src/temp.Rmd')\"")

file.rename(
  file.path("src", "temp.html"),
  file.path("ans", fout)
)
file.remove("src/temp.Rmd")