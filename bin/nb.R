library(tidyverse)
library(stringi)

src_files <- dir('src', pattern = 'Rmd$')
src_files <- setdiff(src_files, "setup.Rmd")

for (j in seq_along(src_files))
{
  # read the file
  x <- read_lines(file.path("src", src_files[j]))

  # replace the answers flags and the name in the header
  x[stri_sub(x, 1L, 11L) == "**Answer**:"] <- "**Answer**:"
  x[stri_sub(x, 1L, 11L) == "**Answer:**"] <- "**Answer**:"
  x <- stri_replace_all(x, "", fixed = " -- Solutions")

  # find the start and end of the questions that need to be cleared
  qstart <- which(stri_detect(x, regex = 'question-[0-9]+'))
  qnums <- stri_sub(stri_extract(x[qstart], regex = 'question-[0-9]+'), 10L, 11L)
  qend <- which(x == "```")
  qend <- map_int(qstart, ~ qend[which(..1 < qend)[1]])
  index <- flatten_int(map2(qstart, qend, ~ seq(..1 + 1L, ..2 - 1L)))
  x[index] <- ""

  # reformat
  x <- stri_paste(x, collapse = "\n")
  x <- stri_replace_all(x, "\n\n", regex = "\n[\n]+")

  # write the output
  fout <- stri_replace_all(src_files[j], "", fixed = "_solutions")
  write_lines(x, file.path("nb", fout))
}
