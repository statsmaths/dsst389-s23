To clear all packages locally to test, use the following code within R:

```{r}
ip <- as.data.frame(installed.packages())
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
path.lib <- unique(ip$LibPath)
pkgs.to.remove <- ip[,1]
sapply(pkgs.to.remove, remove.packages, lib = path.lib)
```

To build the materials zip file, do the following in a terminal:

```{sh}
rm -rf materials
mkdir materials
mkdir materials/data
mkdir materials/funs
mkdir materials/nb

cp nb/setup.Rmd materials/nb
cp nb/notebook00.Rmd materials/nb
cp funs/funs.R materials/funs
cp funs/update.R materials/funs

rm -f materials.zip
zip -r materials.zip materials
rm -rf materials
mv materials.zip extra
```

To make the dev.html file, you can use the following bit of R code:

```{r}
library(tidyverse); library(lubridate); library(stringi)

start_date <- ymd("2023-01-09")
no_number <- ymd(c("2023-01-09", "2023-01-16", "2023-03-06", "2023-03-08"))
no_number <- c(no_number,
  ymd(c("2023-01-30", "2023-02-01",
        "2023-02-20", "2023-02-22",
        "2023-03-27", "2023-03-29",
        "2023-04-17", "2023-04-19"))
)
all_dates <- sort(c(start_date + seq(0, 14) * 7, start_date + seq(0, 14) * 7 + 2))

nums <- rep(1L, length(all_dates))
nums[all_dates %in% no_number] <- 0
nums <- cumsum(nums)
nums <- sprintf("%02d", nums)
nums[all_dates %in% no_number] <- ""

dow <- paste0(wday(all_dates, label = TRUE), ".")
top_row <- sprintf('<td rowspan="2"><b>%s</b></td>\n  ', all_dates)
top_row[dow != "Mon."] <- ""
all_dates <- as.character(all_dates)

rows <- 
'<tr>
  TROW<td>DOW</td>
  <td><a class="DATE reading" href="notes/notesNUM.html">NUM.</a></td>
  <td><a class="DATE slides" href="pslides/NUM_.pdf">[]</a></td>
  <td><a class="DATE solutions" href="ans/notebookNUM_solutions.html">[notebookNUM]</a></td>
</tr>'
rows_no <-
'<tr>
  TROW<td>DOW</td>
  <td></td>
  <td></td>
  <td></td>
</tr>'

rows <- rep(rows, length(all_dates))
rows[all_dates %in% as.character(no_number)] <- rows_no
rows <- stri_replace_all(rows, top_row, fixed = "TROW")
rows <- stri_replace_all(rows, dow, fixed = "DOW")
rows <- stri_replace_all(rows, all_dates, fixed = "DATE")
rows <- stri_replace_all(rows, nums, fixed = "NUM")

clipr::write_clip(rows)

# for the manifest file
manifest <- sprintf('"nb/notebook%s.Rmd","%s"', nums, all_dates)
manifest <- manifest[!(all_dates %in% as.character(no_number))]
clipr::write_clip(manifest)
```
