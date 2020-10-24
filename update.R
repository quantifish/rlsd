# Update DESCRIPTION file and roxygenize

DESCRIPTION <- readLines("DESCRIPTION")
VERSION <- as.numeric(gsub("Version: ", "", DESCRIPTION[3])) + 0.01
DESCRIPTION[3] <- paste("Version:", sprintf("%.2f", VERSION))
DATE <- Sys.Date()
DESCRIPTION[4] <- paste("Date:", DATE)

writeLines(DESCRIPTION, "DESCRIPTION")

roxygen2::roxygenize()
#devtools::document()
