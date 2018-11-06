# Update DESCRIPTION file and roxygenize

DESCRIPTION <- readLines("DESCRIPTION")
VERSION <- as.numeric(gsub("Version: ", "", DESCRIPTION[3])) + 0.01
DESCRIPTION[3] <- paste("Version:", sprintf("%.2f", VERSION))
DATE <- Sys.Date()
DESCRIPTION[4] <- paste("Date:", DATE)

writeLines(DESCRIPTION, "DESCRIPTION")

# Write lsd.version()
filename <- "R/zzz.R"
cat("#' Function to return version number\n", file = filename)
cat("#'\n", file = filename, append = TRUE)
cat("#' @param libname library name\n", file = filename, append = TRUE)
cat("#' @param pkgname package name\n", file = filename, append = TRUE)
cat("#'\n", file = filename, append = TRUE)
cat(".onAttach <- function(libname, pkgname)\n", file = filename, append = TRUE)
cat("{\n", file = filename, append = TRUE)
cat(paste("    packageStartupMessage(\"lsd version: ", VERSION, "\n", "Compile date: ", DATE, "\n\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)

roxygen2::roxygenize()
#devtools::document()
