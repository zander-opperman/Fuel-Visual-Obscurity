# cite_packages.R
# List the packages your app uses, run the script, and get a text file
# containing a citation for each one.

# ---- Edit these ------------------------------------------------------------
pkgs        <- c("shiny", "magick", "colordistance", "countcolors",
                 "tidyverse", "shinyjs", "janitor", "bslib","readr",
                 "shinyWidgets","bsicons", "thematic")   # packages to cite
include_r   <- TRUE                        # also cite base R?
output_file <- "citations.txt"             # where to save the result
# ----------------------------------------------------------------------------

# citation() marks titles with _underscores_ (italics). Replace each
# matched pair with double quotes instead.
fix_quotes <- function(x) {
  gsub("_([^_]+)_", "\"\\1\"", x)
}

cite_one <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(paste0("[", pkg, "] NOT INSTALLED - no citation available."))
  }
  
  version <- as.character(utils::packageVersion(pkg))
  cit     <- fix_quotes(format(utils::citation(pkg), style = "text"))
  
  paste0(
    paste(cit, collapse = "\n\n"),
    "\n(Package: ", pkg, ", version ", version, ")"
  )
}

pkgs <- sort(unique(pkgs))

entries <- character()

if (include_r) {
  entries <- c(
    entries,
    paste0(
      paste(fix_quotes(format(utils::citation(), style = "text")), collapse = "\n\n"),
      "\n(", R.version.string, ")"
    )
  )
}

entries <- c(entries, vapply(pkgs, cite_one, character(1), USE.NAMES = FALSE))

header <- c(
  "Citations",
  paste("Generated:", format(Sys.Date())),
  strrep("=", 60),
  ""
)

writeLines(
  c(header, paste(entries, collapse = "\n\n------------------------------\n\n")),
  output_file
)

message("Wrote ", length(entries), " citations to ", normalizePath(output_file))