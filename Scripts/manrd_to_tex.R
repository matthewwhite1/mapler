library(tools)
library(tidyverse)

# Get man files
man_files <- list.files("man/", full.names = TRUE)
man_files <- man_files[str_detect(man_files, "Rd")]
man_files_nopath <- str_remove(man_files, "man/") |>
  str_remove(".Rd")

# Convert all files to tex
for (i in seq_along(man_files)) {
  out_file <- paste0("doc_latex/", man_files_nopath[i], ".tex")
  Rd2latex(man_files[i], out = out_file)
}
