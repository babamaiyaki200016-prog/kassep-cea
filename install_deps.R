## Minimal dependencies. Run once.
pkgs <- c("ggplot2", "scales", "jsonlite")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Dependencies ready.\n")
