## KASSEP cost-effectiveness analysis -- run everything
## Usage:  Rscript run_all.R
source(file.path("R", "01_model.R"))
source(file.path("R", "02_figures.R"))
cat("\nDone. Results in output/, figures in figures/.\n")
