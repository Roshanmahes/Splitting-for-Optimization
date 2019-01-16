# This file specifies which packages are required in order to run the
# programs successfully.
# To install them, paste 'source("requirements.R")' into your console.

packages <- c("crosstalk","docstring","DT","shiny","tictoc","rstudioapi")

for (package in packages) {
  if (is.na(match(package,installed.packages()[,1]))) {
    install.packages(package)
  }
}

rm(package,packages)
print("Installed all packages successfully!")