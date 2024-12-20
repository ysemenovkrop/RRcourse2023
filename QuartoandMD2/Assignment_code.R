# Quarto #2
# YAML, rendering and parameters
# Reproducible Research 2023
# Wojciech Hardy

library(quarto)

setwd("/Users/yuriisemenov/Documents/GitHub/RRcourse2023/QuartoandMD2")

getwd()

# Converting from Rmd to Qmd

## Step 1) 
knitr::convert_chunk_header(input = "Assignment.Rmd", 
                            output = "Assignment.qmd")

## Step 2)
readLines("QMD_class_1_cut.qmd")[1:5]

readLines("QMD_class_1_cut.qmd") %>%
  stringr::str_replace(
    pattern = "output: html_document", 
    replace = "format: html") %>%
  writeLines(con = "QMD_class_1_cut.qmd")

readLines("QMD_class_1_cut.qmd")[1:5]

# Launching a preview mode
sys::exec_wait("quarto preview QMD_class_2.qmd")

# To create a PDF file you need a TeX installation:
sys::exec_wait("quarto install tinytex")

# Rendering
library(quarto)

quarto_render("Assignment.qmd", output_format = "html")
quarto_render("Assignment.qmd", output_format = "pdf")


