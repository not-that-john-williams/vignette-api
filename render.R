# Script used to render "README.md"

rmarkdown::render("countrylayer-api.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = c(toc = TRUE, toc_depth = 3))