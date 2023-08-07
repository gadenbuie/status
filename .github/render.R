if (requireNamespace("here", quietly = TRUE)) {
  setwd(here::here())
}

# render readme
rmarkdown::render("README.Rmd")

# render status index.html
rmarkdown::render(
  "README.Rmd",
  output_file = "index.html",
  output_format = cleanrmd::html_document_clean(theme = "stylize"),
  params = list(set_main_width = TRUE)
)
