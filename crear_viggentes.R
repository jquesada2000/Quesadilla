


library(markdown)
library(rmarkdown)
library(knitr)
library(pkgdown)
library(devtools)
#
#
# knit('libro.Rmd', 'test.md') # creates md file
#
#
# md_file <- normalizePath("test.md")
# html_file <- normalizePath("test.html")
# markdownToHTML(md_file, html_file)
#
#
#
# markdownToHTML('test.md', 'test.html') # creates html file
#
# #renderMarkdown('test.md')
#
#
# #knit2html('CodigoLibro.md')
#


# construye web
pkgdown::build_site()


rmarkdown::render("libro.Rmd", output_format = "html_document")


browseURL(paste('file://', file.path(getwd(),'Libro.html'), sep='')) # open file in browser



rmarkdown::render("prueba_vignette1.Rmd", output_format = "html_document")
browseURL(paste('file://', file.path(getwd(),'prueba_vignette1.html'), sep='')) # open file in browser

usethis::use_vignette("prueba_vignette1")



devtools::build_vignettes()
devtools::check() # Verifica que no haya errores



















