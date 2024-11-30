


library(markdown)
library(rmarkdown)
library(knitr)
library(pkgdown)
library(devtools)
library(usethis)
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

usethis::use_vignette("libro")




devtools::check() # Verifica que no haya errores


devtools::install()


# ------------------

# crea la carpeta vignettes y el archivo libro.Rmd
usethis::use_vignette("libro2")

# crea la vignette
devtools::build_vignettes()

remove.packages("Quesadilla")
devtools::install_github("jquesada2000/Quesadilla", build_vignettes = TRUE,force = TRUE)


library(Quesadilla)
help("test.cat")

browseVignettes("Quesadilla")


pkgbuild::check_build_tools(debug = TRUE)

devtools::check() # Verifica que no haya errores


