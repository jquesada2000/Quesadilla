


library(markdown)
library(rmarkdown)
library(knitr)
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





rmarkdown::render("libro.Rmd", output_format = "html_document")


browseURL(paste('file://', file.path(getwd(),'Libro.html'), sep='')) # open file in browser
