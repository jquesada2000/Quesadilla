
library(roxygen2)
#
#
# # crear el archivo de documentacion
devtools::document()
#
# # compilamos el paquete
devtools::build_vignettes()

# instalamos el paquete creado en .zip
install.packages("./Quesadilla_0.1.0.tar.gz", repos = NULL, type = "source")

library(Quesadilla)
help("test.cat")


# instalar en Github

devtools::install_github("jquesada2000/Quesadilla")


library(Quesadilla)
