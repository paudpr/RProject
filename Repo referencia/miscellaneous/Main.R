directorio <- "~/BOOTCAMPS/18112019/clasificarContactos/"

setwd(directorio)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)


debug(clasificarContactosApp)
clasificarContactosApp(directorio)
undebug(clasificarContactosApp)