#directorio <- "~/BOOTCAMPS/18112019/clasificarContactos/"
#C:\Users\34653\Documents\RProject\RProject

directorio <- "~/RProject/RProject/"
setwd(directorio)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)




debug(funcion_general)
funcion_general(path = directorio)
undebug(funcion_general)
