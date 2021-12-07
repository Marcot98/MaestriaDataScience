# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)

# Target date
targetDate <- "12-12-2021"
getwd()


#dataFn <- file.path(getwd(), "Feature engineering", targetDate,
#                     "data.rds")
#dataFn <- "D:Users/Tulio/Documentos/Estudios/2021/Product Developer/Proyecto 1/data.rds"
dataFn <- "data.rds"
data <- readRDS(dataFn)


products <- grep("_ult1$", names(data), value = TRUE)
productsSimple <- gsub("^ind_|_ult1$", "", products)


orderedDateFormat <- data$monthFormat[match(sort(unique(data$fecha_dato)),
                                            data$fecha_dato)]


data[, newProdSimple := factor(newProdSimple, levels = productsSimple)]


data[, logRenta := log(renta)]


contVars <- c("age", "antiguedad", "renta", "logRenta")
contVarRanges <- matrix(NA, nrow = length(contVars), ncol = 2, dimnames = 
                   list(contVars, c("min", "max")))
contVarRanges[1, ] <- c(0, 100)
contVarRanges[2, ] <- c(0, 260)
contVarRanges[3, ] <- c(0, 1e6)
contVarRanges[4, ] <- c(8, 16)


catVars <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo",
             "indrel", "indrel_1mes", "tiprel_1mes", "indresi", "indext",
             "conyuemp", "canal_entrada", "indfall", "tipodom", "nomprov",
             "ind_actividad_cliente", "segmento")


aboutString <- "This app was developed by Tom Van de Wiele and relates to the exploratory analysis of the 'Santander product recommendation' competition <br/><br/>"
