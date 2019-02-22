#---- SCRIPT para cargar todos los datos iniciales

#---- Package
install.packages("readxl")
library(readxl)

# ---- Carga de datos
D_OPC <- read_excel("./Datos/D_OPC.xlsx")
D_PARCELAS <- read_excel("./Datos/D_PARCELAS.xlsx")
datos_climaticos_oriental_I <- read_excel("./Datos/datos_climaticos_oriental_I.xlsx")
F_MUESTREOS <- read_excel("./Datos/F_MUESTREOS.xlsx")
T_MUESTREOS <- read_excel("./Datos/T_MUESTREOS.xlsx")


# ----- Guardamos los datos
save.image("./Datos/Datos.RData")
