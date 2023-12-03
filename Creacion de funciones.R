
# Funcion para cargar los url de INEGI

load_inegi <- function(url, file) {

##Creación de directorio temporal
td<- tempdir()
# Descarga del archivo temporal
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)
# unzip
unzip(tf, files=file, exdir=td, 
      overwrite=TRUE)
fpath=file.path(td, file)
unlink(td)

#leer el archivo
read.csv(fpath)
}

#prueba <- load_inegi("https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_viviendas_csv.zip",
#           "viviendas.csv")


#Funcion para concatenar total del ingreso con algun ingreso en especifico

concatenacion <- function (base_de_datos, codigo) {
  
  Datos_por_hogares <- base_de_datos %>%
    group_by(`ï..folioviv`, foliohog) %>%
    summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))
  
  Datos_gasto_codigo <- base_de_datos %>%  filter(clave==codigo) %>%
    group_by(`ï..folioviv`, foliohog) %>%
    summarize(total_ing_tri = sum(ing_tri, na.rm = TRUE))
  
  result <- left_join(Datos_por_hogares, Datos_gasto_codigo, 
                      by = c("ï..folioviv", "foliohog"), 
                      suffix = c("_total", paste("_", codigo)))
  
}

#prueba <- concatenacion(enigh_2022_ingresos, "P104")
