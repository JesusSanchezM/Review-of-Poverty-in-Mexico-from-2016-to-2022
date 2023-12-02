
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
