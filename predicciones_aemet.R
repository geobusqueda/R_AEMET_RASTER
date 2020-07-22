# GEOBUSQUEDA
#
# OBJETIVO:
#
# ESTE SCRIPT OBTIENE LAS PREDICCIONES POR MUNICIPIOS EN XML DE LA WEB DE LA AEMET,
# ALMACENA EN UN DATAFRAME LOS DATOS DE TEMPERATURA Y HUMEDAD PARA EL DIA SIGUIENTE,
# Y OBTIENE LAS CAPAS DE INFORMACION RASTER CORRESPONDIENTES A LAS 12:00
#
# INICIO DEL SCRIPT 

# HORA DE INICIO
paste('Inicio del script predicciones_aemet.R: ',Sys.time()) # MENSAJE

# TAREAS PREVIAS ----

# FICHERO LOG, DIRECTORIO DE TRABAJO, PAQUETES, LIBERAR GLOBAL_ENVIRONMENT
# PARA QUE DEVUELVA INFORMACION DE COMANDOS Y SUS SALIDAS
options(echo=TRUE)
# SE CREA FICHERO LOG EN EL QUE METER LOS MENSAJES
sink("H:/personal/geobusqueda/r_raster_aemet/r_raster_aemet.log", append=FALSE)

# DIRECTORIO DE TRABAJO
setwd("H:/personal/geobusqueda/r_raster_aemet")

# LIBERAR ESPACIO DE TRABAJO
rm(list = ls())

# VERIFICAMOS QUE ESTAN PRESENTES LOS PAQUETES NECESARIOS
paste('Verificando paquetes necesarios: ',Sys.time()) # MENSAJE
list.of.packages <- c("XML", "stringr","readr","plyr","rgdal","tmap","sf","raster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# CARGAMOS LOS PAQUETES
paste('Carga de paquetes necesarios: ',Sys.time()) # MENSAJE
lapply(list.of.packages, library, character.only = TRUE)

# OBTENER FICHEROS XML ----

# LECTURA DEL FICHERO CSV EN EL QUE ESTAN LAS URL DE LOS FICHEROS CON LAS PREDICCIONES
# (FICHERO GENERADO PREVIAMENTE)
codmun <- read_csv("AUXILIAR/codmun.csv")

# BORRAR SI HAY ALGO PRE-EXISTENTE EN LA CARPETA XML
list.of.files <- list.files('XML/', ".xml$", full.names = TRUE)
file.remove(list.of.files)

# DESCARGA DE LOS XML
paste('Comienza la descarga de ficheros xml: ',Sys.time()) # MENSAJE
urls <- codmun$url
destinations <- codmun$destino

for(i in seq_along(urls)){
  download.file(urls[i], destinations[i], mode="wb")
}
paste('Finaliza la descarga de ficheros xml: ',Sys.time()) # MENSAJE

# PARSEAR LOS XML EN UN DATAFRAME ----
# COGEMOS LOS DATOS DE MAÑANA SYSDATE+1
setwd("XML")
files <- list.files() 
parse_xml <-function(FileName) {
  doc <- xmlParse(FileName) 
  data <- xmlRoot(doc)
  localidad <- as.character(xmlToDataFrame(nodes = getNodeSet(doc, "//nombre"), stringsAsFactors = FALSE))
  provincia <- as.character(xmlToDataFrame(nodes = getNodeSet(doc, "//provincia"), stringsAsFactors = FALSE))
  enlace <- as.character(xmlToDataFrame(nodes = getNodeSet(doc, "//origen//enlace"), stringsAsFactors = FALSE))
  codine <- str_sub(enlace,-5,-1)
  tempmax <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/maxima"), stringsAsFactors = FALSE))
  tempmin <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/minima"), stringsAsFactors = FALSE))
  temp06 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/dato[1]"), stringsAsFactors = FALSE))
  temp12 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/dato[2]"), stringsAsFactors = FALSE))
  temp18 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/dato[3]"), stringsAsFactors = FALSE))
  temp24 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/temperatura/dato[4]"), stringsAsFactors = FALSE))
  hummax <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/maxima"), stringsAsFactors = FALSE))
  hummin <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/minima"), stringsAsFactors = FALSE))
  hum06 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/dato[1]"), stringsAsFactors = FALSE))
  hum12 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/dato[2]"), stringsAsFactors = FALSE))
  hum18 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/dato[3]"), stringsAsFactors = FALSE))
  hum24 <- as.numeric(xmlToDataFrame(nodes = getNodeSet(doc, "//prediccion/dia[2]/humedad_relativa/dato[4]"), stringsAsFactors = FALSE))
  
  df <- cbind(localidad,provincia,enlace,codine,tempmax,tempmin,temp06,temp12,temp18,temp24,hummax,hummin,hum06,hum12,hum18,hum24)
} 
prediccion <- ldply(files,parse_xml)

# CONVERSION DE FACTOR A NUMERICO PARA LAS VARIABLES DE INTERES
prediccion$temp12 <- as.numeric(as.character(prediccion$temp12))
prediccion$hum12 <- as.numeric(as.character(prediccion$hum12))
prediccion$temp06 <- as.numeric(as.character(prediccion$temp06))
prediccion$hum06 <- as.numeric(as.character(prediccion$hum06))
paste('Obtenido dataframe de prediciones: ',Sys.time()) # MENSAJE

# VUELTA AL WD
setwd("H:/personal/geobusqueda/r_raster_aemet")

# RELACIONAR DATOS DE LOS XML CON UBICACIONES DE LOS MUNICIPIOS ----

# CARGAR INFORMACION DE VARIABLES FISIOGRAFICAS POR CODIGO INE (FICHERO GENERADO PREVIAMENTE)
mun_fisio <- mun_fisio <- read_delim("AUXILIAR/mun_fisio.csv", ";", 
                                     escape_double = FALSE, col_types = cols(O = col_integer(), 
                                                                             Z = col_integer()), locale = locale(decimal_mark = ",", 
                                                                                                                      grouping_mark = "."), trim_ws = TRUE)

# RELACIONAR CODIGO INE CON INFORMACION FISIOGRAFICA
prediccion_fisio <- merge(x = mun_fisio, y = prediccion, by.x = "COD_INE", by.y = "codine", all = FALSE)

# CARGAR CAPAS RASTER SOBRE LA QUE SE APLICARA EL MODELO ----

# LIMITE DE ANDALUCIA
contorno <- readOGR(dsn = "AUXILIAR", layer = "buffer_contorno_etrs89")
contorno <- spTransform(contorno, CRS("+init=epsg:23030"))

# CREACION DE RASTER VACIO QUE SIRVA DE REFERENCIA A PARTIR DEL CONTORNO ANTERIOR
grd              <- as.data.frame(spsample(contorno, "regular", n=50000, cellsize = 500))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(contorno) # SRS ASOCIADO
grdr <- raster(grd)

# IMPORTAR EN R CAPAS RASTER DE LAS VARIABLES FISIOGRAFICAS A 500 METROS
# RESAMPLEADAS A 500 METROS Y CARGADAS EN INFGEO
x500 <- raster("AUXILIAR/x500.tif")
y500 <- raster("AUXILIAR/y500.tif")
z500 <- raster("AUXILIAR/z500.tif")
d500 <- raster("AUXILIAR/d500.tif")
o500 <- raster("AUXILIAR/o500.tif")

# ASIGNANDO ORIGEN Y EXTENSION COMUN A TODAS LAS CAPAS
x500 <- projectRaster(x500,grdr,method = 'ngb')
y500 <- projectRaster(y500,grdr,method = 'ngb')
z500 <- projectRaster(z500,grdr,method = 'ngb')
d500 <- projectRaster(d500,grdr,method = 'ngb')
o500 <- projectRaster(o500,grdr,method = 'ngb')

# OBTENER MODELO DE REGRESION ----
d0_temp12_lm <- lm(prediccion_fisio$temp12 ~ X+Y+Z+D+O, data=prediccion_fisio)
d0_hum12_lm <- lm(prediccion_fisio$hum12 ~ X+Y+Z+D+O, data=prediccion_fisio)
d0_temp06_lm <- lm(prediccion_fisio$temp06 ~ X+Y+Z+D+O, data=prediccion_fisio)
d0_hum06_lm <- lm(prediccion_fisio$hum06 ~ X+Y+Z+D+O, data=prediccion_fisio)

# APLICAR MODELO DE REGRESION
d0_temp12_rm <- (x500 * d0_temp12_lm$coefficients[2]) + (y500 * d0_temp12_lm$coefficients[3]) + (z500 * d0_temp12_lm$coefficients[4]) + (d500 * d0_temp12_lm$coefficients[5]) + (o500 * d0_temp12_lm$coefficients[6]) + (d0_temp12_lm$coefficients[1])
d0_hum12_rm <- (x500 * d0_hum12_lm$coefficients[2]) + (y500 * d0_hum12_lm$coefficients[3]) + (z500 * d0_hum12_lm$coefficients[4]) + (d500 * d0_hum12_lm$coefficients[5]) + (o500 * d0_hum12_lm$coefficients[6]) + (d0_hum12_lm$coefficients[1])
d0_temp06_rm <- (x500 * d0_temp06_lm$coefficients[2]) + (y500 * d0_temp06_lm$coefficients[3]) + (z500 * d0_temp06_lm$coefficients[4]) + (d500 * d0_temp06_lm$coefficients[5]) + (o500 * d0_temp06_lm$coefficients[6]) + (d0_temp06_lm$coefficients[1])
d0_hum06_rm <- (x500 * d0_hum06_lm$coefficients[2]) + (y500 * d0_hum06_lm$coefficients[3]) + (z500 * d0_hum06_lm$coefficients[4]) + (d500 * d0_hum06_lm$coefficients[5]) + (o500 * d0_hum06_lm$coefficients[6]) + (d0_hum06_lm$coefficients[1])

# INTERPOLAR RESIDUOS ----
d0_temp12_lmr <- as.data.frame(d0_temp12_lm$residuals)
colnames(d0_temp12_lmr) <- c("residuos")

d0_hum12_lmr <- as.data.frame(d0_hum12_lm$residuals)
colnames(d0_hum12_lmr) <- c("residuos")

d0_temp06_lmr <- as.data.frame(d0_temp06_lm$residuals)
colnames(d0_temp06_lmr) <- c("residuos")

d0_hum06_lmr <- as.data.frame(d0_hum06_lm$residuals)
colnames(d0_hum06_lmr) <- c("residuos")

# UNIR RESIDUOS Y DATOS
d0_temp12_vfr <-cbind(prediccion_fisio, d0_temp12_lmr)
d0_hum12_vfr <-cbind(prediccion_fisio, d0_hum12_lmr)

d0_temp06_vfr <-cbind(prediccion_fisio, d0_temp06_lmr)
d0_hum06_vfr <-cbind(prediccion_fisio, d0_hum06_lmr)

# CREAR CAPA DE RESIUDOS
d0_temp12_vfr_sf <- SpatialPointsDataFrame(coords = d0_temp12_vfr[,c("LONGITUD","LATITUD")], data = d0_temp12_vfr,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
d0_hum12_vfr_sf <- SpatialPointsDataFrame(coords = d0_hum12_vfr[,c("LONGITUD","LATITUD")], data = d0_hum12_vfr,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

d0_temp06_vfr_sf <- SpatialPointsDataFrame(coords = d0_temp06_vfr[,c("LONGITUD","LATITUD")], data = d0_temp06_vfr,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
d0_hum06_vfr_sf <- SpatialPointsDataFrame(coords = d0_hum06_vfr[,c("LONGITUD","LATITUD")], data = d0_hum06_vfr,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# TRANSFORMACION DE SRS
d0_temp12_vfr_sf <- spTransform(d0_temp12_vfr_sf, CRS("+init=epsg:23030"))
d0_hum12_vfr_sf <- spTransform(d0_hum12_vfr_sf, CRS("+init=epsg:23030"))

d0_temp06_vfr_sf <- spTransform(d0_temp06_vfr_sf, CRS("+init=epsg:23030"))
d0_hum06_vfr_sf <- spTransform(d0_hum06_vfr_sf, CRS("+init=epsg:23030"))

# REAJUSTE DEL EXTENT PARA QUE PREDIGA MAS ALLA DE DONDE HAY PUNTOS
d0_temp12_vfr_sf@bbox <- contorno@bbox
d0_hum12_vfr_sf@bbox <- contorno@bbox

d0_temp06_vfr_sf@bbox <- contorno@bbox
d0_hum06_vfr_sf@bbox <- contorno@bbox

# IDW, INCLUYENDO CONVERSION A RASTER Y MASCARA
d0_temp12_vfr_sf_idw <- mask(raster(gstat::idw(residuos ~ 1, d0_temp12_vfr_sf, newdata=grd, nmax = 6, idp=3.0)),contorno)
d0_hum12_vfr_sf_idw <- mask(raster(gstat::idw(residuos ~ 1, d0_hum12_vfr_sf, newdata=grd, nmax = 6, idp=3.0)),contorno)
d0_temp06_vfr_sf_idw <- mask(raster(gstat::idw(residuos ~ 1, d0_temp06_vfr_sf, newdata=grd, nmax = 6, idp=3.0)),contorno)
d0_hum06_vfr_sf_idw <- mask(raster(gstat::idw(residuos ~ 1, d0_hum06_vfr_sf, newdata=grd, nmax = 6, idp=3.0)),contorno)

# OBTENER MODELO CORREGIDO ----
# SE SUMA RESULTADO DE LA REGRESION LINEAL Y LOS RESIDUOS
d0_temp12_rm_idw <- d0_temp12_rm + d0_temp12_vfr_sf_idw
d0_hum12_rm_idw <- d0_hum12_rm + d0_hum12_vfr_sf_idw
d0_temp06_rm_idw <- d0_temp06_rm + d0_temp06_vfr_sf_idw
d0_hum06_rm_idw <- d0_hum06_rm + d0_hum06_vfr_sf_idw

# RECLASIFICAR VALORES DE HUMEDAD POR ENCIMA Y POR DEBAJO DE CERO
d0_hum12_rm_idw[d0_hum12_rm_idw>100] <- 100
d0_hum12_rm_idw[d0_hum12_rm_idw<0] <- 0
d0_hum06_rm_idw[d0_hum06_rm_idw>100] <- 100
d0_hum06_rm_idw[d0_hum06_rm_idw<0] <- 0

# REDONDEAR A UN DECIMAL PARA DISMINUIR ESPACIO EN DISCO
d0_temp12_rm_idw <- (as.integer(d0_temp12_rm_idw * 10)/10)
d0_hum12_rm_idw <- (as.integer(d0_hum12_rm_idw * 10)/10)
d0_temp06_rm_idw <- (as.integer(d0_temp06_rm_idw * 10)/10)
d0_hum06_rm_idw <- (as.integer(d0_hum06_rm_idw * 10)/10)

# CONVERTIR EL OBJETO RASTER LAYER A CAPA EN FORMATO TIFF Y ALMACENAR CON FECHA ----
writeRaster(d0_temp12_rm_idw, filename="salidas/d0_temp12_rm_idw.tif", overwrite=TRUE)
writeRaster(d0_hum12_rm_idw, filename="salidas/d0_hum12_rm_idw.tif", overwrite=TRUE)
writeRaster(d0_temp06_rm_idw, filename="salidas/d0_temp06_rm_idw.tif", overwrite=TRUE)
writeRaster(d0_hum06_rm_idw, filename="salidas/d0_hum06_rm_idw.tif", overwrite=TRUE)

# CAMBIO NOMBRE SEGUN LA FECHA, COMO SON LOS DATOS DE MAÑANA
d0 <- format(Sys.Date()+1,"%d/%m/%Y")
file.rename(from = 'salidas/d0_temp12_rm_idw.tif', to = paste('salidas/t_', substring(d0,7,10), '_' , substring(d0,4,5), '_' , substring(d0,1,2) , '_12_p.tif' , sep = ''))
file.rename(from = 'salidas/d0_hum12_rm_idw.tif', to = paste('salidas/h_', substring(d0,7,10), '_' , substring(d0,4,5), '_' , substring(d0,1,2) , '_12_p.tif' , sep = ''))
file.rename(from = 'salidas/d0_temp06_rm_idw.tif', to = paste('salidas/t_', substring(d0,7,10), '_' , substring(d0,4,5), '_' , substring(d0,1,2) , '_06_p.tif' , sep = ''))
file.rename(from = 'salidas/d0_hum06_rm_idw.tif', to = paste('salidas/h_', substring(d0,7,10), '_' , substring(d0,4,5), '_' , substring(d0,1,2) , '_06_p.tif' , sep = ''))

# GARBAGE COLLECTION PARA LIBERAR MEMORIA
# https://stackoverflow.com/questions/11579765/how-to-clean-up-r-memory-without-the-need-to-restart-my-pc
memory.size(max=F)
gc()
memory.size(max=F)

# CREAR UN FICHERO ZIP CON LOS RASTERS DE PREDICCIONES----
file.remove("salidas/predicciones.zip")
setwd("salidas")
files2zip <- list.files('.')
zip(zipfile = 'predicciones', files = files2zip)

# MOVER CAPAS A ENVIADOS
list.of.files <- list.files('.', ".tif$", full.names = TRUE)
file.copy(list.of.files, 'enviados', overwrite = TRUE, copy.mode = TRUE)

# BORRAR CAPAS DE LA CARPETA ACTUAL
file.remove(list.of.files)

#LIBERAR ESPACIO DE TRABAJO
rm(list = ls())

# HORA DE FIN
paste('Fin del script: ',Sys.time())

# SE RESTAURA PARA QUE DEVUELVA LOS LOG EN PANTALLA
sink()

# FIN DEL SCRIPT