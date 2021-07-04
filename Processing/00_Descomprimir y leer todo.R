library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(haven)

#### Descomprimir archivos y guardar de manera ordenada en sistema de carpetas ####

unzip(zipfile = "Input/Entregado por DT/Autoaplicado Encla 2014- pública.zip", exdir = "Input/Descomprimido/Encla2014")
unzip(zipfile = "Input/Entregado por DT/Dirigentes Encla 2014- pública.zip", exdir = "Input/Descomprimido/Encla2014")
unzip(zipfile = "Input/Entregado por DT/Empleadores Encla 2014- pública.zip", exdir = "Input/Descomprimido/Encla2014")
unzip(zipfile = "Input/Entregado por DT/Trabajadores Encla 2014- pública.zip", exdir = "Input/Descomprimido/Encla2014")

#unzip(zipfile = "Input/Entregado por DT/Encla2011.zip", exdir = "Input/Descomprimido/Encla2011")
#untar("Input/Descomprimido/Encla2011/Bases Encla 2011.rar", exdir = "Input/Descomprimido/Encla2011")

#untar("Input/Entregado por DT/Encla 2008.rar", exdir = "Input/Descomprimido/Encla2008")
#untar("Input/Descomprimido/Encla2008/Autoaplicado ENCLA 2008 (Público).rar", exdir = "Input/Descomprimido/Encla2008")
#untar("Input/Descomprimido/Encla2008/Dirigentes Sindicales ENCLA 2008 (Público).rar", exdir = "Input/Descomprimido/Encla2008")
#untar("Input/Descomprimido/Encla2008/Trabajadores ENCLA 2008 (Público).rar", exdir = "Input/Descomprimido/Encla2008")
#untar("Input/Descomprimido/Encla2008/Empleadores ENCLA 2008 (Público).rar", exdir = "Input/Descomprimido/Encla2008")

untar("Input/Entregado por DT/Encla 2006.rar", exdir = "Input/Descomprimido/Encla2006")
untar("Input/Descomprimido/Encla2006/Base Autoaplicado ENCLA 2006 (Público).rar", exdir = "Input/Descomprimido/Encla2006")
untar("Input/Descomprimido/Encla2006/Dirigentes ENCLA 2006(público).rar", exdir = "Input/Descomprimido/Encla2006")
untar("Input/Descomprimido/Encla2006/Trabajadores ENCLA 2006(Público).rar", exdir = "Input/Descomprimido/Encla2006")
untar("Input/Descomprimido/Encla2006/Empleadores ENCLA 2006(Público).rar", exdir = "Input/Descomprimido/Encla2006")

untar("Input/Entregado por DT/Encla 2004.rar", exdir = "Input/Descomprimido/Encla2004")
untar("Input/Descomprimido/Encla2004/ENCLA 2004 AUTOAPLICADO EMPLEADORES  público.rar", exdir = "Input/Descomprimido/Encla2004")
untar("Input/Descomprimido/Encla2004/ENCLA 2004 DIRIGENTES SINDICALES público.rar", exdir = "Input/Descomprimido/Encla2004")
untar("Input/Descomprimido/Encla2004/ENCLA 2004 EMPLEADORES público.rar", exdir = "Input/Descomprimido/Encla2004")
untar("Input/Descomprimido/Encla2004/ENCLA 2004 TRABAJADORES público.rar", exdir = "Input/Descomprimido/Encla2004")

untar("Input/Entregado por DT/Encla 2002.rar", exdir = "Input/Descomprimido/Encla2002")
#untar("Input/Descomprimido/Encla2002/ENCLA 2002 autoaplicado público.rar", exdir = "Input/Descomprimido/Encla2002")
#untar("Input/Descomprimido/Encla2002/ENCLA 2002 Dirigentes sindicales público.rar", exdir = "Input/Descomprimido/Encla2002")
untar("Input/Descomprimido/Encla2002/ENCLA 2002 Empleadores público.rar", exdir = "Input/Descomprimido/Encla2002")
untar("Input/Descomprimido/Encla2002/ENCLA 2002Trabajadores público.rar", exdir = "Input/Descomprimido/Encla2002")

untar("Input/Entregado por DT/Encla 1999.rar", exdir = "Input/Descomprimido/Encla1999")
untar("Input/Descomprimido/Encla1999/ENCLA 1999 Autoaplicado y empleadores público.rar", exdir = "Input/Descomprimido/Encla1999")
untar("Input/Descomprimido/Encla1999/ENCLA 1999 trabajadores y dirigentes sindicales público.rar", exdir = "Input/Descomprimido/Encla1999")

untar("Input/Entregado por DT/Encla 1998.rar", exdir = "Input/Descomprimido/Encla1998")
untar("Input/Descomprimido/Encla1998/Empleadores ENCLA 1998 (pública).rar", exdir = "Input/Descomprimido/Encla1998")
untar("Input/Descomprimido/Encla1998/Trabajadores y Dirigentes Sindicales ENCLA 1998 (pública)-1.rar", exdir = "Input/Descomprimido/Encla1998")


#### Carga bases encla por año ####

## 1998
encla1998<-list.files("Input/Descomprimido/Encla1998")
encla1998
encla1998_e<-read_sav(paste0("Input/Descomprimido/Encla1998/",encla1998[3]))
encla1998_ts<-read_sav(paste0("Input/Descomprimido/Encla1998/",encla1998[9]))

## 1999
encla1999<-list.files("Input/Descomprimido/Encla1999")
encla1999
encla1999_e<-read_sav(paste0("Input/Descomprimido/Encla1999/",encla1999[2]))
encla1999_ts<-read_sav(paste0("Input/Descomprimido/Encla1999/",encla1999[4]))

## 2002
encla2002<-list.files("Input/Descomprimido/Encla2002")
encla2002

encla2002_e<-read_sav(paste0("Input/Descomprimido/Encla2002/",encla2002[6]))
encla2002_t<-read_sav(paste0("Input/Descomprimido/Encla2002/",encla2002[8]))
encla2002_s<-read_sav(paste0("Input/Descomprimido/Encla2002/",encla2002[4]))
encla2002_a<-read_sav(paste0("Input/Descomprimido/Encla2002/",encla2002[2]))


## 2004
encla2004<-list.files("Input/Descomprimido/Encla2004")
encla2004
encla2004_a<-read_sav(paste0("Input/Descomprimido/Encla2004/",encla2004[2]))
encla2004_e<-read_sav(paste0("Input/Descomprimido/Encla2004/",encla2004[6]))
encla2004_s<-read_sav(paste0("Input/Descomprimido/Encla2004/",encla2004[4]))
encla2004_t<-read_sav(paste0("Input/Descomprimido/Encla2004/",encla2004[8]))


## 2006
encla2006<-list.files("Input/Descomprimido/Encla2006")
encla2006
encla2006_e<-read_sav(paste0("Input/Descomprimido/Encla2006/",encla2006[6]))
encla2006_t<-read_sav(paste0("Input/Descomprimido/Encla2006/",encla2006[12]))
encla2006_a<-read_sav(paste0("Input/Descomprimido/Encla2006/",encla2006[2]))
encla2006_s<-read_sav(paste0("Input/Descomprimido/Encla2006/",encla2006[4]))


## 2008
encla2008<-list.files("Input/Descomprimido/Encla2008")
encla2008
encla2008_e<-read_sav(paste0("Input/Descomprimido/Encla2008/",encla2008[6]))
encla2008_t<-read_sav(paste0("Input/Descomprimido/Encla2008/",encla2008[12]))
encla2008_a<-read_sav(paste0("Input/Descomprimido/Encla2008/",encla2008[2]))
encla2008_s<-read_sav(paste0("Input/Descomprimido/Encla2008/",encla2008[4]))

## 2011
encla2011<-list.files("Input/Descomprimido/Encla2011")
encla2011
encla2011_e<-read_sav(paste0("Input/Descomprimido/Encla2011/",encla2011[4]))
encla2011_s<-read_sav(paste0("Input/Descomprimido/Encla2011/",encla2011[3]))
encla2011_t<-read_sav(paste0("Input/Descomprimido/Encla2011/",encla2011[5]))
encla2011_a<-read_sav(paste0("Input/Descomprimido/Encla2011/",encla2011[2]))


## 2014
encla2014<-list.files("Input/Descomprimido/Encla2014")
encla2014
encla2014_e<-read_sav(paste0("Input/Descomprimido/Encla2014/",encla2014[11]))
encla2014_a<-read_sav(paste0("Input/Descomprimido/Encla2014/",encla2014[9]))
encla2014_s<-read_sav(paste0("Input/Descomprimido/Encla2014/",encla2014[10]))
encla2014_t<-read_sav(paste0("Input/Descomprimido/Encla2014/",encla2014[12]))

## 2019
## Archivos descargados de INE
encla2019<-list.files("Input/Descomprimido/Encla2019")
encla2019
get(load(paste0("Input/Descomprimido/Encla2019/",encla2019[13])))
get(load(paste0("Input/Descomprimido/Encla2019/",encla2019[12])))
get(load(paste0("Input/Descomprimido/Encla2019/",encla2019[14])))
get(load(paste0("Input/Descomprimido/Encla2019/",encla2019[15])))

encla2019_e<-empleadores
encla2019_a<-autoaplicado
encla2019_s<-sindicatos
encla2019_t<-trabajadores


## Guardar proyecto
save(encla1998_e,encla1998_ts,
     encla1999_e,encla1999_ts,
     encla2002_a,encla2002_e,encla2002_s,encla2002_t,
     encla2004_a,encla2004_e,encla2004_s,encla2004_t,
     encla2006_a,encla2006_e,encla2006_s,encla2006_t,
     encla2008_a,encla2008_e,encla2008_s,encla2008_t,
     encla2011_a,encla2011_e,encla2011_s,encla2011_t,
     encla2014_a,encla2014_e,encla2014_s,encla2014_t,
     encla2019_a,encla2019_e,encla2019_s,encla2019_t,
     file = "Input/enclas.RData")

