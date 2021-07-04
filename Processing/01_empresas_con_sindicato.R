library(tidyverse)
library(writexl)


load(file = "Input/enclas.RData")



base<-data.frame(ano=c(1998,1999,2002,2004,2006,2008,2011,2014,2019),
                 sin_sindicato=NA,
                 con_sindicato=NA,
                 porcentaje_sindicato=NA)




## Encla 2019

## 79.436 empresas estimadas
sum(encla2019_a$fe_empresa)

base[9,2:4]<-encla2019_a %>% group_by(sindicatos_presencia) %>% 
  summarise(EMPRESAS=sum(fe_empresa)) %>% 
  pivot_wider(names_from = sindicatos_presencia,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2019<-encla2019_a %>% group_by(sindicatos_presencia,tamano) %>% summarise(EMPRESAS=sum(fe_empresa)) %>% pivot_wider(names_from = sindicatos_presencia,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`,tamaño=tamano) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2019) %>% 
  mutate(tamaño=case_when(tamaño==1~"gran",
                          tamaño==2~"median",
                          tamaño==3~"pequeña",
                          tamaño==4~"micro")) %>% 
  mutate(tamaño=case_when(tamaño=="gran"~4,
                          tamaño=="median"~3,
                          tamaño=="pequeña"~2,
                          tamaño=="micro"~1)) %>% arrange(tamaño)

## Por sector

sector_2019<-encla2019_a %>% group_by(sindicatos_presencia,agrupacion_actividad) %>% 
  summarise(EMPRESAS=sum(fe_empresa)) %>% pivot_wider(names_from = sindicatos_presencia,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`,sector=agrupacion_actividad) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2019) %>% 
  cbind(attr(encla2019_a$agrupacion_actividad,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame())


## Sector y tamaño
a<-cbind(attr(encla2019_a$agrupacion_actividad,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% 
  mutate(sector_letra=c("A","B","C","D-E","F","G","H-J","I","K-L","M-N","P","Q","R-S"),
         sector=c(1:13))

sector_tamano_2019<-encla2019_a %>% group_by(sindicatos_presencia,agrupacion_actividad,tamano) %>% 
  summarise(EMPRESAS=sum(fe_empresa)) %>% pivot_wider(names_from = sindicatos_presencia,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`,sector=agrupacion_actividad) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2019) %>% merge(a,by="sector")





## Encla 2014

## 89.580 empresas estimadas
sum(encla2014_a$FX_EMPRESAS)
## 79.672 empresas estimadas
sum(encla2014_a$FX_TRABAJADORES)

## Factor empresa
base[8,2:4]<-encla2014_a %>% group_by(SINDICATO) %>% summarise(EMPRESAS=sum(FX_EMPRESAS)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Factor trabajadores
base[8,2:4]<-encla2014_a %>% group_by(SINDICATO) %>% summarise(EMPRESAS=sum(FX_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2014<-encla2014_a %>% group_by(SINDICATO,TAMAÑO) %>% summarise(EMPRESAS=sum(FX_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,tamaño=TAMAÑO) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2014) 

## Por sector
names(encla2014_a)
sector_2014<-encla2014_a %>% group_by(SINDICATO,ACTIVIDAD) %>% 
  summarise(EMPRESAS=sum(FX_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sector=ACTIVIDAD) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2014) %>% 
  cbind(attr(encla2014_a$ACTIVIDAD,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame())


## Sector y tamaño
a<-cbind(attr(encla2014_a$ACTIVIDAD,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% 
  mutate(sector=c("A","B","C","D","E","F","G","H","I","J","K","M","N","O"))

sector_tamano_2014<-encla2014_a %>% group_by(SINDICATO,ACTIVIDAD,TAMAÑO) %>% 
  summarise(EMPRESAS=sum(FX_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sector=ACTIVIDAD) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2014) %>% merge(a,by="sector")



## Encla 2011

## 89.580 empresas estimadas
sum(encla2011_a$FE_EMPRESAS)
## 79.672 empresas estimadas
sum(encla2011_a$FE_TRABAJADORES)

## Factor empresa
base[7,2:4]<-encla2011_a %>% group_by(SINDICATO) %>% summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2011<-encla2011_a %>% group_by(SINDICATO,TAMAÑO) %>% summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,tamaño=TAMAÑO) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2011)

## Por sector
names(encla2011_a)
sector_2011<-encla2011_a %>% group_by(SINDICATO,RAMA) %>% 
  summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICATO,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sector=RAMA) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2011) %>% 
  cbind(attr(encla2011_a$RAMA,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame())



## Encla 2008

## 89.580 empresas estimadas
sum(encla2008_a$FE_EMPRESAS)
## 79.672 empresas estimadas
sum(encla2008_a$FE_TRABAJADORES)

## Factor empresa
base[6,2:4]<-encla2008_a %>% group_by(SINDICAT) %>% summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICAT,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  select(sin_sindicato,con_sindicato) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2008<-encla2008_a %>% group_by(SINDICAT,TAMAÑO) %>% summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICAT,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`,tamaño=TAMAÑO) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(tamaño,sin_sindicato,con_sindicato,porcentaje_sindicato) %>% 
  mutate(ENCLA=2008)

## Por sector
names(encla2008_a)
sector_2008<-encla2008_a %>% group_by(SINDICAT,RAMA) %>% 
  summarise(EMPRESAS=sum(FE_TRABAJADORES)) %>% pivot_wider(names_from = SINDICAT,values_from = EMPRESAS) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`,sector=RAMA) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2008) 

a<-cbind(attr(encla2008_a$RAMA,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:19)

sector_2008<-merge(sector_2008,a,by="sector",all.x = TRUE)


## Encla 2006

## No hay factor

## Factor empresa
base[5,2:4]<-encla2006_a %>% group_by(Sindicat) %>% 
  tally() %>% pivot_wider(names_from = Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2006<-encla2006_a %>% group_by(Sindicat,Tamaño) %>% tally() %>% 
  pivot_wider(names_from = Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,tamaño=Tamaño) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2006)

## Por sectpr
names(encla2006_a)
sector_2006<-encla2006_a %>% group_by(Sindicat,Rama_nueva) %>% tally() %>% 
  pivot_wider(names_from = Sindicat,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sector=Rama_nueva) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2006) %>% filter(!is.na(sector))

a<-cbind(attr(encla2006_a$Rama_nueva,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:18)

sector_2006<-merge(sector_2006,a,by="sector",all.x = TRUE)


## Encla 2004

names(encla2004_a)
## No hay factor
table(encla2004_a$sindicat)
## Factor empresa
base[4,2:4]<-encla2004_a %>% group_by(sindicat) %>% 
  tally() %>% pivot_wider(names_from = sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(sin_sindicato,con_sindicato,porcentaje_sindicato) 

## Por tamaño
tamano_2004<-encla2004_a %>% group_by(sindicat,tamaño) %>% tally() %>% 
  pivot_wider(names_from = sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(tamaño,sin_sindicato,con_sindicato,porcentaje_sindicato) %>% 
  mutate(ENCLA=2004) %>% arrange(tamaño)

## Por sector
names(encla2004_a)
sector_2004<-encla2004_a %>% group_by(sindicat,rama) %>% tally() %>% 
  pivot_wider(names_from = sindicat,values_from = n) %>% 
  rename(sin_sindicato=`2`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(rama,sin_sindicato,con_sindicato,porcentaje_sindicato) %>% 
  rename(sector=rama) %>% 
  mutate(ENCLA=2004)

a<-cbind(attr(encla2004_a$rama,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:10)

sector_2004<-merge(sector_2004,a,by="sector",all.x = TRUE)




## Encla 2002

## Factor empresa
base[3,2:4]<-encla2002_e %>% group_by(sindical) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_2002<-encla2002_e %>% group_by(sindical,tamaño) %>% tally() %>% 
  pivot_wider(names_from = sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=2002) %>% filter(!is.na(tamaño))

## Por sector
names(encla2002_e)
sector_2002<-encla2002_e %>% group_by(sindical,rec_rama) %>% tally() %>% 
  pivot_wider(names_from = sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  rename(sector=rec_rama) %>% 
  mutate(ENCLA=2002)

a<-cbind(attr(encla2002_e$rec_rama,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:3)

sector_2002<-merge(sector_2002,a,by="sector",all.x = TRUE)




#1999
## Factor empresa
base[2,2:4]<-encla1999_e %>% group_by(sindical) %>% 
  tally() %>% pivot_wider(names_from =sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta)) %>% 
  select(-c(sin_respuesta))

## Por tamaño
tamano_1999<-encla1999_e %>% group_by(sindical,tamaño) %>% tally() %>% 
  pivot_wider(names_from = sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta)) %>% 
  select(-c(sin_respuesta)) %>% 
  mutate(ENCLA=1999)

#sector
names(encla1999_e)
a<-attr(encla1999_e$rama,"labels")
sector_1999<-encla1999_e %>% group_by(sindical,rama) %>% tally() %>% 
  rename(sector=rama) %>% 
  pivot_wider(names_from = sindical,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sin_respuesta=`NA`) %>% 
  mutate(sin_respuesta=as.numeric(sin_respuesta)) %>% 
  mutate(sin_respuesta=if_else(is.na(sin_respuesta),0,sin_respuesta)) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato+sin_respuesta)) %>% 
  select(-c(sin_respuesta)) %>% 
  mutate(ENCLA=1999)

a<-cbind(attr(encla1999_e$rama,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:9)

sector_1999<-merge(sector_1999,a,by="sector",all.x = TRUE)




#1998
table(encla1998_e$sindicato)
## Factor empresa
base[1,2:4]<-encla1998_e %>% group_by(sindicato) %>% 
  tally() %>% pivot_wider(names_from =sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`,con_sindicato=`0`) %>% 
  select(sin_sindicato,con_sindicato) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato))

## Por tamaño
tamano_1998<-encla1998_e %>% group_by(sindicato,tamaño) %>% tally() %>% 
  pivot_wider(names_from = sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`,con_sindicato=`0`) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(tamaño,sin_sindicato,con_sindicato,porcentaje_sindicato) %>% 
  mutate(ENCLA=1998)

## Por sector
names(encla1998_e)
sector_1998<-encla1998_e %>% group_by(sindicato,rama) %>% tally() %>% 
  pivot_wider(names_from = sindicato,values_from = n) %>% 
  rename(sin_sindicato=`1`,con_sindicato=`0`,sector=rama) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  select(sector,sin_sindicato,con_sindicato,porcentaje_sindicato) %>% 
  mutate(ENCLA=1998)

a<-cbind(attr(encla1998_e$rama,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% mutate(sector=1:9)

sector_1998<-merge(sector_1998,a,by="sector",all.x = TRUE)


## Sector y tamaño
a<-cbind(attr(encla1998_e$rama,"labels") %>% as.data.frame() %>% rownames() %>% as.data.frame()) %>% 
  mutate(sector_letra=c("A","B","C","D-E","F","G-I","H-J","K-L","Q-S"),
         sector=c(1:9))

sector_tamano_1998<-encla1998_e %>% group_by(sindicato,rama,tamaño) %>% 
  tally() %>% pivot_wider(names_from = sindicato,values_from = n) %>% 
  rename(sin_sindicato=`0`,con_sindicato=`1`,sector=rama) %>% 
  mutate(porcentaje_sindicato=con_sindicato/(sin_sindicato+con_sindicato)) %>% 
  mutate(ENCLA=1998) %>% merge(a,by="sector")








#### Combinar
base2<-rbind(tamano_1998,tamano_1999,tamano_2002,
      tamano_2004,tamano_2006,tamano_2008,
      tamano_2011,tamano_2014,tamano_2019)

base2 %>% mutate(tamaño=as.factor(tamaño)) %>% 
  ggplot(aes(x=ENCLA,y=porcentaje_sindicato,color=tamaño))+geom_line()+geom_point() + 
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "Porcentaje de empresas con sindicatos según Encuestas Laborales entre 1998-2014",
       subtitle = "Por tamaño de empresa",
       x="Año de la encuesta",
       y="Porcentaje de empresas con sindicato",
       caption = "Fuente: https://observatoriosindical.github.io/
                  Cada punto indica la aplicación de una encuesta.") +
  scale_x_continuous(limits = c(1998,2019),breaks = c(1998,2002,2006,2010,2014,2019))+
  theme(legend.position = "bottom") +
  scale_color_manual(values=c('blue','red',"black","purple"),labels=c("Micro","Pequeñas","Medianas","Grandes"))

ggsave(
  plot = last_plot(),
  filename = "output/grafico2_porcentaje_empresas_sindicato.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 35,
  height = 20
)


#### Exportar tablas

library(writexl)
write_xlsx(base,"Output/cuadro1.Empresas_con_sindicato.xlsx", col_names = TRUE,format_headers = TRUE)

write_xlsx(base2,"Output/cuadro2.Empresas_con_sindicato_tamano.xlsx", col_names = TRUE,format_headers = TRUE)

write_xlsx(list("1998"=sector_1998,"1999"=sector_1999,"2002"=sector_2002,
                "2004"=sector_2004,"2006"=sector_2006,"2008"=sector_2008,
                "2011"=sector_2011,"2014"=sector_2014,"2019"=sector_2019),"Output/cuadro3.Empresas_con_sindicato_sector.xlsx", col_names = TRUE,format_headers = TRUE)


write_xlsx(list("2019"=sector_tamano_2019,
                "2014"=sector_tamano_2014,
                "1998"=sector_tamano_1998),"Output/cuadro4.Empresas_con_sindicato_sector_tamaño.xlsx", col_names = TRUE,format_headers = TRUE)



