library(tidyverse)
library(writexl)


load(file = "../Input/enclas.RData")


names(encla2014_a)


base<-data.frame(ano=c(1998,1999,2002,2004,2006,2008,2011,2014),
                 sin_sindicato=NA,
                 con_sindicato=NA,
                 porcentaje_sindicato=NA)


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


#### Combinar
base2<-rbind(tamano_1998,tamano_1999,tamano_2002,
      tamano_2004,tamano_2006,tamano_2008,
      tamano_2011,tamano_2014)

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
  scale_x_continuous(limits = c(1998,2014),breaks = c(1998,2002,2006,2010,2014))+
  theme(legend.position = "bottom") +
  scale_color_manual(values=c('blue','red',"black","purple"),labels=c("Micro","Pequeñas","Medianas","Grandes"))

ggsave(
  plot = last_plot(),
  filename = "../output/grafico2_porcentaje_empresas_sindicato.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 35,
  height = 20
)


#### Exportar tablas

library(writexl)
write_xlsx(base,"../Output/cuadro1.Empresas_con_sindicato.xlsx", col_names = TRUE,format_headers = TRUE)

write_xlsx(base2,"../Output/cuadro2.Empresas_con_sindicato_tamano.xlsx", col_names = TRUE,format_headers = TRUE)

