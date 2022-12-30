

######### Hugo Castelan ################################################################################################

library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggpubr)

#######################################################################################################################


start <- ymd("2022-01-01")
end<-as.Date("2022-11-30")


metadata_covid$Fecha_de_recoleccion <- lubridate::ymd(metadata_covid$Fecha_de_recoleccion)

metadata_covid <- metadata_covid[metadata_covid$Fecha_de_recoleccion >= start & metadata_covid$Fecha_de_recoleccion <= end,]

Mes_recoleccion <- format_ISO8601(metadata_covid$Fecha_de_recoleccion, precision = "ym") 

Mes_recoleccion_decimal <- floor_date(metadata_covid$Fecha_de_recoleccion, unit = "month") %>% decimal_date()
Semana_recoleccion <- ceiling_date(metadata_covid$Fecha_de_recoleccion, "weeks")
Semana_epi <- epiweek(metadata_covid$Fecha_de_recoleccion)

metadata_covid<- cbind(metadata_covid, Mes_recoleccion = c(Mes_recoleccion), Semana_recoleccion=c(Semana_recoleccion),
                Mes_recoleccion_decimal = c(Mes_recoleccion_decimal),
                Semana_epi = c(Semana_epi))

#rename(count(metadata_covid, Estado), Freq = n)

Northeast<-c("Coahuila", "Nuevo Leon","Tamaulipas")
Northeast2<-filter(metadata_covid, Estado %in% Northeast)
Northeast3<-Northeast2 %>% select("Estado", "Mes_recoleccion") 
Northeast4<-table(Northeast3)
Northeast5<-as.data.frame(Northeast4) 
Northeast6<-ggplot(Northeast5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Northeast 2022") + xlab("months") + ylab("Genomes sequenced")


Northwest <-c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sonora", "Sinaloa")
Northwest2<-filter(metadata_covid, Estado %in% Northwest)
Northwest3<-Northwest2 %>% select("Estado", "Mes_recoleccion")
Northwest4<-table(Northwest3)
Northwest5<-as.data.frame(Northwest4) 
Northwest6<-ggplot(Northwest5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Northwest 2022") + xlab("months") + ylab("Genomes sequenced")


Central_North<-c("Aguascalientes","Guanajuato", "Queretaro", "San Luis Potosi","Zacatecas")
Central_North2<-filter(metadata_covid, Estado %in% Central_North)
Central_North3 <-Central_North2 %>% select("Estado", "Mes_recoleccion")
Central_North4<-table(Central_North3)
Central_North5<-as.data.frame(Central_North4) 
Central_North6<-ggplot(Central_North5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Central_North 2022") + xlab("months") + ylab("Genomes sequenced")


Central_South<- c("Mexico City", "State of Mexico", "Morelos", "Hidalgo", "Puebla","Tlaxcala")
Central_South2<-filter(metadata_covid, Estado %in% Central_South)
Central_South3 <-Central_South2 %>% select("Estado", "Mes_recoleccion")
Central_South4<-table(Central_South3)
Central_South5<-as.data.frame(Central_South4) 
Central_South6<-ggplot(Central_South5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Central_South 2022") + xlab("months") + ylab("Genomes sequenced")


West <- c("Colima", "Jalisco", "Michoacan", "Nayarit")
West2<-filter(metadata_covid, Estado %in% West)
West3 <-West2 %>% select("Estado", "Mes_recoleccion")
West4<-table(West3)
West5<-as.data.frame(West4) 
West6<-ggplot( West5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("West 2022") + xlab("months") + ylab("Genomes sequenced")


Southeast<-c("Guerrero", "Oaxaca", "Chiapas", "Veracruz", "Tabasco")
Southeast2<-filter(metadata_covid, Estado %in% Southeast)
Southeast3 <-Southeast2 %>% select("Estado", "Mes_recoleccion")
Southeast4<-table(Southeast3)
Southeast5<-as.data.frame(Southeast4) 
Southeast6<-ggplot(Southeast5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Southeast 2022") + xlab("months") + ylab("Genomes sequenced")




South<-c("Campeche","Yucatan", "Quintana Roo")
South2<-filter(metadata_covid, Estado %in% South)
South3 <-South2 %>% select("Estado", "Mes_recoleccion")
South4<-table(South3)
South5<-as.data.frame(South4) 
South6<-ggplot(South5, aes(x=Mes_recoleccion, y=Freq, group = Estado, colour =Estado)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("South 2022") + xlab("months") + ylab("Genomes sequenced")

ggarrange(Northeast6,Northwest6,Central_North6,Central_South6,West6,Southeast6,South6, legend="right")






