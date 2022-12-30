

######### Hugo Castelan ################################################################################################

library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggpubr)

#######################################################################################################################


start <- ymd("2021-01-01")
end<-as.Date("2021-11-30")


metadata_covid$Fecha_de_recoleccion <- lubridate::ymd(metadata_covid$Fecha_de_recoleccion)

metadata_covid <- metadata_covid[metadata_covid$Fecha_de_recoleccion >= start & metadata_covid$Fecha_de_recoleccion <= end,]

Mes_recoleccion <- format_ISO8601(metadata_covid$Fecha_de_recoleccion, precision = "ym") 

Mes_recoleccion_decimal <- floor_date(metadata_covid$Fecha_de_recoleccion, unit = "month") %>% decimal_date()
Semana_recoleccion <- ceiling_date(metadata_covid$Fecha_de_recoleccion, "weeks")
Semana_epi <- epiweek(metadata_covid$Fecha_de_recoleccion)

metadata_covid<- cbind(metadata_covid, Mes_recoleccion = c(Mes_recoleccion), Semana_recoleccion=c(Semana_recoleccion),
                Mes_recoleccion_decimal = c(Mes_recoleccion_decimal),
                Semana_epi = c(Semana_epi))



Northeast<-c("Coahuila", "Nuevo Leon","Tamaulipas")
Northeast2<-filter(metadata_covid, Estado %in% Northeast)
Northeast3<-Northeast2 %>% select("Estado", "Mes_recoleccion") 
Northeast4<-table(Northeast3)
Northeast5<-as.data.frame(Northeast4) 
Northeast6<-log(Northeast5$Freq)
Northeast7<-cbind(Northeast5,Northeast6)
colnames(Northeast7)<-c("State", "months", "genomes_sequenced","log")
Northeast7$log[is.infinite(Northeast7$log)] <- 0
Northeast8<-ggplot(Northeast7, aes(x=months, y=log, group = State, colour =State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Northeast 2021") + xlab("months") + ylab("log genomes sequenced")


Northwest <-c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sonora", "Sinaloa")
Northwest2<-filter(metadata_covid, Estado %in% Northwest)
Northwest3<-Northwest2 %>% select("Estado", "Mes_recoleccion") 
Northwest4<-table(Northwest3)
Northwest5<-as.data.frame(Northwest4)
Northwest6<-log(Northwest5$Freq)
Northwest7<-cbind(Northwest5,Northwest6)
colnames(Northwest7)<-c("State", "months", "genomes_sequenced","log")
Northwest7$log[is.infinite(Northwest7$log)] <- 0
Northwest8<-ggplot(Northwest7, aes(x=months, y=genomes_sequenced, group = State, colour = State )) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Northwest 2021") + xlab("months") + ylab("log genomes sequenced")


Central_North<-c("Aguascalientes","Guanajuato", "Querétaro", "San Luis Potosí","Zacatecas")
Central_North2<-filter(metadata_covid, Estado %in% Central_North)
Central_North3<-Central_North2 %>% select("Estado", "Mes_recoleccion") 
Central_North4<-table(Central_North3)
Central_North5<-as.data.frame(Central_North4) 
Central_North6<-log(Central_North5$Freq)
Central_North7<-cbind(Central_North5,Central_North6)
colnames(Central_North7)<-c("State", "months", "genomes_sequenced","log")
Central_North7$log[is.infinite(Central_North7$log)] <- 0
Central_North8<-ggplot(Central_North7, aes(x=months, y=genomes_sequenced, group = State, colour =State )) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Central_North 2021") + xlab("months") + ylab("log genomes sequenced")


Central_South<- c("Mexico City", "State of Mexico", "Morelos", "Hidalgo", "Puebla","Tlaxcala")
Central_South2<-filter(metadata_covid, Estado %in% Central_South)
Central_South3<-Central_South2 %>% select("Estado", "Mes_recoleccion") 
Central_South4<-table(Central_South3)
Central_South5<-as.data.frame(Central_South4) 
Central_South6<-log(Central_South5$Freq)
Central_South7<-cbind(Central_South5,Central_South6)
colnames(Central_South7)<-c("State", "months", "genomes_sequenced","log")
Central_South7$log[is.infinite(Central_South7$log)] <- 0
Central_South8<-ggplot(Central_South7, aes(x=months, y=genomes_sequenced, group = State, colour = State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Central_South 2021") + xlab("months") + ylab("log genomes sequenced")

West <- c("Colima", "Jalisco", "Michoacan", "Nayarit")
West2<-filter(metadata_covid, Estado %in% West)
West3<-West2 %>% select("Estado", "Mes_recoleccion") 
West4<-table(West3)
West5<-as.data.frame(West4) 
West6<-log(West5$Freq)
West7<-cbind(West5,West6)
colnames(West7)<-c("State", "months", "genomes_sequenced","log")
West7$log[is.infinite(West7$log)] <- 0
West8<-ggplot(West7, aes(x=months, y=genomes_sequenced, group = State, colour = State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Central_South 2021") + xlab("months") + ylab("log genomes sequenced")

Southeast<-c("Guerrero", "Oaxaca", "Chiapas", "Veracruz", "Tabasco")
Southeast2<-filter(metadata_covid, Estado %in% Southeast)
Southeast3<-Southeast2 %>% select("Estado", "Mes_recoleccion") 
Southeast4<-table(Southeast3)
Southeast5<-as.data.frame(Southeast4) 
Southeast6<-log(Southeast5$Freq)
Southeast7<-cbind(Southeast5,Southeast6)
colnames(Southeast7)<-c("State", "months", "genomes_sequenced","log")
Southeast7$log[is.infinite(Southeast7$log)] <- 0
Southeast8<-ggplot(Southeast7, aes(x=months, y=log, group = State, colour = State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Southeast 2021") + xlab("months") + ylab("log genomes sequenced")

South<-c("Campeche","Yucatan", "Quintana Roo")
South2<-filter(metadata_covid, Estado %in% South)
South3<-South2 %>% select("Estado", "Mes_recoleccion") 
South4<-table(South3)
South5<-as.data.frame(South4) 
South6<-log(South5$Freq)
South7<-cbind(South5,South6)
colnames(South7)<-c("State", "months", "genomes_sequenced","log")
South7$log[is.infinite(South7$log)] <- 0
South8<-ggplot(South7, aes(x=months, y=genomes_sequenced, group = State, colour = State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("South 2021") + xlab("months") + ylab("log genomes sequenced")

ggarrange(Northeast8,Northwest8,Central_North8,Central_South8,West8,Southeast8,South8,  legend="right")

#####todo el pais





normalize <- function(x) {
     return ((x - min(x)) / (max(x) - min(x)))
 }

normalizado<-normalize(metadata_covid5$Freq)
metadata_covid5<-cbind(metadata_covid5, normalizado)
colnames(metadata_covid5)<-c("State", "months", "freq","Norm_genomes_sequenced")
ggplot(metadata_covid5, aes(x=months, y=Norm_genomes_sequenced, group = State, colour =State)) +geom_line()  + geom_point( size=2, shape=21, fill="white") +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("Mexico 2021") + xlab("months") + ylab("log genomes sequenced")

#HCastelanS221122

#Bien 
ggplot(metadata_covid5, aes(x=months, y=Norm_genomes_sequenced, group = State)) +geom_line(aes(color=State))   +  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme( axis.title.x = element_text(size = 9),axis.text.x = element_text(size = 6))+ggtitle("SARS-CoV-2 genomes sequenced between 2020-2021") + xlab("months") + ylab("norm genomes sequenced")+ scale_color_manual(values=c("darkred", "forestgreen", "gold4", "darkslateblue", "deeppink4", "khaki4", "mediumpurple", "limegreen", "orange3", "violetred4", "steelblue", "chocolate1", "cyan3", "darkgrey", "darksalmon", "darkslategrey", "hotpink4", "lightseagreen", "mistyrose4", "olivedrab3", "plum3", "palegreen1", "royalblue4", "seagreen", "sienna3", "slategray2", "yellowgreen", "midnightblue", "deepskyblue3", "firebrick2", "indianred3", "sandybrown"))

