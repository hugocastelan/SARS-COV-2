
#############################################################################################################################
######### SARS-CoV-2 Regiones de México ###############################################################
#############################################################################################################################

######### Hugo Castelan ################################################################################################

library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggpubr)


######### Read metadata and sequence data ###################################################################################



start <- ymd("2021-12-01")
end <- as.Date("2022-06-08")

#metada_covid <- read_excel("Downloads/metada_covid.xlsx")

metada_covid$Fecha_de_recoleccion <- lubridate::ymd(metada_covid$Fecha_de_recoleccion)

metada_covid <- metada_covid[metada_covid$Fecha_de_recoleccion >= start & metada_covid$Fecha_de_recoleccion <= end,]

metada_covid<- metada_covid[metada_covid$Fecha_de_recoleccion >= start & metada_covid$Fecha_de_recoleccion <= end,]

Mes_recoleccion <- format_ISO8601(metada_covid$Fecha_de_recoleccion, precision = "ym") 
Mes_recoleccion_decimal <- floor_date(metada_covid$Fecha_de_recoleccion, unit = "month") %>% decimal_date()
Semana_epi <- epiweek(metada_covid$Fecha_de_recoleccion)
metada_covid<- cbind(metada_covid, Mes_recoleccion = c(Mes_recoleccion),
                Mes_recoleccion_decimal = c(Mes_recoleccion_decimal),
                Semana_epi = c(Semana_epi))

#Frecuencia de linajes
#rename(count(omicron, Linaje), Freq = n)



metada_covid <- metada_covid %>%
  mutate(linajeOMS=case_when(Linaje== "B.1.1.7" ~ "Alpha", Linaje=="B.1.1.519" ~ "B.1.1.519", Linaje=="B.1.1.222" ~ "B.1.1.222",
                               Linaje=="B.1.617.2"| Linaje=="AY.1"|Linaje=="AY.2" | Linaje=="AY.3" | Linaje=="AY.3.1" | Linaje=="AY.4" |
                               Linaje =="AY.4.1" | Linaje=="AY.4.2" | Linaje=="AY.4.2.1" | Linaje=="AY.4.2.2" | Linaje=="AY.4.3" |
                               Linaje=="AY.4.4"| Linaje=="AY.4.5"| Linaje=="AY.5"| Linaje=="AY.5.1"| Linaje=="AY.5.2" | Linaje=="AY.5.3" |
                               Linaje=="AY.5.4" | Linaje=="AY.6" | Linaje=="AY.7" | Linaje=="AY.7.1" | Linaje=="AY.7.2" | Linaje=="AY.8" |
                               Linaje=="AY.9" | Linaje=="AY.9.1" | Linaje=="AY.9.2" | Linaje=="AY.9.2.1" | Linaje=="AY.10" |
                               Linaje=="AY.11" | Linaje=="AY.13" | Linaje=="AY.14" | Linaje=="AY.15" | Linaje=="AY.16" | Linaje=="AY.16.1" |
                               Linaje=="AY.17" | Linaje=="AY.18" | Linaje=="AY.19" | Linaje=="AY.20" | Linaje=="AY.20.1" | Linaje=="AY.21" |
                               Linaje=="AY.22" | Linaje=="AY.23" | Linaje=="AY.23.1" | Linaje=="AY.24" | Linaje=="AY.25" | Linaje=="AY.26" |
                               Linaje=="AY.27" | Linaje=="AY.28" | Linaje=="AY.29" | Linaje=="AY.29.1" | Linaje=="AY.30" | Linaje=="AY.31" |
                               Linaje=="AY.32" | Linaje=="AY.33" | Linaje=="AY.34" | Linaje=="AY.34.1" | Linaje=="AY.34.1.1" |
                               Linaje=="AY.35" | Linaje=="AY.36" | Linaje=="AY.37" | Linaje=="AY.38" | Linaje=="AY.39" | Linaje=="AY.39.1" |
                               Linaje=="AY.39.1.1" | Linaje=="AY.39.2" | Linaje=="AY.40" | Linaje=="AY.41" | Linaje=="AY.42" |
                               Linaje=="AY.43" | Linaje=="AY.43.1" | Linaje=="AY.43.2" | Linaje=="AY.43.3" | Linaje=="AY.43.4" |
                               Linaje=="AY.44" | Linaje=="AY.45"| Linaje=="AY.46"| Linaje=="AY.46.1" | Linaje=="AY.46.2" | Linaje=="AY.46.3" |
                               Linaje=="AY.46.4" | Linaje=="AY.46.5" | Linaje=="AY.46.6" | Linaje=="AY.47" | Linaje=="AY.48" | Linaje=="AY.49" |
                               Linaje=="AY.50" | Linaje=="AY.51" | Linaje=="AY.52" | Linaje=="AY.53" | Linaje=="AY.54" | Linaje=="AY.55" |
                               Linaje=="AY.56" | Linaje=="AY.57" | Linaje=="AY.58" | Linaje=="AY.59" | Linaje=="AY.60" | Linaje=="AY.61" |
                               Linaje=="AY.62" | Linaje=="AY.63" | Linaje=="AY.64" | Linaje=="AY.65" | Linaje=="AY.66" | Linaje=="AY.67" |
                               Linaje=="AY.68" | Linaje=="AY.69" | Linaje=="AY.70" | Linaje=="AY.71" | Linaje=="AY.72" | Linaje=="AY.73" |
                               Linaje=="AY.74" | Linaje=="AY.75" | Linaje=="AY.75.2" | Linaje=="AY.75.3" | Linaje=="AY.76" |Linaje=="AY.77" |
                               Linaje=="AY.78" | Linaje=="AY.79" | Linaje=="AY.80" | Linaje=="AY.81" | Linaje=="AY.82" | Linaje=="AY.83" |
                               Linaje=="AY.84" | Linaje=="AY.85" | Linaje=="AY.86" | Linaje=="AY.87" | Linaje=="AY.88" | Linaje=="AY.89" |
                               Linaje=="AY.90" | Linaje=="AY.91" | Linaje=="AY.91.1" | Linaje=="AY.92" | Linaje=="AY.93" | Linaje=="AY.94" |
                               Linaje=="AY.95" | Linaje=="AY.96" | Linaje=="AY.97" | Linaje=="AY.98" | Linaje=="AY.98.1" | Linaje=="AY.99" |
                               Linaje=="AY.99.1" | Linaje=="AY.99.2" | Linaje=="AY.100" | Linaje=="AY.101" | Linaje=="AY.102" |
                               Linaje=="AY.103" | Linaje=="AY.104" | Linaje=="AY.105" | Linaje=="AY.106" | Linaje=="AY.107" | Linaje=="AY.108" |
                               Linaje=="AY.109" | Linaje=="AY.110" | Linaje=="AY.111" | Linaje=="AY.112"| Linaje=="AY.113" | Linaje=="AY.114" |
                               Linaje=="AY.115" | Linaje=="AY.116" | Linaje=="AY.116.1" | Linaje=="AY.117" | Linaje=="AY.118" |
                               Linaje=="AY.119" | Linaje=="AY.120" | Linaje=="AY.120.1" | Linaje=="AY.120.2" | Linaje=="AY.120.2.1" |
                               Linaje=="AY.121" | Linaje=="AY.121.1"| Linaje=="AY.122" | Linaje=="AY.122.1" | Linaje=="AY.123" | Linaje=="AY.124" | Linaje=="AY.125" | Linaje=="AY.126" | Linaje=="AY.116.1" | Linaje=="AY.119.1" | Linaje=="AY.119.2" | Linaje=="AY.122.4" | Linaje=="AY.124.1.1" | Linaje=="AY.25.1" | Linaje=="AY.3" | Linaje=="AY.3.3" | Linaje=="AY.4.10" | Linaje=="AY.43.8" ~ "Delta", 
                               Linaje=="P.1" | Linaje=="P.1.1" | Linaje=="P.1.2" |   Linaje=="P.1.3" | Linaje=="P.1.4" | Linaje=="P.1.5" | Linaje =="P.1.6" | Linaje =="P.1.7" | Linaje =="P.1.7.1" | Linaje=="P.1.8" | Linaje=="P.1.9"| Linaje=="P.1.10" | Linaje=="P.1.10.1" | Linaje=="P.1.10.2" | Linaje=="P.1.11" | Linaje=="P.1.12" | Linaje=="P.1.12.1" | Linaje=="P.1.13"| Linaje=="P.1.14" | Linaje=="P.1.15" | Linaje=="P.1.17.1"~ "Gamma",
                               Linaje=="BA.1" ~ "BA.1", Linaje=="BA.1.1" ~ "BA.1.1", Linaje=="BA.1.1.10" ~ "BA.1.1.10",Linaje=="BA.1.1.11" ~ "BA.1.1.11", Linaje=="BA.1.1.14"~"BA.1.1.14", Linaje=="BA.1.1.15"~"BA.1.1.15",Linaje=="BA.1.1.16"~"BA.1.1.16",Linaje=="BA.1.1.2"~"BA.1.1.2",
                               Linaje=="BA.1.1.6"~"BA.1.1.6", Linaje=="BA.1.1.8"~"BA.1.1.8", Linaje=="BA.1.13"~"BA.1.13", Linaje=="BA.1.14"~"BA.1.14", Linaje=="BA.1.15"~"BA.1.15", Linaje=="BA.1.15.1"~"BA.1.15.1", Linaje=="BA.1.16"~"BA.1.16",Linaje=="BA.1.17"~"BA.1.17",Linaje=="BA.1.17.2"~"BA.1.17.2",
                               Linaje=="BA.1.18"~"BA.1.18",Linaje=="BA.1.19"~"BA.1.19",Linaje=="BA.1.20"~"BA.1.20", Linaje=="BA.1.21"~"BA.1.21",Linaje=="BA.1.2"~"BA.1.2",Linaje=="BA.1.4"~"BA.1.4",Linaje=="BA.1.5"~"BA.1.5",
                               Linaje=="BA.1.6"~"BA.1.6",Linaje=="BA.1.7"~"BA.1.7",Linaje=="BA.1.9"~"BA.1.9",Linaje=="BA.2"~"BA.2",Linaje=="BA.2.1"~"BA.2.1",Linaje=="BA.2.3"~"BA.2.3",Linaje=="BA.2.10"~"BA.2.10",Linaje=="BA.2.12"~"BA.2.12",Linaje=="BA.2.12.1"~"BA.2.12.1",Linaje=="BA.2.13"~"BA.2.13", Linaje=="BA.2.18"~"BA.2.18",
                               Linaje=="BA.2.2"~"BA.2.2",Linaje=="BA.2.21"~"BA.2.21",Linaje=="BA.2.22"~"BA.2.22",Linaje=="BA.2.23"~"BA.2.23",Linaje=="BA.2.26"~"BA.2.26",Linaje=="BA.2.3.2"~"BA.2.3.2", Linaje=="BA.2.3.3"~"BA.2.3.3", Linaje=="BA.2.3.4"~"BA.2.3.4", Linaje=="BA.2.2.31"~"BA.2.2.31",Linaje=="BA.4"~"BA.4", Linaje=="BA.5"~"BA.5", Linaje=="BA.2.26"~"BA.2.26", Linaje=="BA.2.3.3"~"BA.2.3.3", Linaje=="BA.2.4"~"BA.2.4", Linaje=="BA.2.22"~"BA.2.22", Linaje=="BA.2.9"~"BA.2.9",
                               TRUE  ~ "Other"))






filter1 <-  metada_covid %>% group_by(Estado,Fecha_de_recoleccion, Mes_recoleccion, linajeOMS) %>% summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
filter2<-filter(filter1, percentage>1)

color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.1.1.8"="peru", "BA.1.14"="azure4", "BA.1.15"="magenta3", "BA.1.17"="purple","BA.1.17.2"="orchid4","BA.1.20"="sienna3", "BA.1.21"= "red2", "BA.2"="yellowgreen", "BA.2.1"="antiquewhite", "BA.2.12"="indianred4", "BA.2.13"="honeydew", "BA.2.3"="darkseagreen3","BA.2.3"="red", "BA.2.9"="pink", "BA.4"="gainsboro", "BA.5"="lightgreen","Delta"="darkgoldenrod", "Other"="slateblue4")


#### Make the relative abundance plot #####


linajeOMS <- filter2$linajeOMS
Fecha_de_recoleccion <- filter2$Fecha_de_recoleccion
df_forRelativeAbundance_plots <- data.frame (Fecha_de_recoleccion, linajeOMS)
df_forRelativeAbundance_plots$number_week<-strftime(df_forRelativeAbundance_plots$Fecha_de_recoleccion, format = "%V")
df_forRelativeAbundance_plots$week_final<-(ceiling_date(df_forRelativeAbundance_plots$Fecha_de_recoleccion, "weeks"))
df_pervariant<-data.frame(table(df_forRelativeAbundance_plots$week_final, df_forRelativeAbundance_plots$linajeOMS))
names(df_pervariant)<-c("week_final","Variant","Frequency")
db_variante_percentage <- df_pervariant%>% group_by(week_final,Variant) %>% summarise(count=sum(Frequency)) %>% mutate(percentage=count/sum(count))
pdf ("relative_abundance_variants.pdf")
ggplot(data=db_variante_percentage, aes(x=week_final)) + geom_bar(aes(y=percentage,fill=Variant),stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = color)
dev.off()




Northeast<-c("Coahuila", "Nuevo Leon","Tamaulipas")
Northeast2<-filter(filter2, Estado %in% Northeast)
Northeast3 <- Northeast2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Northeast4 <- Northeast3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Northeast4[is.na(Northeast4)] <- 0
Northeast4<-filter(Northeast4, percentage >1)
#Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ggtitle("Northeast")
#Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noreste Mexico")+theme_void()+scale_fill_manual(values = color)
Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noreste Mexico")+theme_light()+scale_fill_manual(values = color)



Northwest <-c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sonora", "Sinaloa")
Northwest2<-filter(filter2, Estado %in% Northwest)

Northwest3 <- Northwest2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Northwest4 <- Northwest3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Northwest4[is.na(Northwest4)] <- 0
Northwest4<-filter(Northwest4, percentage >1)
#Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ ggtitle("Northwest")
#Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noroeste Mexico")+theme_void()+scale_fill_manual(values = color)
Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noroeste Mexico")+theme_light()+scale_fill_manual(values = color)


Central_North<-c("Aguascalientes","Guanajuato", "Querétaro", "San Luis Potosí","Zacatecas")
Central_North2<-filter(filter2, Estado %in% Central_North)
Central_North3 <- Central_North2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Central_North4 <- Northwest3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Central_North4[is.na(Central_North4)] <- 0
Central_North4<-filter(Central_North4, percentage >1)
#Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ggtitle("Central_North")
#Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Norte Mexico")+theme_void()+scale_fill_manual(values = color)
Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Norte Mexico")+theme_light()+scale_fill_manual(values = color)

Central_South<- c("Mexico City", "State of Mexico", "Morelos", "Hidalgo", "Puebla","Tlaxcala")
Central_South2<-filter(filter2, Estado %in% Central_South)
Central_South3 <- Central_South2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Central_South4 <- Central_South3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Central_South4[is.na(Central_South4)] <- 0
Central_South4<-filter(Central_South4, percentage > 1)
#Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ggtitle("Central_South")
#Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Sur")+theme_void()+scale_fill_manual(values = color)
Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Sur")+theme_light()+scale_fill_manual(values = color)


West<-c("Colima", "Jalisco", "Michoacan", "Nayarit")
West2<-filter(filter2, Estado %in% West)

West3 <- West2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
West4 <- West3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
West4[is.na(West4)] <- 0
West4<-filter(West4, percentage > 1)
#West5<-ggplot(West4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ ggtitle("West")
#West5<-ggplot(West4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Oeste Mexico")+theme_void()+scale_fill_manual(values = color)
West5<-ggplot(West4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Oeste Mexico")+theme_light()+scale_fill_manual(values = color)




Southeast<-c("Guerrero", "Oaxaca", "Chiapas", "Veracruz", "Tabasco")
Southeast2<-filter(filter2, Estado %in% Southeast)

Southeast3 <- Southeast2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Southeast4 <- Southeast3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Southeast4[is.na(Southeast4)] <- 0
Southeast4<-filter(Southeast4, percentage > 1)
#Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ggtitle("Southeast")
#Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sureste Mexico")+theme_void()+scale_fill_manual(values = color)
Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sureste Mexico")+theme_light()+scale_fill_manual(values = color)

South<-c("Campeche","Yucatan", "Quintana Roo")
South2<-filter(filter2, Estado %in% South)

South3 <- South2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
South4 <-South3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
South4[is.na(South4)] <- 0
South4<-filter(South4, percentage > 1)
#South5<-ggplot(South4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(labels = c('BA.1', 'BA.1.1', 'BA.1.15','BA.1.17','BA.1.17.2','BA.1.20','BA.2','BA.2.12','BA.2.12.1','BA.2.22','BA.2.9', 'Delta','Other'), values = c("Light Sea Green", "Sienna 2", "Sky Blue", "#5436D9", "#D93840", "Dark Olive Green",  "#BBBBBB", "#445599","Dark Sea Green 2", "Deep Sky Blue 2", "Violet Red 3", "Thistle","Cornflower Blue","antiquewhite4","azure3","azure4","gainsboro","gray","gray56","honeydew","antiquewhite4"))+ggtitle("South")
#South5<-ggplot(South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sur Mexico")+theme_void()+scale_fill_manual(values = color)
South5<-ggplot(South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sur Mexico")+theme_light()+scale_fill_manual(values = color)

ggarrange(Northeast5,Northwest5,Central_North5,Central_South5,West5,Southeast5,South5, common.legend = TRUE, legend="right")






