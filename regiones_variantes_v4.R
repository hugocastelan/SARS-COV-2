
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


######### Read metadatata and sequence data ###################################################################################



args <- commandArgs(TRUE)
file_in <- as.character(args[1])
metadata_covid <- read_excel(file_in)

#Primera ola 
#start <- ymd("2020-02-01")
#end<-as.Date("2020-10-03")

#segunda ola
#start <- ymd("2020-10-04")
#end <- as.Date("2021-05-15")

#tercera ola
#start <- ymd("2021-05-16")
#end <- as.Date("2021-12-18")

#Cuarta ola
#start <- ymd("2021-12-19")
#end <- as.Date("2022-04-24")

#Quinta ola 
start <- ymd("2022-08-01")
end <- as.Date("2022-10-20")



#metadata_covid <- read_excel("Downloads/metadata_covid.xlsx")

metadatata_covid$Fecha_de_recoleccion <- lubridate::ymd(metadata_covid$Fecha_de_recoleccion)

metadatata_covid <- metadata_covid[metadata_covid$Fecha_de_recoleccion >= start & metadata_covid$Fecha_de_recoleccion <= end,]

metadata_covid<- metadata_covid[metadata_covid$Fecha_de_recoleccion >= start & metadata_covid$Fecha_de_recoleccion <= end,]

Mes_recoleccion <- format_ISO8601(metadata_covid$Fecha_de_recoleccion, precision = "ym") 

Mes_recoleccion_decimal <- floor_date(metadata_covid$Fecha_de_recoleccion, unit = "month") %>% decimal_date()
Semana_recoleccion <- ceiling_date(metadata_covid$Fecha_de_recoleccion, "weeks")
Semana_epi <- epiweek(metadata_covid$Fecha_de_recoleccion)

metadata_covid<- cbind(metadata_covid, Mes_recoleccion = c(Mes_recoleccion), Semana_recoleccion=c(Semana_recoleccion),
                Mes_recoleccion_decimal = c(Mes_recoleccion_decimal),
                Semana_epi = c(Semana_epi))

#Frecuencia de linajes
#rename(count(omicron, Linaje), Freq = n)



metadata_covid <- metadata_covid %>%
mutate(linajeOMS=case_when(Linaje=="B.1.1.7"|Linaje=="Q3"~"Alpha", Linaje=="B.1.1.519" ~ "B.1.1.519", Linaje=="B.1.1.222" ~ "B.1.1.222",Linaje== "A" ~ "A",
                               Linaje=="A.1"~"A1",Linaje=="A.2 "~ "A2",Linaje=="A.5"~ "A5",Linaje=="B"~"B",Linaje=="B.1"~"B.1",Linaje=="B.1.1"~"B.1.1",Linaje=="B.1.621"~"B.1.621",
                               Linaje=="B.1.1.1"~"B.1.1.1",Linaje=="B.1.1.10"~"B.1.1.10",Linaje=="B.1.1.122"~"B.1.1.122",Linaje=="B.1.1.228"~"B.1.1.228",Linaje=="B.1.634"~"B.1.634",
                               Linaje=="B.1.1.244"~"B.1.1.244",Linaje=="B.1.1.28"~"B.1.1.28",Linaje=="B.1.1.285"~"B.1.1.285",Linaje=="B.1.576"~"B.1.576",Linaje=="B.1.1.517"~"B.1.1.517",
                               Linaje=="B.1.1.322"~"B.1.1.322",Linaje=="B.1.1.33"~"B.1.1.33",Linaje=="B.1.1.344"~"B.1.1.344",Linaje=="B.1.1.403"~"B.1.1.403",Linaje=="B.1.588"~"B.1.588",
                               Linaje=="B.1.1.432"~"B.1.1.432", Linaje=="B.1.1.512"~"B.1.1.512",Linaje=="B.1.1.518"~"B.1.1.518",Linaje=="B.1.1.71"~"B.1.1.71", Linaje=="B.1.36.31"~"B.1.36.31",
                               Linaje=="B.1.1.8"~"B.1.1.8",Linaje=="B.1.1.93"~"B.1.1.93",Linaje=="B.1.111"~"B.1.111", Linaje=="B.1.280"~"B.1.280",Linaje=="B.1.320"~"B.1.320",
                               Linaje=="B.1.366"~"B.1.366",Linaje=="B.1.369"~"B.1.369",Linaje=="B.1.396"~"B.1.396",Linaje=="B.1.397"~"B.1.397",Linaje=="B.1.399"~"B.1.399",Linaje=="B.1.1.434"~"B.1.1.434",
                               Linaje=="B.1.400"~"B.1.400",Linaje=="B.1.405"~"B.1.405",Linaje=="B.1.415"~"B.1.415",Linaje=="B.1.427"~"B.1.427",Linaje=="B.1.429"~"B.1.429",Linaje=="B.1.240"~"B.1.240",
                               Linaje=="B.1.499"~"B.1.499",Linaje=="B.1.507"~"B.1.507",Linaje=="B.1.533"~"B.1.533",Linaje=="B.1.551"~"B.1.551",Linaje=="B.1.558"~"B.1.558",Linaje=="B.1.595"~"B.1.595",
                               Linaje=="B.1.561"~"B.1.561",Linaje=="B.1.567"~"B.1.567",Linaje=="B.1.572"~"B.1.572",Linaje=="B.1.572"~"B.1.572",Linaje=="B.1.609"~"B.1.609",Linaje=="C.23"~"C.23",
                               Linaje=="B.1.147"~"B.1.147",Linaje=="B.1.189"~"B.1.189",Linaje=="B.1.2"~"B.1.2",Linaje=="B.1.1.189"~"B.1.1.189",Linaje=="B.1.404"~"B.1.404",Linaje=="B.1.582"~"B.1.582",
                               Linaje=="B.1.201"~"B.1.201",Linaje=="B.1.206"~"B.1.206",Linaje=="B.1.229"~"B.1.229",Linaje=="B.1.232"~"B.1.232",Linaje=="B.1.1.329"~"B.1.1.329",Linaje=="B.1.1.207"~"B.1.1.207",
                               Linaje=="B.1.239"~"B.1.239",Linaje=="B.1.241"~"B.1.241",Linaje=="B.1.245"~"B.1.245",Linaje=="B.1.267"~"B.1.267",
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
                               Linaje=="AY.43" | Linaje=="AY.43.1" | Linaje=="AY.43.2" | Linaje=="AY.43.3" | Linaje=="AY.43.4" | Linaje=="AY.39"|
                               Linaje=="AY.44" | Linaje=="AY.45"| Linaje=="AY.46"| Linaje=="AY.46.1" | Linaje=="AY.46.2" | Linaje=="AY.46.3" |
                               Linaje=="AY.46.4" | Linaje=="AY.46.5" | Linaje=="AY.46.6" | Linaje=="AY.47" | Linaje=="AY.48" | Linaje=="AY.49" |
                               Linaje=="AY.50" | Linaje=="AY.51" | Linaje=="AY.52" | Linaje=="AY.53" | Linaje=="AY.54" | Linaje=="AY.55" |
                               Linaje=="AY.56" | Linaje=="AY.57" | Linaje=="AY.58" | Linaje=="AY.59" | Linaje=="AY.60" | Linaje=="AY.61" |
                               Linaje=="AY.62" | Linaje=="AY.63" | Linaje=="AY.64" | Linaje=="AY.65" | Linaje=="AY.66" | Linaje=="AY.67" |
                               Linaje=="AY.68" | Linaje=="AY.69" | Linaje=="AY.70" | Linaje=="AY.71" | Linaje=="AY.72" | Linaje=="AY.73" | Linaje=="AY.113"|
                               Linaje=="AY.74" | Linaje=="AY.75" | Linaje=="AY.75.2" | Linaje=="AY.75.3" | Linaje=="AY.76" |Linaje=="AY.77" |
                               Linaje=="AY.78" | Linaje=="AY.79" | Linaje=="AY.80" | Linaje=="AY.81" | Linaje=="AY.82" | Linaje=="AY.83" |
                               Linaje=="AY.84" | Linaje=="AY.85" | Linaje=="AY.86" | Linaje=="AY.87" | Linaje=="AY.88" | Linaje=="AY.89" |
                               Linaje=="AY.90" | Linaje=="AY.91" | Linaje=="AY.91.1" | Linaje=="AY.92" | Linaje=="AY.93" | Linaje=="AY.94" |
                               Linaje=="AY.95" | Linaje=="AY.96" | Linaje=="AY.97" | Linaje=="AY.98" | Linaje=="AY.98.1" | Linaje=="AY.99" |
                               Linaje=="AY.99.1" | Linaje=="AY.99.2" | Linaje=="AY.100" | Linaje=="AY.101" | Linaje=="AY.102" |
                               Linaje=="AY.103" | Linaje=="AY.104" | Linaje=="AY.105" | Linaje=="AY.106" | Linaje=="AY.107" | Linaje=="AY.108" |
                               Linaje=="AY.109" | Linaje=="AY.110" | Linaje=="AY.111" | Linaje=="AY.112"| Linaje=="AY.113" | Linaje=="AY.114" |
                               Linaje=="AY.115" | Linaje=="AY.116" | Linaje=="AY.116.1" | Linaje=="AY.117" | Linaje=="AY.118" |
                               Linaje=="AY.119" | Linaje=="AY.120" | Linaje=="AY.120.1" | Linaje=="AY.120.2" | Linaje=="AY.120.2.1"|
                               Linaje=="AY.121" | Linaje=="AY.121.1"| Linaje=="AY.122" | Linaje=="AY.122.1" | Linaje=="AY.123" | Linaje=="AY.124" | 
                               Linaje=="AY.125" | Linaje=="AY.126" | Linaje=="AY.116.1" | Linaje=="AY.119.1" | Linaje=="AY.119.2" | Linaje=="AY.122.4" | Linaje=="AY.124.1.1" | 
                               Linaje=="AY.25.1" | Linaje=="AY.3" | Linaje=="AY.3.3" | Linaje=="AY.4.10" | Linaje=="AY.43.8"~"Delta",
                               Linaje=="P.1" | Linaje=="P.1.1" | Linaje=="P.1.2" |   Linaje=="P.1.3" | Linaje=="P.1.4" | Linaje=="P.1.5" | Linaje =="P.1.6" | Linaje =="P.1.7" | 
                               Linaje=="P.1.7.1" | Linaje=="P.1.8" | Linaje=="P.1.9"| Linaje=="P.1.10" | Linaje=="P.1.10.1" | Linaje=="P.1.10.2" | Linaje=="P.1.11" | Linaje=="P.1.12" |
                               Linaje=="P.1.12.1" | Linaje=="P.1.13"| Linaje=="P.1.14" | Linaje=="P.1.15" | Linaje=="P.1.17.1"~ "Gamma",
                               Linaje=="BA.1"~"BA.1", Linaje=="BA.1.1"~"BA.1.1", Linaje=="BA.1.1.1"~"BA.1.1.1",Linaje=="BA.1.1.7"~"BA.1.1.7",
                               Linaje=="BA.1.1.9"~"BA.1.1.9",Linaje=="BA.1.1.10"~"BA.1.1.10",Linaje=="BA.1.1.14"~"BA.1.1.14",
                               Linaje=="BA.1.1.15"~"BA.1.1.15",Linaje=="BA.1.1.16"~"BA.1.1.16",Linaje=="BA.1.1.18"~"BA.1.1.18",Linaje=="BA.1.1.2"~"BA.1.1.2",
                               Linaje=="BA.1.13"~"BA.1.13",Linaje=="BA.1.14"~"BA.1.14",Linaje=="BA.1.14.1"~"BA.1.14.1",Linaje=="BA.1.14.2"~"BA.1.14.2", Linaje=="B.1.632"~"B.1.632",
                               Linaje=="BA.1.15"~"BA.1.15",Linaje=="BA.1.15.1"~"BA.1.15.1",Linaje=="BA.1.15.2"~"BA.1.15.2",Linaje=="BA.1.15.3"~"BA.1.15.3", Linaje=="BA.1.16"~"BA.1.16",
                               Linaje=="BA.1.17"~"BA.1.17", Linaje=="BA.1.17.2"~"BA.1.17.2",  Linaje=="BA.1.18"~"BA.1.18",Linaje=="BA.1.19"~"BA.1.19", 
                               Linaje=="BA.2.12.1"~"BA.2.12.1", Linaje=="BA.1.20"~"BA.1.20", Linaje=="BA.1.21"~"BA.1.21",  
                               Linaje=="BA.1.23"~"BA.1.23",Linaje=="BA.1.5"~"BA.1.5",Linaje=="BA.1.6"~"BA.1.6",Linaje=="BA.1.7"~"BA.1.7",
                               Linaje=="BA.2"~"BA.2", Linaje=="BA.2"~"BA.2",  Linaje=="BA.2.1"~"BA.2.1",  Linaje=="BA.2.10"~"BA.2.10", Linaje=="BA.2.10.1"~"BA.2.10.1",Linaje=="BA.2.12"~"BA.2.12",
                               Linaje=="BA.2.12"~"BA.2.12",Linaje=="BA.2.12.1"~"BA.2.12.1",Linaje=="BA.2.13"~"BA.2.13", Linaje=="BA.2.13"~"BA.2.13",Linaje=="BA.2.18"~"BA.2.18",Linaje=="BA.2.2"~"BA.2.2",Linaje=="BA.2.21"~"BA.2.21",
                               Linaje=="BA.2.22"~"BA.2.22",Linaje=="BA.2.23"~"BA.2.23", Linaje=="BA.2.23.1"~"BA.2.23.1",Linaje=="BA.2.4"~"BA.2.24", Linaje=="BA.2.3"~"BA.2.3",Linaje=="BA.2.3.10"~"BA.2.3.10",Linaje=="BA.2.3.15"~"BA.2.3.15",
                               Linaje=="BA.2.3.17"~"BA.2.3.17", Linaje=="BA.2.3.2"~"BA.2.3.2",Linaje=="BA.2.3.4"~"BA.2.3.4", Linaje=="BA.2.3.6"~"BA.2.3.6",
                               Linaje=="BA.2.29"~"BA.2.29",Linaje=="BA.2.31"~"BA.2.31",Linaje=="BA.2.35"~"BA.2.35",Linaje=="BA.2.36"~"BA.2.36",Linaje=="BA.2.37"~"BA.2.37",Linaje=="BA.2.38"~"BA.2.38",
                               Linaje=="BA.2.4"~"BA.2.4",Linaje=="BA.2.40"~"BA.2.40", Linaje=="BA.2.40.1"~"BA.2.40.1",Linaje=="BA.2.44"~"BA.2.44",Linaje=="BA.2.49"~"BA.2.49",Linaje=="BA.2.52"~"BA.2.52",Linaje=="BA.2.56"~"BA.2.56",
                               Linaje=="BA.2.57"~"BA.2.57",Linaje=="BA.2.6"~"BA.2.6", Linaje=="BA.2.62"~"BA.2.62",Linaje=="BA.2.65"~"BA.2.65",Linaje=="BA.2.66"~"BA.2.66",Linaje=="BA.2.71"~"BA.2.71", Linaje=="BA.2.72"~"BA.2.72",Linaje=="BA.2.8"~"BA.2.8",
                               Linaje=="BA.2.9"~"BA.2.9",Linaje=="BA.2.9.2"~"BA.2.9.2",Linaje=="BA.2.9.3"~"BA.2.9.3",Linaje=="BA.4"~"BA.4",Linaje=="BA.1.15.2"~"BA.1.15.2",
                               Linaje=="BA.4.1"~"BA.4.1",Linaje=="BA.4.1.1"~"BA.4.1.1", Linaje=="BA.4.2"~"BA.4.2",Linaje=="BA.4.4"~"BA.4.4",Linaje=="BA.4.6"~"BA.4.6",Linaje=="BA.5"~"BA.5",Linaje=="BA.5.1"~"BA.5.1",
                               Linaje=="BA.5.1.1"~"BA.5.1.1",Linaje=="BA.5.1.2"~"BA.5.1.2",
                               Linaje=="BA.5.1.3"~"BA.5.1.3",Linaje=="BA.5.1.4"~"BA.5.1.4",Linaje=="BA.5.2"~"BA.5.2",Linaje=="B.1.526"~"B.1.526",Linaje=="BA.1.20"~"BA.1.20", Linaje=="BA.2.38"~"BA.2.38",
                               Linaje=="BA.5.2.1"~"BA.5.2.1",Linaje=="BA.5.3"~"BA.5.3",Linaje=="BA.5.3.1"~"BA.5.3.1",Linaje=="BA.5.2.3"~"BA.5.2.3",Linaje=="BA.5.2.4"~"BA.5.2.4",Linaje=="BA.5.5"~"BA.5.5",Linaje=="BA.5.6"~"BA.5.6",Linaje=="BB.2"~"BB.2", 
                               Linaje=="BE.1"~"BE.1", Linaje=="BE.1.1"~"BE.1.1",Linaje=="BE.2"~"BE.2",Linaje=="BE.3"~"BE.3",Linaje=="BF.1"~"BF.1",Linaje=="BF.4"~"BF.4",
                               Linaje=="BF.5"~"BF.5",Linaje=="BG.1"~"BG.1",Linaje=="BG.2"~"BG.2",Linaje=="BG.3"~"BG.3",Linaje=="XAF"~"XAF", Linaje=="XAG"~"XAG",Linaje=="XAH"~"XAH",Linaje=="XZ"~"XZ",Linaje=="XB"~"XB",Linaje=="C.37"~"Lambda",Linaje=="B.1.1.316"~"B.1.1.316",Linaje=="B.1.243.2"~"B.1.243.2",Linaje=="BA.5.2.9"~"BA.5.2.9",
                               Linaje=="BA.5.1.6"~"BA.5.1.6",Linaje=="BA.4.1.8"~"BA.4.1.8",Linaje=="BA.5.1.10"~"BA.5.1.10",Linaje=="BA.5.1.21"~"BA.5.1.21",Linaje=="BA.5.1.22"~"BA.5.1.22",Linaje=="BA.5.1.23"~"BA.5.1.23",Linaje=="BA.5.1.25"~"BA.5.1.25",Linaje=="BA.5.1.6"~"BA.5.1.6",Linaje=="BA.5.1.7"~"BA.5.1.7",
                               Linaje=="BA.5.10"~"BA.5.10",Linaje=="BA.5.2.22"~"BA.5.2.22",Linaje=="BA.5.2.24"~"BA.5.2.24",Linaje=="BA.5.2.9"~"BA.5.2.9",Linaje=="BA.5.6.1"~"BA.5.6.1",Linaje=="BA.5.6.2"~"BA.5.6.2",Linaje=="BA.5.6.2"~"BA.5.6.2",Linaje=="BF.10"~"BF.10",Linaje=="BF.11"~"BF.11",Linaje=="BF.26"~"BF.2",
                               Linaje=="BF.8"~"BF.8",Linaje=="BK.1"~"BK.1",Linaje=="BW.1"~"BW.1",Linaje=="XAS"~"XAS",
                               TRUE ~ "Other"))



props_df_1 <- metadata_covid %>% group_by(Semana_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
props_df_2 <- props_df_1 %>% ungroup() %>% drop_na() %>% complete(Semana_recoleccion, linajeOMS, fill = list(`n()` = 0))
props_df_2[is.na(props_df_2)] <- 0
#Filtamos
filter2<-filter(props_df_2, percentage>1)
filter3<- filter2 %>% ungroup() %>% drop_na() %>% complete(Semana_recoleccion, linajeOMS, fill = list(`n()` = 0))
filter3[is.na(filter3)] <- 0

#color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.1.1.8"="peru", "BA.1.14"="azure4", "BA.1.15"="magenta3", "BA.1.17"="purple","BA.1.17.2"="orchid4","BA.1.20"="sienna3", "BA.1.21"= "red2", "BA.2"="yellowgreen", "BA.2.1"="antiquewhite", "BA.2.12"="indianred4", "BA.2.13"="honeydew", "BA.2.3"="darkseagreen3","BA.2.3"="red", "BA.2.9"="pink", "BA.4"="gainsboro", "BA.5"="lightgreen","Delta"="darkgoldenrod", "Other"="slateblue4")
#Año 2022
#color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.1.1.1"="skyblue", "BA.1.1.10"="peru","BA.1.1.14"="azure4","BA.1.1.15"="magenta3","BA.1.1.16"="darkmagenta","BA.1.17"="purple","BA.1.17.2"="orchid4","BA.1.1.18"="darkolivegreen", "BA.1.1.2"="darkorange4","BA.1.1.2"="firebrick1","BA.1.1.6"="darkorchid2", "BA.1.13"="darkorange","BA.1.14"="forestgreen","BA.1.14.1"="gold4","BA.1.14.2"="greenyellow","BA.1.14.2"="lightblue4","BA.1.15"="lightgoldenrod4","BA.1.15.1"="lightpink3", "BA.1.15.2"="mediumvioletred", "BA.1.15.3"="navajowhite4", "BA.1.16"="olivedrab","BA.1.17"="navy","BA.1.17.2"="orange3","BA.1.18"="powderblue","BA.1.19"="purple4","BA.1.20"="dodgerblue4","BA.1.21"="darkslateblue", "BA.1.21"="darkred","BA.1.6"="cyan4","BA.2"="yellow","BA.2.1"="steelblue","BA.2.10"="yellowgreen","BA.2.10.1"="tan","BA.2.12"="slategray1","BA.2.12"="violetred","BA.2.12.1"="seagreen2","BA.2.13"="salmon", "BA.2.18"="cornsilk2","BA.2.2"="darkcyan","BA.2.21"="darkolivegreen3","BA.2.22"="darkslategrey","BA.2.23"="darkseagreen","BA.2.23"="cadetblue","BA.2.23"="darkgoldenrod1","BA.2.23.1"="deepskyblue","BA.2.26"="lightgoldenrodyellow","BA.2.3"="lightgrey","BA.2.3.10"="aquamarine2","BA.2.3.11"="bisque2","BA.2.3.15"="blue","BA.2.3.17"="brown1","BA.2.3.2"="burlywood","BA.2.3.4"="chartreuse4","BA.2.3.6"="chocolate4","BA.2.31"="cornflowerblue", "BA.2.36"="chartreuse3","BA.2.37"="darkorchid2","BA.2.38"="gray67","BA.2.4"="firebrick4","BA.2.40"="darkorchid1","BA.2.40.1"="darkred","BA.2.49"="darkslategray","BA.2.52"="darkviolet","BA.2.52"="palevioletred2","BA.2.56"="seagreen","BA.2.57"="tan4","BA.2.6"="slateblue","BA.2.62"="turquoise","BA.2.65"="wheat","BA.2.66"="steelblue","BA.2.71"="yellowgreen","BA.2.72"="tan","BA.2.8"="snow4","BA.2.9"="thistle2","BA.2.9.3"="seagreen2","BA.4"="peachpuff4", "BA.4.1"="palevioletred","BA.4.1.1"="palegreen1","BA.5"="orchid4","BA.5.1"="powderblue","BA.5.2"="olivedrab1","BA.5.2.1"="maroon3","BA.5.3"="lightsteelblue1", "BA.5.3.1"="mistyrose4","BA.5.5"="navyblue","BA.5.6"="darkseagreen1","BB.2"="orange","BE.1"="palegreen","BF.1"="paleturquoise1","BG.1"="rosybrown1","BG.2"="springgreen3", "BA.1.1.7"="dimgray","BA.1.1.9"="forestgreen","BA.1.23"="goldenrod3","BA.1.5"="darkslategray4", "BA.1.7"="darkviolet","BA.2.24"="burlywood","BA.2.29"="darkolivegreen4", "BA.2.35"="lightblue","BA.2.44"="lightgoldenrod","BA.2.9.2"="lightgray","BA.4.2"="lightsalmon","BA.4.4"="mediumvioletred","BA.4.6"="olivedrab4","BA.5.1.1"="red3","BA.5.1.2"="royalblue3","BA.5.1.3"="seagreen1","BA.5.1.4"="skyblue1","BA.5.2.3"="seashell4","BA.5.2.4"="tomato4","BA.5.6"="turquoise","BE.1.1"="violet","BE.2"="springgreen4","BE.3"="yellow2","BF.1"="yellowgreen","BF.4"="tan2","BF.5"="wheat4","BG.1"="tomato1","BG.2"="thistle4","BG.3"="palegreen3","XAF"="saddlebrown","XAG"="lightslateblue","XAH"="lightcoral","XZ"="lavenderblush4","Delta"="darkgoldenrod", "Other"="slateblue4")

#Sacar los colores
#table(filter2$linajeOMS)
#Colores
#color=c("BA.1"="#1c2541","BA.1.1"="#3a506b","BA.1.1.18"="#5bc0be", "BA.1.15"="#6fffe9", "BA.1.15.2"="#ff8811", "BA.1.17"="#f4d06f","BA.1.17.2"="#fff8f0","BA.1.18"="#9dd9d2","BA.1.20"="#eeebd3","BA.2"="#F04C05","BA.2.10"="#B74211","BA.2.12.1"= "azure4","BA.2.65"="firebrick1","BA.2.18"="#7E391D","BA.2.3" ="#442F29","BA.2.37"="#0B2535", "BA.2.38"="#15640F","BA.2.56"="#4A5414","BA.2.9"="#7F4419","BA.4"="#B4331E","BA.4.1"="#E92323", "BA.4.2"="#E92323","BA.4.4"="#EAAD12","BA.5"="#B3A647","BA.5.1"="#7BA07C","BA.5.1.1"="darkolivegreen","BA.5.1.3"="mediumvioletred","BA.5.2"="navy", "BA.5.2.1"="darkslateblue","BA.5.3.1"="#4499B0","BA.5.5"="#0C92E5","BA.5.6"="#0C92E5", "BE.1"="#0C92E5","BE.2"="#0C92E5","BE.3"="#0C92E5","BF.1"="#0C92E5","Delta"="#5BF30A","Other"="#60B835")
#Color primera ola
#color=c("A"="firebrick4","A.1"="gold4","A.2"="darkturquoise","A.5"="brown", "B"="cornflowerblue","B.1"="aquamarine1","B.1.1"="burlywood4","B.1.1.1"="lightcoral","B.1.1.10"="hotpink3","B.1.1.122"="limegreen","B.1.1.222"="tan1",  "B.1.1.228"="yellow","B.1.1.244"="wheat4","B.1.1.28"="slateblue4","B.1.1.285"="sienna2","B.1.1.322"="slategrey","B.1.1.33"="lightsalmon2","B.1.1.344"="lawngreen","B.1.1.403"="darksalmon","B.1.1.432"="aquamarine","B.1.1.512"="darkgoldenrod1", "B.1.1.518"="blueviolet","B.1.1.519"="olivedrab", "B.1.1.71"="orangered2","B.1.1.8"="rosybrown2","B.1.1.93"="olivedrab1","B.1.111"="lightyellow1","B.1.147"="lightseagreen","B.1.189"="lightgray","B.1.2"="hotpink4","B.1.201"="darkslategray1","B.1.206"="gainsboro","B.1.229"="dimgrey","B.1.232"="deepskyblue2","B.1.239"="chartreuse3", "B.1.241"="burlywood1","B.1.243"="tan","B.1.245"="lightskyblue","B.1.267"="steelblue2", "B.1.280"="plum3","B.1.320"="lightgoldenrodyellow","B.1.324"="deeppink","B.1.366"="darkviolet","B.1.369"="lavenderblush4","B.1.396"="lavender","B.1.397"="darkorange","B.1.399"="azure2","B.1.400"="lavenderblush4","B.1.405"="darkorange1" ,"B.1.415"="floralwhite","B.1.427"="darkturquoise","B.1.429"="gray79","B.1.499"="khaki2","B.1.507"="lightskyblue3","B.1.533"="mediumvioletred", "B.1.551"="lightsalmon","B.1.558"="burlywood ","B.1.561"="navajowhite4","B.1.567"="orangered2","B.1.572"="aliceblue","B.1.609"="gray33","C.23"="ivory4", "Delta"="#5BF30A","Other"="#60B835")
#Color segunda ola
#color=c("A"="firebrick4","A.1"="gold4","A.2"="darkturquoise","A.5"="brown", "B"="cornflowerblue","B.1"="aquamarine1","B.1.1"="burlywood4","B.1.1.1"="lightcoral","B.1.1.10"="hotpink3","B.1.1.122"="limegreen","B.1.1.222"="tan1","Alpha"="dodgerblue1", "Gamma"="indianred1", "B.1.1.228"="yellow","B.1.1.244"="wheat4","B.1.1.28"="slateblue4","B.1.1.285"="sienna2","B.1.1.322"="slategrey","B.1.1.33"="lightsalmon2","B.1.1.344"="lawngreen","B.1.1.403"="darksalmon","B.1.1.432"="aquamarine","B.1.1.512"="darkgoldenrod1", "B.1.1.518"="blueviolet","B.1.1.519"="olivedrab", "B.1.1.71"="orangered2","B.1.1.8"="rosybrown2","B.1.1.93"="olivedrab1","B.1.111"="lightyellow1","B.1.147"="lightseagreen","B.1.189"="lightgray","B.1.2"="hotpink4","B.1.201"="darkslategray1","B.1.206"="gainsboro","B.1.229"="dimgrey","B.1.232"="deepskyblue2","B.1.239"="chartreuse3", "B.1.241"="burlywood1","B.1.243"="tan","B.1.245"="lightskyblue","B.1.267"="steelblue2", "B.1.280"="plum3","B.1.320"="lightgoldenrodyellow","B.1.324"="deeppink","B.1.366"="darkviolet","B.1.369"="lavenderblush4","B.1.396"="lavender","B.1.397"="darkorange","B.1.399"="azure2","B.1.400"="lavenderblush4","B.1.405"="darkorange1" ,"B.1.415"="floralwhite","B.1.427"="darkturquoise","B.1.429"="gray79","B.1.499"="khaki2","B.1.507"="lightskyblue3","B.1.533"="mediumvioletred", "B.1.551"="lightsalmon","B.1.558"="burlywood ","B.1.561"="navajowhite4","B.1.567"="orangered2","B.1.572"="aliceblue","B.1.609"="gray33","C.23"="ivory4", "B.1.1.189"="lightgray", "BA.1.1"="skyblue","B.1.1.316"="floralwhite","B.1.1.329"="firebrick2","B.1.404"="gold1","B.1.595"="darkslategray","Lambda"="chocolate4","B.1.1.434"="gray1","B.1.240"="hotpink4","B.1.576"="lightsalmon1","B.1.582"="lightsteelblue","B.1.1.207"="mediumaquamarine","B.1.36.31"="orangered2","B.1.588"="azure4","B.1.621"="cornsilk3","B.1.1.517"="cadetblue3","B.1.634"="aliceblue","XB"="darkcyan","Delta"="#5BF30A","Other"="#60B835")
#Color tercera ola 
#color=c("B"="cornflowerblue","B.1.1.122"="limegreen","B.1.1.519"="olivedrab","B.1.243.2"="darkslategray3", "XB"="darkcyan", "B.1.526"="lavenderblush2","B.1.621"="cornsilk3","B.1.632"="lightpink2","B.1.634"="aliceblue","Alpha"="dodgerblue1","Gamma"="indianred1", "Lambda"="chocolate4","Delta"="#5BF30A","BA.1"="lightslateblue","BA.1.1"="skyblue","BA.1.15"="magenta3","Other"="#60B835")
#Color cuarta ola 
#color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.1.1.18"="#5bc0be",  "BA.1.15"="magenta3", "BA.1.15.2"="lightcoral","BA.1.17"="purple","BA.1.17.2"="orchid4", "BA.1.17.2"="orchid4","BA.1.18"="powderblue","BA.1.20"="sienna3", "BA.2.12.1"= "azure4", "BA.2"="yellowgreen", "BA.2.1"="antiquewhite", "BA.2.3"="darkseagreen3", "BA.2.37"="darkorchid2","BA.2.65"="lightcyan3","BA.2.9"="pink", "Delta"="#5BF30A", "Other"="#60B835")
#Color cuarta quinta ola
#color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.2"="yellowgreen", "BA.2.10"="#B74211", "BA.2.12.1"= "azure4","BA.2.18"="#7E391D",  "BA.2.1"="antiquewhite", "BA.2.3"="darkseagreen3", "BA.2.37"="darkorchid2","BA.2.38"="ivory3","BA.2.65"="lightcyan3","BA.2.9"="pink","BA.4"="peachpuff4", "BA.4.1"="palevioletred","BA.4.2"="lightsalmon","BA.4.4"="mediumvioletred","BA.5"="orchid4","BA.5.1"="powderblue", "BA.5.1.1"="red3","BA.5.1.3"="seagreen1","BA.5.2"="olivedrab1","BA.5.2.1"="maroon3","BA.5.3"="lightsteelblue1", "BA.5.3.1"="mistyrose4","BA.5.5"="navyblue","BA.5.6"="darkseagreen1","BE.1"="palegreen","BF.1"="paleturquoise1","BG.1"="rosybrown1","BE.2"="springgreen4","BE.3"="yellow2","BF.1"="yellowgreen")
color=c("BA.1"="lightslateblue", "BA.1.1"="skyblue", "BA.2"="yellowgreen", "BA.2.10"="#B74211", "BA.2.12.1"= "azure4","BA.2.18"="#7E391D",  "BA.2.1"="antiquewhite", "BA.2.3"="darkseagreen3", "BA.2.37"="darkorchid2","BA.2.38"="ivory3","BA.2.65"="lightcyan3","BA.2.9"="pink","BA.4"="peachpuff4", "BA.4.1"="palevioletred","BA.4.2"="lightsalmon","BA.4.4"="mediumvioletred","BA.5"="orchid4","BA.5.1"="powderblue", "BA.5.1.1"="red3","BA.5.1.3"="seagreen1","BA.5.1.6"="green4","BA.5.1.23"="hotpink2","BA.5.1.25"="indianred","BA.5.1.22"="mediumorchid4","BA.5.2.24"="wheat","BA.5.2"="olivedrab1","BA.5.2.1"="maroon3","BA.5.2.24"="brown4","BA.5.3"="lightsteelblue1", "BA.5.3.1"="mistyrose4","BA.5.5"="navyblue","BA.5.6"="darkseagreen1","BA.5.6.2"="coral4", "BE.1"="palegreen","BF.1"="paleturquoise1","BG.1"="rosybrown1","BE.2"="springgreen4","BE.3"="yellow2","BA.5.2.1"="mistyrose4","BW.1"="olivedrab4","BA.5.2.9"="plum4","BF.1"="yellowgreen", "BF.8"="yellow2", "BA.5.1.6"="darkblue", "BF.8"="seagreen1", "BF.10"="darkgoldenrod4", "BF.1"="cornflowerblue", "Other"="cornsilk2", "BA.4.1.8"="mediumturquoise","XAS"="cyan2")
#Grafico de barras 
pdf(file = "Grafica_barra_sarccov2.pdf")
ggplot(filter2, aes(Semana_recoleccion,n, fill= linajeOMS))+geom_bar(position ="fill",stat = "identity")+scale_fill_manual(values = color)
dev.off()

##Grafico de densidad
pdf(file = "Grafica_densidad_sarccov2.pdf")
ggplot(filter3, aes(Semana_recoleccion,n, fill= linajeOMS))+geom_density(position ="fill",stat = "identity")+labs(fill = "Pango lineage/WHO VOC", y = "Porcentajes de secuencias") +theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "plain"), legend.position = "bottom", axis.title.x = element_blank()) +scale_x_date(date_breaks = "1 week", date_labels = "%d/%m")+scale_fill_manual(values = color)
dev.off()


#Fitrado por zona geografica 
Northeast<-c("Coahuila", "Nuevo Leon","Tamaulipas")
Northeast2<-filter(metadata_covid, Estado %in% Northeast)
Northeast3 <- Northeast2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Northeast4 <- Northeast3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Northeast4[is.na(Northeast4)] <- 0
Northeast4<-filter(Northeast4, percentage >1)
pdf(file = "Northeast.pdf")
#Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +ggtitle("Northeast")+theme(legend.text = element_text(size=5))
Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noreste Mexico")+theme_void()+scale_fill_manual(values = color)
Northeast5
#Northeast5<-ggplot(Northeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noreste Mexico")+theme_light()+scale_fill_manual(values = color)
dev.off()




Northwest <-c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sonora", "Sinaloa")
Northwest2<-filter(metadata_covid, Estado %in% Northwest)
Northwest3 <- Northwest2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Northwest4 <- Northwest3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Northwest4[is.na(Northwest4)] <- 0
Northwest4<-filter(Northwest4, percentage >1)
pdf(file = "Northwest.pdf")
#Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_fill_manual(values = color)+ ggtitle("Northwest")+theme(legend.text = element_text(size=5))
Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noroeste Mexico")+theme_void()+scale_fill_manual(values =color)
#Northwest5<-ggplot(Northwest4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Noroeste Mexico")+theme_light()+scale_fill_manual(values = color)
Northwest5
dev.off()


Central_North<-c("Aguascalientes","Guanajuato", "Querétaro", "San Luis Potosí","Zacatecas")
Central_North2<-filter(metadata_covid,, Estado %in% Central_North)
Central_North3 <- Central_North2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Central_North4 <- Northwest3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Central_North4[is.na(Central_North4)] <- 0
Central_North4<-filter(Central_North4, percentage >1)
pdf(file = "Central_North.pdf")
#Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Norte Mexico")+theme_void()+scale_fill_manual(values = color)
Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Norte")+theme_void()+scale_fill_manual(values = color)
#Central_North5<-ggplot(Central_North4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Norte Mexico")+theme_light()+scale_fill_manual(values = color)
Central_North5
dev.off()

Central_South<- c("Mexico City", "State of Mexico", "Morelos", "Hidalgo", "Puebla","Tlaxcala")
Central_South2<-filter(metadata_covid,, Estado %in% Central_South)
Central_South3 <- Central_South2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Central_South4 <- Central_South3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Central_South4[is.na(Central_South4)] <- 0
Central_South4<-filter(Central_South4, percentage > 1)
pdf(file = "Central_South.pdf")
#Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = color)+ggtitle("Central_South")+theme(legend.text = element_text(size=5))
Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Sur")+theme_void()+scale_fill_manual(values = color)
#Central_South5<-ggplot(Central_South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Centro Sur")+theme_light()+scale_fill_manual(values = color)
Central_South5
dev.off()

West<-c("Colima", "Jalisco", "Michoacan", "Nayarit")
West2<-filter(metadata_covid,, Estado %in% West)
West3 <- West2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
West4 <- West3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
West4[is.na(West4)] <- 0
West4<-filter(West4, percentage > 1)
pdf(file = "West.pdf")
#West5<-ggplot(West4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_fill_manual(values = color)+ ggtitle("West")+theme(legend.text = element_text(size=5))
West5<-ggplot(West4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Oeste Mexico")+theme_void()+scale_fill_manual(values = color)
#West5<-ggplot(West4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Oeste Mexico")+theme_light()+scale_fill_manual(values = color)
West5
dev.off()



Southeast<-c("Guerrero", "Oaxaca", "Chiapas", "Veracruz", "Tabasco")
Southeast2<-filter(metadata_covid,, Estado %in% Southeast)
Southeast3 <- Southeast2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
Southeast4 <- Southeast3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
Southeast4[is.na(Southeast4)] <- 0
Southeast4<-filter(Southeast4, percentage > 1)
pdf(file = "Southeast.pdf")
#Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = color)+ggtitle("Southeast")+theme(legend.text = element_text(size=5))
Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sureste Mexico")+theme_void()+scale_fill_manual(values = color)
#Southeast5<-ggplot(Southeast4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sureste Mexico")+theme_light()+scale_fill_manual(values = color)
Southeast5
dev.off()


South<-c("Campeche","Yucatan", "Quintana Roo")
South2<-filter(metadata_covid,, Estado %in% South)
South3 <- South2 %>% group_by(Mes_recoleccion, linajeOMS) %>%
  summarise(n = n()) %>% mutate(percentage = n/sum(n)*100)
South4 <-South3 %>% ungroup() %>% drop_na() %>% complete(Mes_recoleccion, linajeOMS, fill = list(`n()` = 0))
South4[is.na(South4)] <- 0
South4<-filter(South4, percentage > 1)
pdf(file = "South.pdf")
#South5<-ggplot(South4, aes(Mes_recoleccion, n, fill = linajeOMS)) + geom_bar(stat = "identity") + theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_manual(values = color)+ggtitle("South")+theme(legend.text = element_text(size=5))
South5<-ggplot(South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sur Mexico")+theme_void()+scale_fill_manual(values = color)
#South5<-ggplot(South4, aes(Mes_recoleccion, percentage, fill = linajeOMS))+geom_bar(stat="identity")+ coord_polar(start = 0)+ ggtitle("Sur Mexico")+theme_light()+scale_fill_manual(values = color)
South5
dev.off()

pdf(file = "Areas_geograficas.pdf")
ggarrange(Northeast5,Northwest5,Central_North5,Central_South5,West5,Southeast5,South5, common.legend = TRUE, legend="right")
dev.off()





