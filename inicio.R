library(tidyverse)
library(rio)
library(ggplot2)
library(dplyr)
library(ggThemeAssist)
library(tidyquant)
library(dygraphs)
library(rworldmap)
library(maps)
library(ggmap)
library(viridis)
library(hrbrthemes)
library(png)
install.packages("png")
#dataframes
df <- rio::import("./datos/global.1751_2014v2.csv")
df2 <- rio::import("./datos/nation.1751_2014v2.csv")
df4 <- rio::import("./datos/co_emissions_per_capita.csv")
df5 <- rio::import("./datos/greenhouse-gas-emissions-by-sector.csv")
df6 <- rio::import("./datos/annual-co-emissions-by-region.csv")


#borramos las filas que no nos interesan.Las que contiene el autor de la base de datos.

df2 <- df2[-c(1, 2), ]

#cambiar nombres, escribimos todos los nombres de las columnas
df3 <- df2 #copia

names(df3) <- c("Nation","Year","Total CO2 Emissions","Solid fuel","Liquid fuel","Gas fuel","Cement production","Gas flaring","Per Capita CO2","Bunker fuels")

names(df) <- c("Year","Total CO2 Emissions","Gas fuel","Liquid fuel","Solid fuel","Cement production","Gas flaring","Per Capita CO2")

#emisiones globales acumuladas por region

list_of_values2 <- c("EU-28","Europe (other)","United States","Africa","Middle East","India","China","Asia and Pacific (other)")
Global <- filter(df6,`Entity` %in% list_of_values2)

Global <- select(Global,1,3:4)

names(Global) <- c("Region","Año","Emisiones")


ggplot(Global,aes(x = Año, y = Emisiones/1000000,fill=Region))  +geom_line(color="dark") + theme_minimal()+ geom_area(alpha=0.6 , size=.5, colour="white") +scale_fill_viridis(discrete = T) + theme_ipsum() + ggtitle("Emisiones Totales por Región(en millones de toneladas")

list_of_values3 <- c("Germany","Spain","Italy","United Kingdom","France","Netherlands")
Paises <- filter(df6,`Entity` %in% list_of_values3)

Paises <- select(Paises,1,3:4)

names(Paises) <- c("Region","Año","Emisiones")

ggplot(Paises) + geom_line(aes(Año, Emisiones, color = Region)) + facet_wrap(vars(Region), nrow = 2, ncol = 3 )


# Emisiones de CO2 por sectores en España (1990-2010)

los6<- df5 %>% filter (Entity == "Spain") %>% filter(Year>1989) %>% filter(Year<2011) %>% select(-c(1:2,10:11))
names(los6) <- c("Año","Otros","Basura","Industria","Hogares y Comercios","Transporte","Agricultura","Energia")

dygraph(los6,main = "Emisiones CO2 x SECTORES") %>% dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector() %>% dyLegend(width = 400, show="follow")


#Emisiones en españa por consumo

espana <- df3 %>% filter(Nation == "SPAIN")

#deja de mostrar los que no nos interesa en este momento(columnas)

espana <- select(espana,-c(`Gas flaring`,`Bunker fuels`,`Per Capita CO2`))

espana <- select(espana,-c(Nation))

#Hay que hacer los datos Tidy

espana <- gather(espana,TipoEmi,CantCO2,2:6)

#hacemos el gráfico

Emis_ends <- espana %>% group_by(`TipoEmi`) %>% top_n(1, Year) %>% pull(`CantCO2`)

espana %>% ggplot(aes(x = Year, y = CantCO2, color=TipoEmi)) + geom_line(size=1)  +
labs(title = "Emisiones de CO2 en España" ,  caption = "En miles de toneladas" , x = "Año" , y = "Emisiones") + scale_y_continuous(sec.axis = sec_axis(~ ., breaks = Emis_ends)) +   #- sec_axis() especifica un eje secundario
     scale_x_continuous(expand = c(0, 0))

#Grafico de emisiones totales a nivel mundial, por tipo


CO2mundial <- df %>% filter(Year == 2014)

CO2mundial <- select(CO2mundial,-c(8))

CO2mundial <- gather(CO2mundial,TipoEmi,CantCo2,2:7)

ggplot(CO2mundial,aes(TipoEmi,CantCo2, fill=TipoEmi)) + geom_bar(stat = "identity") + coord_flip() + theme_minimal() + theme(legend.position="none") + labs(title = "EMISIONES DE CO2 EN 2014 A NIVEL GLOBAL" ,  caption = "En millones de toneladas" , x = "Cantidad CO2" , y = "Tipo de emisión")


#Emisiones en 2014 a nivel mundial.Top 10 absoluto

mundialtop <- df3 %>% filter(Year==2014)

mundialtop <- select(mundialtop, 1,3)

mundialtop <- mundialtop %>% arrange(desc(`Total CO2 Emissions`))

#mundialtop <- mundialtop %>% top_n(10, `Total CO2 Emissions`)

mundialtop <- mundialtop[c(1:10,25),]

mundialtop <- gather(mundialtop,Emision_total, CantidadCO2, 2)

ggplot(mundialtop,aes(reorder(Nation,CantidadCO2), CantidadCO2/1000,fill=Nation)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none")+coord_flip() + labs(title = "TOP EMISIONES A NIVEL GLOBAL EN 2014" ,  caption = "En millones de toneladas" , x = "PAIS" , y = "CANTIDAD DE CO2")

#Evolución de los paises estudiados anteriormente a lo largo del tiempo.

mundialevol <- df3

mundialevol <- select(mundialevol,1:3)

list_of_values2 <- c("CHINA","UNITED STATES OF AMERICA","INDIA","RUSSIAN FEDERATION","JAPAN","GERMANY","SPAIN","USSR")
mundialevol <- filter(mundialevol,`Nation` %in% list_of_values2)
mundialevol <- mundialevol %>% filter(`Year`> 1900) %>% group_by(Nation)

mundialevol <- spread(mundialevol,`Nation`,`Total CO2 Emissions`)

#mundialevol <- select(mundialevol,1,2,4)
#mundialevol <- mundialevol %>% select(`Year`, everything())

#dygraph(mundialevol,main = "Emisiones España segun tipo") %>% dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector() %>% dyLegend(width = 400)


dygraph(mundialevol,main = "Emisiones totales del Top") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE)%>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 3, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>% dyLegend(width = 500,show = "follow")


#Emisiones por emisiones pc.


mundialpc <- df3 %>% filter(Year==2014)

#transformamos Per Capita CO2 a numérico y comprobamos

mundialpc$"Per Capita CO2" <- as.numeric(as.character(mundialpc$"Per Capita CO2"))
sapply(mundialpc, mode)

mundialpc <- mundialpc %>% arrange(desc(`Per Capita CO2`))

#En condiciones normales hariamos un Top_n con los 10 primeros pero queremos añadir la posicion de España
#mundialpc <- mundialpc %>% top_n(10,`Per Capita CO2`)
mundialpc <- mundialpc[c(1:10,77),]
mundialpc <- select(mundialpc, 1,2,9)

#transformamos los datos en tidy

mundialpc <- gather(mundialpc,EmisionPC,CantidadPC,3)

mundialpc <- select(mundialpc,1,3,4)

ggplot(mundialpc,aes(reorder(Nation,CantidadPC), CantidadPC,fill=Nation)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none")+coord_flip() + labs(title = "EMISIONES DE CO2 PC EN 2014 A NIVEL GLOBAL" ,  caption = "En toneladas métricas" , x = "Cantidad CO2 PC" , y = "TOP PAISES")


#Quien contamina menos pc


mundialpcmin <- df2 %>% filter(Year==2014) %>% arrange(`Per capita CO2 emissions (metric tons of carbon)`)

mundialpcmin <- mundialpcmin %>% top_n(-20,`Per capita CO2 emissions (metric tons of carbon)`)

data.table <- mundialpcmin

#Emisiones mundiales PC 2014


World2 <- df4 %>% filter(Year==2014)
World2 <-select(World2,1,2,4)
World2 <- World2[-c(140),] #eliminamos qatar, valor anómalo.

spdf <- joinCountryData2Map(World2, joinCode="NAME", nameJoinColumn="Entity")
mapCountryData(spdf, nameColumnToPlot="Per capita COâ‚‚ emissions (tonnes per capita)", catMethod="fixedWidth")

#Emisiones Totales mapa mundial(absoluto)

Map <- df2 %>% filter(Year == 2014)

Map <- select(Map,1,3)

names(Map) <- c("Pais","Emisiones")

spdf <- joinCountryData2Map(Map, joinCode="NAME", nameJoinColumn="Pais")
mapCountryData(spdf, nameColumnToPlot="Emisiones", catMethod="fixedWidth")
install.packages(png)
library(readpng)
img1_path <- "datos/EmisionesPC.png"
