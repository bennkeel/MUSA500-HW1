library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(tmap)
library(gridExtra)
install.packages("BAMMtools")
library(BAMMtools)

#No Scientific Notation
options(scipen=999)
options(tigris_class = "sf")

#Loading methods from our sourcebook repository
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

#Loading Custom Palettes
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
paletteR <- c("#9AD2FF","#80A1F0","#706EFF","#633DB3","#4A1080")
paletteP <- c("#FFD02E","#FFB52B","#FF8D29","#FF642B","#FA3D30")
paletteD <- c("#FA3D30","#FF642B","#FF8D29","#FFB52B","#FFD02E")
paletteG <- c("#39EB78","#72F067","#C6FF50","#F6FF5B","#FFFFCB")
paletteY <- c("#7231BD","#D63A8D","#FF8052","#FFBE45","#FFF433")
paletteZ <- c("#FFD02E","#FFB52B","#FF8D29","#FF642B","#FA3D30")

#Loading Census API Key
census_api_key("3c9540be1434ac4b38e6e55d60e8ee95909f2254", overwrite = TRUE)

SD_crs = 'ESRI:102411' #This measurement-based CRS projection is for the West Coast
g <- glimpse

#Shapefile Reading
RD <- st_read("D:\\MUSA500\\HW 1\\RegressionData.shp")
st_crs(RD)
g(RD)

st_is_valid(RD)

PHL_union <- st_union(st_make_valid(RD$geometry))

#Plots
plot1 <- ggplot()+
  geom_sf(data=PHL_union, color="black", size=1.1)+
  geom_sf(data=RD, aes(fill=q5(LNMEDHVAL)), color=NA)+
  scale_fill_manual(values = palette5,
                    labels = round(assignColorBreaks(RD$LNMEDHVAL, NCOLORS=4, method="jenks"), 2), 
                    na.value="grey50",
                    name = "LN() value")+
  labs(title = "Median House Value, Normalized", 
       subtitle = "LNMEDHVAL Variable")+
  mapTheme()


plot2 <- ggplot()+
  geom_sf(data=PHL_union, color="black", size=1.1)+
  geom_sf(data=RD, aes(fill=q5(PCTVACANT)), color=NA)+
  scale_fill_manual(values = palette5,
                    labels = paste(round(assignColorBreaks(RD$PCTVACANT, NCOLORS=4, method="jenks"), 0), "%"), 
                    na.value="grey50",
                    name = "Percentage")+
  labs(title = "Vacant Units (%)", 
       subtitle = "PCTVACANT Variable")+
  mapTheme()

plot2

plot3 <- ggplot()+
  geom_sf(data=PHL_union, color="black", size=1.1)+
  geom_sf(data=RD, aes(fill=q5(PCTSINGLES)), color=NA)+
  scale_fill_manual(values = palette5,
                    labels = paste(round(assignColorBreaks(RD$PCTSINGLES, NCOLORS=4, method="jenks"), 0), "%"), 
                    na.value="grey50",
                    name = "Percentage")+
  labs(title = "Single House Units (%)", 
       subtitle = "PCTSINGLES Variable")+
  mapTheme()

plot4 <- ggplot()+
  geom_sf(data=PHL_union, color="black", size=1.1)+
  geom_sf(data=RD, aes(fill=q5(PCTBACHMOR)), color=NA)+
  scale_fill_manual(values = palette5,
                    labels = paste(round(assignColorBreaks(RD$PCTBACHMOR, NCOLORS=4, method="jenks"), 0), "%"), 
                    na.value="grey50",
                    name = "Percentage")+
  labs(title = "Population with Higher Education (%)", 
       subtitle = "PCTBACHMOR Variable (Bachelor's or Higher)")+
  mapTheme()

plot5 <- ggplot()+
  geom_sf(data=PHL_union, color="black", size=1.1)+
  geom_sf(data=RD, aes(fill=q5(NBelPov100)), color=NA)+
  scale_fill_manual(values = palette5,
                    labels = assignColorBreaks(RD$NBelPov100, NCOLORS=4, method="jenks"), 
                    na.value="grey50",
                    name = "Households (#)")+
  labs(title = "Households Below Poverty Level", 
       subtitle = "NBelPov100 Variable")+
  mapTheme()

plot5

plot1
grid.arrange(plot2, plot3, plot4, plot5)
grid.arrange(plot5, plot5.2)
