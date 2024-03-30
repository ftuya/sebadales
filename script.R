##############Data analysis: Seagrass spatio-temporal patterns####
##############Author: Fernando Tuya (January 2024) ##############
###########Asistencia técnica al Ministerio ###############

# Load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggthemes)
library(ggtext)
library(cowplot)
library(ggrepel)
library(ggstatsplot)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(mgcv)
library(performance)
library(knitr)
library(kableExtra)
library(glm2)
library(broom)

# Read ecological data from folder "data" in project and data wrangling
data = read.table("./data/datos.txt", sep="\t", header=TRUE) %>%view()
data %>% as_tibble %>% glimpse()
data$Fecha=dmy(data$Fecha) #set "Fecha" with "date" format
data %>% mutate(Fecha = ymd(Fecha)) %>% glimpse()
data$Isla = as.factor(data$Isla) #set "Isla" and "ZEC" as factors
data$ZEC = as.factor(data$ZEC)
data$Cober = as.numeric(data$Cober)  #set "Cobertura as numeric"
data$Lat = as.numeric(data$Lat)
data$Long = as.numeric(data$Long)
data = data %>% mutate_at(vars(Fecha), funs(year, month)) %>% glimpse() #let's separate each date into two columns corresponding to month and year
data$month = as.factor(data$month) 
data = data %>% mutate(Epoca = ifelse(month %in%c("5", "6", "7", "8", "9"), "crecimiento", "senescencia")) #let's create a new factor that considers months 5 to 10 as "growth" (spring and summer) and the rest as "senescence" (autumn and winter) 
str(data)
dim(data)

# Density and violin plots for response variables faceted by ZEC
data %>% drop_na(Densidad) %>%
  ggdensity(x = "Densidad",
          add = "mean", rug = TRUE,
          color = "ZEC", fill = "ZEC", alpha = 0.4) +
          xlab(expression("Densidad de haces (" ~ m^2 ~ ")")) + ylab("Función de densidad") +
          theme(text = element_text(size = 12))
ggsave("densidad.tiff")

data %>% drop_na(Densidad) %>%
  ggviolin(y = "Densidad", x = "ZEC",
            add = "mean", rug = TRUE,
            color = "ZEC", fill = "ZEC", alpha = 0.4) +
  geom_point() + geom_jitter(size = 0.8, alpha = 0.4) +
  xlab(" ") + ylab(expression("Densidad de haces (" ~ m^2 ~ ")")) +
  theme(text = element_text(size = 12))
ggsave("densidad2.tiff")

data %>% drop_na(Cober) %>%
  ggdensity(x = "Cober",
            add = "mean", rug = TRUE,
            color = "ZEC", fill = "ZEC", alpha = 0.4) +
            xlab("Cobertura (%)") + ylab("Función de densidad") +
            theme(text = element_text(size = 12))
ggsave("cobertura.tiff")

data %>% drop_na(Cober) %>%
  ggviolin(y = "Cober", x = "ZEC",
           add = "mean", rug = TRUE,
           color = "ZEC", fill = "ZEC", alpha = 0.4) +
  geom_point() + geom_jitter(size = 0.8, alpha = 0.4) +
  xlab(" ") + ylab("Cobertura (%)") +
  theme(text = element_text(size = 12))
ggsave("cobertura2.tiff")

data %>% drop_na(Longitud) %>%
  ggdensity(x = "Longitud",
            add = "mean", rug = TRUE,
            color = "ZEC", fill = "ZEC", alpha = 0.4) +
  xlab("Longitud de hoja (cm)") + ylab("Función de densidad") +
  theme(text = element_text(size = 12))
ggsave("longitud.tiff")

data %>% drop_na(Longitud) %>%
  ggviolin(y = "Longitud", x = "ZEC",
           add = "mean", rug = TRUE,
           color = "ZEC", fill = "ZEC", alpha = 0.4) +
  geom_point() + geom_jitter(size = 0.8, alpha = 0.4) +
  xlab(" ") + ylab("Longitud (cm)") +
  theme(text = element_text(size = 12))
ggsave("Longitud2.tiff")

# Density plot for the depth of meadows
data %>% drop_na(Profundidad) %>%
  ggdensity(x = "Profundidad",
            add = "mean", rug = TRUE,
            color = "ZEC", fill = "ZEC", alpha = 0.4) +
  xlab("Profundidad (m)") + ylab("Función de densidad") +
  theme(text = element_text(size = 12))
ggsave("prof.tiff")

# Plots for density and cover through islands and times. We fitted GAMs (k = 3)
data %>% drop_na(Densidad) %>%
  ggplot(aes(x = year, y= Densidad)) + geom_point(size = 2, alpha = 0.4) +
  xlab(" Año") + ylab(expression("Densidad de haces (" ~ m^2 ~ ")")) +
  theme_bw() + geom_smooth(size = 2, method = "glm", se = T, method.args = list(family = "poisson")) +
  facet_wrap(vars(Isla)) + theme(text = element_text(size = 12)) 
ggsave("densidad_t.tiff")

data %>% drop_na(Densidad) %>%
  ggplot(aes(x = year, y= Densidad)) + geom_point(size = 2, alpha = 0.4) +
  xlab(" Año") + ylab(expression("Densidad de haces (" ~ m^2 ~ ")")) +
  theme_bw() + geom_smooth(size = 2, method = "glm", se = T, method.args = list(family = "poisson")) +
  facet_wrap("ZEC") + theme(text = element_text(size = 12)) 
ggsave("densidad_ZEC.tiff")

data %>% drop_na(Cober) %>%
  ggplot(aes(x = year, y= Cober)) + geom_point(size = 2, alpha = 0.4) +
  xlab(" Año") + ylab("Cobertura (%)") +
  theme_bw() + geom_smooth(size = 2, method = "glm", se = T, method.args = list(family = "poisson")) +
  facet_wrap(vars(Isla)) + theme(text = element_text(size = 12))
ggsave("cobertura_t.tiff")

data %>% drop_na(Cober) %>%
  ggplot(aes(x = year, y= Cober)) + geom_point(size = 2, alpha = 0.4) +
  xlab(" Año") + ylab("Cobertura (%)") +
  theme_bw() + geom_smooth(size = 2, method = "glm", se = T, method.args = list(family = "poisson")) +
  facet_wrap("ZEC") + theme(text = element_text(size = 12))
ggsave("cobertura_ZEC.tiff")

# Mapping of data points used above using leaflet
canary_islands_map = leaflet() %>%
  setView(lng = -16, lat = 28, zoom = 7)  

# Add a base map layer
canary_islands_map = addTiles(canary_islands_map)
canary_islands_map = addScaleBar(canary_islands_map, position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
canary_islands_map = addMarkers(canary_islands_map, data = data, lat = ~Lat, lng = ~Long, label = FALSE)
canary_islands_map

# Mapping of points using ggplot
# Create an sf object 
coords_sf = st_as_sf(data, coords = c("Long", "Lat"), crs = 4326, agr = "constant")

# Get the map data for Canary Islands 
world = ne_countries(scale = "medium", returnclass = "sf")

# Filter for Spain (which includes the Canary Islands)
spain = world[world$admin == "Spain", ]

# Plot the map with the coordinates
ggplot() + geom_sf(data = spain, fill = "lightgray", color = "black") +
  geom_sf(data = coords_sf, color = "darkgreen", size = 0.5, shape = 16, alpha = 1) +
  theme_minimal() + xlab("Longitud") + ylab("Latitud") +
  coord_sf(xlim = c(-19, -12.5), ylim = c(27.5, 29.5), expand = FALSE)
ggsave("map.tiff")

# Fit GLMs to temporal patterns in response variables
GLM.den = glm(Densidad ~ year + ZEC + year:ZEC, data=data, family=poisson())
summary(GLM.den)
plot(GLM.den)

GLM.den = glm(Densidad ~ year + ZEC + year:ZEC, data=data, family=poisson())
summary(GLM.den)
plot(GLM.den)

GLM.cober = glm(Cober ~ year + ZEC + year:ZEC, data=data, family=poisson())
summary(GLM.cober)
plot(GLM.cober)
tidy_model.GLM.cober = tidy(GLM.cober)
summary_table.GLM.cober = tidy_model.GLM.cober  %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)
print(summary_table.GLM.cober)

# Reading shape files of the 2024 cartography
shapefile_LG = st_read("./cartography/LG.shp")
shapefile_LZ = st_read("./cartography/LZ.shp")
shapefile_FV = st_read("./cartography/FV.shp")
shapefile_FV = st_read("./cartography/GC.shp")
shapefile_FV = st_read("./cartography/TF.shp")

plot(shapefile_LG)
plot(shapefile_LZ)
plot(shapefile_FV)
plot(shapefile_GC)
plot(shapefile_TF)

# Ploting coordinate data to define AC
data_AC = read.table("./data/AC.txt", sep="\t", header=TRUE) %>% as_tibble %>% glimpse()

# Add a base map layer
canary_islands_map_AC = addTiles(canary_islands_map)
canary_islands_map_AC = addScaleBar(canary_islands_map, position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
canary_islands_map_AC = addMarkers(canary_islands_map, data = data_AC, lat = ~lat, lng = ~long, label = FALSE)
canary_islands_map_AC

# Mapping of points using ggplot
# Create an sf object 
coords_sf_AC = st_as_sf(data_AC, coords = c("long", "lat"), crs = 4326, agr = "constant")

# Get the map data for Canary Islands 
world = ne_countries(scale = "medium", returnclass = "sf")

# Filter for Spain (which includes the Canary Islands)
spain = world[world$admin == "Spain", ]

# Plot the map with the coordinates
ggplot() + geom_sf(data = spain, fill = "lightgray", color = "black") +
  geom_sf(data = coords_sf_AC, color = "darkgreen", size = 1, shape = 16, alpha = 1) +
  theme_minimal() + xlab("Longitud") + ylab("Latitud") +
  coord_sf(xlim = c(-19, -12.5), ylim = c(27.5, 29.5), expand = FALSE)
ggsave("map_AC.tiff")