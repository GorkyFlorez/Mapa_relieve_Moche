#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx, tmap)
#------------------------------------------------------------------------
library(extrafont)
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_moche       <- subset(Cuencas_peru , NOMB_UH_N6  == "Moche")
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_moche)

dem  = raster("Raster/ASTGTM_S08W079_dem.tif")
dem2 = raster("Raster/ASTGTM_S09W079_dem.tif")
dem3 = raster("Raster/ASTGTM_S09W080_dem.tif")
DEM_total <- raster::merge(dem, dem2,dem3)

Cuenca_moche_alt     <- crop(DEM_total, Cuenca_moche)
Cuenca_moche_alt     <- Cuenca_moche_alt  <- mask(Cuenca_moche_alt , Cuenca_moche)
plot(Cuenca_moche_alt)

slope = terrain(Cuenca_moche_alt, opt = "slope") 
aspect = terrain(Cuenca_moche_alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)


colores1 <- sequential_hcl(100, palette = "YlOrRd", rev = T)

Mapa =tm_shape(hill) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE, alpha=0.8)+
  tm_shape(Cuenca_moche_alt) +
  tm_raster(alpha = 0.8, palette = colores1 ,n=20, style="cont",
            legend.show = T, title="Elevacion \n(m.s.n.m)")+
  tm_shape(Cuencas_rios)+
  tm_lines("blue",lwd=0.2, size=0.5)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.86, 0.05), text.color = "black")+
  tm_layout( title = "Google Earth Engine \nRelieve, \nModelo de Elevacion Digital",
             bg.color="white", 
             legend.title.size=.9,
             title.color  = "black",
             legend.bg.color = "#416076", 
             legend.text.color = "white",
             legend.title.color = "white",
             legend.position = c(0.005,0.25) , scale=0.61, legend.frame = T,
             fontface="bold",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))+
  tm_credits("   MAPA de RELIEVE de \n     CUENCA HIDROGRAFICA de \n                 MOCHE", position = c(.7, .8), col = "black", fontface="bold", size=2, fontfamily = "serif")+
  tm_credits("Data: https://www.https://code.earthengine.google.com/ \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo", position = c(0.5, .04), col = "black", fontface="bold",fontfamily = "serif")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.70, 0.05))+
  tm_grid(col = 'gray', alpha = 0.5)


tmap_save(Mapa, "Mapa/Cuenca Mochee.png", dpi = 1200, width = 13, height = 7)
