## NEP LOI Map

library(tidyverse)
#library(ggmap)
library(sf)
library(ggspatial)


DataDir <- "C:/Users/ctw1/OneDrive - USNH/PREP/PREP-GIS/"

PREPboundary <- st_read("Data/PREP_HUC8/PREP_HUC8.shp")
HUC10 <- st_read("Data/NH_Watershed_Boundaries_HUC10/NH_Watershed_Boundaries_(HUC10).shp")
HUC12 <- st_read("Data/NH_Watershed_Boundaries_HUC12/NH_Watershed_Boundaries_(HUC12).shp")
NHoutline <- st_read("Data/NH_Outline/NH_Merge/NH_Dissolve.shp")
MEoutline <- st_read("Data/ME_Outline/Maine_State_Boundary_Polygon_Feature.shp")
MAoutline <- st_read("Data/MA_Outline/OUTLINE25k_POLY.shp")
NHWaterbodies <-
  st_read(
    paste(
      DataDir,
      "Data/NH_Hydrography_Waterbodies/New_Hampshire_Hydrography_Dataset_(Waterbody).shp",
      sep = ""
    )
  )
NHDwaterbodies <-
  st_read(
    paste(
      DataDir,
      "Data/NHD/The_National_Map%3A_National_Hydrography_Dataset_Map_Service.shp",
      sep = ""
    )
  )

PREPboundary <- st_transform(PREPboundary, crs = st_crs(NHWaterbodies))
NHoutline <- st_transform(NHoutline, crs = st_crs(NHWaterbodies))
MEoutline <- st_transform(MEoutline, crs = st_crs(NHWaterbodies))
MAoutline <- st_transform(MAoutline, crs = st_crs(NHWaterbodies))
NHDwaterbodies <- st_transform(NHDwaterbodies, crs = st_crs(NHWaterbodies))

PREPNHwaterbodies <- st_intersection(NHWaterbodies, PREPboundary)
PREPMEwaterbodies <- st_intersection(NHDwaterbodies, PREPboundary)

## Write intersectioned shapeiles to file

st_write(PREPNHwaterbodies, "Data/PREP_NH_Waterbodies/PREP_NH_Waterbodies.shp")
st_write(PREPMEwaterbodies, "Data/PREP_ME_Waterbodies/PREP_ME_Waterbodies.shp")

rm(NHWaterbodies, NHDwaterbodies)

MainMap <-
  ggplot() +
  geom_sf(data = NHoutline, fill = "#e5e5e5", color = NA) +
  geom_sf(data = MEoutline, fill = "#e5e5e5", color = NA) +
  geom_sf(data = MAoutline, fill = "#e5e5e5", color = NA) +
  geom_sf(
    data = PREPboundary,
    aes(color = "Piscataqua Region Watershed",
        fill = "Piscataqua Region Watershed"),
    linewidth = 1
    #key_glyph = "rect"
  ) + #, aes(fill = "PREP NEP Boundary")
  geom_sf(
    data = HUC12 %>%
      filter(Name %in% c("Oyster River")),
    aes(fill = "Oyster River"),
    #color = "Oyster River"),
    alpha = 0.3,
    color = NA,
    key_glyph = "rect"
  ) +
  geom_sf(
    data = HUC10 %>%
      filter(Name == "Lamprey River"),
    aes(fill = "Lamprey River"),
    #color = "Lamprey River"),
    alpha = 0.3,
    color = NA,
    key_glyph = "rect"
  ) +
  geom_sf(
    data = PREPNHwaterbodies,
    fill = "#73b2ff",
    color = NA,
    alpha = 0.3
  ) +
  geom_sf(
    data = PREPMEwaterbodies,
    fill = "#73b2ff",
    color = NA,
    alpha = 0.3
  ) +
  geom_sf(
    data = PREPNHwaterbodies %>% filter(OBJECTID == 12630),
    fill = "#73b2ff",
    color = NA
  ) + #, aes(fill = "Great Bay")
  coord_sf(
    xlim = c(1020000, 1280000),
    ylim = c(120000, 420000),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(aes(location = "br")) +
  ggspatial::annotation_north_arrow(aes(location = "br"),
                                    pad_y = unit(1, "cm")) +
  scale_color_manual(
    name = "",
    breaks = c("Piscataqua Region Watershed",
               "Lamprey River",
               "Oyster River"),
    labels = c("Piscataqua Region\nWatershed",
               "Lamprey River",
               "Oyster River"),
    values = c("black", NA, NA)
  ) +
  scale_fill_manual(
    name = "",
    breaks = c("Piscataqua Region Watershed",
               "Lamprey River",
               "Oyster River"),
    labels = c("Piscataqua Region\nWatershed",
               "Lamprey River",
               "Oyster River"),
    values = c("white", "#fcc0eb", "#7dab66"
               #"Great Bay" = "#73b2ff"
    )
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.6),
    legend.text = element_text(size = 8),
    legend.margin = margin(-5, 0, 0, 0, "pt"),
    panel.background = element_rect(fill = "#73b2ff"),
    plot.background = element_blank()
  ) +
  guides(color = "none", 
         fill = guide_legend(override.aes = list(color = c("black", NA, NA))))

Locus <-
  ggplot() +
  geom_sf(data = NHoutline, fill = "#d3ffbe", color = NA) +
  geom_sf(data = MEoutline, fill = "#ffffbe", color = NA) +
  geom_sf(data = MAoutline, fill = "#ffeabe", color = NA) +
  geom_sf(
    data = PREPboundary,
    color = "black",
    fill = NA,
    linewidth = 0.8
    #key_glyph = "rect"
  ) +
  coord_sf(
    xlim = c(745000, 1500000),
    ylim = c(65000, 1000000),
    expand = TRUE
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank())

LOImap <-
  cowplot::ggdraw(MainMap) +
  cowplot::draw_plot(Locus,
                     x = 0.24,
                     y = 0.65,
                     width = 0.18,
                     height = 0.34)

ggsave(file = paste0("Plots/NEP_LOI_Map_", Sys.Date(), ".png"),
       LOImap,
       dpi = 300)














