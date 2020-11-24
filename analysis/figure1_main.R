## Clear workspace
rm(list = ls())

## Load packages
library(tidyverse)
library(data.table)
library(tigris)
library(viridis)
library(ggplot2)
library(acs)
library(usmap)
library(zoo)
library(cowplot)

# Read in the data
out_race <- fread("~/COVID/acs_household_analysis/puma_estimates_withse_race_final.csv")

puma_map <- readRDS("~/COVID/acs_household_analysis/puma_map.RDS")

puma_map$GEOID_NUM <- paste0(as.numeric(puma_map$STATEFP10), "_", as.numeric(puma_map$PUMACE10))
puma_map <- fortify(puma_map, region = "GEOID_NUM")

hh_df <- copy(out_race)
hh_df <- hh_df[, id:=paste0(as.numeric(hh_df$STATEFIP), "_", as.numeric(hh_df$PUMA))]

puma_map <- merge(puma_map, hh_df, by = "id")
puma_map <- as.data.table(puma_map)
puma_map <- puma_map[poc_ind==1, hh_type:="People of Color"]
puma_map <- puma_map[poc_ind==0, hh_type:="Non-Hispanic White"]

puma_map <- puma_map[, room_essential_perc_topcoded_new:= crowd_essential_point_perc]
puma_map <- puma_map[room_essential_perc_topcoded_new > .2, room_essential_perc_topcoded_new:=.2]

pdf("~/COVID/acs_household_analysis/figure1_main.pdf", width = 10, height = 8)

plot_a<-ggplot(data = puma_map, aes(x = long, y = lat, group = group, fill = room_essential_perc_topcoded_new)) +
  geom_polygon() + scale_fill_distiller(labels = scales::percent, limits = c(0, .2), palette = "BuGn", direction = 1) +
  facet_wrap(~hh_type, ncol = 2) +
  labs(x = "", y = "", title = "",
       fill = "") +
  theme_void() +
  theme(text = element_text(size = 16), legend.text = element_text(size = 12),
                       legend.title = element_text(size = 12),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_equal() +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))
print(plot_a)

dev.off()