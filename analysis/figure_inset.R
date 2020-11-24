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
out <- fread("~/COVID/acs_household_analysis/puma_estimates_withse.csv")

## PUMA to COUNTY MAP
puma_mapping <- fread("~/COVID/acs_household_analysis/puma_mapping_2010.csv")
puma_mapping <- puma_mapping[STATEFP==46 & COUNTYFP==113, COUNTYFP:=102]

data(fips_codes)
fips_codes <- as.data.table(fips_codes)
setnames(fips_codes, c("state_code", "county_code"), c("STATEFP", "COUNTYFP"))
fips_codes <- fips_codes[STATEFP==46 & COUNTYFP==113, COUNTYFP:=102]
fips_codes <- fips_codes[, STATEFP_num:=as.numeric(STATEFP)]
fips_codes <- fips_codes[, COUNTYFP_num:=as.numeric(COUNTYFP)]

setnames(puma_mapping, c("STATEFP", "COUNTYFP"), c("STATEFP_num", "COUNTYFP_num"))
puma_mapping <- merge(puma_mapping, fips_codes, by = c("STATEFP_num", "COUNTYFP_num"))
puma_mapping <- puma_mapping[, id:=paste0(as.numeric(STATEFP), "_", as.numeric(PUMA))]
puma_mapping <- unique(puma_mapping[,.(STATEFP, COUNTYFP, PUMA, id, state_name, county)])
puma_mapping <- puma_mapping[!(id==46200 & county == "Shannon County")]

# Select top five counties based on population (aggregating the NYC counties)
la_pumas <- unique(puma_mapping$id[puma_mapping$state_name == "California" & puma_mapping$county=="Los Angeles County" & puma_mapping$id!="6_3768"])
chi_pumas <- unique(puma_mapping$id[puma_mapping$state_name == "Illinois" & puma_mapping$county=="Cook County"])
harris_pumas <- unique(puma_mapping$id[puma_mapping$state_name == "Texas" & puma_mapping$county=="Harris County"])
arz_pumas <- unique(puma_mapping$id[puma_mapping$state_name == "Arizona" & puma_mapping$county=="Maricopa County"])
nyc_pumas <- unique(puma_mapping$id[puma_mapping$state_name == "New York" & puma_mapping$county%in%c("Kings County", "Queens County", "New York County", "Bronx County", "Richmond County", "Nassau County", "Suffolk County", "Rockland County", "Westchester County")])

# Read in PUMA shapefile
puma_map <- readRDS("~/COVID/acs_household_analysis/puma_map.RDS")

puma_map$GEOID_NUM <- paste0(as.numeric(puma_map$STATEFP10), "_", as.numeric(puma_map$PUMACE10))
puma_map <- fortify(puma_map, region = "GEOID_NUM")

hh_df <- copy(out)
hh_df <- hh_df[, id:=paste0(as.numeric(out$STATEFIP), "_", as.numeric(out$PUMA))]

puma_map <- merge(puma_map, hh_df, by = "id")
puma_map <- merge(puma_map, puma_mapping[id%in%c(nyc_pumas, la_pumas, harris_pumas, arz_pumas,
                                                 chi_pumas, miami_pumas),.(id, state_name, county)], by = "id")

puma_map <- as.data.table(puma_map)
puma_map <- puma_map[order(order)]
puma_map <- puma_map[state_name=="New York", county:="New York City"]

puma_map <- puma_map[, room_essential_perc_topcoded_new:= crowd_essential_point_perc]
puma_map <- puma_map[room_essential_perc_topcoded_new > .2, room_essential_perc_topcoded_new:=.2]

## Plot Figure 1
plot_out <- NULL
for (i in unique(puma_map$county)) {
  s <- unique(puma_map$state_name[puma_map$county==i])
  plot_out[[i]] <- ggplot(data = puma_map[county == i], aes(x = long, y = lat, group = group, fill = room_essential_perc_topcoded_new)) +
    geom_polygon() +
    scale_fill_distiller(labels = scales::percent, limits = c(0, .2), palette = "BuGn", direction = 1) +
    labs(x = "", y = "", title = paste0(i, ", ", s),
         fill = "% of People Living\nin High-Risk Households") +
    theme_void() + theme(text = element_text(size = 16), legend.text = element_text(size = 12), plot.margin = margin(1, 1, 1, 1, "cm")) + coord_equal() +
    theme(legend.position = "right", legend.key.width = unit(1, "cm"), plot.title = element_text(hjust = 0.5)) #+
}

legend <- get_legend(plot_out$`Los Angeles County`)

plot_out <- NULL
for (i in unique(puma_map$county)) {
  s <- unique(puma_map$state_name[puma_map$county==i])
  plot_out[[i]] <- ggplot(data = puma_map[county == i], aes(x = long, y = lat, group = group, fill = room_essential_perc_topcoded_new)) +
    geom_polygon() +
    scale_fill_distiller(labels = scales::percent, limits = c(0, .2), palette = "BuGn", direction = 1) +
    labs(x = "", y = "", title = paste0(i), subtitle = paste0(s),
         fill = "% of People Living in High-Risk Households") +
    theme_void() + theme(text = element_text(size = 16), legend.text = element_text(size = 12), plot.margin = margin(.2, .2, .2, .2, "cm")) + coord_equal() +
    theme(legend.position = "none", legend.key.width = unit(1, "cm"), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #+
}

pdf("~/COVID/acs_household_analysis/figure1_inset.pdf", height = 6, width = 20)
c_plots <- grid.arrange(plot_out$`Cook County`, plot_out$`Harris County`,
                        plot_out$`Los Angeles County`, plot_out$`Maricopa County`,
                        plot_out$`New York City`, ncol =5)
print(c_plots)
dev.off()