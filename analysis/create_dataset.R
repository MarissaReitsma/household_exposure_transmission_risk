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

## Read in ACS data downloaded from https://usa.ipums.org/usa/
df <- fread("~/Downloads/usa_00008.csv")

########################
## Indicator Preparation
########################

## Cut unnecessary variables
df <- df[,c("YEAR", "MULTYEAR", "SAMPLE", "CBSERIAL", "HHWT", "HHTYPE", "CLUSTER", "REGION",
            "STRATA", "BEDROOMS", "REPWTP", "SEX", "AGE", "RACED", "HISPAND", "HCOVANY",
            "EMPSTATD", "LABFORCE", "POVERTY"):=NULL]

## Restrict to non-institutionalized population
df <- df[GQ %in% c(1, 2, 5)]
df <- df[, GQ:=NULL]

## Essential worker status
essential_list <- readRDS("~/COVID/acs_household_analysis/est_occ_crit.RDS")
essential_list <- unique(essential_list)

## Deal with codes that are < 6 digits
df <- df[, OCCSOC:=trimws(OCCSOC, which = "both")]
df <- df[, OCCSOC:=sub("0$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("Y$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("0$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("Y$", "", OCCSOC)]
df <- df[, OCCSOC:=sub("X$", "", OCCSOC)]
df <- df[OCCSOC=="", OCCSOC:="0"]

df <- merge(df, essential_list, by = "OCCSOC", all.x=T)
df <- df[OCCSOC=="99992", OCCSOC:="0"]

df <- df[is.na(Critical), Critical:=0] # Not critical if not working
df <- df[EMPSTAT!=1, Critical:=0] # Not critical if not working
df <- df[, hh_essential := max(Critical, na.rm=T), by = c("SERIAL")] # Household level characteristic
df <- df[, c("EMPSTAT", "OCCSOC", "Occupation Description", "Critical"):=NULL]

rm(essential_list)

## BIPOC Status
df <- df[RACE==1 & HISPAN == 0, race_grp:="WHITE"]
df <- df[HISPAN %in% c(1, 2, 3, 4), race_grp:="LATINX"]
df <- df[is.na(race_grp) & RACE == 2, race_grp:="BLACK"]
df <- df[is.na(race_grp) & RACE %in% c(3), race_grp:="INDIGENOUS"]
df <- df[is.na(race_grp) & RACE %in% c(4, 5, 6), race_grp:="ASIAN"]
df <- df[is.na(race_grp), race_grp:="OTHER"] # Note "OTHER" contains ~ 30% Indigenous, 70% multiracial, 10% other)

df <- df[, c("RACE", "HISPAN"):=NULL]

# This is a big dataset, so splitting into state-level data before collapsing to preseve memory
for (i in unique(df$STATEFIP)) {
  temp <- df[STATEFIP==i]
  for (p in unique(temp$PUMA)) {
    temp2 <- temp[PUMA==p]
    write.csv(temp2, paste0("~/COVID/acs_household_analysis/PUMA_microdata/", i, "_", p, ".csv"))
  }
}

rm(df, i)

########################
## Collapse
########################

## Loop through states and collapse
files <- list.files("~/COVID/acs_household_analysis/state_microdata/")

# All race/ethnicity
out <- NULL
for (f in files) {
  temp <- fread(paste0("~/COVID/acs_household_analysis/state_microdata/", f))
  temp <- temp[, V1:=NULL]
  temp <- melt(temp, id.vars = c("hh_essential", "poc_ind", "SERIAL", "NUMPREC", "PUMA", "ROOMS", "PERNUM", "STATEFIP"))
  
  # Make estimates for each replicate
  temp <- temp[, tot_persons_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable")]
  temp <- temp[ROOMS < NUMPREC, crowd_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable")]
  temp <- temp[, crowd_rep:=mean(crowd_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable")]
  temp <- temp[ROOMS < NUMPREC, crowd_essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable")]
  temp <- temp[, crowd_essential_rep:=mean(crowd_essential_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable")]
  temp <- temp[, essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable")]
  
  # Make a variable with the point estimates using the main weights
  temp <- temp[variable=="PERWT", tot_persons_point:=tot_persons_rep]
  temp <- temp[variable=="PERWT", essential_point:=essential_rep]
  temp <- temp[variable=="PERWT", crowd_essential_point:=crowd_essential_rep]
  temp <- temp[variable=="PERWT", crowd_point:=crowd_rep]
  
  # Fill the point estimate for SDR calculation
  temp <- temp[, tot_persons_point:=mean(tot_persons_point, na.rm=T), by = c("PUMA", "STATEFIP")]
  temp <- temp[, essential_point:=mean(essential_point, na.rm=T), by = c("PUMA", "STATEFIP")]
  temp <- temp[, crowd_essential_point:=mean(crowd_essential_point, na.rm=T), by = c("PUMA", "STATEFIP")]
  temp <- temp[, crowd_point:=mean(crowd_point, na.rm=T), by = c("PUMA", "STATEFIP")]
  
  # Collapse the dataset
  temp <- unique(temp[,.(PUMA, STATEFIP, variable, tot_persons_rep, crowd_rep, crowd_essential_rep, essential_rep,
                         tot_persons_point, essential_point, crowd_essential_point, crowd_point)])
  
  # Make percent vars
  for (i in c("crowd", "crowd_essential", "essential")) {
    for (j in c("rep", "point")) {
      temp <- temp[, paste0(i, "_", j, "_perc"):=get(paste0(i, "_", j))/get(paste0("tot_persons_", j))]
    }
  }
  
  # Make squared error
  for (i in c("crowd", "essential", "crowd_essential")) {
    temp <- temp[, paste0(i, "_square_error_num") := (get(paste0(i, "_rep"))-get(paste0(i, "_point")))^2]
    temp <- temp[, paste0(i, "_square_error_perc") := (get(paste0(i, "_rep_perc"))-get(paste0(i, "_point_perc")))^2]
  }
  
  # Remove the point estimate row
  temp <- temp[variable!="PERWT"]
  
  # Calculate SE and CI
  for (i in c("crowd", "essential", "crowd_essential")) {
    temp <- temp[, paste0(i, "_se"):=sqrt(sum(get(paste0(i, "_square_error_num")))*4/80), by = c("STATEFIP", "PUMA")]
    temp <- temp[, paste0(i, "_perc_se"):=sqrt(sum(get(paste0(i, "_square_error_perc")))*4/80), by = c("STATEFIP", "PUMA")]
  }
  
  temp <- unique(temp[, .(PUMA, STATEFIP, tot_persons_point, essential_point, crowd_essential_point, crowd_point, crowd_essential_point_perc,
                          essential_point_perc, crowd_point_perc, crowd_se, essential_se, crowd_essential_se, crowd_perc_se,
                          essential_perc_se, crowd_essential_perc_se)])
  
  out <- rbind(out, temp, fill = T)
}

write.csv(out, "~/COVID/acs_household_analysis/puma_estimates_withse.csv", na = "", row.names = F)

# By Race
out_race <- NULL
for (f in files) {
  print(f)
  temp <- fread(paste0("~/COVID/acs_household_analysis/state_microdata/", f))
  temp <- temp[, V1:=NULL]
  temp <- temp[, SERIAL:=NULL]
  temp <- temp[, PERNUM:=NULL]
  
  temp <- melt(temp, id.vars = c("hh_essential", "poc_ind", "NUMPREC", "PUMA", "ROOMS", "STATEFIP"))
  
  # Make estimates for each replicate
  temp <- temp[, tot_persons_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[ROOMS < NUMPREC, crowd_rep:=sum(value), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[, crowd_rep:=mean(crowd_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[is.na(crowd_rep), crowd_rep:=0]
  temp <- temp[ROOMS < NUMPREC, crowd_essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[, crowd_essential_rep:=mean(crowd_essential_rep, na.rm=T), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[is.na(crowd_essential_rep), crowd_essential_rep:=0]
  temp <- temp[, essential_rep:=sum(value*hh_essential), by = c("PUMA", "STATEFIP", "variable", "poc_ind")]
  temp <- temp[is.na(essential_rep), essential_rep:=0]
  
  # Make a variable with the point estimates using the main weights
  temp <- temp[variable=="PERWT", tot_persons_point:=tot_persons_rep]
  temp <- temp[variable=="PERWT", essential_point:=essential_rep]
  temp <- temp[variable=="PERWT", crowd_essential_point:=crowd_essential_rep]
  temp <- temp[variable=="PERWT", crowd_point:=crowd_rep]
  
  # Fill the point estimate for SDR calculation
  temp <- temp[, tot_persons_point:=mean(tot_persons_point, na.rm=T), by = c("PUMA", "STATEFIP", "poc_ind")]
  temp <- temp[, essential_point:=mean(essential_point, na.rm=T), by = c("PUMA", "STATEFIP", "poc_ind")]
  temp <- temp[, crowd_essential_point:=mean(crowd_essential_point, na.rm=T), by = c("PUMA", "STATEFIP", "poc_ind")]
  temp <- temp[, crowd_point:=mean(crowd_point, na.rm=T), by = c("PUMA", "STATEFIP", "poc_ind")]
  
  # Collapse the dataset
  temp <- unique(temp[,.(PUMA, STATEFIP, variable, tot_persons_rep, crowd_rep, crowd_essential_rep, essential_rep,
                         tot_persons_point, essential_point, crowd_essential_point, crowd_point, poc_ind)])
  
  # Make percent vars
  for (i in c("crowd", "crowd_essential", "essential")) {
    for (j in c("rep", "point")) {
      temp <- temp[, paste0(i, "_", j, "_perc"):=get(paste0(i, "_", j))/get(paste0("tot_persons_", j))]
    }
  }
  
  # Make squared error
  for (i in c("crowd", "essential", "crowd_essential")) {
    temp <- temp[, paste0(i, "_square_error_num") := (get(paste0(i, "_rep"))-get(paste0(i, "_point")))^2]
    temp <- temp[, paste0(i, "_square_error_perc") := (get(paste0(i, "_rep_perc"))-get(paste0(i, "_point_perc")))^2]
  }
  
  # Remove the point estimate row
  temp <- temp[variable!="PERWT"]
  
  # Calculate SE and CI
  for (i in c("crowd", "essential", "crowd_essential")) {
    temp <- temp[, paste0(i, "_se"):=sqrt(sum(get(paste0(i, "_square_error_num")))*4/80), by = c("STATEFIP", "PUMA", "poc_ind")]
    temp <- temp[, paste0(i, "_perc_se"):=sqrt(sum(get(paste0(i, "_square_error_perc")))*4/80), by = c("STATEFIP", "PUMA", "poc_ind")]
  }
  
  temp <- unique(temp[, .(PUMA, STATEFIP, tot_persons_point, essential_point, crowd_essential_point, crowd_point, crowd_essential_point_perc,
                          essential_point_perc, crowd_point_perc, crowd_se, essential_se, crowd_essential_se, crowd_perc_se,
                          essential_perc_se, crowd_essential_perc_se, poc_ind)])
  
  out_race <- rbind(out_race, temp, fill = T)
}

write.csv(out_race, "~/COVID/acs_household_analysis/puma_estimates_withse_race_final.csv", na = "", row.names = F)
