## Final Project - R 
##
## Question: How have the types of heating and cooling equipment used in 
##           commercial buildings construced before 1980 and after 1980 changed 
##           within different climate regions in the United States?  
## 
## Data: 2012 US Commercial Building Energy Consumption Survey
##(https://www.eia.gov/consumption/commercial/data/2012/index.php?view=microdata)
##
## Author(s): Yawen Hu, yawenhu@umich.edu
## Updated: December 6, 2020 

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

# directories: ----------------------------------------------------------------
path = "~/Desktop/stats506/final_project"
# data: -----------------------------------------------------------------------
data_file = sprintf("%s/2012_public_use_data_aug2016.csv", path)
cbecs_raw = read_delim(data_file, delim = ',')
cbecs = cbecs_raw %>%
  filter(PUBCLIM != 7) %>%
  transmute(id = PUBID,
            climate = as.factor(PUBCLIM),
            cool = MAINCL,
            heat = MAINHT,
            year = as.numeric(YRCONC),
            year = ifelse(year < 6, "Before 1980", "After 1980"),
            w = FINALWT
            ) %>%
  filter(!is.na(cool) & !is.na(heat))

cbecs_rw = cbecs_raw %>%
  select(PUBID, starts_with("FINALWT")) %>%
  select(-FINALWT) %>%
  rename(id = PUBID)

# Point Estimates: ------------------------------------------------------------
## cool: 
cool = cbecs %>%
      group_by(climate, year, cool) %>%
      summarise(count = sum(w), .groups = "drop_last") %>%
      group_by(climate, year) %>%
      summarise(cool, prop = count / sum(count), .groups = "drop_last")

## heat: 
heat = cbecs %>%
  group_by(climate, year, heat) %>%
  summarise(count = sum(w), .groups = "drop_last") %>%
  group_by(climate, year) %>%
  summarise(heat, prop = count / sum(count), .groups = "drop_last")

# CIs: ------------------------------------------------------------------------
cbecs_rw_long = cbecs_rw %>%
  pivot_longer(
    cols = starts_with("FINALWT"),
    names_to = "rw_id",
    values_to = "rw"
  )

cool_rw = cbecs %>%
  left_join(cbecs_rw_long, by = "id") %>%
  group_by(climate, rw_id, year, cool) %>%
  summarise(count = sum(rw), .groups = "drop_last") %>%
  group_by(climate, rw_id, year) %>%
  summarise(cool, prop_rw = count / sum(count), .groups = "drop_last") %>%
  left_join(cool, by = c("climate", "year", "cool")) %>%
  group_by(climate, year, cool) %>%
  summarise(prop = mean(prop), v = mean({prop_rw - prop}^2) / {{1 - 0.5}^2}, .groups = "drop_last")

m = qnorm(0.975)
cool_rw = cool_rw %>%
  transmute(
    climate,
    year,
    cool,
    prop,
    se = sqrt(v),
    lwr = pmax(prop - m * se, 0),
    upr = pmin(prop + m * se, 1) 
  )

heat_rw = cbecs %>%
  left_join(cbecs_rw_long, by = "id") %>%
  group_by(climate, rw_id, year, heat) %>%
  summarise(count = sum(rw), .groups = "drop_last") %>%
  group_by(climate, rw_id, year) %>%
  summarise(heat, prop_rw = count / sum(count), .groups = "drop_last") %>%
  left_join(heat, by = c("climate", "year", "heat")) %>%
  group_by(climate, year, heat) %>%
  summarise(prop = mean(prop), v = mean({prop_rw - prop}^2) / {{1 - 0.5}^2}, .groups = "drop_last")

heat_rw = heat_rw %>%
  transmute(
    climate,
    year,
    heat,
    prop,
    se = sqrt(v),
    lwr = pmax(prop - m * se, 0),
    upr = pmin(prop + m * se, 1) 
  )

# Decode of data: -------------------------------------------------------------
cool_label = c("AC without chilled water",
               "Packaged AC units",
               "Central chillers",
               "District chilled water",
               "Heat pumps",
               "Individual room AC",
               "Swamp/evaporative coolers",
               "Others")

heat_label = c("Furnaces",
               "Packaged central unit",
               "Inside boilers",
               "District steam/hot water",
               "Heat pumps",
               "Individual space heaters",
               "Others")

climate_label = c("Very cold/Cold",
                  "Mixed-humid",
                  "Hot/Mixed-dry/Hot-humid",
                  "Marine")

cool_tab = cool_rw %>%
  transmute(
    climate = factor(climate, levels = c(1,2,3,5), labels = climate_label),
    year,
    `Equipment Type`= factor(cool, levels = c(1:8), labels = cool_label),
    `Proportion(cooling)` = sprintf('<div>%4.1f</div> <div>(%4.1f, %4.1f)</div>',
                                    100*prop, 100*lwr, 100*upr),
  ) %>%
  pivot_wider(
    id_cols = c("Equipment Type", "year"),
    names_from = c("climate", "year"),
    values_from = "Proportion(cooling)"
  )

heat_tab = heat_rw %>%
  transmute(
    climate = factor(climate, levels = c(1,2,3,5), labels = climate_label),
    year,
    `Equipment Type` = factor(heat, levels = c(1:7), labels = heat_label),
    `Proportion(heating)` = sprintf('<div>%4.1f</div> <div>(%4.1f, %4.1f)</div>',
                                    100*prop, 100*lwr, 100*upr),
  ) %>%
  pivot_wider(
    id_cols = c("Equipment Type"),
    names_from = c("climate", "year"),
    values_from = "Proportion(heating)"
  )

tab = rbind(cool_tab, heat_tab) %>%
  rename(
    `<div>Very cold/Cold</div> <div>(1980+)</div>` = `Very cold/Cold_After 1980`,
    `<div>Very cold/Cold</div> <div>(1980-)</div>` = `Very cold/Cold_Before 1980`,
    `<div>Mixed-humid</div> <div>(1980+)</div>` = `Mixed-humid_After 1980`,
    `<div>Mixed-humid</div> <div>(1980-)</div>` = `Mixed-humid_Before 1980`,
    `<div>Hot/Mixed-dry</div> <div>/Hot-humid</div> <div>(1980+)</div>` = `Hot/Mixed-dry/Hot-humid_After 1980`,
    `<div>Hot/Mixed-dry</div> <div>/Hot-humid</div> <div>(1980-)</div>` = `Hot/Mixed-dry/Hot-humid_Before 1980`,
    `<div>Marine</div> <div>(1980+)</div>` = `Marine_After 1980`,
    `<div>Marine</div> <div>(1980-)</div>` = `Marine_Before 1980`,
  )
  

# 79: -------------------------------------------------------------------------
