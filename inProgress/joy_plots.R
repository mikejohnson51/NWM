library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(httr)
library(ggjoy)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(leaflet)
library(httr)

site <- "09380000" # Lee's Ferry
variable <- "00060" # daily discharge
discharge <- readNWISdv(site, variable)
discharge %>% select(-agency_cd, -X_00060_00003_cd) %>%
  rename(cfs = X_00060_00003, date = Date) %>%
  mutate(year = as.factor(-year(date)), julian = yday(date)) -> discharge

# Plot a single station for all years
sc <- (max(discharge$cfs) / min(discharge$cfs) * 0.035) # intelligent scaling of mountain height
ggplot(discharge, aes(julian, year, height = cfs, group = year, fill = year)) +
  geom_joy(stat="identity", scale = sc, size=0.4) +
  xlab("Day of Year") +
  theme(legend.position = "none") +
  ggtitle("Colorado River flow at Lee's Ferry") +
  theme(axis.text=element_text(size=8)) +
  theme(panel.background = element_blank())
sc
