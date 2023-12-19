#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# OBIS in KBay                                                                ##
# Script created 2023-12-19                                                   ##
# Data source: OBIS                                                           ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-12-19                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Exploration of OBIS data collected from in and around Kachemak Bay, AK


# Required Files (check that script is loading latest version):
# FILE.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# RECENT CHANGES TO SCRIPT                                                     +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(robis)
library(tidyverse)
library(viridis)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

KBAY_obis <- occurrence(geometry = "POLYGON ((-151.70629072505 59.8526549425597,-152.392156856879 59.7996038894368,-152.430508951768 58.9931766913775,-151.875584722435 59.1180953324996,-150.599819606163 59.8354639536068,-151.07985347074 59.8827499328968,-151.70629072505 59.8526549425597))", mof = TRUE)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# remove vertebrates
KBAY_obis_novert <- KBAY_obis %>%
  filter(subphylum != "vertebrata")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VISUALIZATIONS                                                            ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# how many observations by phylum?

KBAY_obis_novert %>%
  ggplot(aes(x = forcats::fct_infreq(phylum))) +
  geom_histogram(stat = "count")

# observations by year

KBAY_obis_novert %>%
  ggplot(aes(x = date_year)) +
  geom_histogram(stat = "count")

# phylum frequency in the 70's
KBAY_obis_novert %>%
  filter(date_year %in% c(1970:1980)) %>%
  ggplot(aes(x = forcats::fct_infreq(phylum))) +
  geom_histogram(stat = "count")
  
# who did collections in the 70's
KBAY_obis_novert %>%
  filter(date_year %in% c(1970:1980)) %>%
  distinct(recordedBy) %>%
  print(n = 50)

# Katrin's collections by collector
KBAY_obis_novert %>%
  filter(date_year %in% c(2003:2010)) %>%
  ggplot(aes(x = forcats::fct_infreq(recordedBy))) +
            geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


# Katrin's collections by species count
KBAY_obis_novert %>%
  filter(date_year %in% c(2003:2010)) %>%
  group_by(date_year) %>%
  summarise(species_num = n_distinct(species)) %>%
  ggplot(aes(x = date_year, y = species_num)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))

# parsing out counts of orgs by sample date

KBAY_obis_novert %>%
  filter(date_year %in% c(2003:2010)) %>%
  mutate(date_ext = ymd(as.Date(eventDate))) %>%
  group_by(date_ext, species) %>%
  summarise(count_num = sum(as.numeric(individualCount), na.rm = TRUE)) %>%
  ggplot(aes(x = date_ext, y = count_num)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####







