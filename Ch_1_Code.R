#Chapter 1 Analysis
#Script by: Paige Kleindl
#12-2-2025

#R version 4.1.1

#Clear workspace
rm(list=ls())

#Load libraries
library(readr) #version 2.1.5
library(ggplot2) #version 4.0.0
library(rstatix) #version 0.7.2
library(car) #version 3.1.3
library(mgcv) #version 1.9.3
library(tidyverse) #version 2.0.0
library(Hmisc) #version 5.2.3
library(ggcorrplot) #version 0.1.4.1
library(FSA) #version 0.10.0

#Make graph appear in a new window  
dev.new()

#Literature Review####

#Load literature review datasets
#macrophyte dataset
lit_rev_macro <- read_csv("D:\\Extra FIU Files\\Dissertation\\Chapter 1\\Data Publication\\FCE1294_Lit_Review_Macro.csv")

#microbial mat dataset
lit_rev_micro <- read_csv("D:\\Extra FIU Files\\Dissertation\\Chapter 1\\Data Publication\\FCE1294_Lit_Review_Benthic_Micro.csv")

#phytoplankton dataset
lit_rev_phyto <- read_csv("D:\\Extra FIU Files\\Dissertation\\Chapter 1\\Data Publication\\FCE1294_Lit_Review_Phyto.csv")


#Macrophyte biomass among freshwater benthic ecosystem types####

#Subset dataset to contain only needed variables 
lit_rev_macro_bio <- lit_rev_macro %>%
  select(publication_citation, habitat_type, category, group, dm_gm2, log_dm_gm2) %>%
  filter(dm_gm2 != -9999, log_dm_gm2 != -9999)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_macro_bio$habitat_type = factor(lit_rev_macro_bio$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                   "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                   "pond", "shallow_lake", "shallow_reservoir", "bog", 
                                                                                   "constructed_treatment_wetland", "dune_slack", "fen", 
                                                                                   "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                                   "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                                                                                   "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_macro_bio$category = factor(
  lit_rev_macro_bio$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean macrophyte biomass per freshwater benthic ecosystem type
mean_macro_habitat_bio <- lit_rev_macro_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_biomass = mean(dm_gm2)) %>%
  print(n = 21)

#1 eutrophic_lake_littoral_zone            422.
#2 lake_littoral_zone                      258.
#3 mesotrophic_lake_littoral_zone         1449.
#4 oligotrophic_lake_littoral_zone         285.
#5 pond                                    785.
#6 shallow_lake                           1580.
#7 shallow_reservoir                       138.
#8 bog                                    6346.
#9 constructed_treatment_wetland          1845.
#10 dune_slack                              208.
#11 fen                                     490.
#12 floodplain_wetland                      736.
#13 marsh                                   248.
#14 non_tidal_coastal_wetland              1182.
#15 prairie_marsh                           376.
#16 swamp                                  3865. 
#17 tidal_marsh                             699.
#18 wetland                                 874.
#19 Everglades                              258.
#20 Everglades_other                        514.
#21 karstic_wetland                         694.

#Standard error macrophyte biomass per freshwater benthic ecosystem type
se_macro_habitat_bio <- lit_rev_macro_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(se_biomass = se(dm_gm2)) %>%
  print(n = 21)

#1 eutrophic_lake_littoral_zone         221. 
#2 lake_littoral_zone                    47.3
#3 mesotrophic_lake_littoral_zone       529. 
#4 oligotrophic_lake_littoral_zone       85.8
#5 pond                                 217. 
#6 shallow_lake                         329. 
#7 shallow_reservoir                     72.3
#8 bog                                 1450. 
#9 constructed_treatment_wetland        129. 
#10 dune_slack                            42.5
#11 fen                                   98.5
#12 floodplain_wetland                   139. 
#13 marsh                                 41.8
#14 non_tidal_coastal_wetland            236. 
#15 prairie_marsh                         49.4
#16 swamp                               2016. 
#17 tidal_marsh                          229. 
#18 wetland                               97.0
#19 Everglades                            21.9
#20 Everglades_other                      28.6
#21 karstic_wetland                      304.

#Count number of macrophyte biomass values per freshwater benthic ecosystem type
lit_rev_macro_bio %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 21)

#1 eutrophic_lake_littoral_zone       46
#2 lake_littoral_zone                294
#3 mesotrophic_lake_littoral_zone     11
#4 oligotrophic_lake_littoral_zone    13
#5 pond                               28
#6 shallow_lake                       57
#7 shallow_reservoir                   5
#8 bog                                11
#9 constructed_treatment_wetland     278
#10 dune_slack                         10
#11 fen                               105
#12 floodplain_wetland                 65
#13 marsh                              77
#14 non_tidal_coastal_wetland          66
#15 prairie_marsh                      73
#16 swamp                               4
#17 tidal_marsh                        13
#18 wetland                           190
#19 Everglades                         42
#20 Everglades_other                  120
#21 karstic_wetland                     8

#Mean macrophyte biomass per freshwater benthic ecosystem category
mean_macro_habitat_bio_cat <- lit_rev_macro_bio %>%
  group_by(category) %>%
  dplyr::summarize(mean_biomass = mean(dm_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone            315.
#2 shallow_lake_and_pond        1252.
#3 wetland                      1120.
#4 karstic_wetland               460.

#Standard error macrophyte biomass per freshwater benthic ecosystem category
se_macro_habitat_bio_cat <- lit_rev_macro_bio %>%
  group_by(category) %>%
  dplyr::summarize(se_biomass = se(dm_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone          50.8
#2 shallow_lake_and_pond      224. 
#3 wetland                     61.6
#4 karstic_wetland             26.5

#Count number of macrophyte biomass values per freshwater benthic ecosystem category
lit_rev_macro_bio %>%
  group_by(category) %>%
  tally() %>%
  print(n = 4)

#1 lake_littoral_zone      364
#2 shallow_lake_and_pond    90
#3 wetland                 892
#4 karstic_wetland         170

#Count number of publications containing macrophyte biomass per freshwater benthic ecosystem category
lit_rev_macro_bio %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 lake_littoral_zone                 51
#2 shallow_lake_and_pond              21
#3 wetland                           127
#4 karstic_wetland                     9

#Count total number of macrophyte biomass values
lit_rev_macro_bio %>%
  tally()

#1516

#Count total number of publications containing macrophyte biomass
lit_rev_macro_bio %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#206

#Macrophyte biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_macro_bio, aes(x = as.factor(category), y = as.numeric(dm_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("paleturquoise1", "cadetblue2", "deepskyblue1",
                               "deepskyblue3", "mediumblue", "navyblue", "purple4",
                               "yellow1", "yellow2", "yellow3", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "limegreen", 
                               "forestgreen", "darkgreen", "palegreen4", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", "Lake Littoral Zone",
                               "Mesotrophic Lake Littoral Zone", "Oligotrophic Lake Littoral Zone",
                               "Pond", "Shallow Lake", "Shallow Reservoir", "Bog",
                               "Constructed Treatment Wetland", "Dune Slack", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Prairie Marsh", "Swamp", "Tidal Marsh",
                               "Wet Meadow", "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                               "pond", "shallow_lake", "shallow_reservoir", "bog", 
                               "constructed_treatment_wetland", "dune_slack", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                               "wetland",  "Everglades", "Everglades_other","karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Macrophyte Biomass (DM  ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - macrophyte biomass
kruskal.test(dm_gm2 ~ category, data = lit_rev_macro_bio)

#data:  dm_gm2 by category
#Kruskal-Wallis chi-squared = 314.45, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - macrophyte biomass
dunnTest(as.numeric(dm_gm2) ~ category,
         data = lit_rev_macro_bio,
         method = "bonferroni")

#                                  Comparison           Z       P.unadj       P.adj
#1       karstic_wetland - lake_littoral_zone   9.8315510 8.234073e-23 4.940444e-22
#2    karstic_wetland - shallow_lake_and_pond  -0.7977815 4.249973e-01 1.000000e+00
#3 lake_littoral_zone - shallow_lake_and_pond  -8.6416335 5.541495e-18 3.324897e-17
#4                  karstic_wetland - wetland  -2.1129478 3.460523e-02 2.076314e-01
#5               lake_littoral_zone - wetland -17.5274232 8.848978e-69 5.309387e-68
#6            shallow_lake_and_pond - wetland  -0.6584785 5.102307e-01 1.000000e+00

#Log-transformed macrophyte biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_macro_bio, aes(x = as.factor(category), y = as.numeric(log_dm_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("paleturquoise1", "cadetblue2", "deepskyblue1",
                               "deepskyblue3", "mediumblue", "navyblue", "purple4",
                               "yellow1", "yellow2", "yellow3", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "limegreen", 
                               "forestgreen", "darkgreen", "palegreen4", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", "Lake Littoral Zone",
                               "Mesotrophic Lake Littoral Zone", "Oligotrophic Lake Littoral Zone",
                               "Pond", "Shallow Lake", "Shallow Reservoir", "Bog",
                               "Constructed Treatment Wetland", "Dune Slack", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Prairie Marsh", "Swamp", "Tidal Marsh",
                               "Wet Meadow", "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                               "pond", "shallow_lake", "shallow_reservoir", "bog", 
                               "constructed_treatment_wetland", "dune_slack", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                               "wetland",  "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Macrophyte Biomass (DM  ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - log-transformed macrophyte biomass
kruskal.test(log_dm_gm2 ~ category, data = lit_rev_macro_bio)

#data:  log_dm_gm2 by category
#Kruskal-Wallis chi-squared = 314.96, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - log-transformed macrophyte biomass
dunnTest(as.numeric(log_dm_gm2) ~ category,
         data = lit_rev_macro_bio,
         method = "bonferroni")

#                                   Comparison          Z       P.unadj       P.adj
#1       karstic_wetland - lake_littoral_zone   9.8281541 8.516541e-23 5.109925e-22
#2    karstic_wetland - shallow_lake_and_pond  -0.7946693 4.268059e-01 1.000000e+00
#3 lake_littoral_zone - shallow_lake_and_pond  -8.6355067 5.846728e-18 3.508037e-17
#4                  karstic_wetland - wetland  -2.1278494 3.334957e-02 2.000974e-01
#5               lake_littoral_zone - wetland -17.5449908 6.496331e-69 3.897798e-68
#6            shallow_lake_and_pond - wetland  -0.6733125 5.007485e-01 1.000000e+00

#Macrophyte total phosphorus stock among freshwater benthic ecosystem types####

#Subset dataset to contain only needed variables 
lit_rev_macro_tp <- lit_rev_macro %>%
  select(publication_citation, habitat_type, category, group, tp_stock_gm2, log_tp_stock_gm2) %>%
  filter(tp_stock_gm2 != -9999, log_tp_stock_gm2 != -9999)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_macro_tp$habitat_type = factor(lit_rev_macro_tp$habitat_type, levels = c("lake_littoral_zone", 
                                                                                 "pond", "shallow_lake", "bog", 
                                                                                 "constructed_treatment_wetland", "fen", 
                                                                                 "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                                 "tidal_marsh", "wetland", "Everglades", 
                                                                                 "Everglades_other", "karstic_wetland"))

lit_rev_macro_tp$category = factor(
  lit_rev_macro_tp$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean macrophyte total phosphorus stock per freshwater benthic ecosystem type
mean_macro_habitat_tp <- lit_rev_macro_tp %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_tp_stock = mean(tp_stock_gm2)) %>%
  print(n = 13)

#1 lake_littoral_zone                   48.8  
#2 pond                                  0.148
#3 shallow_lake                          1.06 
#4 bog                                   0.5
#5 constructed_treatment_wetland         5.58 
#6 fen                                   0.867
#7 floodplain_wetland                    2.12 
#8 marsh                                 0.99 
#9 non_tidal_coastal_wetland             1.53 
#10 tidal_marsh                           0.915
#11 wetland                               1.23 
#12 Everglades                            0.244
#13 karstic_wetland                       0.15  

#Standard error macrophyte total phosphorus stock per freshwater benthic ecosystem type
se_macro_habitat_tp <- lit_rev_macro_tp %>%
  group_by(habitat_type) %>%
  dplyr::summarize(se_tp_stock = se(tp_stock_gm2)) %>%
  print(n = 13)

#1 lake_littoral_zone                7.55  
#2 pond                              0.0353
#3 shallow_lake                      0.189 
#4 bog                              NA     
#5 constructed_treatment_wetland     0.984 
#6 fen                               0.134 
#7 floodplain_wetland                1.98  
#8 marsh                             0.55  
#9 non_tidal_coastal_wetland         0.315 
#10 tidal_marsh                       0.0868
#11 wetland                           0.125 
#12 Everglades                        0.0833
#13 karstic_wetland                  NA  

#Count number of macrophyte total phosphorus stock values per freshwater benthic ecosystem type
lit_rev_macro_tp %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 13)

#1 lake_littoral_zone               46
#2 pond                             24
#3 shallow_lake                     21
#4 bog                               1
#5 constructed_treatment_wetland   224
#6 fen                              56
#7 floodplain_wetland                7
#8 marsh                             2
#9 non_tidal_coastal_wetland        39
#10 tidal_marsh                       8
#11 wetland                          22
#12 Everglades                       36
#13 karstic_wetland                   1

#Mean macrophyte total phosphorus stock per freshwater benthic ecosystem category
mean_macro_habitat_tp_cat <- lit_rev_macro_tp %>%
  group_by(category) %>%
  dplyr::summarize(mean_tp_stock = mean(tp_stock_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone          48.8  
#2 shallow_lake_and_pond        0.573
#3 wetland                      3.93 
#4 karstic_wetland              0.242

#Standard error macrophyte total phosphorus stock per freshwater benthic ecosystem category
se_macro_habitat_tp_cat <- lit_rev_macro_tp %>%
  group_by(category) %>%
  dplyr::summarize(se_tp_stock = se(tp_stock_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone        7.55  
#2 shallow_lake_and_pond     0.112 
#3 wetland                   0.626 
#4 karstic_wetland           0.0810


#Count number of macrophyte total phosphorus stock values per freshwater benthic ecosystem category
lit_rev_macro_tp %>%
  group_by(category) %>%
  tally() %>%
  print(n = 4)

#1 lake_littoral_zone       46
#2 shallow_lake_and_pond    45
#3 wetland                 359
#4 karstic_wetland          37

#Count number of publications containing macrophyte total phosphorus stock per freshwater benthic ecosystem category
lit_rev_macro_tp %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 lake_littoral_zone                  1
#2 shallow_lake_and_pond               4
#3 wetland                            55
#4 karstic_wetland                     2

#Count total number of macrophyte total phosphorus stock values
lit_rev_macro_tp %>%
  tally()

#487

#Count total number of publications containing macrophyte total phosphorus stock
lit_rev_macro_tp %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#62

#Macrophyte total phosphorus stock among freshwater benthic ecosystem types and categories
ggplot(lit_rev_macro_tp, aes(x = as.factor(category), y = as.numeric(tp_stock_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("cadetblue2", "mediumblue", "navyblue", "yellow1", "yellow2", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "darkgreen", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Lake Littoral Zone",
                               "Pond", "Shallow Lake", "Bog",
                               "Constructed Treatment Wetland", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Tidal Marsh",
                               "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("lake_littoral_zone", "pond", "shallow_lake", "bog", 
                               "constructed_treatment_wetland", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "tidal_marsh", "wetland",  "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Macrophyte TP Stock ( ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - macrophyte total phosphorus stock
kruskal.test(tp_stock_gm2 ~ category, data = lit_rev_macro_tp)

#data:  tp_stock_gm2 by category
#Kruskal-Wallis chi-squared = 157.62, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - macrophyte total phosphorus stock
dunnTest(as.numeric(tp_stock_gm2) ~ category,
         data = lit_rev_macro_tp,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1       karstic_wetland - lake_littoral_zone -11.034332 2.609842e-28 1.565905e-27
#2    karstic_wetland - shallow_lake_and_pond  -1.361007 1.735115e-01 1.000000e+00
#3 lake_littoral_zone - shallow_lake_and_pond  10.181166 2.406614e-24 1.443968e-23
#4                  karstic_wetland - wetland  -6.847116 7.535349e-12 4.521209e-11
#5               lake_littoral_zone - wetland   8.010512 1.142318e-15 6.853909e-15
#6            shallow_lake_and_pond - wetland  -5.566058 2.605665e-08 1.563399e-07

#Log-transformed macrophyte total phosphorus stock among freshwater benthic ecosystem types and categories
ggplot(lit_rev_macro_tp, aes(x = as.factor(category), y = as.numeric(log_tp_stock_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("cadetblue2", "mediumblue", "navyblue", "yellow1", "yellow2", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "darkgreen", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Lake Littoral Zone",
                               "Pond", "Shallow Lake", "Bog",
                               "Constructed Treatment Wetland", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Tidal Marsh",
                               "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("lake_littoral_zone", "pond", "shallow_lake", "bog", 
                               "constructed_treatment_wetland", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "tidal_marsh", "wetland",  "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Macrophyte TP Stock ( ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test -log-transformed macrophyte total phosphorus stock
kruskal.test(log_tp_stock_gm2 ~ category, data = lit_rev_macro_tp)

#data:  log_tp_stock_gm2 by category
#Kruskal-Wallis chi-squared = 149.06, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - log-transformed macrophyte total phosphorus stock
dunnTest(as.numeric(log_tp_stock_gm2) ~ category,
         data = lit_rev_macro_tp,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1       karstic_wetland - lake_littoral_zone -11.031342 2.698072e-28 1.618843e-27
#2    karstic_wetland - shallow_lake_and_pond  -1.896990 5.782929e-02 3.469758e-01
#3 lake_littoral_zone - shallow_lake_and_pond   9.610714 7.204757e-22 4.322854e-21
#4                  karstic_wetland - wetland  -6.808539 9.859471e-12 5.915682e-11
#5               lake_littoral_zone - wetland   8.048830 8.358940e-16 5.015364e-15
#6            shallow_lake_and_pond - wetland  -4.771772 1.826119e-06 1.095671e-05

#Benthic algal biomass among freshwater benthic ecosystem types####

#Subset dataset to contain only needed variables 
lit_rev_micro_bio <- lit_rev_micro %>%
  select(publication_citation, habitat_type, category, group, chla_gm3, log_chla_gm3) %>%
  filter(chla_gm3 != -9999, log_chla_gm3 != -9999)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_micro_bio$habitat_type = factor(lit_rev_micro_bio$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                   "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                   "reservoir_littoral_zone", "pond", "shallow_lagoon", 
                                                                                   "shallow_lake", "shallow_reservoir",
                                                                                   "constructed_treatment_wetland", "fen", 
                                                                                   "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                                   "prairie_marsh", "swamp",
                                                                                   "wetland", "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_micro_bio$category = factor(
  lit_rev_micro_bio$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean benthic algal biomass per freshwater benthic ecosystem type
mean_micro_habitat_bio <- lit_rev_micro_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_biomass = mean(chla_gm3)) %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone          0.110 
#2 lake_littoral_zone                    0.169 
#3 mesotrophic_lake_littoral_zone        0.272 
#4 oligotrophic_lake_littoral_zone      21.8   
#5 reservoir_littoral_zone               0.0471
#6 pond                                  1.54  
#7 shallow_lagoon                        0.1   
#8 shallow_lake                         10.3   
#9 shallow_reservoir                     0.107 
#10 constructed_treatment_wetland        19.1   
#11 fen                                   0.026 
#12 floodplain_wetland                    0.0577
#13 marsh                                 0.209 
#14 non_tidal_coastal_wetland             0.453 
#15 prairie_marsh                         0.138 
#16 swamp                                 3.31  
#17 wetland                               0.179 
#18 Everglades                            0.0924
#19 Everglades_other                      1.46  
#20 karstic_wetland                       0.368 

#Standard error benthic algal biomass per freshwater benthic ecosystem type
se_micro_habitat_bio <- lit_rev_micro_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(se_biomass = se(chla_gm3)) %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone       0.0223 
#2 lake_littoral_zone                 0.0235 
#3 mesotrophic_lake_littoral_zone     0.118  
#4 oligotrophic_lake_littoral_zone    6.12   
#5 reservoir_littoral_zone            0.0109 
#6 pond                               0.824  
#7 shallow_lagoon                     0.0583 
#8 shallow_lake                       6.59   
#9 shallow_reservoir                  0.0917 
#10 constructed_treatment_wetland     11.0    
#11 fen                                0.00812
#12 floodplain_wetland                 0.00835
#13 marsh                              0.167  
#14 non_tidal_coastal_wetland          0.123  
#15 prairie_marsh                      0.0622 
#16 swamp                              1.41   
#17 wetland                            0.0796 
#18 Everglades                         0.0218 
#19 Everglades_other                   1.31   
#20 karstic_wetland                    0.229  

#Count number of benthic algal biomass values per freshwater benthic ecosystem type
lit_rev_micro_bio %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone      130
#2 lake_littoral_zone                107
#3 mesotrophic_lake_littoral_zone     30
#4 oligotrophic_lake_littoral_zone   159
#5 reservoir_littoral_zone            17
#6 pond                               21
#7 shallow_lagoon                      9
#8 shallow_lake                       47
#9 shallow_reservoir                   3
#10 constructed_treatment_wetland       9
#11 fen                                 5
#12 floodplain_wetland                 22
#13 marsh                              15
#14 non_tidal_coastal_wetland          33
#15 prairie_marsh                       4
#16 swamp                              20
#17 wetland                            15
#18 Everglades                         42
#19 Everglades_other                    6
#20 karstic_wetland                     5

#Mean benthic algal biomass per freshwater benthic ecosystem category
mean_micro_habitat_bio_cat <- lit_rev_micro_bio %>%
  group_by(category) %>%
  dplyr::summarize(mean_biomass = mean(chla_gm3)) %>%
  print(n = 4)

#1 lake_littoral_zone           7.93 
#2 shallow_lake_and_pond        6.49 
#3 wetland                      2.12 
#4 karstic_wetland              0.274

#Standard error benthic algal biomass per freshwater benthic ecosystem category
se_micro_habitat_bio_cat <- lit_rev_micro_bio %>%
  group_by(category) %>%
  dplyr::summarize(se_biomass = se(chla_gm3)) %>%
  print(n = 4)

#1 lake_littoral_zone         2.25 
#2 shallow_lake_and_pond      3.89 
#3 wetland                    0.912
#4 karstic_wetland            0.151

#Count number of benthic algal biomass values per freshwater benthic ecosystem category
lit_rev_micro_bio %>%
  group_by(category) %>%
  tally() %>%
  print(n = 4)

#1 lake_littoral_zone      443
#2 shallow_lake_and_pond    80
#3 wetland                 123
#4 karstic_wetland          53

#Count number of publications with benthic algal biomass per freshwater benthic ecosystem category
lit_rev_micro_bio %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 lake_littoral_zone                 48
#2 shallow_lake_and_pond              27
#3 wetland                            19
#4 karstic_wetland                     5

#Count total number of benthic algal biomass values
lit_rev_micro_bio %>%
  tally()

#699

#Count total number of publications containing benthic algal biomass
lit_rev_micro_bio %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#95

#Benthic algal biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_micro_bio, aes(x = as.factor(category), y = as.numeric(chla_gm3), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "olivedrab4", "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Benthic Algal Biomass (Chl-a   ",g," ",m^-3,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - benthic algal biomass
kruskal.test(chla_gm3 ~ category, data = lit_rev_micro_bio)

#data:  chla_gm3 by category
#Kruskal-Wallis chi-squared = 16.273, df = 3, p-value = 0.0009968

#Post-hoc test: Dunn's Test - benthic algal biomass
dunnTest(as.numeric(chla_gm3) ~ category,
         data = lit_rev_micro_bio,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj       P.adj
#1       karstic_wetland - lake_littoral_zone  0.5316123 0.594994575 1.000000000
#2    karstic_wetland - shallow_lake_and_pond -1.0635642 0.287526149 1.000000000
#3 lake_littoral_zone - shallow_lake_and_pond -2.1866613 0.028767257 0.172603541
#4                  karstic_wetland - wetland -1.8410425 0.065615326 0.393691956
#5               lake_littoral_zone - wetland -3.7262093 0.000194381 0.001166286
#6            shallow_lake_and_pond - wetland -0.7946377 0.426824292 1.000000000

#Log-transformed benthic algal biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_micro_bio, aes(x = as.factor(category), y = as.numeric(log_chla_gm3), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "olivedrab4", "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Benthic Algal Biomass (Chl-a   ",g," ",m^-3,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - log-transformed benthic algal biomass
kruskal.test(log_chla_gm3 ~ category, data = lit_rev_micro_bio)

#data:  log_chla_gm3 by category
#Kruskal-Wallis chi-squared = 9.8745, df = 3, p-value = 0.01966

#Post-hoc test: Dunn's Test - log-transformed benthic algal biomass
dunnTest(as.numeric(log_chla_gm3) ~ category,
         data = lit_rev_micro_bio,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj        P.adj
#1       karstic_wetland - lake_littoral_zone  0.5818344 0.5606782425 1.000000000
#2    karstic_wetland - shallow_lake_and_pond -1.1039761 0.2696035112 1.000000000
#3 lake_littoral_zone - shallow_lake_and_pond -2.3056679 0.0211291973 0.126775184
#4                  karstic_wetland - wetland -1.7842236 0.0743873395 0.446324037
#5               lake_littoral_zone - wetland -3.7062286 0.0002103685 0.001262211
#6            shallow_lake_and_pond - wetland -0.6798072 0.4966265210 1.000000000


#Benthic microbial total phosphorus stock among freshwater benthic ecosystem types####

#Subset dataset to contain only needed variables 
lit_rev_micro_tp <- lit_rev_micro %>%
  select(publication_citation, habitat_type, category, group, tp_stock_gm2, log_tp_stock_gm2) %>%
  filter(tp_stock_gm2 != -9999, log_tp_stock_gm2 != -9999)

unique(lit_rev_micro_tp$habitat_type)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_micro_tp$habitat_type = factor(lit_rev_micro_tp$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                 "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                 "shallow_lake", "constructed_treatment_wetland", 
                                                                                 "prairie_marsh", "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_micro_tp$category = factor(
  lit_rev_micro_tp$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean benthic microbial total phosphorus stock per freshwater benthic ecosystem type
mean_micro_habitat_tp <- lit_rev_micro_tp %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_tp_stock = mean(tp_stock_gm2)) %>%
  print(n = 10)

#1 eutrophic_lake_littoral_zone         56.0    
#2 lake_littoral_zone                    0.864  
#3 mesotrophic_lake_littoral_zone        0.004  
#4 oligotrophic_lake_littoral_zone       0.00839
#5 shallow_lake                         20.0    
#6 constructed_treatment_wetland         0.85   
#7 prairie_marsh                         0.31   
#8 Everglades                            0.00650
#9 Everglades_other                      0.0974 
#10 karstic_wetland                       0.0126 

#Standard error benthic microbial total phosphorus stock per freshwater benthic ecosystem type
se_micro_habitat_tp <- lit_rev_micro_tp %>%
  group_by(habitat_type) %>%
  dplyr::summarize(se_tp_stock = se(tp_stock_gm2)) %>%
  print(n = 10)

#1 eutrophic_lake_littoral_zone      36.8     
#2 lake_littoral_zone                 0.364   
#3 mesotrophic_lake_littoral_zone    NA       
#4 oligotrophic_lake_littoral_zone    0.00431 
#5 shallow_lake                      10.7     
#6 constructed_treatment_wetland     NA       
#7 prairie_marsh                     NA       
#8 Everglades                         0.000801
#9 Everglades_other                   0.0243  
#10 karstic_wetland                    0.00468 

#Count number of benthic microbial total phosphorus stock values per freshwater benthic ecosystem type
lit_rev_micro_tp %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 10)

#1 eutrophic_lake_littoral_zone       55
#2 lake_littoral_zone                 23
#3 mesotrophic_lake_littoral_zone      1
#4 oligotrophic_lake_littoral_zone     8
#5 shallow_lake                       15
#6 constructed_treatment_wetland       1
#7 prairie_marsh                       1
#8 Everglades                         42
#9 Everglades_other                   96
#10 karstic_wetland                     5

#Mean benthic microbial total phosphorus stock per freshwater benthic ecosystem category
mean_micro_habitat_stock_cat <- lit_rev_micro_tp %>%
  group_by(category) %>%
  dplyr::summarize(mean_tp_stock = mean(tp_stock_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone         35.6   
#2 shallow_lake_and_pond      20.0   
#3 wetland                     0.58  
#4 karstic_wetland             0.0677

#Standard error benthic microbial total phosphorus stock per freshwater benthic ecosystem category
se_micro_habitat_stock_cat <- lit_rev_micro_tp %>%
  group_by(category) %>%
  dplyr::summarize(se_tp_stock = se(tp_stock_gm2)) %>%
  print(n = 4)

#1 lake_littoral_zone       23.4   
#2 shallow_lake_and_pond    10.7   
#3 wetland                   0.27  
#4 karstic_wetland           0.0167

#Count number of benthic microbial total phosphorus stock values per freshwater benthic ecosystem category
lit_rev_micro_tp %>%
  group_by(category) %>%
  tally() %>%
  print(n = 4)

#1 lake_littoral_zone       87
#2 shallow_lake_and_pond    15
#3 wetland                   2
#4 karstic_wetland         143

#Count number of publications with benthic microbial total phosphorus stock per freshwater benthic ecosystem category
lit_rev_micro_tp %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 lake_littoral_zone                 10
#2 shallow_lake_and_pond               3
#3 wetland                             2
#4 karstic_wetland                    12

#Count total number of benthic microbial total phosphorus stock values
lit_rev_micro_tp %>%
  tally()

#247

#Count total number of publications containing benthic microbial total phosphorus stock
lit_rev_micro_tp %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#27

#Benthic microbial total phosphorus stock among freshwater benthic ecosystem types and categories
ggplot(lit_rev_micro_tp, aes(x = as.factor(category), y = as.numeric(tp_stock_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "navyblue",
                                "yellow2",  "limegreen", 
                                "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Shallow Lake",
                               "Constructed Treatment Wetland", "Prairie Marsh",
                               "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "shallow_lake",
                               "constructed_treatment_wetland", "prairie_marsh",
                               "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Benthic Microbial TP Stock ( ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - benthic microbial total phosphorus stock
kruskal.test(tp_stock_gm2 ~ category, data = lit_rev_micro_tp)

#data:  tp_stock_gm2 by category
#Kruskal-Wallis chi-squared = 36.607, df = 3, p-value = 5.571e-08

#Post-hoc test: Dunn's Test - benthic microbial total phosphorus stock
dunnTest(as.numeric(tp_stock_gm2) ~ category,
         data = lit_rev_micro_tp,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1       karstic_wetland - lake_littoral_zone -5.5422814 2.985560e-08 1.791336e-07
#2    karstic_wetland - shallow_lake_and_pond -2.4912397 1.272982e-02 7.637893e-02
#3 lake_littoral_zone - shallow_lake_and_pond  0.2770007 7.817796e-01 1.000000e+00
#4                  karstic_wetland - wetland -2.3614734 1.820248e-02 1.092149e-01
#5               lake_littoral_zone - wetland -1.2973901 1.944970e-01 1.000000e+00
#6            shallow_lake_and_pond - wetland -1.3354896 1.817162e-01 1.000000e+00

#Log-transformed benthic microbial total phosphorus stock among freshwater benthic ecosystem types and categories
ggplot(lit_rev_micro_tp, aes(x = as.factor(category), y = as.numeric(log_tp_stock_gm2), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "navyblue",
                                "yellow2",  "limegreen", 
                                "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Shallow Lake",
                               "Constructed Treatment Wetland", "Prairie Marsh",
                               "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "shallow_lake",
                               "constructed_treatment_wetland", "prairie_marsh",
                               "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Benthic Microbial TP Stock ( ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - log-transformed benthic microbial total phosphorus stock
kruskal.test(log_tp_stock_gm2 ~ category, data = lit_rev_micro_tp)

#data:  log_tp_stock_gm2 by category
#Kruskal-Wallis chi-squared = 36.553, df = 3, p-value = 5.72e-08

#Post-hoc test: Dunn's Test - log-transformed benthic microbial total phosphorus stock
dunnTest(as.numeric(log_tp_stock_gm2) ~ category,
         data = lit_rev_micro_tp,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1       karstic_wetland - lake_littoral_zone -5.5365351 3.085141e-08 1.851085e-07
#2    karstic_wetland - shallow_lake_and_pond -2.4936162 1.264492e-02 7.586951e-02
#3 lake_littoral_zone - shallow_lake_and_pond  0.2718989 7.856997e-01 1.000000e+00
#4                  karstic_wetland - wetland -2.3608759 1.823183e-02 1.093910e-01
#5               lake_littoral_zone - wetland -1.2978877 1.943259e-01 1.000000e+00
#6            shallow_lake_and_pond - wetland -1.3340676 1.821817e-01 1.000000e+00

#Phytoplankton biomass among freshwater benthic ecosystem types####

#Subset dataset to contain only needed variables 
lit_rev_phyto_bio <- lit_rev_phyto %>%
  select(publication_citation, habitat_type, category, group, chla_ugl, log_chla_ugl) %>%
  filter(chla_ugl != -9999, log_chla_ugl != -9999)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_phyto_bio$habitat_type = factor(lit_rev_phyto_bio$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                   "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                   "shallow_lake", "shallow_reservoir",
                                                                                   "constructed_treatment_wetland", "non_tidal_coastal_wetland",
                                                                                   "floodplain_wetland", "swamp", "wet_meadow", "wetland"))

lit_rev_phyto_bio$category = factor(
  lit_rev_phyto_bio$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean phytoplankton biomass per freshwater benthic ecosystem type
mean_phyto_habitat_bio <- lit_rev_phyto_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_biomass = mean(chla_ugl)) %>%
  print(n = 12)

#1 eutrophic_lake_littoral_zone           18.8 
#2 lake_littoral_zone                     11.0 
#3 mesotrophic_lake_littoral_zone        100.  
#4 oligotrophic_lake_littoral_zone         5.87
#5 shallow_lake                         1790.  
#6 shallow_reservoir                       7.91
#7 constructed_treatment_wetland         340.  
#8 non_tidal_coastal_wetland              10.8 
#9 floodplain_wetland                     23.9 
#10 swamp                                  24.8 
#11 wet_meadow                              9.83
#12 wetland                                30.1 

#Standard error phytoplankton biomass per freshwater benthic ecosystem type
se_phyto_habitat_bio <- lit_rev_phyto_bio %>%
  group_by(habitat_type) %>%
  dplyr::summarize(se_biomass = se(chla_ugl)) %>%
  print(n = 12)

# 1 eutrophic_lake_littoral_zone          3.27
#2 lake_littoral_zone                    1.54
#3 mesotrophic_lake_littoral_zone       70.5 
#4 oligotrophic_lake_littoral_zone       4.15
#5 shallow_lake                        967.  
#6 shallow_reservoir                     1.12
#7 constructed_treatment_wetland       175.  
#8 non_tidal_coastal_wetland             4.04
#9 floodplain_wetland                    4.60
#10 swamp                                 1.15
#11 wet_meadow                            4.05
#12 wetland                              13.1 

#Count number of phytoplankton biomass values per freshwater benthic ecosystem type
lit_rev_phyto_bio %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 12)

#1 eutrophic_lake_littoral_zone       50
#2 lake_littoral_zone                 78
#3 mesotrophic_lake_littoral_zone     14
#4 oligotrophic_lake_littoral_zone    38
#5 shallow_lake                       73
#6 shallow_reservoir                  11
#7 constructed_treatment_wetland      17
#8 non_tidal_coastal_wetland          28
#9 floodplain_wetland                 27
#10 swamp                               2
#11 wet_meadow                          7
#12 wetland                            22

#Mean phytoplankton biomass per freshwater benthic ecosystem category
mean_phyto_habitat_bio_cat <- lit_rev_phyto_bio %>%
  group_by(category) %>%
  dplyr::summarize(mean_biomass = mean(chla_ugl)) %>%
  print(n = 3)

#1 lake_littoral_zone            19.0
#2 shallow_lake_and_pond       1557. 
#3 wetland                       73.0

#Standard error phytoplankton biomass per freshwater benthic ecosystem category
se_phyto_habitat_bio_cat <- lit_rev_phyto_bio %>%
  group_by(category) %>%
  dplyr::summarize(se_biomass = se(chla_ugl)) %>%
  print(n = 3)

#1 lake_littoral_zone          5.77
#2 shallow_lake_and_pond     842.  
#3 wetland                    30.7 

#Count number of phytoplankton biomass values per freshwater benthic ecosystem category
lit_rev_phyto_bio %>%
  group_by(category) %>%
  tally() %>%
  print(n = 3)

#1 lake_littoral_zone      180
#2 shallow_lake_and_pond    84
#3 wetland                 103

#Count number of publications with phytoplankton biomass per freshwater benthic ecosystem category
lit_rev_phyto_bio %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#category           Unique_Elements
#<chr>                     <int>
#1 lake_littoral_zone                 29
#2 shallow_lake_and_pond              15
#3 wetland                            17

#Count total number of phytoplankton biomass values
lit_rev_phyto_bio %>%
  tally()

#367

#Count total number of publications containing phytoplankton biomass
lit_rev_phyto_bio %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#58

#Phytoplankton biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_phyto_bio, aes(x = as.factor(category), y = as.numeric(chla_ugl), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3",
                                "navyblue", "purple4",
                                "yellow2", "olivedrab1", "green", 
                                "forestgreen", "palegreen4", "olivedrab4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir", "Shallow Lake",
                               "Constructed Treatment Wetland", "Floodplain Wetland",
                               "Non-Tidal Coastal Wetland", "Swamp", "Wet Meadow", "Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "floodplain_wetland",
                               "non_tidal_coastal_wetland", "swamp", "wet_meadow", "wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Phytoplankton Biomass (Chl-a   ",µg," ",L^-1,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - phytoplankton biomass
kruskal.test(chla_ugl ~ category, data = lit_rev_phyto_bio)

#data:  chla_ugl by category
#Kruskal-Wallis chi-squared = 30.552, df = 2, p-value = 2.322e-07

#Post-hoc test: Dunn's Test - phytoplankton biomass
dunnTest(as.numeric(chla_ugl) ~ category,
         data = lit_rev_phyto_bio,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1 lake_littoral_zone - shallow_lake_and_pond -4.6741824 2.951269e-06 8.853808e-06
#2               lake_littoral_zone - wetland -4.3647277 1.272812e-05 3.818437e-05
#3            shallow_lake_and_pond - wetland  0.5331289 5.939443e-01 1.000000e+00

#Log-transformed phytoplankton biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_phyto_bio, aes(x = as.factor(category), y = as.numeric(log_chla_ugl), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3",
                                "navyblue", "purple4",
                                "yellow2", "olivedrab1", "green", 
                                "forestgreen", "palegreen4", "olivedrab4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir", "Shallow Lake",
                               "Constructed Treatment Wetland", "Floodplain Wetland",
                               "Non-Tidal Coastal Wetland", "Swamp", "Wet Meadow", "Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "floodplain_wetland",
                               "non_tidal_coastal_wetland", "swamp", "wet_meadow", "wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Phytoplankton Biomass (Chl-a   ",µg," ",L^-1,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - log-transformed phytoplankton biomass
kruskal.test(log_chla_ugl ~ category, data = lit_rev_phyto_bio)

#data:  log_chla_ugl by category
#Kruskal-Wallis chi-squared = 30.54, df = 2, p-value = 2.335e-07

#Post-hoc test: Dunn's Test - log-transformed phytoplankton biomass
dunnTest(as.numeric(log_chla_ugl) ~ category,
         data = lit_rev_phyto_bio,
         method = "bonferroni")

#                                   Comparison         Z       P.unadj        P.adj
#1 lake_littoral_zone - shallow_lake_and_pond -4.6729437 2.969132e-06 8.907397e-06
#2               lake_littoral_zone - wetland -4.3643102 1.275245e-05 3.825736e-05
#3            shallow_lake_and_pond - wetland  0.5323664 5.944723e-01 1.000000e+00


#Water column total phosphorus among freshwater benthic ecosystem types####

#Subset datasets to contain only needed variables 
lit_rev_phyto_tp_wc <- lit_rev_phyto %>%
  select(publication_citation, habitat_type, category, group, water_tp_ugl, log_water_tp_ugl) %>%
  filter(water_tp_ugl != -9999, log_water_tp_ugl != -9999)

lit_rev_micro_tp_wc <- lit_rev_micro %>%
  select(publication_citation, habitat_type, category, group, water_tp_ugl, log_water_tp_ugl) %>%
  filter(water_tp_ugl != -9999, log_water_tp_ugl != -9999)

#Combine datasets together
lit_rev_phyto_micro_tp_wc <- rbind(lit_rev_micro_tp_wc, lit_rev_phyto_tp_wc)

lit_rev_phyto_micro_tp_wc$water_tp_ugl <- as.numeric(lit_rev_phyto_micro_tp_wc$water_tp_ugl)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_phyto_micro_tp_wc$habitat_type = factor(lit_rev_phyto_micro_tp_wc$habitat_type, 
                                                levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                           "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                           "reservoir_littoral_zone", "pond", "shallow_lagoon",
                                                           "shallow_lake", "shallow_reservoir",
                                                           "constructed_treatment_wetland", "fen", 
                                                           "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                           "prairie_marsh", "swamp", "wet_meadow", 
                                                           "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_phyto_micro_tp_wc$category = factor(
  lit_rev_phyto_micro_tp_wc$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Mean water column total phosphorus per freshwater benthic ecosystem type
mean_phyto_micro_habitat_tp_wc <- lit_rev_phyto_micro_tp_wc %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_water_tp = mean(water_tp_ugl)) %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone          111.  
#2 lake_littoral_zone                     27.7 
#3 mesotrophic_lake_littoral_zone         67.1 
#4 oligotrophic_lake_littoral_zone        12.2 
#5 reservoir_littoral_zone                23.1 
#6 pond                                  245.  
#7 shallow_lagoon                        302.  
#8 shallow_lake                          242.  
#9 shallow_reservoir                      17.9 
#10 constructed_treatment_wetland        2422.  
#11 fen                                    21.8 
#12 floodplain_wetland                    901.  
#13 marsh                                  27.2 
#14 non_tidal_coastal_wetland             272.  
#15 prairie_marsh                         160   
#16 swamp                                  62.5 
#17 wet_meadow                            133.  
#18 wetland                               151.  
#19 Everglades_other                       13.5 
#20 karstic_wetland                         9.78

#Standard error water column total phosphorus per freshwater benthic ecosystem type
se_phyto_micro_habitat_tp_wc <- lit_rev_phyto_micro_tp_wc %>%
  group_by(habitat_type) %>%
  dplyr::summarize(mean_water_tp = se(water_tp_ugl)) %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone          11.5  
#2 lake_littoral_zone                     2.42 
#3 mesotrophic_lake_littoral_zone        16.8  
#4 oligotrophic_lake_littoral_zone        1.15 
#5 reservoir_littoral_zone                7.01 
#6 pond                                  64.3  
#7 shallow_lagoon                        97.8  
#8 shallow_lake                          21.6  
#9 shallow_reservoir                      1.77 
#10 constructed_treatment_wetland        450.   
#11 fen                                   NA    
#12 floodplain_wetland                   169.   
#13 marsh                                  4.84 
#14 non_tidal_coastal_wetland            111.   
#15 prairie_marsh                          0    
#16 swamp                                 40.4  
#17 wet_meadow                            51.5  
#18 wetland                               35.2  
#19 Everglades_other                       4.19 
#20 karstic_wetland                        0.135

#Count number of water column total phosphorus values per freshwater benthic ecosystem type
lit_rev_phyto_micro_tp_wc %>%
  group_by(habitat_type) %>%
  tally() %>%
  print(n = 20)

#1 eutrophic_lake_littoral_zone      115
#2 lake_littoral_zone                161
#3 mesotrophic_lake_littoral_zone     36
#4 oligotrophic_lake_littoral_zone   111
#5 reservoir_littoral_zone             9
#6 pond                               10
#7 shallow_lagoon                      9
#8 shallow_lake                       99
#9 shallow_reservoir                  12
#10 constructed_treatment_wetland      21
#11 fen                                 1
#12 floodplain_wetland                 43
#13 marsh                               4
#14 non_tidal_coastal_wetland          55
#15 prairie_marsh                       2
#16 swamp                              22
#17 wet_meadow                          7
#18 wetland                            22
#19 Everglades_other                   35
#20 karstic_wetland                     5

#Mean water column total phosphorus per freshwater benthic ecosystem category
mean_lit_rev_phyto_micro_tp_wc_cat <- lit_rev_phyto_micro_tp_wc %>%
  group_by(category) %>%
  dplyr::summarize(mean_water_tp = mean(water_tp_ugl)) %>%
  print(n = 4)

#1 lake_littoral_zone            49.0
#2 shallow_lake_and_pond        226. 
#3 wetland                      625. 
#4 karstic_wetland               13.0

#Standard error water column total phosphorus per freshwater benthic ecosystem category
se_lit_rev_phyto_micro_tp_wc_cat <- lit_rev_phyto_micro_tp_wc %>%
  group_by(category) %>%
  dplyr::summarize(se_water_tp = se(water_tp_ugl)) %>%
  print(n = 4)

#1 lake_littoral_zone          3.97
#2 shallow_lake_and_pond      19.2 
#3 wetland                    92.8 
#4 karstic_wetland             3.66

#Count number of water column total phosphorusvalues per freshwater benthic ecosystem category
lit_rev_phyto_micro_tp_wc %>%
  group_by(category) %>%
  tally() %>%
  print(n = 4)

#1 lake_littoral_zone      432
#2 shallow_lake_and_pond   130
#3 wetland                 177
#4 karstic_wetland          40

#Count number of publications with water column total phosphorus per freshwater benthic ecosystem category
lit_rev_phyto_micro_tp_wc %>%
  group_by(category) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 lake_littoral_zone                 48
#2 shallow_lake_and_pond              28
#3 wetland                            22
#4 karstic_wetland                     6

#Count total number of water column total phosphorus values
lit_rev_phyto_micro_tp_wc %>%
  tally()

#779

#Count total number of publications containing water column total phosphorus
lit_rev_phyto_micro_tp_wc %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#99

#water column total phosphorus among freshwater benthic ecosystem types and categories
ggplot(lit_rev_phyto_micro_tp_wc, aes(x = as.factor(category), y = as.numeric(water_tp_ugl), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("paleturquoise1", "cadetblue2", "deepskyblue1",
                               "deepskyblue3", "turquoise4",
                               "mediumblue", "slateblue4", "navyblue", "purple4",
                               "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                               "forestgreen", "palegreen4", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", "Lake Littoral Zone",
                               "Mesotrophic Lake Littoral Zone", "Oligotrophic Lake Littoral Zone",
                               "Reservoir Littoral Zone", "Pond", "Shallow Lagoon",
                               "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland",  "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Prairie Marsh", "Swamp",
                               "Wet Meadow", "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone","lake_littoral_zone", 
                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                               "reservoir_littoral_zone", "pond", "shallow_lagoon",
                               "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "prairie_marsh", "swamp", "wet_meadow", 
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Water TP ( ",µg," ",L^-1,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - water column total phosphorus
kruskal.test(water_tp_ugl ~ category, data = lit_rev_phyto_micro_tp_wc)

#data:  total_p_ugl by category
#Kruskal-Wallis chi-squared = 231.43, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - water column total phosphorus
dunnTest(as.numeric(water_tp_ugl) ~ category,
         data = lit_rev_phyto_micro_tp_wc,
         method = "bonferroni")

#                                  Comparison          Z      P.unadj         P.adj
#1       karstic_wetland - lake_littoral_zone  -3.670876 2.417202e-04 1.450321e-03
#2    karstic_wetland - shallow_lake_and_pond  -9.784292 1.315128e-22 7.890767e-22
#3 lake_littoral_zone - shallow_lake_and_pond -11.619926 3.264201e-31 1.958520e-30
#4                  karstic_wetland - wetland  -8.689767 3.631848e-18 2.179109e-17
#5               lake_littoral_zone - wetland -10.248616 1.200500e-24 7.202998e-24
#6            shallow_lake_and_pond - wetland   2.145108 3.194420e-02 1.916652e-01

#Log-transformed water column total phosphorus among freshwater benthic ecosystem types and categories
ggplot(lit_rev_phyto_micro_tp_wc, aes(x = as.factor(category), y = as.numeric(log_water_tp_ugl), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("paleturquoise1", "cadetblue2", "deepskyblue1",
                               "deepskyblue3", "turquoise4",
                               "mediumblue", "slateblue4", "navyblue", "purple4",
                               "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                               "forestgreen", "palegreen4", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", "Lake Littoral Zone",
                               "Mesotrophic Lake Littoral Zone", "Oligotrophic Lake Littoral Zone",
                               "Reservoir Littoral Zone", "Pond", "Shallow Lagoon",
                               "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland",  "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Prairie Marsh", "Swamp",
                               "Wet Meadow", "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                               "reservoir_littoral_zone", "pond", "shallow_lagoon",
                               "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "prairie_marsh", "swamp", "wet_meadow", 
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Log Water TP ( ",µg," ",L^-1,")", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none")

#One-way Kruskal Wallis test - log-transformed water column total phosphorus
kruskal.test(log_water_tp_ugl ~ category, data = lit_rev_phyto_micro_tp_wc)

#data:  log_total_p_ugl by category
#Kruskal-Wallis chi-squared = 231.48, df = 3, p-value < 2.2e-16

#Post-hoc test: Dunn's Test - log-transformed water column total phosphorus
dunnTest(as.numeric(log_water_tp_ugl) ~ category,
         data = lit_rev_phyto_micro_tp_wc,
         method = "bonferroni")

#                                  Comparison          Z       P.unadj        P.adj
#1       karstic_wetland - lake_littoral_zone  -3.670140 2.424181e-04 1.454509e-03
#2    karstic_wetland - shallow_lake_and_pond  -9.784090 1.317756e-22 7.906539e-22
#3 lake_littoral_zone - shallow_lake_and_pond -11.620778 3.231797e-31 1.939078e-30
#4                  karstic_wetland - wetland  -8.690248 3.616498e-18 2.169899e-17
#5               lake_littoral_zone - wetland -10.250925 1.172167e-24 7.033001e-24
#6            shallow_lake_and_pond - wetland   2.144063 3.202787e-02 1.921672e-01

#Create legend containing all relevant freshwater benthic ecosystem types for above boxplots####

#Create datasets with same biomass title
lit_rev_macro_biomass <- lit_rev_macro_bio %>%
  rename(biomass = dm_gm2, log_biomass = log_dm_gm2)

lit_rev_micro_biomass <- lit_rev_micro_bio %>%
  rename(biomass = chla_gm3, log_biomass = log_chla_gm3)

lit_rev_phyto_biomass <- lit_rev_phyto_bio %>%
  rename(biomass = chla_ugl, log_biomass = log_chla_ugl)

#Combine datasets together
lit_rev_all_bio <- rbind(lit_rev_macro_biomass, lit_rev_micro_biomass, lit_rev_phyto_biomass)

lit_rev_all_bio$biomass <- as.numeric(lit_rev_all_bio$biomass)

lit_rev_all_bio$log_biomass <- as.numeric(lit_rev_all_bio$log_biomass)

unique(lit_rev_all_bio$habitat_type)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_all_bio$habitat_type = factor(lit_rev_all_bio$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                               "reservoir_littoral_zone", "pond", "shallow_lagoon",
                                                                               "shallow_lake", "shallow_reservoir", "bog", 
                                                                               "constructed_treatment_wetland", "dune_slack", "fen", 
                                                                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                               "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                                                                               "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_all_bio$category = factor(
  lit_rev_all_bio$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#All biomass among freshwater benthic ecosystem types and categories
ggplot(lit_rev_all_bio, aes(x = as.factor(category), y = as.numeric(biomass), fill = as.factor(habitat_type))) +
  geom_boxplot(width = 1, position = position_dodge2(preserve = "single")) +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("paleturquoise1", "cadetblue2", "deepskyblue1",
                               "deepskyblue3", "turquoise4", "mediumblue",
                               "slateblue4", "navyblue", "purple4",
                               "yellow1", "yellow2", "yellow3", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "limegreen", 
                               "forestgreen", "darkgreen", "palegreen4", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", "Lake Littoral Zone",
                               "Mesotrophic Lake Littoral Zone", "Oligotrophic Lake Littoral Zone",
                               "Reservoir Littoral Zone", "Pond", "Shallow Lagoon",
                               "Shallow Lake", "Shallow Reservoir", "Bog",
                               "Constructed Treatment Wetland", "Dune Slack", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Prairie Marsh", "Swamp", "Tidal Marsh",
                               "Wet Meadow", "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                               "reservoir_littoral_zone", "pond", "shallow_lagoon",
                               "shallow_lake", "shallow_reservoir", "bog", 
                               "constructed_treatment_wetland", "dune_slack", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                               "wetland",  "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_x_discrete(
    drop = FALSE,
    name = 'Categories',
    labels = c('Littoral Zone', 'Shallow Lake and Pond', 'Wetland', 'Karstic Wetland')) +
  labs(fill = "Freshwater
Benthic Ecosystem Types") +
  ylab(expression(paste("Biomass", sep = ""))) +
  xlab(expression(paste("Categories", sep = ""))) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "grey60", 
               position = position_dodge2(width = 1, preserve = "single")) +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16))

#Benthic algal biomass vs. water column total phosphorus####

#Subset dataset to contain only needed variables 
lit_rev_micro_bio_vs_tp <- lit_rev_micro %>%
  select(publication_citation, habitat_type, category, group, chla_gm3,
         log_chla_gm3, water_tp_ugl, log_water_tp_ugl) %>%
  filter(chla_gm3 != -9999, log_chla_gm3 != -9999, water_tp_ugl != -9999, water_tp_ugl != -9999) %>%
  mutate(log_water_tp_ugl = as.numeric(as.character(log_water_tp_ugl)),
         log_chla_gm3 = as.numeric(as.character(log_chla_gm3)))

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_micro_bio_vs_tp$habitat_type = factor(lit_rev_micro_bio_vs_tp$habitat_type,
                                              levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                         "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                         "reservoir_littoral_zone", "pond", "shallow_lagoon", 
                                                         "shallow_lake", "shallow_reservoir",
                                                         "constructed_treatment_wetland", "fen", 
                                                         "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                         "prairie_marsh", "swamp",
                                                         "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_micro_bio_vs_tp$category = factor(
  lit_rev_micro_bio_vs_tp$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

lit_rev_micro_bio_vs_tp$group = factor(
  lit_rev_micro_bio_vs_tp$group,
  levels = c("other_fw_benthic_system", "wetland"))

#Count number of benthic algal biomass and water column total phosphorus values per freshwater benthic ecosystem category
lit_rev_micro_bio_vs_tp %>%
  group_by(group) %>%
  tally() %>%
  print(n = 2)

#1 other_fw_benthic_system   184
#2 wetland                    78

#Count number of publications with benthic algal biomass and water column total phosphorus per freshwater benthic ecosystem category
lit_rev_micro_bio_vs_tp %>%
  group_by(group) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 other_fw_benthic_system              33
#2 wetland                               9

#Count total number of benthic algal biomass and water column total phosphorus values
lit_rev_micro_bio_vs_tp %>%
  tally()

#262

#Count total number of publications containing benthic algal biomass and water column total phosphorus
lit_rev_micro_bio_vs_tp %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#42

#Log-transformed benthic algal biomass vs. log-transformed water column total phosphorus across freshwater benthic ecosystem types
ggplot(lit_rev_micro_bio_vs_tp) + theme_classic() +
  geom_jitter(aes(log_water_tp_ugl, log_chla_gm3, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_water_tp_ugl, log_chla_gm3, colour = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "olivedrab4", "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Benthic Algal Biomass (Chl-a   ",g," ",m^-3,")", sep = ""))) +
  xlab(expression(paste("Log Water TP (",µg," ",L^-1,")", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 

#Subset dataset to only include values from other freshwater benthic systems group
lit_rev_micro_bio_vs_tp_other <- lit_rev_micro_bio_vs_tp %>%
  filter(group == "other_fw_benthic_system")

#Linear model: other freshwater benthic systems group - log-transformed benthic algal biomass vs. log-transformed water column total phosphorus
other_micro_tp_lm <- lm(log_chla_gm3 ~ log_water_tp_ugl, data = lit_rev_micro_bio_vs_tp_other)

#Coefficients: other freshwater benthic systems group - log-transformed benthic algal biomass vs. log-transformed water column total phosphorus
coef(other_micro_tp_lm)

#(Intercept) log_total_p_ugl 
#-1.4697141       0.2175416

#Statistical results: other freshwater benthic systems group - log-transformed benthic algal biomass vs. log-transformed water column total phosphorus
summary(other_micro_tp_lm)

#Residuals:
#     Min      1Q  Median      3Q     Max 
#-3.0363 -0.5500  0.0574  0.5590  3.2503 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -1.4697     0.1922  -7.648 1.14e-12 ***
#log_total_p_ugl   0.2175     0.1109   1.962   0.0513 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.9315 on 182 degrees of freedom
#Multiple R-squared:  0.02072,	Adjusted R-squared:  0.01534 
#F-statistic: 3.851 on 1 and 182 DF,  p-value: 0.05125

#Subset dataset to only include values from wetlands group
lit_rev_micro_bio_vs_tp_wet <- lit_rev_micro_bio_vs_tp %>%
  filter(group == "wetland")

#Linear model: wetlands group - log-transformed benthic algal biomass vs. log-transformed water column total phosphorus
wet_micro_tp_lm <- lm(log_chla_gm3 ~ log_water_tp_ugl, data = lit_rev_micro_bio_vs_tp_wet)

#Coefficients: wetlands group - log-transformed benthic algal biomass vs. log-transformed water column total phosphorus
coef(wet_micro_tp_lm)

#(Intercept) log_total_p_ugl 
#-0.2719069      -0.2626559

#Statistical results: wetlands group - benthic algal biomass vs. water column total phosphorus
summary(wet_micro_tp_lm)

#Residuals:
#    Min      1Q   Median     3Q     Max 
#-3.3284 -0.3043 -0.0673  0.3705  1.5789 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -0.27191    0.14522  -1.872 0.065007 .  
#log_total_p_ugl -0.26266    0.07072  -3.714 0.000387 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7252 on 76 degrees of freedom
#Multiple R-squared:  0.1536,	Adjusted R-squared:  0.1425 
#F-statistic: 13.79 on 1 and 76 DF,  p-value: 0.000387

#Phytoplankton biomass vs. water column total phosphorus####

#Subset dataset to contain only needed variables 
lit_rev_phyto_bio_vs_tp <- lit_rev_phyto %>%
  select(publication_citation, habitat_type, category, group, chla_ugl,
         log_chla_ugl, water_tp_ugl, log_water_tp_ugl) %>%
  filter(chla_ugl != -9999, log_chla_ugl != -9999)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_phyto_bio_vs_tp$habitat_type = factor(lit_rev_phyto_bio_vs_tp$habitat_type,
                                              levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                         "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                         "shallow_lake", "shallow_reservoir",
                                                         "constructed_treatment_wetland", "non_tidal_coastal_wetland",
                                                         "floodplain_wetland", "swamp", "wet_meadow", "wetland"))

lit_rev_phyto_bio_vs_tp$category = factor(
  lit_rev_phyto_bio_vs_tp$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Count number of phytoplankton biomass and water column total phosphorus values per freshwater benthic ecosystem category
lit_rev_phyto_bio_vs_tp %>%
  group_by(group) %>%
  tally() %>%
  print(n = 2)

#1 other_fw_benthic_system   264
#2 wetland                  103

#Count number of publications with phytoplankton biomass and water column total phosphorus per freshwater benthic ecosystem category
lit_rev_phyto_bio_vs_tp %>%
  group_by(group) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 other_fw_benthic_system              42
#2 wetland                              17

#Count total number of phytoplankton biomass and water column total phosphorus values
lit_rev_phyto_bio_vs_tp %>%
  tally()

#367

#Count total number of publications containing phytoplankton biomass and water column total phosphorus
lit_rev_phyto_bio_vs_tp %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#58

#Log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus across freshwater benthic ecosystem types
ggplot(lit_rev_phyto_bio_vs_tp) + theme_classic() +
  geom_jitter(aes(log_water_tp_ugl, log_chla_ugl, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_water_tp_ugl, log_chla_ugl, colour = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3",
                                "navyblue", "purple4",
                                "yellow2", "olivedrab1", "green", 
                                "forestgreen", "palegreen4", "olivedrab4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir", "Shallow Lake",
                               "Constructed Treatment Wetland", "Floodplain Wetland",
                               "Non-Tidal Coastal Wetland", "Swamp", "Wet Meadow", "Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "floodplain_wetland",
                               "non_tidal_coastal_wetland", "swamp", "wet_meadow", "wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Phytoplankton Biomass (Chl-a   ",µg," ",L^-1,")", sep = ""))) +
  xlab(expression(paste("Log Water TP (",µg," ",L^-1,")", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 

#Subset dataset to only include values from other freshwater benthic systems group
lit_rev_phyto_bio_vs_tp_other <- lit_rev_phyto_bio_vs_tp %>%
  filter(group == "other_fw_benthic_system")

#Linear model: other freshwater systems group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
other_phyto_tp_lm <- lm(log_chla_ugl ~ log_water_tp_ugl, data = lit_rev_phyto_bio_vs_tp_other)

#Coefficients:  other freshwater systems group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
coef(other_phyto_tp_lm)

#(Intercept) log_total_p_ugl 
# -0.1004215        0.6125265 

#Statistical results: other freshwater benthic systems group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
summary(other_phyto_tp_lm)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-1.2469 -0.4202 -0.0029  0.3187  3.3185 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.10042    0.10492  -0.957    0.339    
#log_water_tp_ugl  0.61253    0.06206   9.870   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6785 on 262 degrees of freedom
#Multiple R-squared:  0.271,	Adjusted R-squared:  0.2682 
#F-statistic: 97.41 on 1 and 262 DF,  p-value: < 2.2e-16

#Subset dataset to only include values from wetlands group
lit_rev_phyto_bio_vs_tp_wet <- lit_rev_phyto_bio_vs_tp %>%
  filter(group == "wetland")

#Linear model: wetlands group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
wet_phyto_tp_lm <- lm(log_chla_ugl ~ log_water_tp_ugl, data = lit_rev_phyto_bio_vs_tp_wet)

#Coefficients: wetlands group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
coef(wet_phyto_tp_lm)

#(Intercept) log_total_p_ugl 
#0.02666696       0.49856067 

#Statistical results: wetlands group - log-transformed phytoplankton biomass vs. log-transformed water column total phosphorus
summary(wet_phyto_tp_lm)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-1.71930 -0.41809  0.04189  0.37128  1.65075 

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       0.02667    0.20975   0.127    0.899    
#log_water_tp_ugl  0.49856    0.09568   5.211 9.98e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6247 on 101 degrees of freedom
#Multiple R-squared:  0.2119,	Adjusted R-squared:  0.2041 
#F-statistic: 27.15 on 1 and 101 DF,  p-value: 9.983e-07

#Create legend containing all relevant freshwater benthic ecosystem types for above scatterplots####

#Create datasets with same biomass title
lit_rev_micro_biomass_tp <- lit_rev_micro_bio_vs_tp %>%
  rename(biomass = chla_gm3, log_biomass = log_chla_gm3)

lit_rev_phyto_biomass_tp <- lit_rev_phyto_bio_vs_tp %>%
  rename(biomass = chla_ugl, log_biomass = log_chla_ugl)

#Combine datasets together
lit_rev_all_bio_vs_tp <- rbind(lit_rev_micro_biomass_tp, lit_rev_phyto_biomass_tp)

lit_rev_all_bio_vs_tp$biomass <- as.numeric(lit_rev_all_bio_vs_tp$biomass)

lit_rev_all_bio_vs_tp$log_biomass <- as.numeric(lit_rev_all_bio_vs_tp$log_biomass)

unique(lit_rev_all_bio_vs_tp$habitat_type)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_all_bio_vs_tp$habitat_type = factor(lit_rev_all_bio_vs_tp$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                           "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                           "reservoir_littoral_zone", "pond", "shallow_lagoon",
                                                                                           "shallow_lake", "shallow_reservoir", 
                                                                                           "constructed_treatment_wetland", "fen", 
                                                                                           "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                                           "prairie_marsh", "swamp", "wet_meadow", 
                                                                                           "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_all_bio_vs_tp$category = factor(
  lit_rev_all_bio_vs_tp$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Log-transformed benthic algal biomass and phytoplankton biomass vs. log-transformed water column total phosphorus across freshwater benthic ecosystem types and groups
ggplot(lit_rev_all_bio_vs_tp) + theme_classic() +
  geom_jitter(aes(log_water_tp_ugl, log_biomass, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_water_tp_ugl, log_biomass, colour = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "palegreen4", "olivedrab4", "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp", "Wet Meadow",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp", "wet_meadow",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Biomass", sep = ""))) +
  xlab(expression(paste("Log Water Column TP ",µg," ",L^-1,")", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 


#Benthic algal biomass against benthic microbial total phosphorus stock####

#Subset dataset to contain only needed variables 
lit_rev_micro_bio_vs_tp_stock <- lit_rev_micro %>%
  select(publication_citation, habitat_type, category, group, chla_gm3,
         log_chla_gm3, tp_stock_gm3, log_tp_stock_gm3) %>%
  filter(chla_gm3 != -9999, log_chla_gm3 != -9999, tp_stock_gm3 != -9999, log_tp_stock_gm3 != -9999) %>%
  filter(log_tp_stock_gm3 < 20) %>%
  mutate(log_tp_stock_gm3 = as.numeric(as.character(log_tp_stock_gm3)),
         log_chla_gm3 = as.numeric(as.character(log_chla_gm3)))

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_micro_bio_vs_tp_stock$habitat_type = factor(lit_rev_micro_bio_vs_tp_stock$habitat_type,
                                                          levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                     "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                     "reservoir_littoral_zone", "pond", "shallow_lagoon", 
                                                                     "shallow_lake", "shallow_reservoir",
                                                                     "constructed_treatment_wetland", "fen", 
                                                                     "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                     "prairie_marsh", "swamp",
                                                                     "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_micro_bio_vs_tp_stock$category = factor(
  lit_rev_micro_bio_vs_tp_stock$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

lit_rev_micro_bio_vs_tp_stock$group = factor(
  lit_rev_micro_bio_vs_tp_stock$group,
  levels = c("other_fw_benthic_system", "wetland"))

#Count number of benthic algal biomass and benthic microbial total phosphorus stock values per freshwater benthic ecosystem category
lit_rev_micro_bio_vs_tp_stock %>%
  group_by(group) %>%
  tally() %>%
  print(n = 2)

#1 other_fw_benthic_system   27
#2 wetland                  52

#Count number of publications with benthic algal biomass and benthic microbial total phosphorus stock per freshwater benthic ecosystem category
lit_rev_micro_bio_vs_tp_stock %>%
  group_by(group) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 other_fw_benthic_system              4
#2 wetland                              5

#Count total number of benthic algal biomass and benthic microbial total phosphorus stock values
lit_rev_micro_bio_vs_tp_stock %>%
  tally()

#79

#Count total number of publications containing benthic algal biomass and benthic microbial total phosphorus stock
lit_rev_micro_bio_vs_tp_stock %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#9

#Log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock across freshwater benthic ecosystem types
ggplot(lit_rev_micro_bio_vs_tp_stock) + theme_classic() +
  geom_jitter(aes(log_tp_stock_gm3, log_chla_gm3, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_tp_stock_gm3, log_chla_gm3, color = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "olivedrab4", "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Benthic Algal Biomass (Chl-a   ",g," ",m^-3,")", sep = ""))) +
  xlab(expression(paste("Log Benthic Microbial TP Stock (",g," ",m^-3,")", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 

#Subset dataset to only include values from other freshwater benthic systems group
lit_rev_micro_bio_vs_tp_stock_other <- lit_rev_micro_bio_vs_tp_stock %>%
  filter(group == "other_fw_benthic_system")

#Linear model: other freshwater systems group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
other_micro_tp_stock_lm <- lm(log_chla_gm3 ~ log_tp_stock_gm3, data = lit_rev_micro_bio_vs_tp_stock_other)

#Coefficients: other freshwater systems group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
coef(other_micro_tp_stock_lm)

# (Intercept) log_tp_stock_gm3 
#-0.4462875        0.4122430 

#Statistical results: other freshwater systems group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
summary(other_micro_tp_stock_lm)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-1.2631 -0.4591 -0.2091  0.2438  2.1482 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.44629    0.15841  -2.817  0.00932 ** 
#log_tp_stock_gm3  0.41224    0.07998   5.154  2.5e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7938 on 25 degrees of freedom
#Multiple R-squared:  0.5152,	Adjusted R-squared:  0.4958 
#F-statistic: 26.57 on 1 and 25 DF,  p-value: 2.501e-05e-05

#Subset dataset to only include values from wetlands group
lit_rev_micro_bio_vs_tp_stock_wet <- lit_rev_micro_bio_vs_tp_stock %>%
  filter(group == "wetland")

#Linear model: wetlands group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
wet_micro_tp_stock_lm <- lm(log_chla_gm3 ~ log_tp_stock_gm3, data = lit_rev_micro_bio_vs_tp_stock_wet)

#Coefficients: wetlands group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
coef(wet_micro_tp_stock_lm)

#(Intercept) log_tp_stock_gm3 
# -0.4702395        0.4601303

#Statistical results: wetlands group - log-transformed benthic algal biomass vs. log-transformed benthic microbial total phosphorus stock
summary(wet_micro_tp_stock_lm)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2.46790 -0.30960 -0.08139  0.37498  2.75363 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#(Intercept)       -0.4702     0.2631  -1.787  0.07998 . 
#log_tp_stock_gm3   0.4601     0.1426   3.226  0.00221 **
# ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6478 on 50 degrees of freedom
#Multiple R-squared:  0.1723,	Adjusted R-squared:  0.1558 
#F-statistic: 10.41 on 1 and 50 DF,  p-value: 0.002213

#Macrophyte biomass stock vs. macrophyte total phosphorus stock####

#Subset dataset to contain only needed variables 
lit_rev_macro_bio_vs_tp_stock <- lit_rev_macro %>%
  select(publication_citation, habitat_type, category, group, dm_gm2,
         log_dm_gm2, tp_stock_gm2, log_tp_stock_gm2) %>%
  filter(dm_gm2 != -9999, log_dm_gm2 != -9999, tp_stock_gm2 != -9999, log_tp_stock_gm2 != -9999) %>%
  mutate(log_tp_stock_gm2 = as.numeric(as.character(log_tp_stock_gm2)),
         log_dm_gm2 = as.numeric(as.character(log_dm_gm2)))

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_macro_bio_vs_tp_stock$habitat_type = factor(lit_rev_macro_bio_vs_tp_stock$habitat_type,
                                                          levels = c("lake_littoral_zone", 
                                                                     "pond", "shallow_lake", "bog", 
                                                                     "constructed_treatment_wetland", "fen", 
                                                                     "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                     "tidal_marsh", "wetland", "Everglades", 
                                                                     "Everglades_other", "karstic_wetland"))

lit_rev_macro_bio_vs_tp_stock$category = factor(
  lit_rev_macro_bio_vs_tp_stock$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

lit_rev_macro_bio_vs_tp_stock$group = factor(
  lit_rev_macro_bio_vs_tp_stock$group,
  levels = c("other_fw_benthic_system", "wetland"))

#Count number of macrophyte biomass and macrophyte total phosphorus stock values per freshwater benthic ecosystem category
lit_rev_macro_bio_vs_tp_stock %>%
  group_by(group) %>%
  tally() %>%
  print(n = 2)

#1 other_fw_benthic_system   15
#2 wetland                  296

#Count number of publications with macrophyte biomass and macrophyte total phosphorus stock per freshwater benthic ecosystem category
lit_rev_macro_bio_vs_tp_stock %>%
  group_by(group) %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#1 other_fw_benthic_system              1
#2 wetland                            37

#Count total number of macrophyte biomass and macrophyte total phosphorus stock values
lit_rev_macro_bio_vs_tp_stock %>%
  tally()

#311

#Count total number of publications containing macrophyte biomass and macrophyte total phosphorus stock 
lit_rev_macro_bio_vs_tp_stock %>%
  summarise(Unique_Elements = n_distinct(publication_citation))

#38

#Log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock across freshwater benthic ecosystem types
ggplot(lit_rev_macro_bio_vs_tp_stock) + theme_classic() +
  geom_jitter(aes(log_tp_stock_gm2, log_dm_gm2, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_tp_stock_gm2, log_dm_gm2, color = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c("cadetblue2", "mediumblue", "navyblue", "yellow1", "yellow2", "gold3",
                               "olivedrab1",  "chartreuse1", "green", "darkgreen", "olivedrab4", 
                               "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Lake Littoral Zone",
                               "Pond", "Shallow Lake", "Bog",
                               "Constructed Treatment Wetland", "Fen", 
                               "Floodplain Wetland", "Marsh", "Non-Tidal Coastal Wetland",
                               "Tidal Marsh",
                               "Wetland", "Everglades - This Study", 
                               "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("lake_littoral_zone", "pond", "shallow_lake", "bog", 
                               "constructed_treatment_wetland", "fen", 
                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                               "tidal_marsh", "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Macrophyte Biomass ( ",g," ",m^-2,")", sep = ""))) +
  xlab(expression(paste("Log Macrophyte TP Stock (",g," ",m^-2,")", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 

#Subset dataset to only include values from other freshwater benthic systems group
lit_rev_macro_bio_vs_tp_stock_other <- lit_rev_macro_bio_vs_tp_stock %>%
  filter(group == "other_fw_benthic_system")

#Linear model: other freshwater benthic systems group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
other_macro_tp_stock_lm <- lm(log_dm_gm2 ~ log_tp_stock_gm2, data = lit_rev_macro_bio_vs_tp_stock_other)

#Coefficients: other freshwater benthic systems group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
coef(other_macro_tp_stock_lm)

#(Intercept) log_tp_stock_gm2
#2.7802035        0.2469143 

#Statistical results: other freshwater benthic systems group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
summary(other_macro_tp_stock_lm)

#Residuals:
#     Min       1Q    Median      3Q      Max 
#-1.04750 -0.07447  0.08870  0.12749  0.86822 

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        2.7802     0.1166  23.837 4.11e-12 ***
#log_tp_stock_gm2   0.2469     0.1245   1.983   0.0689 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4268 on 13 degrees of freedom
#Multiple R-squared:  0.2323,	Adjusted R-squared:  0.1732 
#F-statistic: 3.933 on 1 and 13 DF,  p-value: 0.06888

#Subset dataset to only include values from wetlands group
lit_rev_macro_bio_vs_tp_stock_wet <- lit_rev_macro_bio_vs_tp_stock %>%
  filter(group == "wetland")

#Linear model: wetlands group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
wet_macro_tp_stock_lm <- lm(log_dm_gm2 ~ log_tp_stock_gm2, data = lit_rev_macro_bio_vs_tp_stock_wet)

#Coefficients: wetlands group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
coef(wet_macro_tp_stock_lm)

#(Intercept) log_tp_stock_gm2
#2.8569149        0.4863607

#Statistical results: wetlands group - log-transformed macrophyte biomass vs. log-transformed macrophyte total phosphorus stock 
summary(wet_macro_tp_stock_lm)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2.60852 -0.21576  0.02279  0.29886  0.82462 

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       2.85691    0.02611  109.42   <2e-16 ***
#log_tp_stock_gm2  0.48636    0.03383   14.38   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4473 on 294 degrees of freedom
#Multiple R-squared:  0.4128,	Adjusted R-squared:  0.4108 
#F-statistic: 206.7 on 1 and 294 DF,  p-value: < 2.2e-16

#Create legend containing all relevant freshwater benthic ecosystem types for above scatterplots####

#Create datasets with same biomass title
lit_rev_micro_biomass_tp_vs_stock <- lit_rev_micro_bio_vs_tp_stock %>%
  rename(biomass = chla_gm3, log_biomass = log_chla_gm3, tp_stock = tp_stock_gm3, log_tp_stock = log_tp_stock_gm3)

lit_rev_macro_biomass_tp_vs_stock <- lit_rev_macro_bio_vs_tp_stock %>%
  rename(biomass = dm_gm2, log_biomass = log_dm_gm2, tp_stock = tp_stock_gm2, log_tp_stock = log_tp_stock_gm2)

#Combine datasets together
lit_rev_all_tp_vs_stock <- rbind(lit_rev_micro_biomass_tp_vs_stock, lit_rev_macro_biomass_tp_vs_stock)

lit_rev_all_tp_vs_stock$biomass <- as.numeric(lit_rev_all_tp_vs_stock$biomass)

lit_rev_all_tp_vs_stock$log_biomass <- as.numeric(lit_rev_all_tp_vs_stock$log_biomass)

unique(lit_rev_all_tp_vs_stock$habitat_type)

#Order and make each freshwater benthic ecosystem type and category a factor
lit_rev_all_tp_vs_stock$habitat_type = factor(lit_rev_all_tp_vs_stock$habitat_type, levels = c("eutrophic_lake_littoral_zone", "lake_littoral_zone", 
                                                                                               "mesotrophic_lake_littoral_zone", "oligotrophic_lake_littoral_zone",
                                                                                               "reservoir_littoral_zone", "pond", "shallow_lagoon",
                                                                                               "shallow_lake", "shallow_reservoir", 
                                                                                               "constructed_treatment_wetland", "fen", 
                                                                                               "floodplain_wetland", "marsh", "non_tidal_coastal_wetland",
                                                                                               "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow", 
                                                                                               "wetland",  "Everglades", "Everglades_other", "karstic_wetland"))

lit_rev_all_tp_vs_stock$category = factor(
  lit_rev_all_tp_vs_stock$category,
  levels = c("lake_littoral_zone", "shallow_lake_and_pond", "wetland", "karstic_wetland"))

#Log-transformed benthic algal biomass and macrophyte biomass vs. benthic microbial total phosphorus stock and macrophyte total phosphorus stock, respectively, across freshwater benthic ecosystem types
ggplot(lit_rev_all_tp_vs_stock) + theme_classic() +
  geom_jitter(aes(log_tp_stock, log_biomass, fill = habitat_type),
              pch = 21, size = 3) +
  geom_smooth(aes(log_tp_stock, log_biomass, colour = group),
              method = "lm", se = FALSE) +
  scale_fill_manual(values = c( "paleturquoise1", "cadetblue2",
                                "deepskyblue1", "deepskyblue3", "turquoise4",
                                "mediumblue", "slateblue4", "navyblue", "purple4",
                                "yellow2", "gold3", "olivedrab1",  "chartreuse1", "green", "limegreen", 
                                "forestgreen", "darkgreen", "palegreen4", "olivedrab4", 
                                "lightsalmon1", "tomato2", "indianred4"), 
                    labels = c("Eutrophic Lake Littoral Zone", 
                               "Lake Littoral Zone", "Mesotrophic Lake Littoral Zone", 
                               "Oligotrophic Lake Littoral Zone", "Reservoir Littoral Zone", 
                               "Pond", "Shallow Lagoon", "Shallow Lake", "Shallow Reservoir",
                               "Constructed Treatment Wetland", "Fen", "Floodplain Wetland",
                               "Marsh", "Non-Tidal Coastal Wetland", "Prairie Marsh", "Swamp", "Tidal Marsh", "Wet Meadow",
                               "Wetland", "Everglades - This Study", "Everglades - Other Studies", "Karstic Wetland"),
                    breaks = c("eutrophic_lake_littoral_zone",
                               "lake_littoral_zone", "mesotrophic_lake_littoral_zone", 
                               "oligotrophic_lake_littoral_zone", "reservoir_littoral_zone", 
                               "pond", "shallow_lagoon", "shallow_lake", "shallow_reservoir",
                               "constructed_treatment_wetland", "fen", "floodplain_wetland",
                               "marsh", "non_tidal_coastal_wetland", "prairie_marsh", "swamp", "tidal_marsh", "wet_meadow",
                               "wetland", "Everglades", "Everglades_other", "karstic_wetland")) +
  scale_color_manual(values = c( "dodgerblue3", "darkgreen"), 
                     labels = c( "Other FW Benthic Systems", "Wetlands")) +
  ylab(expression(paste("Log Biomass", sep = ""))) +
  xlab(expression(paste("Log TP Stock", sep = ""))) +
  labs(color = "Groups", fill = "Freshwater
Benthic Ecosystem Types") +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  facet_wrap(~group, scales = "free", labeller = labeller(group = 
                                                            c("other_fw_benthic_system" = "Other FW Benthic Systems",
                                                              "wetland" = "Wetlands"))) 


#Everglades analysis####

#Load Everglades case study dataset
ever <- read_csv("D:\\Extra FIU Files\\Dissertation\\Chapter 1\\Data Publication\\FCE1294_Ever_Case_Study.csv") 

#Benthic algal biomass vs. benthic microbial total phosphorus concentration####

#Mean benthic algal biomass per wetland and habitat type
ever_mean_micro <- ever %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(mean_biomass = mean(micro_chla_gm3)) %>%
  print(n = 4)

#1 srs_ridge              0.0468
#2 srs_slough             0.0627
#3 ts_ridge               0.0711
#4 ts_slough              0.212

#Standard error benthic algal biomass per wetland and habitat type
ever_se_micro <- ever %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(se_biomass = se(micro_chla_gm3)) %>%
  print(n = 4)

#1 srs_ridge            0.0120
#2 srs_slough           0.0183
#3 ts_ridge             0.0276
#4 ts_slough            0.0855

#Mean benthic microbial total phosphorus stock per wetland and habitat type
ever_mean_micro_stock <- ever %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(mean_tp_stock = mean(micro_tp_stock_gm2)) %>%
  print(n = 4)

#1 srs_ridge              0.00583
#2 srs_slough             0.00817
#3 ts_ridge               0.00544
#4 ts_slough              0.00622

#Standard error benthic microbial total phosphorus stock per wetland and habitat type
ever_se_micro_stock <- ever %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(se_tp_stock = se(micro_tp_stock_gm2)) %>%
  print(n = 4)

#1 srs_ridge            0.00131
#2 srs_slough           0.00193
#3 ts_ridge             0.00179
#4 ts_slough            0.00127

#Make a new column for plot color purposes
ever <- ever %>%
  add_column(micro_type = "micro")

#Benthic algal biomass vs. benthic algal total phosphorus concentration
micro_bio_vs_tp_conc <- 
  ggplot(ever, aes(y = micro_chla_gm2, x = micro_tp_conc_ugg, color = as.factor(micro_type), shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(micro_tp_conc_ugg, micro_chla_gm2, group = as.factor(micro_type)), method = "gam", colour = "darkolivegreen3",  se = FALSE) +
  scale_colour_manual(values = c("darkolivegreen3"), labels = c("Benthic Algae")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Benthic Algal Biomass (Chl-a", g, " ", m^-2, ")", sep = ""))) +
  xlab(expression(paste("Benthic Microbial TP Concentration (", µg, " ", g^-1, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

micro_bio_vs_tp_conc

#Generalized additive model- benthic algal biomass vs. benthic microbial total phosphorus concentration
micro_bio_vs_tp_conc_gam <- gam(micro_chla_gm2 ~ s(micro_tp_conc_ugg, bs = "cs"), method = "REML", data = ever)
summary(micro_bio_vs_tp_conc_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.020048   0.002106   9.521 1.42e-11 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                       edf Ref.df     F p-value   
#s(micro_tp_conc_ugg) 3.288      9 2.025  0.0015 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.308   Deviance explained = 36.3%
#-REML = -110.27  Scale est. = 0.00018623  n = 42

#Benthic algal biomass vs. benthic microbial total phosphorus stock####

#Benthic algal biomass vs. benthic algal total phosphorus stock
micro_bio_vs_tp_stock <- 
  ggplot(ever, aes(y = micro_chla_gm3, x = micro_tp_stock_gm3, color = as.factor(micro_type), shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(micro_tp_stock_gm3, micro_chla_gm3, group = as.factor(micro_type)), method = "gam", colour = "darkolivegreen3",  se = FALSE) +
  scale_colour_manual(values = c("darkolivegreen3"), labels = c("Benthic Algae")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Benthic Algal Biomass (Chl-a", g, " ", m^-3, ")", sep = ""))) +
  xlab(expression(paste("Benthic Microbial TP Stock (", g, " ", m^-3, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

micro_bio_vs_tp_stock

length(unique(ever$micro_tp_stock_gm3))

#Generalized additive model - benthic algal biomass vs. benthic algal total phosphorus stock
micro_bio_gm3_vs_tp_stock_gam <- gam(micro_chla_gm3 ~ s(micro_tp_stock_gm3, bs = "cs"), method = "REML", data = ever)
summary(micro_bio_gm3_vs_tp_stock_gam)

#Parametric coefficients:
#(Intercept)  0.09196    0.01702   5.403 3.38e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                        edf Ref.df    F  p-value    
#s(micro_tp_stock_gm3) 1.535      9 2.87 9.78e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.386   Deviance explained = 40.9%
#-REML = -28.497  Scale est. = 0.012169  n = 42

#Macrophyte biomass vs. macrophyte total phosphorus concentration####

#Filter out missing values
ever_macro <- ever %>%
  filter(total_macro_dm_gm2 != -9999, total_macro_tp_conc_ugg != -9999)

#Mean macrophyte biomass per wetland and habitat type
ever_mean_macro <- ever_macro %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(mean_biomass = mean(total_macro_dm_gm2)) %>%
  print(n = 4)

#1 srs_ridge                790.
#2 srs_slough               259.
#3 ts_ridge                 634.
#4 ts_slough                321.

#Standard error macrophyte biomass per wetland and habitat type
ever_se_macro <- ever_macro %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(se_biomass = se(total_macro_dm_gm2)) %>%
  print(n = 4)

#1 srs_ridge              60.4
#2 srs_slough             33.0
#3 ts_ridge               67.0
#4 ts_slough              70.2

#Mean macrophyte total phosphorus stock per wetland and habitat type
ever_mean_macro_stock <- ever_macro %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(mean_stock = mean(total_macro_tp_stock_gm2)) %>%
  print(n = 4)

#1 srs_ridge            0.123 
#2 srs_slough           0.0822
#3 ts_ridge             0.289 
#4 ts_slough            0.0750

#Standard error macrophyte total phosphorus stock per wetland and habitat type
ever_se_macro_stock <- ever_macro %>%
  group_by(slough_plot_type) %>%
  dplyr::summarize(se_stock = se(total_macro_tp_stock_gm2)) %>%
  print(n = 4)

#1 srs_ridge          0.0104
#2 srs_slough         0.0140
#3 ts_ridge           0.174 
#4 ts_slough          0.0106

#make a new column for plot color purposes
ever_macro <- ever_macro %>%
  add_column(macro_type = "macro")

#Macrophyte biomass vs. macrophyte total phosphorus concentration
macro_bio_vs_tp_conc <- 
  ggplot(ever_macro, aes(y = total_macro_dm_gm2, x = total_macro_tp_conc_ugg, color = as.factor(macro_type), shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(total_macro_tp_conc_ugg, total_macro_dm_gm2, group = as.factor(macro_type)), method = "gam", colour = "cadetblue",  se = FALSE) +
  scale_colour_manual(values = c("cadetblue"), labels = c("Macrophyte
Community")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Macrophyte Biomass (", g, " ", m^-2, ")", sep = ""))) +
  xlab(expression(paste("Macrophyte TP Concentration (", µg, " ", g^-1, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

macro_bio_vs_tp_conc

#Generalized additive model - macrophyte biomass vs. macrophyte total phosphorus concentration
macro_bio_vs_tp_conc_gam <- gam(total_macro_dm_gm2 ~ s(total_macro_tp_conc_ugg, bs = "cs"), method = "REML", data = ever_macro)
summary(macro_bio_vs_tp_conc_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    500.7       46.7   10.72 1.33e-12 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                                 edf Ref.df F p-value
#s(total_macro_tp_conc_ugg) 0.0002889      9 0   0.564

#R-sq.(adj) =  -1.46e-06   Deviance explained = 0.000679%
#-REML =  248.7  Scale est. = 78503     n = 36


#Macrophyte biomass vs. macrophyte total phosphorus stock####

#Filter out missing values
ever_macro_stock_gm2 <- ever %>%
  filter(total_macro_dm_gm2 != -9999, total_macro_tp_stock_gm2 != -9999, total_macro_tp_stock_gm2 < 1)

#Make a new column for plot color purposes
ever_macro_stock_gm2 <- ever_macro_stock_gm2 %>%
  add_column(macro_type = "macro")

#Macrophyte biomass vs. macrophyte total phosphorus stock
macro_bio_vs_tp_stock_gm2 <- 
  ggplot(ever_macro_stock_gm2, aes(y = total_macro_dm_gm2, x = total_macro_tp_stock_gm2, color = macro_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(total_macro_tp_stock_gm2, total_macro_dm_gm2, group = macro_type), method = "gam", colour = "cadetblue",  se = FALSE) +
  scale_colour_manual(values = c("cadetblue"), labels = c("Macrophyte
Community")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Macrophyte Biomass (", g, " ", m^-2, ")", sep = ""))) +
  xlab(expression(paste("Macrophyte TP Stock (", g, " ", m^-2, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

macro_bio_vs_tp_stock_gm2

#Generalized additive model - macrophyte biomass vs. macrophyte total phosphorus stock
macro_bio_gm2_vs_tp_stock_gm2_gam <- gam(total_macro_dm_gm2 ~ s(total_macro_tp_stock_gm2, bs = "cs"), method = "REML", data = ever_macro_stock_gm2)
summary(macro_bio_gm2_vs_tp_stock_gm2_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   490.26      35.57   13.78 4.47e-15 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                               edf Ref.df     F  p-value    
#s(total_macro_tp_stock_gm2) 1.712      9 2.774 2.67e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.423   Deviance explained = 45.2%
#-REML = 233.91  Scale est. = 44283     n = 35

#Total producer biomass vs. total producer total phosphorus concentration####

#Filter out missing values
ever_prod <- ever %>%
  filter(total_prod_tp_conc_ugg != -9999, total_prod_bio_gm2 != -9999)

#Make a new column for plot color purposes
ever_prod <- ever_prod %>%
  add_column(prod_type = "producer")

#Total producer biomass vs. total producer total phosphorus concentration
prod_bio_vs_tp_conc <- 
  ggplot(ever_prod, aes(y = total_prod_bio_gm2, x = total_prod_tp_conc_ugg, color = prod_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(total_prod_tp_conc_ugg, total_prod_bio_gm2, group = prod_type), method = "gam", colour = "purple4",  se = FALSE) +
  scale_colour_manual(values = c("purple4"), labels = c("Producer")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS Ridge", "TS Slough")) +
  ylab(expression(paste("Total Producer Biomass (", g, " ", m^-2, ")", sep = ""))) +
  xlab(expression(paste("Total Producer TP Concentration (", µg, " ", g^-1, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

prod_bio_vs_tp_conc

#Generalized additive model - total producer biomass vs. total producer total phosphorus concentration
prod_bio_vs_tp_conc_gam <- gam(total_prod_bio_gm2 ~ s(total_prod_tp_conc_ugg, bs = "cs"), method = "REML", data = ever_prod)
summary(prod_bio_vs_tp_conc_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    500.7       46.7   10.72 1.33e-12 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                                edf Ref.df F p-value
#s(total_prod_tp_conc_ugg) 0.0001735      9 0   0.666

#R-sq.(adj) =  -2.62e-06   Deviance explained = 0.000234%
#-REML =  248.7  Scale est. = 78502     n = 36

#Total producer biomass vs. total producer total phosphorus stock####

#Filter out missing values
ever_prod_stock <- ever %>%
  filter(total_prod_tp_stock_gm3 != -9999, total_prod_bio_gm2 != -9999, total_prod_tp_stock_gm3 < 2)

#Make a new column for plot color purposes
ever_prod_stock <- ever_prod_stock %>%
  add_column(prod_type = "producer")

#Total producer biomass stock vs. total producer total phosphorus stock
prod_bio_gm3_vs_tp_stock <- 
  ggplot(ever_prod_stock, aes(y = total_prod_bio_gm3, x = total_prod_tp_stock_gm3, color = prod_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(total_prod_tp_stock_gm3, total_prod_bio_gm3, group = prod_type), method = "gam", colour = "purple4",  se = FALSE) +
  scale_colour_manual(values = c("purple4"), labels = c("Producer")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS Ridge", "TS Slough")) +
  ylab(expression(paste("Total Producer Biomass (", g, " ", m^-3, ")", sep = ""))) +
  xlab(expression(paste("Total Producer TP Stock (", g, " ", m^-3, ")", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

prod_bio_gm3_vs_tp_stock

#Generalized additive model - total producer biomass stock vs. total producer total phosphorus stock
prod_bio_gm3_vs_tp_stock_gam <- gam(total_prod_bio_gm3 ~ s(total_prod_tp_stock_gm3, bs = "cs"), data = ever_prod_stock)
summary(prod_bio_gm3_vs_tp_stock_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     1736        129   13.45 1.19e-14 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#edf Ref.df     F p-value    
#s(total_prod_tp_stock_gm3) 2.289      9 8.293  <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.686   Deviance explained = 70.7%
#GCV = 6.43e+05  Scale est. = 5.8257e+05  n = 35

#Clajam and elecel biomass vs. clajam and elecel total phosphorus concentration####

#Subset dataset to contain only needed variables 
ever_clajam <- ever %>%
  select(slough_plot_type, total_clajam_dm_gm2, total_clajam_dm_gm3, total_clajam_tp_conc_ugg, 
         total_clajam_tp_stock_gm2, total_clajam_tp_stock_gm3) %>%
  filter(total_clajam_dm_gm2 != -9999, total_clajam_dm_gm2 != -1111, 
         total_clajam_dm_gm3 != -9999, total_clajam_dm_gm3 != -1111,
         total_clajam_tp_conc_ugg != -9999, total_clajam_tp_conc_ugg != -1111,
         total_clajam_tp_stock_gm2 != -9999, total_clajam_tp_stock_gm2 != -1111,
         total_clajam_tp_stock_gm3 != -9999, total_clajam_tp_stock_gm3 != -1111) %>%
  rename(biomass = total_clajam_dm_gm2, biomass_stock = total_clajam_dm_gm3, 
         tp_conc = total_clajam_tp_conc_ugg, tp_stock = total_clajam_tp_stock_gm2,
         tp_stock_gm3 = total_clajam_tp_stock_gm3) %>%
  mutate(macro_type = "total_clajam")

ever_elecel <- ever %>%
  select(slough_plot_type, total_elecel_dm_gm2, total_elecel_dm_gm3, total_elecel_tp_conc_ugg,
         total_elecel_tp_stock_gm2, total_elecel_tp_stock_gm3) %>%
  filter(         total_elecel_dm_gm2 != -9999, total_elecel_dm_gm2 != -1111,
                  total_elecel_dm_gm3 != -9999, total_elecel_dm_gm3 != -1111,
                  total_elecel_tp_conc_ugg != -9999, total_elecel_tp_conc_ugg != -1111,
                  total_elecel_tp_stock_gm2 != -9999, total_elecel_tp_stock_gm2 != -1111,
                  total_elecel_tp_stock_gm3 != -9999, total_elecel_tp_stock_gm3 != -1111) %>%
  rename(biomass = total_elecel_dm_gm2, biomass_stock = total_elecel_dm_gm3, 
         tp_conc = total_elecel_tp_conc_ugg, tp_stock = total_elecel_tp_stock_gm2,
         tp_stock_gm3 = total_elecel_tp_stock_gm3) %>%
  mutate(macro_type = "total_elecel")

#Combine datasets
ever_macro_stacked <- rbind(ever_clajam, ever_elecel)

#Macrophyte biomass vs. macrophyte total phosphorus concentration per macrophyte species
macro_stacked_bio_vs_tp_conc <- 
  ggplot(ever_macro_stacked, aes(y = biomass, x = tp_conc, color = macro_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(tp_conc, biomass, group = macro_type), method = "gam",  se = FALSE) +
  scale_color_manual(values = c( "cadetblue2", "deepskyblue1"), 
                     labels = c( "Cladium 
jamaicense", "Eleocharis 
cellulosa")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Macrophyte Biomass (", g, " ", m^-2, ")", sep = ""))) +
  labs(color = "Macrophyte Type", shape = "Wetland Habitat") +
  xlab(expression(paste("Macrophyte TP Concentration (", µg, " ", g^-1, ")", sep = ""))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

macro_stacked_bio_vs_tp_conc

#Generalized additive model - clajam biomass vs. clajam total phosphorus concentration
clajam_bio_vs_tp_conc_gam <- gam(biomass ~ s(tp_conc, bs = "cs"), method = "REML", data = ever_clajam)
summary(clajam_bio_vs_tp_conc_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   390.97      49.66   7.872 7.17e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#             edf Ref.df     F p-value   
#s(tp_conc) 1.148      9 0.971  0.0049 **
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.215   Deviance explained = 24.3%
#-REML = 229.42  Scale est. = 81396     n = 33

#Generalized additive model - elecel biomass vs. elecel total phosphorus concentration
elecel_bio_vs_tp_conc_gam <- gam(biomass ~ s(tp_conc, bs = "cs"), method = "REML", data = ever_elecel)
summary(elecel_bio_vs_tp_conc_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    86.02      15.20   5.661 4.58e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#                 edf Ref.df F p-value
#s(tp_conc) 0.0001892      9 0   0.565

#R-sq.(adj) =  -1.44e-06   Deviance explained = 0.000531%
#-REML = 164.74  Scale est. = 6695.8    n = 29

#Clajam and elecel biomass stock vs. clajam and elecel total phosphorus stock####

#Macrophyte biomass vs. macrophyte total phosphorus stock per macrophyte species
macro_stacked_bio_vs_tp_stock_gm2 <- 
  ggplot(ever_macro_stacked, aes(y = biomass, x = tp_stock, color = macro_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(tp_stock, biomass, group = macro_type), method = "gam",  se = FALSE) +
  scale_color_manual(values = c( "cadetblue2", "deepskyblue1"), 
                     labels = c( "Cladium 
jamaicense", "Eleocharis 
cellulosa")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Macrophyte Biomass (", g, " ", m^-2, ")", sep = ""))) +
  labs(color = "Macrophyte Type", shape = "Wetland Habitat") +
  xlab(expression(paste("Macrophyte TP Stock (", g, " ", m^-2, ")", sep = ""))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

macro_stacked_bio_vs_tp_stock_gm2

#Generalized additive model - clajam biomass vs. clajam total phosphorus stock
clajam_bio_vs_tp_stock_gam <- gam(biomass ~ s(tp_stock, bs = "cs"), method = "REML", data = ever_clajam)
summary(clajam_bio_vs_tp_stock_gam)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   390.97      16.77   23.31   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#              edf Ref.df     F p-value    
#s(tp_stock) 3.078      9 36.14  <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =   0.91   Deviance explained = 91.9%
#-REML = 198.41  Scale est. = 9281.7    n = 33

#Generalized additive model - elecel biomass vs. elecel total phosphorus stock
elecel_bio_vs_tp_stock_gam <- gam(biomass ~ s(tp_stock, bs = "cs"), method = "REML", data = ever_elecel)
summary(elecel_bio_vs_tp_stock_gam)

#Parametric coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   86.015      5.261   16.35 1.29e-14 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#               edf Ref.df     F p-value    
#s(tp_stock) 3.699      9 22.84  <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =   0.88   Deviance explained = 89.6%
#-REML = 141.85  Scale est. = 802.78    n = 29

#Create legend containing all relevant producer types for above scatterplots####

#Subset dataset to contain only needed variables 
ever_clajam_bio <- ever %>%
  select(slough_plot_type, total_clajam_dm_gm2) %>%
  rename(biomass = total_clajam_dm_gm2) %>%
  mutate(prod_type = "total_clajam")

ever_elecel_bio <- ever %>%
  select(slough_plot_type, total_elecel_dm_gm2) %>%
  rename(biomass = total_elecel_dm_gm2) %>%
  mutate(prod_type = "total_elecel")

ever_macro_bio <- ever %>%
  select(slough_plot_type, total_macro_dm_gm2) %>%
  rename(biomass = total_macro_dm_gm2) %>%
  mutate(prod_type = "total_macro")

ever_micro_bio <- ever %>%
  select(slough_plot_type, micro_chla_gm3) %>%
  rename(biomass = micro_chla_gm3) %>%
  mutate(prod_type = "micro")

ever_prod_bio <- ever %>%
  select(slough_plot_type, total_prod_bio_gm3) %>%
  rename(biomass = total_prod_bio_gm3) %>%
  mutate(prod_type = "total_prod")

#Combine datasets
ever_macro_stacked_all <- rbind(ever_micro_bio, ever_macro_bio, ever_clajam_bio, ever_elecel_bio, ever_prod_bio)

#Scatterplot of total producer biomass vs. total producer biomass
macro_stacked_bio_all <- 
  ggplot(ever_macro_stacked_all, aes(y = biomass, x = biomass, color = prod_type, shape = slough_plot_type)) +
  geom_point(size = 5) +
  geom_smooth(aes(biomass, biomass, group = prod_type), method = "gam",  se = FALSE) +
  scale_color_manual(values = c( "darkolivegreen3", "cadetblue", "cadetblue2", "deepskyblue1", "purple4"), 
                     labels = c( "Benthic Algae", "Macrophyte Community", "Cladium jamaicense", "Eleocharis cellulosa", "Benthic Producer Community")) +
  scale_shape_manual(values = c(19, 21, 17, 24), labels = c("SRS Ridge", "SRS Slough", "TS/Ph Ridge", "TS/Ph Slough")) +
  ylab(expression(paste("Biomas", sep = ""))) +
  labs(color = "Producer Type", shape = "Wetland Habitat") +
  xlab(expression(paste("Biomass", sep = ""))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

macro_stacked_bio_all
