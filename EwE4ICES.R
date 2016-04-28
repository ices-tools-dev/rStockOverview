rm(list = ls())
##
################
library(rICES)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(XML)
# library(parallel)
# library(foreach)
# library(data.table)
plotDir = "output/"
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- brewer.pal(n = 9, name = 'Set1')
ltyList <- c(1,3:6)
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
tt <- getSummaryTable(2015)
year <- 2015

####################
# Select ecosystem #
####################

# Consider time frame #

# Consider spatial extent #

###########################################################
# A EwE model must represent the main species and trophic #
# levels that are present in the modelled ecosystem and   #
# are of relevance for the policy or research question    #
# that is to be addressed                                 #
###########################################################

# Determine species to consider in the model #
## Maybe look at the frequency from various surveys for the area?


# Determine trophic 

# Fitting the model #

# SSB
# Recruitment
# F
# F-at-age
# Survey biomass index


# Parameters for the model
# Biomass per species
# Z (total mortality)
# Consumption
# # von Bertalanffy K, Linf, Winf, average temperature in the ecosystem

# Diet data by area
