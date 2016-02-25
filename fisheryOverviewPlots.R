rm(list = ls())
################
# devtools::install_github("ICES-dk/rICES", force = TRUE)
library(rICES)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(XML)
library(dplyr)
# 
plotDir = "output/"
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- brewer.pal(n = 9, name = 'Set1')
# ltyList <- c(1,3:6)
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
# Load data on ecosystem, books, guilds, etc.
# GuildList taken from this master file:
#  "https://community.ices.dk/Advice/Advice2015/Advice_books_2015/Master%20file%20for%20advice%20books%202015.xlsx"
#  Much too large of an .xlsx to load... info will soon be in the RECO database to streamline process.
guildList <- read.csv("~/git/ices-dk/rStockOverview/fisheryGuild.csv")
colnames(guildList) <- c("STOCKID", "FisheryGuild")
guildList$STOCKID <- tolower(guildList$STOCKID)
guildList$STOCKID <- gsub("[[:space:]]", "",  guildList$STOCKID)
# 
stockTable <- getSummaryTable(year = 2015)
# Remove the extra turbot...
stockTable <- stockTable[stockTable$SpeciesName != "Psetta maxima (historic name)", ]
stockTable <- merge(guildList, stockTable, by = "STOCKID", all.y = TRUE)
#
# 
# Clean up the fishing pressure descriptions and add a column describing the type of Fmsy
stockTable$fishingPressureDescription <- gsub("Fishing Pressure: " , "", stockTable$fishingPressureDescription)
stockTable$fishingPressureDescription <- gsub("Fishing pressure: " , "", stockTable$fishingPressureDescription)
stockTable$FmsyDescription <- "FMSY"
# stockTable$FmsyDescription[!is.na(stockTable$Fcap)] <- "Fcap"
# stockTable$FMSY[stockTable$FmsyDescription == "Fcap"] <- stockTable$Fcap[stockTable$FmsyDescription == "Fcap"]

# Clean up the stock size descriptions and add a column describing the type of MSYBtrigger
stockTable$stockSizeDescription[stockTable$stockSizeDescription == "NA"] <- "Stock Size: Relative"
stockTable$stockSizeDescription <- gsub("Stock Size: ", "", stockTable$stockSizeDescription)
# stockTable$BmsyDescription <- "MSYBtrigger"
# stockTable$BmsyDescription[!is.na(stockTable$MSYBescapement)] <- "MSYBescapement"
# stockTable$MSYBtrigger[stockTable$BmsyDescription == "MSYBescapement"] <- 
# stockTable$MSYBescapement[stockTable$BmsyDescription == "MSYBescapement"]

# # Drop F/FMSY and B/BMSY stocks
stockTable <- stockTable[stockTable$stockSizeDescription != "B/BMSY" |
                           stockTable$fishingPressureDescription != "F/FMSY",]
# 
df <- melt(stockTable, 
           id.vars = c("AssessmentYear",  "EcoRegion", "FisheryGuild","STOCKID", "Year"),
           measure.vars = c("F", "SSB"),
           variable.name = "METRIC",
           value.name = "VALUE")
#
colnames(df) <- c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR", "METRIC", "VALUE")
#
bookNames<- c("The Barents Sea and the Norwegian Sea", "Faroe Plateau", "Celtic Seas", "Greater North Sea",
  "Bay of Biscay and Iberian waters", "The Baltic Sea")
# 
levels(df$ECOREGION) <- c(levels(df$ECOREGION), bookNames)
df$ECOREGION[df$ECOREGION == "Barents Sea and Norwegian Sea"] <- "The Barents Sea and the Norwegian Sea"
df$ECOREGION[df$ECOREGION == "Faroe Plateau Ecosystem"] <- "Faroe Plateau"
df$ECOREGION[df$ECOREGION == "Celtic Sea and West of Scotland"] <- "Celtic Seas"
df$ECOREGION[df$ECOREGION == "North Sea"] <- "Greater North Sea"
df$ECOREGION[df$ECOREGION == "Bay of Biscay and Iberian Sea"] <- "Bay of Biscay and Iberian waters"
df$ECOREGION[df$ECOREGION == "Baltic Sea"] <- "The Baltic Sea"
df$ECOREGION <- factor(df$ECOREGION)

  # if(structure == c("ecoregion all guilds mean")){
  # Plot #1 (Ecoregion all guilds mean)
  datMean <- df %>%
    filter(!is.na(VALUE)) %>% # remove NA VALUE
    group_by(ECOREGION, GUILD, METRIC) %>%
    mutate(tsMean = mean(VALUE)) %>%  # Average F|SSB (over time series) for each guild
    group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
    mutate(guildMean = mean(VALUE),     # Average F|SSB (annual) for each guild 
           plotValue = guildMean / tsMean)         # Annual F|SSBavg / F|SSBavg (time series)
  # 
  oMean <- datMean %>%
    group_by(ECOREGION, METRIC, YEAR) %>% 
    mutate(plotValue = mean(plotValue),
           GUILD = "MEAN")   # Average F|SSB / F|SSBavg for each ecoregion
    allDat <- rbind(datMean, oMean)
    # 
    allDat <- allDat %>%
      select(pageGroup = ECOREGION,
             lineGroup = GUILD,
             YEAR,
             plotGroup = METRIC,
             plotValue) 
    # assign each lineGroup within each pageGroup a color
    plotList <- allDat %>%
      group_by(pageGroup) %>%
      select(pageGroup, lineGroup) %>%
      mutate(nLines = n_distinct(lineGroup),
             COLOR = NA) %>%
      distinct(lineGroup) %>%
      arrange(pageGroup)
    # 
    # For each pageGroup, attach colList
    # Used to keep the same lineGroup the same color for different pageGroups... (clunky but works)
    plotList$COLOR <- unlist(lapply(unique(plotList$pageGroup), 
                                    function(x) colList[1:unique(plotList$nLines[plotList$pageGroup == x])]))
    # When there are more than 9 lineGroups, make them all grey80, and mean grey40
    plotList$COLOR[plotList$nLines > 9] <- "grey80"
    plotList$COLOR[plotList$lineGroup == "MEAN"] <- "grey40"
    # 
    allDat <- full_join(plotList, allDat, by = c("pageGroup", "lineGroup"))
    #
    allDat <-
      allDat %>%
      group_by(pageGroup) %>%
      mutate(nLines = n_distinct(lineGroup)) %>%
      filter(nLines > 2 | lineGroup != "MEAN") # Remove the "MEAN" when only one lineGroup
    #
 # close ecoregion all guilds mean


  
  # if(structure == "ecoregion by guild, guild mean") {
    # Plot #2 (all stocks by ecoregion and guild, guild mean)
    datMean <- df %>%
    filter(!is.na(VALUE)) %>% # remove NA VALUE
    group_by(ECOREGION, STOCKID, METRIC) %>%
    mutate(tsMean = mean(VALUE),
           plotValue = VALUE / tsMean)   # Average F|SSB (over time series) for each stock
    # 
    oMean <- datMean %>%
      group_by(ECOREGION, GUILD, METRIC, YEAR) %>% 
      mutate(plotValue = mean(plotValue),
             STOCKID = "MEAN")   # Average F|SSB / F|SSBavg for each stock
    allDat <- rbind(datMean, oMean)
    # 
    allDat <- allDat %>%
      mutate(ECOGUILD = paste0(ECOREGION, ", ", GUILD)) %>%
      select(pageGroup = ECOGUILD,
             lineGroup = STOCKID,
             YEAR,
             plotGroup = METRIC,
             plotValue) 
    # assign each lineGroup within each pageGroup a color
    plotList <- allDat %>%
      group_by(pageGroup) %>%
      select(pageGroup, lineGroup) %>%
      mutate(nLines = n_distinct(lineGroup),
             COLOR = NA) %>%
      distinct(lineGroup) %>%
      arrange(pageGroup)
    # 
    # For each pageGroup, attach colList
    # Used to keep the same lineGroup the same color for different pageGroups... (clunky but works)
    plotList$COLOR <- unlist(lapply(unique(plotList$pageGroup), 
                                    function(x) colList[1:unique(plotList$nLines[plotList$pageGroup == x])]))
    # When there are more than 9 lineGroups, make them all grey80, and mean grey40
    plotList$COLOR[plotList$nLines > 9] <- "grey80"
    plotList$COLOR[plotList$lineGroup == "MEAN"] <- "grey40"
    # 
    allDat <- full_join(plotList, allDat, by = c("pageGroup", "lineGroup"))
    #
    allDat <-
      allDat %>%
      group_by(pageGroup) %>%
      mutate(nLines = n_distinct(lineGroup)) %>%
      filter(nLines > 2 | lineGroup != "MEAN") # Remove the "MEAN" when only one lineGroup
  #
# close ecoregion all stocks, guild mean


# if(structure == "ecoregion all stocks, ecoregion mean") {
  # Plot #3 (all stocks by ecoregion ecoregion mean)
  datMean <- df %>%
    filter(!is.na(VALUE)) %>% # remove NA VALUE
    group_by(ECOREGION, STOCKID, METRIC) %>%
    mutate(tsMean = mean(VALUE),
           plotValue = VALUE / tsMean)   # Average F|SSB (over time series) for each stock
  # 
  oMean <- datMean %>%
    group_by(ECOREGION, METRIC, YEAR) %>% 
    mutate(plotValue = mean(plotValue),
           STOCKID = "MEAN")   # Average F|SSB / F|SSBavg for each ecoregion
  allDat <- rbind(datMean, oMean)
  # 
  allDat <- allDat %>%
    select(pageGroup = ECOREGION,
           lineGroup = STOCKID,
           YEAR,
           plotGroup = METRIC,
           plotValue) 
  # assign each lineGroup within each pageGroup a color
  plotList <- allDat %>%
    group_by(pageGroup) %>%
    select(pageGroup, lineGroup) %>%
    mutate(nLines = n_distinct(lineGroup),
           COLOR = NA) %>%
    distinct(lineGroup) %>%
    arrange(pageGroup)
  # 
  # For each pageGroup, attach colList
  # Used to keep the same lineGroup the same color for different pageGroups... (clunky but works)
  plotList$COLOR <- unlist(lapply(unique(plotList$pageGroup), 
                                  function(x) colList[1:unique(plotList$nLines[plotList$pageGroup == x])]))
  # When there are more than 9 lineGroups, make them all grey80, and mean grey40
  plotList$COLOR[plotList$nLines > 9] <- "grey80"
  plotList$COLOR[plotList$lineGroup == "MEAN"] <- "grey40"
  # 
  allDat <- full_join(plotList, allDat, by = c("pageGroup", "lineGroup"))
  #
  allDat <-
    allDat %>%
    group_by(pageGroup) %>%
    mutate(nLines = n_distinct(lineGroup)) %>%
    filter(nLines > 2 | lineGroup != "MEAN") # Remove the "MEAN" when only one lineGroup
  #
# }

  # Data should be in this format for the plotting functions, below:
# Plot 1
plotFisheryOverview(data = allDat, overallMean = TRUE, plotDir = plotDir, plotTitle = "_allGuilds-AVG_v2")
# Plot 2
plotFisheryOverview(data = allDat, overallMean = TRUE, plotDir = plotDir, plotTitle = "_allStocks-AVG_v2")
# Plot 3
plotFisheryOverview(data = allDat, overallMean = TRUE, plotDir = plotDir, plotTitle = "_allStocks_v2")


plotFisheryOverview <- function(data, overallMean = TRUE, plotDir = "~/", plotTitle) {
   allDat <- data
  for(pgGroup in unique(allDat$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
    all.pg <- allDat[allDat$pageGroup == pgGroup,]
    # 
    plotFileName = paste0(plotDir, pgGroup, plotTitle, ".png")
    # PLOT AVG
    png(filename = plotFileName,
        width = 172.4,
        height = 162.6,
        units = "mm",
        res = 600)
    #
    par(mfrow = c(2,1),
        mar=c(2.15, 2.25, 0.45, 0.25),
        oma = c(0, 0, 1.25, 0),
        usr = c(0,1,0,1),
        mgp=c(3, .35, 0),
        tck=-0.01,
        family = "Calibri")
    # Order the lineGroup to make sure mean is plotted
    if(overallMean == TRUE) {
      if(any(all.pg$lineGroup == "MEAN")) {
        lineGroupOrd <- relevel(factor(unique(all.pg$lineGroup),
                                       ordered = F),
                                ref = "MEAN")
        if(length(lineGroupOrd) >= 10) {
          lineGroupOrd <- factor(lineGroupOrd, levels = rev(levels(lineGroupOrd)))
        } # close >= 10
      } #  reorder lineGroupOrd if overallMean == T
      else {
        lineGroupOrd <- factor(unique(all.pg$lineGroup),
                                      ordered = T)
        } # reorder lineGroupOrd if overallMean == F
      } # TRUE overallMean
    if(overallMean == FALSE ) {
      lineGroupOrd <- factor(unique(all.pg$lineGroup),
                             ordered = T)
      } # FALSE overallMean
    # 
    for(plGroup in unique(all.pg$plotGroup)) { # Data grouped by PLot (e.g., F or SSB)
      #
      all.pl <- all.pg[all.pg$plotGroup == plGroup,]
      # 
      yRange <- c(0, max(all.pl$plotValue, na.rm =T) + max(all.pl$plotValue, na.rm = T) * .15)
      xRange <- c(min(all.pl$YEAR[!is.na(all.pl$plotValue)]),
                    max(all.pl$YEAR[!is.na(all.pl$plotValue)]))
      #
      plot(NA,
           type = "l",
           ylim = yRange,
           xlim = xRange,
           yaxt = "n",
           xaxt = "n",
           ann = FALSE)
      abline(h = 1.0, lty = 2, col = "black", lwd = 1)
      #
      # Add lines according to the lnGroup
      for(lnGroup in levels(lineGroupOrd)) {
        if(all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          lnGroup <- NA
          next
        } # close next if all NA
        if(!all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          d <- data.frame(all.pl$plotValue[all.pl$lineGroup == lnGroup],
                          all.pl$YEAR[all.pl$lineGroup == lnGroup])
          d <- d[order(d[,2]),]
          col.d <- as.character(unique(all.pl$COLOR[all.pl$lineGroup == lnGroup]))
          lin.d <- ifelse(lnGroup == "MEAN", 4, 2) 
          lines(d[,2], d[,1], col = col.d, lwd = lin.d)
        } # close line plotting
      } # close lnGroup
      #
      # Label axes
      axis(1, at = pretty(xRange), cex.axis = .85)
      mtext("Year", side = 1, line = 1.25, cex= 1)
      axis(2, at = pretty(yRange), cex.axis = .75, las = 1)
      if(plGroup == "F") {
        mtext(expression("F/F"["average"]), side = 2, line = 1, cex= 1)
      } # close F
      if(plGroup == "SSB") {
        mtext(expression("SSB/SSB"["average"]), side = 2, line = 1, cex= 1)
      } # close SSB 
      mtext(pgGroup, side = 3, outer = T, cex= 1.5, font = 2)
      #
      # Legend
      if(length(lineGroupOrd) <= 9) {
        legend("topright",
               legend = as.character(unique(all.pl$lineGroup)),
               fill = as.character(unique(all.pl$COLOR)),
               bty="n",
               ncol = 3,
               cex = .85)
      } # Close less than 9 lines legend
      if(length(lineGroupOrd) >= 10) {
        legend("topright",
               legend = "MEAN",
               fill = "grey40",
               bty="n",
               ncol = 1,
               cex = .85)
      } # close more than 10 lines legend
    } # Close plGroup
    dev.off()
  }# Close pgGroup
} # Close function