rm(list = ls())
################
# library(plyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(XML)
library(dplyr)
library(tidyr)
# 
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- brewer.pal(n = 9, name = 'Set1')
ltyList <- c(1,3:6)
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
# Load data on ecosystem, books, guilds, etc.
load("allDataPlotOverview_v001.rdat")
stockInfo <- stockInfo[, c("speciesID", "Type")]
colnames(stockInfo) <- c("speciesID", "Guild")
stockInfo <- stockInfo[!duplicated(stockInfo),]
# 
# Load most recent data from Stock Assessment Graphs database
source("~/git/ices-dk/rICES/R/getSummaryTable.R")
stockTable <- getSummaryTable(year = 2016)
# 
# Set output directory
plotDir = "output/2016/"
# 
# Clean up the fishing pressure descriptions and add a column describing the type of Fmsy
stockTable$fishingPressureDescription <- gsub("Fishing Pressure: " , "", stockTable$fishingPressureDescription)
stockTable$fishingPressureDescription <- gsub("Fishing pressure: " , "", stockTable$fishingPressureDescription)
stockTable$fishingPressureDescription <- gsub("msy" , "MSY", stockTable$fishingPressureDescription)
stockTable$FmsyDescription <- "FMSY"
keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure")
relativeF <- c("F/FMSY", "Harvest rate/FMSY") 
stockTable$FmsyDescription[stockTable$fishingPressureDescription %in% relativeF] <- "F/FMSY"
# 
# Clean up the stock size descriptions and add a column describing the type of MSYBtrigger
stockTable$stockSizeDescription[stockTable$stockSizeDescription == "NA"] <- "Stock Size: Relative"
stockTable$stockSizeDescription <- gsub("Stock Size: ", "", stockTable$stockSizeDescription)
stockTable$stockSizeDescription <- gsub("Stock size: ", "", stockTable$stockSizeDescription)
stockTable$stockSizeDescription <- gsub("msy", "MSY", stockTable$stockSizeDescription)
stockTable$BmsyDescription <- "MSYBtrigger"
stockTable$BmsyDescription[!is.na(stockTable$MSYBescapement)] <- "MSYBescapement"
stockTable$MSYBtrigger[stockTable$BmsyDescription == "MSYBescapement"] <- stockTable$MSYBescapement[stockTable$BmsyDescription == "MSYBescapement"]
keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
relativeSSB <- c("B/BMSY", "Harvest rate/FMSY") 
stockTable$BmsyDescription[stockTable$BmsyDescription %in% relativeSSB] <- "SSB/BMSY"
# 
# Drop relative stocks and stocks with indices
stockTable <- stockTable[stockTable$stockSizeDescription %in%  keeperSSB |
                         stockTable$fishingPressureDescription %in% keeperF, ]
# 
# To make the SSB/MSYBtrigger comparison, B/BMSY stocks have already been calculated, so for B/BMSY stocks MSYBtrigger == 1
stockTable$MSYBtrigger[stockTable$stockSizeDescription %in% relativeSSB] <- 1
stockTable$FMSY[stockTable$fishingPressureDescription %in% relativeF] <- 1
stockTable$MSYBtrigger[!stockTable$stockSizeDescription %in% keeperSSB] <- NA
stockTable$FMSY[!stockTable$fishingPressureDescription %in% keeperF] <- NA
# 
stockTable  <- merge(stockTable,
                    stockInfo,
                    by = "speciesID", all.x = T, all.y = F)
# 
df <- melt(stockTable, 
           id.vars = c("AssessmentYear", "EcoRegion", "Guild", "STOCKID", "Year", "speciesID"),
           measure.vars = c("F", "SSB", "FMSY", "MSYBtrigger"),
           variable.name = "METRIC",
           value.name = "VALUE")
#
colnames(df) <- c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR", "speciesID", "METRIC", "VALUE")
#
# Clean up MSYBtrigger == 0
df$VALUE[df$VALUE == 0 &
         df$METRIC == "MSYBtrigger"] <- NA
# 
df$VALUE[df$VALUE == 0 &
         df$METRIC == "FMSY"] <- NA
# 
df$ECOREGION[df$ECOREGION == "Barents Sea and Norwegian Sea"] <- "The Barents Sea and the Norwegian Sea"
df$ECOREGION[df$ECOREGION == "Faroe Plateau Ecosystem"] <- "Faroe Plateau"
df$ECOREGION[df$ECOREGION == "Celtic Sea and West of Scotland"] <- "Celtic Seas"
df$ECOREGION[df$ECOREGION == "North Sea"] <- "Greater North Sea"
df$ECOREGION[df$ECOREGION == "Bay of Biscay and Iberian Sea"] <- "Bay of Biscay and Iberian waters"
df$ECOREGION[df$ECOREGION == "Baltic Sea"] <- "The Baltic Sea"
# 
df$GUILD[df$speciesID %in% c("bli", "lin")] <- "Demersal"
df$GUILD[df$speciesID %in% c("ghl")] <- "Flatfish"
#
df <- df[!is.na(df$GUILD),]
# 
dfAverage <- df %>%
  filter(!is.na(VALUE),
         METRIC %in% c("F", "SSB")) %>% # remove NA VALUE
  select(-speciesID) %>%
  group_by(ECOREGION, METRIC, STOCKID) %>%
  mutate(valueMean = mean(VALUE, na.rm = TRUE), # F|SSB Average for each STOCKID
         stockValue = VALUE / valueMean) %>%  # F|SSB / valueMean
  group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
  mutate(guildValue = mean(stockValue, na.rm = TRUE)) %>% # stockMean Average F|SSB for each GUILD
  group_by(ECOREGION, METRIC, YEAR) %>%
  mutate(ecoValue = mean(stockValue, na.rm = TRUE))  # stockMean Average F|SSB for each ECOREGION
# 
dfFMSY <- df %>%
  filter(!is.na(VALUE) &
         METRIC %in% c("F", "FMSY") &
         YEAR < 2016) %>% # remove NA VALUE, select F values, and get rid of 2016 values
  spread(METRIC, VALUE) %>%
  mutate(METRIC = "F_FMSY",
         VALUE = F/ FMSY) %>%
  select(-F, -FMSY, -speciesID) %>%
  mutate(stockValue = VALUE) %>%  
  group_by(ECOREGION, GUILD, YEAR) %>%
  mutate(guildValue = mean(VALUE, na.rm = TRUE)) %>% # stockMean Average F|SSB for each GUILD
  group_by(ECOREGION, YEAR) %>%
  mutate(ecoValue = mean(VALUE, na.rm = TRUE)) # stockMean Average F|SSB for each ECOREGION
# 
dfMSYBtrigger <- df %>%
  filter(!is.na(VALUE) &
           METRIC %in% c("SSB", "MSYBtrigger")) %>% # remove NA VALUE, select F values, and get rid of 2016 values
  spread(METRIC, VALUE) %>%
  mutate(METRIC = "SSB_MSYBtrigger",
         VALUE = SSB/ MSYBtrigger) %>%
  select(-SSB, -MSYBtrigger, -speciesID) %>%
  mutate(stockValue = VALUE) %>%  
  group_by(ECOREGION, GUILD, YEAR) %>%
  mutate(guildValue = mean(VALUE, na.rm = TRUE)) %>% # stockMean Average F|SSB for each GUILD
  group_by(ECOREGION, YEAR) %>%
  mutate(ecoValue = mean(VALUE, na.rm = TRUE)) # stockMean Average F|SSB for each ECOREGION
# 
dfMSY <- dfMSYBtrigger %>%
  full_join(dfFMSY)
# 
# write.csv(td, file = "20160718_StockOverviewData.csv", row.names = FALSE)
plotDat <- function(data, plotType = c("plot1", "plot2", "plot3")){
  #
  if(!plotType %in% c("plot1", "plot2", "plot3")) {
    stop("Please specify the type of plot type.")
  }
  if(plotType == "plot1") {
    # Plot #1 (Ecoregion all guilds mean)
    allDat <- data %>%
      select(pageGroup = ECOREGION,
             lineGroup = GUILD,
             YEAR,
             plotGroup = METRIC,
             plotValue = guildValue) %>%
      filter(!is.na(plotValue))
    # 
    oMean <- data %>%
      distinct(ECOREGION, METRIC, YEAR) %>% 
      select(pageGroup = ECOREGION,
             YEAR,
             plotGroup = METRIC,
             plotValue = ecoValue) %>%
      mutate(lineGroup = "MEAN") %>%
      filter(!is.na(plotValue))
  } #close plot1
  #
  if(plotType == "plot2") {
    # Plot #2 (all stocks by ecoregion and guild, guild mean)
    allDat <- data %>%
      mutate(ECOGUILD = paste0(ECOREGION, ", ", GUILD)) %>%
      select(pageGroup = ECOGUILD,
             # select(pageGroup = ECOREGION,
             lineGroup = STOCKID,
             YEAR,
             plotGroup = METRIC,
             plotValue = stockValue) %>%
      filter(!is.na(plotValue))
    # 
    oMean <- data %>%
      mutate(ECOGUILD = paste0(ECOREGION, ", ", GUILD)) %>%
      distinct(ECOGUILD, METRIC, YEAR) %>% 
      select(pageGroup = ECOGUILD,
             YEAR,
             plotGroup = METRIC,
             plotValue = guildValue) %>%
      mutate(lineGroup = "MEAN") %>%
      filter(!is.na(plotValue))
  } #close plot2
  #
  if(plotType == "plot3") {
    # Plot #3 (all stocks by ecoregion mean)
    allDat <- data %>%
      select(pageGroup = ECOREGION,
             lineGroup = STOCKID,
             YEAR,
             plotGroup = METRIC,
             plotValue = stockValue) %>%
      filter(!is.na(plotValue))
    # 
    oMean <- data %>%
      distinct(ECOREGION, METRIC, YEAR) %>% 
      select(pageGroup = ECOREGION,
             YEAR,
             plotGroup = METRIC,
             plotValue = ecoValue) %>%
      mutate(lineGroup = "MEAN") %>%
      filter(!is.na(plotValue))
  } # close plot3
  # 
  allDat <- rbind(oMean, allDat)
  # assign each lineGroup within each pageGroup a color
  plotList <- allDat %>%
    group_by(pageGroup) %>%
    select(pageGroup, lineGroup) %>%
    mutate(nLines = n_distinct(lineGroup),
           COLOR = NA) %>%
    distinct(lineGroup) %>%
    arrange(pageGroup, lineGroup)
  # 
  if(length(unique(plotList$lineGroup)) <= 10) {
    jColors <- data.frame(lineGroup = unique(plotList$lineGroup),
                          COLOR = colList[1:length(unique(plotList$lineGroup))])
    plotList$COLOR <- as.character(jColors$COLOR[match(plotList$lineGroup, jColors$lineGroup)])
  } else {
    # For each pageGroup, attach colList
    # Used to keep the same lineGroup the same color for different pageGroups... (clunky but works)
    plotList$COLOR <- unlist(lapply(unique(plotList$pageGroup), 
                                    function(x) colList[1:unique(plotList$nLines[plotList$pageGroup == x])]))
  }
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
  return(allDat)
} # close plotDat function
# 
# 
plotFisheryOverview <- function(data, overallMean = TRUE, plotDir = "~/", plotTitle) {
  allDat <- data
  for(pgGroup in unique(allDat$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
    all.pg <- allDat[allDat$pageGroup == pgGroup,]
    # 
    if(any(names(all.pg) == "plotGroup") == FALSE) {
      all.pg$plotGroup <- "NONE"
    }
    # 
    all.pg$plotGroup <- factor(all.pg$plotGroup)   
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
    for(plGroup in unique(levels(all.pg$plotGroup))) { # Data grouped by PLot (e.g., F or SSB)
      #
      all.pl <- all.pg[all.pg$plotGroup == plGroup,]
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
      if(plGroup == "F_FMSY") {
        mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex= 1)
      } # close F
      if(plGroup == "SSB_MSYBtrigger") {
        mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= 1)
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
# 
# 
# Plot 1
plotFisheryOverview(data = plotDat(data = dfAverage, plotType = "plot1"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allGuilds-AVG_2016")
# 
plotFisheryOverview(data = plotDat(data = dfMSY, plotType = "plot1"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allGuilds-MSY_2016")
# Plot 2
plotFisheryOverview(data = plotDat(data = dfAverage, plotType = "plot2"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allStocks-AVG_2016")
# 
plotFisheryOverview(data = plotDat(data = dfMSY, plotType = "plot2"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allStocks-MSY_2016")
# Plot 3
plotFisheryOverview(data = plotDat(data = dfAverage, plotType = "plot3"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allStocks-AVG_2016")
# 
plotFisheryOverview(data = plotDat(data = dfMSY, plotType = "plot3"),
                    overallMean = TRUE, 
                    plotDir = plotDir, 
                    plotTitle = "_allStocks-MSY_2016")

