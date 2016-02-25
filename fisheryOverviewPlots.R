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
# dat <- df

  # Plot #1 (Ecoregion all guilds mean)
  # 
  datMean <- df %>%
    filter(!is.na(VALUE)) %>% # remove NA VALUE
    group_by(ECOREGION, GUILD, METRIC) %>%
    mutate(tsMean = mean(VALUE)) %>%  # Average F|SSB (over time series) for each guild
    group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
    mutate(guildMean = mean(VALUE),     # Average F|SSB (annual) for each guild 
           plotValue = guildMean / tsMean)         # Annual F|SSBavg / F|SSBavg (time series)
  # if(overallMean == TRUE) {
  oMean <- datMean %>%
    group_by(ECOREGION, METRIC, YEAR) %>% 
    mutate(plotValue = mean(plotValue),
           GUILD = "MEAN")   # Average F|SSB / F|SSBavg for each ecoregion
    allDat <- rbind(datMean, oMean)
  # 
  allDat <- allDat[colnames(allDat) %in% c("ECOREGION", "GUILD", "YEAR", "METRIC", "plotValue")]
  colnames(allDat) <- c("pageGroup", "lineGroup", "YEAR", "plotGroup",  "plotValue")
  #
  plot1Col <- data.frame(lineGroup = unique(allDat$lineGroup),
                         COLOR = brewer.pal(n =length(unique(allDat$lineGroup)),
                                            name = 'Set1'))
  levels(plot1Col$COLOR) <- c(levels(plot1Col$COLOR), "grey40")
  plot1Col$COLOR[plot1Col$lineGroup == "MEAN"] <- "grey40"
  # 
  allDat <- allDat %>%
      group_by(pageGroup) %>%
      mutate(nLines = n_distinct(lineGroup)) %>%
      filter(nLines >= 3 | lineGroup != "MEAN") # Remove the "MEAN" when only one lineGroup

  allDat <- full_join(plot1Col, allDat, by = "lineGroup")

  
  
  
  
  
  
  # Data should be in this format for the plotting functions, below:
  # 
  # pgGroup = "Bay of Biscay and Iberian Sea"
  # plGroup = "F"
  # lnGroup = "MEAN"
  # # Identify colors for lines - do it here so consistent within same page
  # plot1Col <- data.frame(lineGroup = unique(allDat$lineGroup),
  #                        COLOR = brewer.pal(n =length(unique(allDat$lineGroup)),
  #                                           name = 'Set1'))
  # levels(plot1Col$COLOR) <- c(levels(plot1Col$COLOR), "grey40")
  # plot1Col$COLOR[plot1Col$lineGroup == "MEAN"] <- "grey40"
  # #
  # all.pg$COLOR <- NA
  # all.pg$COLOR <- plot1Col$COLOR[match(all.pg$lineGroup, plot1Col$lineGroup)]
  # #
   
  for(pgGroup in unique(allDat$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
    all.pg <- allDat[allDat$pageGroup == pgGroup,]
    # 
    plotFileName = paste0(plotDir, pgGroup, "_allGuilds-AVG_v1.png")
    overallMean = TRUE
    #
    # PLOT AVGs
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
        }
      else {
        lineGroupOrd <- factor(unique(all.pg$lineGroup),
                                      ordered = T)
        }  
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
      # if(any(all.pl$lineGroup == "MEAN")) {
      #   all.pl <- all.pl[sort(order(all.pl$lineGroup == "MEAN")),] # Sort by mean
      # } 
      # 
      yRange <- c(0, max(all.pl$plotValue, na.rm =T) + max(all.pl$plotValue, na.rm = T) * .15)
      xRange <- c(min(all.pl$YEAR[!is.na(all.pl$plotValue)]),
                    max(all.pl$YEAR[!is.na(all.pl$plotValue)]))
      
      
      # guild.i <- factor(unique(all.pl$lineGroup), ordered = T)
      # if(length(stocks.i) >= 10) {
      #   levels(stocks.avg.i$COLOR) <- c(levels(stocks.avg.i$COLOR), "grey80")
      #   stocks.avg.i$COLOR[stocks.avg.i$CODE != "MEAN"] <- "grey80"
      #   #         }
      #   #         if(length(stocks.i) >= 10) {
      #   stocks.i <- rev(levels(stocks.i))
      # }
      # if(length(stocks.i) < 10) {
      #   stocks.i <- levels(stocks.i)
      # }
      # if(length(stocks.i) <= 2) {
      #   stocks.i <- stocks.i[stocks.i != "MEAN"]
      #   plotDat.i <- plotDat.i[plotDat.i$Stock.code %in% stocks.i,]
      # }

      plot(NA,
           type = "l",
           ylim = yRange,
           xlim = xRange,
           yaxt = "n",
           xaxt = "n",
           ann = FALSE)
      abline(h = 1.0, lty = 2, col = "black", lwd = 1)
      #
      for(lnGroup in levels(lineGroupOrd)) {
        if(all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          lnGroup <- NA
          next
        }
        if(!all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          d <- data.frame(all.pl$plotValue[all.pl$lineGroup == lnGroup],
                          all.pl$YEAR[all.pl$lineGroup == lnGroup])
          d <- d[order(d[,2]),]
          col.d <- as.character(unique(all.pl$COLOR[all.pl$lineGroup == lnGroup]))
          lin.d <- ifelse(lnGroup == "MEAN", 4, 2) 
          lines(d[,2], d[,1], col = col.d, lwd = lin.d)
        }
      }
      #
      axis(1, at = pretty(xRange), cex.axis = .85)
      #
      mtext("Year", side = 1, line = 1.25, cex= 1)
      #
      axis(2, at = pretty(yRange), cex.axis = .75, las = 1)
      #
      if(plGroup == "F") {
        mtext(expression("F/F"["average"]), side = 2, line = 1, cex= 1)
      }
      if(plGroup == "SSB") {
        mtext(expression("SSB/SSB"["average"]), side = 2, line = 1, cex= 1)
      }
      # Legend
      # tt <- all.pl$COLOR[all.pl$lineGroup %in% lineGroupOrd]
      if(length(lineGroupOrd) <= 9) {
        legend("topright",
               legend = as.character(unique(all.pl$lineGroup)),
               fill = as.character(unique(all.pl$COLOR)),
               bty="n",
               ncol = 3,
               cex = .85)
      }
      if(length(lineGroupOrd) >= 10) {
        legend("topright",
               legend = "MEAN",
               fill = "grey40",
               bty="n",
               ncol = 1,
               cex = .85)
      }
    mtext(pgGroup, side = 3, outer = T, cex= 1.5, font = 2)
    } # Close plGroup
    dev.off()
      
  }# Close pgGroup
  # if(overallMean == FALSE)

  # 
  # } # Close FALSE overallMean
  
  # pageGroup = ECOREGION, lineGroup = GUILD, plotGroup = METRIC



    
    
  plotDat <- data.frame(plotGroup = rep(unique(allDat$plotGroup), each = length(lineGroupOrd)),
                        lineGroup = lineGroupOrd[order(lineGroupOrd)],
                        COLOR = c("grey40", colList[1:length(lineGroupOrd) -1]),
                        LWD = c(4, rep(2, length(lineGroupOrd) -1)))

  # Add plotting information match colors with guilds


    plGroup = "F"
  if(length(lineGroupOrd) >= 10) {
    # For more than 10 line colors, change to grey80 and make the mean
    # overplot the lines
    levels(plotDat$COLOR[plotDat$plotGroup == plGroup]) <- c(levels(plotDat$COLOR[plotDat$plotGroup == plGroup]),
                                                            "grey80")
    plotDat$COLOR[plotDat$lineGroup != "MEAN" &
                  plotDat$plotGroup == plGroup] <- "grey80"
    lineGroupOrd <- rev(levels(lineGroupOrd))


  }
  if(length(lineGroupOrd) < 10) {
    # For less than 10 line colors, leave the same and colors overplot
    # the mean
    lineGroupOrd <- levels(lineGroupOrd)
  }
  if(length(lineGroupOrd) <= 2) {
    # When only 2 line colors (MEAN + a lineGroup), remove MEAN and only
    # plot the color
    lineGroupOrd <- lineGroupOrd[lineGroupOrd != "MEAN"]
    
    plotDat.i <- plotDat.i[plotDat.i$Stock.code %in% stocks.i,]
  }
  lab.i <- metricList_AVG[i]
  
  # 
  allDat <- merge(allDat, plotDat, by = "lineGroup", sort = F)
  
  # 
  # guild_ecoMeanCol <- data.frame("CODE" = unique(guild_ecoMean$GUILD),
  #                                "COLOR" = c("grey40", colList[1:length(unique(guild_ecoMean$GUILD)) -1]),
  #                                "LWD" = c(4, rep(2, length(unique(guild_ecoMean$GUILD)) -1)))
  # 

  
  
  
  # Overall mean
  # overallMean <- ddply(guild_ecoMean, .(ECOREGION, METRIC, YEAR), summarize,
  #                      overallMean = mean(guild_ecoMean, na.rm = T))
  # 
  # guild_ecoMean <- rbind(data.frame(ECOREGION = overallMean$ECOREGION,
  #                                   GUILD = "MEAN",
  #                                   METRIC = overallMean$METRIC,
  #                                   YEAR = overallMean$YEAR,
  #                                   guild_ecoMean = overallMean$overallMean),
  #                  guild_ecoMean)
  # Match colors with guilds
  # guild_ecoMeanCol <- data.frame("CODE" = unique(guild_ecoMean$GUILD),
  #                                "COLOR" = c("grey40", colList[1:length(unique(guild_ecoMean$GUILD)) -1]),
  #                                "LWD" = c(4, rep(2, length(unique(guild_ecoMean$GUILD)) -1)))
  
  # guild_ecoMean <- merge(guild_ecoMean, guild_ecoMeanCol, by.x = "GUILD", by.y = "CODE")

  
  # Plot #2 (Ecoregion all stocks with guild mean)
  # datMean$stockMean
  # datMean$guild_ecoMean
  
  # Plot #3 (Ecoregion all stocks with ecoregion mean)
  # datMean$stockMean
  # datMean$ecoMean
  
  
  # F/Fmean & SSB/SSBmean
  # #
  # # GUILD by ECOREGION
  # datMean <- ddply(dat, .(ECOREGION, GUILD, METRIC), mutate,
  #                  guild_ecoMean = VALUE / mean(VALUE, na.rm = T))
  # #
  # # ECOREGION
  # datMean <- ddply(datMean, .(ECOREGION, METRIC), mutate,
  #                  ecoMean = VALUE / mean(VALUE, na.rm = T))
  # #
  # # STOCKID by ECOREGION
  # datMean <- ddply(datMean, .(STOCKID, METRIC), mutate,
  #                  stockMean = VALUE / mean(VALUE, na.rm = T))
  # #
  # # GUILD
  # datMean <- ddply(datMean, .(GUILD, METRIC), mutate,
  #                  guildMean = VALUE / mean(VALUE, na.rm = T))
  # # dta <- dat[!duplicated(dat),]
  
  
  
  # plotDat <- data.frame(YEAR = datMean$YEAR,
  #                       STOCKID = datMean$STOCKID,
  #                       meanVal = datMean$meanVal,
  #                       Metric = datMean$METRIC)
  # # Overall mean
  # overallMean <- ddply(datMean, .(YEAR, METRIC), summarize,
  #                      overallMean = mean(meanVal, na.rm = T))
  # plotDat <- rbind(data.frame(YEAR = overallMean$YEAR,
  #                             Stock.code = "MEAN",
  #                             meanVal = overallMean$overallMean,
  #                             Metric = overallMean$METRIC),
  #                  plotDat)
  
  

  #
  
  #     metricList_AVG <- lapply(factor(unique(plotDat$Metric[!is.na(plotDat$meanVal)]),
  #                              levels = c("F", "SSB")), as.character)
  #     stocks.avg <- lapply(stocks.avg, as.character)
  # stocks.avg <- data.frame("CODE" = unique(plotDat$Stock.code),
  #                          "COLOR" = c("grey40", colList[1:length(unique(plotDat$Stock.code)) -1]),
  #                          "LWD" = c(4, rep(2, length(unique(plotDat$Stock.code)) -1)))
  # 
  # metricList_AVG <- c("F", "SSB")
  # metricFactor_AVG <- factor(unique(guild_ecoMean$METRIC[!is.na(guild_ecoMean$guild_ecoMean)]),
  #                            levels = c("F", "SSB"))
  
  metricList_AVG <- unique(guild_ecoMean$METRIC[guild_ecoMean$METRIC %in% c("F", "SSB")])
  # stocks.avg <- lapply(stocks.avg, as.character)
  #
  i = "F"
  # met <- NULL
##  for(i in unique(plotDat$plotGroup)) {
    #         met <- metricList_AVG
    #         if(met[i] == "F") {
    #           par(mfg = c(1,1))
    #           }
    #         if(met[i] == "SSB") {
    #           par(mfg = c(2,1))
    #         }
  i = "SSB"
    plotDat.i <- allDat[plotDat$plotGroup == i,]
    
    ## PLOT F Avg ##
    plotDat.i <- guild_ecoMean[guild_ecoMean$METRIC == i &
                               guild_ecoMean$ECOREGION == "Baltic Sea",]
    plotDat.i <- plotDat.i[sort(order(plotDat.i$GUILD == "MEAN")),]
    
    stocks.avg.i <- stocks.avg
    
    
    # Identify plotting parameters
    yRange.i <- c(0, max(plotDat.i$guild_ecoMean, na.rm =T) + max(plotDat.i$guild_ecoMean, na.rm = T) * .15)
    xRange.i <- c(min(plotDat.i$YEAR[!is.na(plotDat.i$guild_ecoMean)]),
                  max(plotDat.i$YEAR[!is.na(plotDat.i$guild_ecoMean)]))
    guild.i <- factor(unique(plotDat.i$GUILD), ordered = T)
    # if(length(stocks.i) >= 10) {
    #   levels(stocks.avg.i$COLOR) <- c(levels(stocks.avg.i$COLOR), "grey80")
    #   stocks.avg.i$COLOR[stocks.avg.i$CODE != "MEAN"] <- "grey80"
    #   #         }
    #   #         if(length(stocks.i) >= 10) {
    #   stocks.i <- rev(levels(stocks.i))
    # }
    # if(length(stocks.i) < 10) {
    #   stocks.i <- levels(stocks.i)
    # }
    # if(length(stocks.i) <= 2) {
    #   stocks.i <- stocks.i[stocks.i != "MEAN"]
    #   plotDat.i <- plotDat.i[plotDat.i$Stock.code %in% stocks.i,]
    # }
    lab.i <- i
    #
    plot(NA,
         type = "l",
         ylim = yRange.i,
         xlim = xRange.i,
         yaxt = "n",
         xaxt = "n",
         ann = FALSE)
    abline(h = 1.0, lty = 2, col = "black", lwd = 1)
    #
    j = "benthic"
    for(j in guild.i) {
      if(all(is.na(plotDat.i$guild_ecoMean[plotDat.i$GUILD == j]))) {
        j <- NA
        next
      }
      if(!all(is.na(plotDat.i$guild_ecoMean[plotDat.i$GUILD == j]))) {
        d <- data.frame(plotDat.i$guild_ecoMean[plotDat.i$GUILD == j],
                        plotDat.i$YEAR[plotDat.i$GUILD == j])
        col.d <- as.character(unique(plotDat.i$COLOR[plotDat.i$GUILD == j]))
        lin.d <- unique(as.numeric(as.character(plotDat.i$LWD[plotDat.i$GUILD == j])))
        lines(d[,2], d[,1], col = col.d, lwd = lin.d)
      }
    }
    #
    axis(1, at = pretty(xRange.i), cex.axis = .85)
    #
    mtext("Year", side = 1, line = 1.25, cex= 1)
    #
    axis(2, at = pretty(yRange.i), cex.axis = .75, las = 1)
    #
    if(lab.i == "F") {
      mtext(expression("F/F"["average"]), side = 2, line = 1, cex= 1)
    }
    if(lab.i == "SSB") {
      mtext(expression("SSB/SSB"["average"]), side = 2, line = 1, cex= 1)
    }
    # Legend
    if(length(stocks.i) <= 9) {
      legend("topright",
             legend = stocks.avg.i$CODE[stocks.avg.i$CODE %in% stocks.i],
             fill = stocks.avg.i$COLOR[stocks.avg.i$CODE %in% stocks.i],
             bty="n",
             ncol = 3,
             cex = .85)
    }
    if(length(stocks.i) >= 10) {
      legend("topright",
             legend = "MEAN",
             fill = "grey40",
             bty="n",
             ncol = 1,
             cex = .85)
    }
    #         if(logo == T) {
    #         logoFun(img, x = 0.5, y = 0.5, size = .5, alpha = alpha)
    #         }
  } # Close average loop
  mtext(paste0(Book, ", ", Guild), side = 3, outer = T, cex= 1.5, font = 2)
  dev.off()
  
  
  
  
  
  ############
  # PLOT msy #
  ############
  #   
  FmsyDat <- dat[dat$METRIC %in% c("F", "FMSY"),]
  # 
  FmsyDat <- dcast(FmsyDat, ASSESSMENTYEAR + ECOREGION + GUILD + STOCKID + YEAR ~ METRIC, value.var = "VALUE")
  FmsyDat$F.Fmsy <- FmsyDat$F / FmsyDat$FMSY
  FmsyDat <- melt(FmsyDat, id.vars = c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR"),
                  measure.vars = c("F", "FMSY", "F.Fmsy"),
                  variable.name = "METRIC",
                  value.name = "VALUE")
  Fmsy <- FmsyDat[FmsyDat$METRIC == "F.Fmsy",]
  FmsyNA <- sapply(unique(Fmsy$STOCKID), function(x) all(is.na(Fmsy$VALUE[Fmsy$STOCKID == x])))
  Fmsy <- Fmsy[Fmsy$STOCKID %in% names(FmsyNA[FmsyNA == "FALSE"]),]
  FmsyStockID <- unique(Fmsy$STOCKID[Fmsy$STOCKID %in% names(FmsyNA[FmsyNA == "FALSE"])])
  #
  BmsyDat <- dat[dat$METRIC %in% c("SSB", "MSYBtrigger"),]
  BmsyDat <- dcast(BmsyDat, ASSESSMENTYEAR + ECOREGION + GUILD + STOCKID + YEAR ~ METRIC, value.var = "VALUE")
  BmsyDat$SSB.Btrigger <- BmsyDat$SSB / BmsyDat$MSYBtrigger
  BmsyDat <- melt(BmsyDat, id.vars = c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR"),
                  measure.vars = c("SSB", "MSYBtrigger", "SSB.Btrigger"),
                  variable.name = "METRIC",
                  value.name = "VALUE")
  Bmsy <- BmsyDat[BmsyDat$METRIC == "SSB.Btrigger",]
  BmsyNA <- sapply(unique(Bmsy$STOCKID), function(x) all(is.na(Bmsy$VALUE[Bmsy$STOCKID == x])))
  Bmsy <- Bmsy[Bmsy$STOCKID %in% names(BmsyNA[BmsyNA == "FALSE"]),]
  #
  msyDat <- rbind(Fmsy, Bmsy)
  msyDat <- msyDat[,c("YEAR","ECOREGION", "STOCKID", "GUILD", "VALUE", "METRIC")]
  #   
  ## TO MAKE THE SUMMARY TABLE ##
  rj <-  ddply(msyDat, .(ECOREGION, GUILD,STOCKID, METRIC, YEAR), summarize,
               out = VALUE)
  sumOut <- data.frame()
  for(i in unique(rj$STOCKID)) {
    rj.i <-  rj[rj$STOCKID == i &
                  complete.cases(rj),]
    # 
    if(nrow(rj.i[rj.i$METRIC == "F.Fmsy",]) == 0){
      fMax <- data.frame(rj.i[1,1:4], 
                         F.Year = NA, 
                         F.Fmsy = NA)
      # 
    } else {
      fMax <- rj.i[rj.i$METRIC == "F.Fmsy",]
      fMax <- fMax[fMax$YEAR == max(fMax$YEAR, na.rm = T),]
      colnames(fMax)[5:6] <- c("F.Year", "F.Fmsy")
    }
    #
    if(nrow(rj.i[rj.i$METRIC == "SSB.Btrigger",]) == 0){
      bMax <- data.frame(rj.i[1,1:4],
                         SSB.Year = NA, 
                         SSB.Btrigger = NA)
    } else {
      bMax <- rj.i[rj.i$METRIC == "SSB.Btrigger",]
      bMax <- bMax[bMax$YEAR == max(bMax$YEAR, na.rm = T),]
      colnames(bMax)[5:6] <- c("SSB.Year", "SSB.Btrigger")
    }
    sumOut <- rbind(sumOut, cbind(fMax[-4], bMax[5:6]))
  }
  #
  if(summaryOutput == TRUE) {
    write.csv(sumOut, file = paste0(plotDir, "plotSummary_v", VERSION, ".csv"), row.names = F)
    cat("Summary saved as: ", paste0(plotDir, "plotSummary_v", VERSION, ".csv"))
  }
  # 
  msyDat <- msyDat[msyDat$ECOREGION == ecoregion,]
  #   
  if(nrow(msyDat) > 0) {
    # 
    if(guildAvg == T){
      overallMSY <- ddply(msyDat, .(METRIC, YEAR), summarize,
                          VALUE = mean(VALUE, na.rm = T),
                          STOCKID = "MEAN")
      
      guildMSY <- ddply(msyDat, .(METRIC, YEAR, GUILD), summarize,
                        VALUE = mean(VALUE, na.rm = T))
      colnames(guildMSY)[colnames(guildMSY) == "GUILD"] <- "STOCKID"
      msyDat <- rbind(overallMSY, guildMSY)
      #
    } else {      
      overallMSY <- ddply(msyDat, .(METRIC, YEAR), summarize,
                          VALUE = mean(VALUE, na.rm = T),
                          STOCKID = "MEAN",
                          GUILD = "NA")
      msyDat <- rbind(overallMSY, msyDat)
    }
    #
    stocks.msy <- data.frame("CODE" = unique(msyDat$STOCKID),
                             "COLOR" = c("grey40", colList[1:length(unique(msyDat$STOCKID)) -1]),
                             "LWD" = c(4, rep(2, length(unique(msyDat$STOCKID)) -1)))
    # 
    stocks.msy <- lapply(stocks.msy, as.character)
    #
    METRICList_MSY <- c("F.Fmsy", "SSB.Btrigger")
    # 
    METRICFactor_MSY <- factor(unique(msyDat$METRIC[!is.na(msyDat$VALUE)]),
                               levels = c("F.Fmsy", "SSB.Btrigger"))
    #
    METRICList_MSY <- METRICList_MSY[METRICList_MSY %in% METRICFactor_MSY]
    #
    xRange_MSY <- range(msyDat$YEAR, na.rm = TRUE)
    # 
    png(filename = paste0(plotDir, ecoregion, "_", guild, "_MSY_v", VERSION, ".png"),
        width = 172.4,
        height = 81.3 * length(METRICList_MSY),
        units = "mm",
        res = 600)
    #
    par(mfrow = c(length(METRICList_MSY), 1),
        mar=c(2.15, 2.25, 0.45, 0.25),
        oma = c(0, 0, 0, 0),
        usr = c(0, 1, 0, 1),
        mgp=c(3, .35, 0),
        tck=-0.01,
        family = "Calibri")
    # k = 1
    for(k in 1:length(METRICList_MSY)) {
      #             met <- levels(metricList_MSY)
      #             if(met[k] == "F") {
      #               par(mfg = c(1,1))
      #             }
      #             if(met[k] == "SSB") {
      #               par(mfg = c(2,1))
      #             }
      ## PLOT F Avg ##
      msyDat.k <- msyDat[msyDat$METRIC == METRICList_MSY[k],]
      # Identify plotting parameters
      yRange.k <- c(0, max(msyDat.k$VALUE, na.rm =T) + max(msyDat.k$VALUE, na.rm = T) * .15)
      #       xRange.k <- c(min(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]),
      #                     max(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]))
      stocks.k <- relevel(factor(unique(msyDat.k$STOCKID), ordered = F),
                          ref = "MEAN")
      stocks.msy.k <- stocks.msy
      #
      if(length(stocks.k) >= 10) {
        #               if(nrow(stocks.msy) >= 10) {
        levels(stocks.msy.k$COLOR) <- c(levels(stocks.msy.k$COLOR), "grey80")
        stocks.msy.k$COLOR[stocks.msy.k$CODE != "MEAN"] <- "grey80"
        #                         }
        stocks.k <- rev(levels(stocks.k))
      }
      if(length(stocks.k) < 10) {
        stocks.k <- levels(stocks.k)
      }
      if(length(stocks.k) <= 2) {
        stocks.k <- stocks.k[stocks.k != "MEAN"]
        msyDat.k <- msyDat.k[msyDat.k$STOCKID %in% stocks.k,]
      }
      #
      lab.k <- METRICList_MSY[k]
      #
      plot(NA,
           type = "l",
           ylim = yRange.k,
           xlim = xRange_MSY,
           yaxt = "n",
           xaxt = "n",
           ann = FALSE)
      abline(h = 1.0, lty = 2, col = "black", lwd = 1)
      #
      for(l in 1:length(stocks.k)) {
        if(all(is.na(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]]))) {
          stocks.k[l] <- NA
          next
        }
        if(!all(is.na(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]]))) {
          d <- data.frame(msyDat.k$VALUE[msyDat.k$STOCKID == stocks.k[l]],
                          msyDat.k$YEAR[msyDat.k$STOCKID == stocks.k[l]])
          d <- d[order(d[,2]),]
          col.d <- as.character(stocks.msy.k$COLOR[stocks.msy.k$CODE == stocks.k[l]])
          lin.d <- as.numeric(as.character(stocks.msy.k$LWD[stocks.msy.k$CODE == stocks.k[l]]))
          lines(d[,2], d[,1], col = col.d, lwd = lin.d)
        }
      }
      axis(1, at = pretty(xRange_MSY), cex.axis = .85)
      #
      mtext("Year", side = 1, line = 1.25, cex= 1)
      #
      axis(2, at = pretty(yRange.k), cex.axis = .75, las = 1)
      #
      if(lab.k == "F.Fmsy") {
        mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex= 1)
      }
      if(lab.k == "SSB.Btrigger") {
        mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= 1)
      }
      # Legend
      if(length(stocks.k) <= 9) {
        legend("topright",
               legend = stocks.msy.k$CODE[stocks.msy.k$CODE %in% stocks.k],
               fill = stocks.msy.k$COLOR[stocks.msy.k$CODE %in% stocks.k],
               bty = "n",
               ncol = 3,
               cex = .85)
      }
      #
      if(length(stocks.k) >= 10) {
        legend("topright",
               legend = "MEAN",
               fill = "grey40",
               bty = "n",
               ncol = 1,
               cex = .85)
      }
      #           if(logo == T) {
      #             logoFun(img, x = 0.5, y = 0.5, size = .5, alpha = alpha)
      #           }
    } # Close l loop
    #       mtext(paste0(ecoregion, ", ", guild), side = 3, outer = T, cex= 1.5, font = 2)
    dev.off()
  } # Close Fmsy and Btrigger "if" statement
} # Close plotMSY function



