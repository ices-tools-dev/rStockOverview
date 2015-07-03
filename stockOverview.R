rm(list = ls())
################
library(rICES)
library(plyr)
library(reshape2)
#
# Load data on ecosystem, books, guilds, etc.
load("allDataPlotOverview_v001.rdat")
stockInfo <- allDat[,c("ICES.Book", "Species", "Area", 
                       "Stock.code", "Type")]
stockInfo <- stockInfo[!duplicated(stockInfo),]
#
stockDF <- getSummaryTable(year = 2015)
#
# Merge stockDF tables with stockInfo
stockTable <- merge(stockDF$referencePoints, stockDF$keys, by = c("FishStockName", "AssessmentYear", "key"))
stockTable <- merge(stockDF$summaryTable, 
                    stockTable, 
                    by.x = c("fishstock", "AssessmentYear"),
                    by.y = c("FishStockName", "AssessmentYear"))
stockTable <- merge(stockInfo, stockTable, by.x = "Stock.code", by.y = "fishstock", all = T)
#
# Calculate F/Faverage and SSB/SSBaverage
stockTable <- ddply(stockTable, .(AssessmentYear, Stock.code), mutate,
                    Faverage = mean(F, na.rm = T),
                    SSBaverage = mean(SSB, na.rm = T))
#
stockTable <- ddply(stockTable, .(Stock.code), mutate,
                    FFavg = F/Faverage,
                    SSBSSBavg = SSB/SSBaverage,
                    FFmsy = F/FMSY,
                    SSBMSYBtrig = SSB/MSYBtrigger)
#
stockTable$ECOREGION <- ifelse(is.na(stockTable$EcoRegion), stockTable$ICES.Book, stockTable$EcoRegion)
stockTable$speciesID <- gsub( "-.*$", "", as.character(stockTable$Stock.code))
# Give the same guild to species that are already defined
speciesGuild <- ddply(stockTable, .(Species), summarize,
                      speciesGuild = unique(speciesID))

tm <- stockTable[is.na(stockTable$Type),]
#
df <- melt(stockTable, 
           id.vars = c("AssessmentYear", "ECOREGION", "Type", "Stock.code", "Year"),
           measure.vars = c("FFavg", "SSBSSBavg", "FFmsy", "SSBMSYBtrig"),
           variable.name = "VARIABLE",
           value.name = "VALUE")
#
colnames(df) <- c("ASSESSMENTYEAR", "ECOREGION", "GUILD", "STOCKID", "YEAR", "VARIABLE", "VALUE")
#


tr <- df[!apply(df, 1, function(x) all(is.na(x))),]
# badsMSYBtrigger <- unique(stockTable$Stock.code[stockTable$MSYBtrigger == 0 ])
# badsFMSY <- unique(stockTable$Stock.code[stockTable$FMSY == 0 ])
#
fMetric <- c("F", "HR", "F/Fmsy", "F/average")
bMetric <- c("SSB", "B/Bmsy", "B/Btrigger", "SSB/average", "TV abund", "B")
#
Book <- "Bay of Biscay and Iberian Coast"
Guild <- "Pelagic"
All.Stocks <- T
All.Books <- T
#
#
#
tt <- df[df$ECOREGION == "Greater North Sea" &
         df$GUILD == "Demersal",]

#F/Faverage SSB/SSBaverage
plotAVG <- df[df$ECOREGION == "Greater North Sea" &
              df$GUILD == "Demersal" &
              df$VARIABLE %in% c("FFavg", "SSBSSBavg"),]
#
td <- unique(df$STOCKID[is.na(df$GUILD)])


unique(plotAVG$STOCKID)
#
plotAVG.F <- plotAVG[plotAVG$VARIABLE == "FFavg",]
plotAVG.SSB <- plotAVG[plotAVG$VARIABLE == "SSBSSBavg",]

  



plotFun <- function(Guild, Book, All.Stocks = F, All.Books = F) {
  #
  if(All.Stocks == T) {
    Guild <- "All stocks"
    dat <- allDat[allDat$ICES.Book == Book,]
  }
  if(All.Stocks == F) {
    dat <- allDat[allDat$ICES.Book == Book &
                    allDat$Type %in% Guild,]
    if(nrow(dat) == 0) {
      message("Data for ", Guild, " in ", Book, "does not exist.")
    }
  }
  if(All.Books == T) {
    Guild <- "All stocks"
    Book <- "All Ecoregions"
    dat <- allDat
    message("Plotting all guilds and books.")
  }
  
  if(nrow(dat) > 0) {
    dat$METRIC[dat$Variable %in% fMetric] <- "F"
    dat$METRIC[dat$Variable %in% bMetric] <- "SSB"
    #
    # Calculate mean
    #     anb-8c9a  and pan-sknd- Note that for these two stocks to calculate
    # the plot of F/Fmsy no additional division should be made but to calculate
    # the SSB/MSY Btrigger you should divide by 0.5 (because MSY Btrigger is 0.5 of BMSY).
    
    datMean <- ddply(dat, .(Stock.code, METRIC), mutate,
                     meanVal = VALUE/ mean(VALUE, na.rm = T))
    #
    plotDat <- data.frame(YEAR = datMean$YEAR,
                          Stock.code = datMean$Stock.code,
                          meanVal = datMean$meanVal,
                          Metric = datMean$METRIC)
    # Overall mean
    overallMean <- ddply(datMean, .(YEAR, METRIC), summarize,
                         overallMean = mean(meanVal, na.rm = T))
    plotDat <- rbind(data.frame(YEAR = overallMean$YEAR,
                                Stock.code = "MEAN",
                                meanVal = overallMean$overallMean,
                                Metric = overallMean$METRIC),
                     plotDat)
    # PLOT AVGs
    png(filename = paste0(plotDir, Book,"_", Guild, "_AVG_v005.png"),
        width = 172.4,
        height = 162.6,
        units = "mm",
        res = 600)
    #
    # dev.off()
    par(mfrow = c(2,1),
        mar=c(2.15, 2.25, 0.45, 0.25),
        oma = c(0, 0, 1.25, 0),
        usr = c(0,1,0,1),
        mgp=c(3, .35, 0),
        tck=-0.01,
        family = "Calibri")
    #
    
    #     metricList_AVG <- lapply(factor(unique(plotDat$Metric[!is.na(plotDat$meanVal)]),
    #                              levels = c("F", "SSB")), as.character)
    #     stocks.avg <- lapply(stocks.avg, as.character)
    stocks.avg <- data.frame("CODE" = unique(plotDat$Stock.code),
                             "COLOR" = c("grey40", colList[1:length(unique(plotDat$Stock.code)) -1]),
                             "LWD" = c(4, rep(2, length(unique(plotDat$Stock.code)) -1)))
    
    metricList_AVG <- c("F", "SSB")
    metricFactor_AVG <- factor(unique(plotDat$Metric[!is.na(plotDat$meanVal)]),
                               levels = c("F", "SSB"))
    
    metricList_AVG <- metricList_AVG[metricList_AVG %in% metricFactor_AVG]
    stocks.avg <- lapply(stocks.avg, as.character)
    #
    # i = 1
    # met <- NULL
    for(i in 1:length(metricList_AVG)) {
      #         met <- metricList_AVG
      #         if(met[i] == "F") {
      #           par(mfg = c(1,1))
      #           }
      #         if(met[i] == "SSB") {
      #           par(mfg = c(2,1))
      #         }
      
      ## PLOT F Avg ##
      plotDat.i <- plotDat[plotDat$Metric == metricList_AVG[i],]
      plotDat.i <- plotDat.i[sort(order(plotDat.i$Stock.code == "MEAN")),]
      stocks.avg.i <- stocks.avg
      
      
      # Identify plotting parameters
      yRange.i <- c(0, max(plotDat.i$meanVal, na.rm =T) + max(plotDat.i$meanVal, na.rm = T) * .15)
      xRange.i <- c(min(plotDat.i$YEAR[!is.na(plotDat.i$meanVal)]),
                    max(plotDat.i$YEAR[!is.na(plotDat.i$meanVal)]))
      stocks.i <- factor(unique(plotDat.i$Stock.code), ordered = T)
      if(length(stocks.i) >= 10) {
        levels(stocks.avg.i$COLOR) <- c(levels(stocks.avg.i$COLOR), "grey80")
        stocks.avg.i$COLOR[stocks.avg.i$CODE != "MEAN"] <- "grey80"
        #         }
        #         if(length(stocks.i) >= 10) {
        stocks.i <- rev(levels(stocks.i))
      }
      if(length(stocks.i) < 10) {
        stocks.i <- levels(stocks.i)
      }
      if(length(stocks.i) <= 2) {
        stocks.i <- stocks.i[stocks.i != "MEAN"]
        plotDat.i <- plotDat.i[plotDat.i$Stock.code %in% stocks.i,]
      }
      lab.i <- metricList_AVG[i]
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
      for(j in 1:length(stocks.i)) {
        if(all(is.na(plotDat.i$meanVal[plotDat.i$Stock.code == stocks.i[j]]))) {
          stocks.i[j] <- NA
          next
        }
        if(!all(is.na(plotDat.i$meanVal[plotDat.i$Stock.code == stocks.i[j]]))) {
          d <- data.frame(plotDat.i$meanVal[plotDat.i$Stock.code == stocks.i[j]],
                          plotDat.i$YEAR[plotDat.i$Stock.code == stocks.i[j]])
          col.d <- as.character(stocks.avg.i$COLOR[stocks.avg.i$CODE == stocks.i[j]])
          lin.d <- as.numeric(as.character(stocks.avg.i$LWD[stocks.avg.i$CODE == stocks.i[j]]))
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
    FmsyDat <- dat[dat$Variable %in% fMetric &
                     dat$Fmsy != 0 ,]
    FmsyDat <- FmsyDat[!apply(FmsyDat, 1, function(x){all(is.na(x))}),]
    BmsyDat <- dat[dat$Variable %in% bMetric &
                     dat$Btrigger != 0,]
    BmsyDat <-  BmsyDat[!apply(BmsyDat, 1, function(x){all(is.na(x))}),]
    #
    if(nrow(BmsyDat) > 0 & nrow(FmsyDat) > 0) {
      FmsyDat <- ddply(FmsyDat, .(Stock.code, METRIC), mutate,
                       F.Fmsy = VALUE/ Fmsy)
      BmsyDat <- ddply(BmsyDat, .(Stock.code, METRIC), mutate,
                       SSB.Btrigger = VALUE/ Btrigger)
      Bmsy <- data.frame(YEAR = BmsyDat$YEAR,
                         Stock.code = BmsyDat$Stock.code,
                         VALUE = BmsyDat$SSB.Btrigger,
                         Metric = BmsyDat$METRIC)
      Fmsy <- data.frame(YEAR = FmsyDat$YEAR,
                         Stock.code = FmsyDat$Stock.code,
                         VALUE = FmsyDat$F.Fmsy,
                         Metric = FmsyDat$METRIC)
      msyDat <- rbind(Fmsy, Bmsy)
      #
      overallMSY <- ddply(msyDat, .(YEAR, Metric), summarize,
                          overallMean = mean(VALUE, na.rm = T))
      #
      msyDat <- rbind(data.frame(YEAR = overallMSY$YEAR,
                                 Stock.code = "MEAN",
                                 VALUE = overallMSY$overallMean,
                                 Metric = overallMSY$Metric),
                      msyDat)
      #
      stocks.msy <- data.frame("CODE" = unique(msyDat$Stock.code),
                               "COLOR" = c("grey40", colList[1:length(unique(msyDat$Stock.code)) -1]),
                               "LWD" = c(4, rep(2, length(unique(msyDat$Stock.code)) -1)))
      #           if(nrow(stocks.msy) >= 10) {
      #             levels(stocks.msy$COLOR) <- c(levels(stocks.msy$COLOR), "grey80")
      #             stocks.msy$COLOR[stocks.msy$CODE != "MEAN"] <- "grey80"
      #           }
      stocks.msy <- lapply(stocks.msy, as.character)
      #
      png(filename = paste0(plotDir, Book, "_", Guild, "_MSY_v005.png"),
          width = 172.4,
          height = 162.6,
          units = "mm",
          res = 600)
      #
      par(mfrow = c(2,1),
          mar=c(2.15, 2.25, 0.45, 0.25),
          oma = c(0, 0, 1.25, 0),
          usr = c(0, 1, 0, 1),
          mgp=c(3, .35, 0),
          tck=-0.01,
          family = "Calibri")
      #
      metricList_MSY <- c("F", "SSB")
      metricFactor_MSY <- factor(unique(msyDat$Metric[!is.na(msyDat$VALUE)]),
                                 levels = c("F", "SSB"))
      
      metricList_MSY <- metricList_MSY[metricList_MSY %in% metricFactor_MSY]
      # k = 1
      #
      for(k in 1:length(metricList_MSY)) {
        #             met <- levels(metricList_MSY)
        #             if(met[k] == "F") {
        #               par(mfg = c(1,1))
        #             }
        #             if(met[k] == "SSB") {
        #               par(mfg = c(2,1))
        #             }
        ## PLOT F Avg ##
        msyDat.k <- msyDat[msyDat$Metric == metricList_MSY[k],]
        # Identify plotting parameters
        yRange.k <- c(0, max(msyDat.k$VALUE, na.rm =T) + max(msyDat.k$VALUE, na.rm = T) * .15)
        xRange.k <- c(min(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]),
                      max(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]))
        stocks.k <- factor(unique(msyDat.k$Stock.code), ordered = T)
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
          msyDat.k <- msyDat.k[msyDat.k$Stock.code %in% stocks.k,]
        }
        #
        lab.k <- metricList_MSY[k]
        #
        plot(NA,
             type = "l",
             ylim = yRange.k,
             xlim = xRange.k,
             yaxt = "n",
             xaxt = "n",
             ann = FALSE)
        abline(h = 1.0, lty = 2, col = "black", lwd = 1)
        #
        for(l in 1:length(stocks.k)) {
          if(all(is.na(msyDat.k$VALUE[msyDat.k$Stock.code == stocks.k[l]]))) {
            stocks.k[l] <- NA
            next
          }
          if(!all(is.na(msyDat.k$VALUE[msyDat.k$Stock.code == stocks.k[l]]))) {
            d <- data.frame(msyDat.k$VALUE[msyDat.k$Stock.code == stocks.k[l]],
                            msyDat.k$YEAR[msyDat.k$Stock.code == stocks.k[l]])
            col.d <- as.character(stocks.msy.k$COLOR[stocks.msy.k$CODE == stocks.k[l]])
            lin.d <- as.numeric(as.character(stocks.msy.k$LWD[stocks.msy.k$CODE == stocks.k[l]]))
            lines(d[,2], d[,1], col = col.d, lwd = lin.d)
          }
        }
        axis(1, at = pretty(xRange.k), cex.axis = .85)
        #
        mtext("Year", side = 1, line = 1.25, cex= 1)
        #
        axis(2, at = pretty(yRange.k), cex.axis = .75, las = 1)
        #
        if(lab.k == "F") {
          mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex= 1)
        }
        if(lab.k == "SSB") {
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
      mtext(paste0(Book, ", ", Guild), side = 3, outer = T, cex= 1.5, font = 2)
      dev.off()
    } # Close Fmsy and Btrigger "if" statement
  } # Close no guild data "if" statement
  #
} # Close plotFun function



books <- unique(allDat$ICES.Book)
books <- "Celtic Seas"
guilds = unique(allDat$Type)
# guilds <- guilds[i]
for(i in 1:length(books)) {
  guilds <- unique(allDat$Type[allDat$ICES.Book == books[i]])
  sapply(guilds, plotFun, Book = books[i], All.Stocks = T)
}

plotFun(Guild = "Any", Book = "Any", All.Stocks = T, All.Books = T)

