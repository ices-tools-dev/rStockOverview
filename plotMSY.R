
plotFun <- function(guild, ecoregion, all.stock = F, all.ecoregion = F, all.guild = F) {
  #
  if(all.stock == T) {
    guild <- "All stocks"
    dat <- df[df$ECOREGION == ecoregion,]
  }
  if(all.stock == F) {
    dat <- df[df$ECOREGION == ecoregion &
                df$GUILD %in% guild,]
    if(nrow(dat) == 0) {
      message("Data for ", guild, " in ", ecoregion, " does not exist.")
    }
  }
  if(all.ecoregion == T) {
    guild <- "All stocks"
    ecoregion <- "All Ecoregions"
    dat <- df
    message("Plotting all guilds and ecoregions.")
  }
  if(all.guild == T) {
    dat <- df[df$GUILD == guild,]
    ecoregion <- "All Ecoregions"
    message("Plotting all guilds.")
  }
  if(guildAvg == T) {
    dat <- df[df$ECOREGION == ecoregion,]
  }
  
  if(nrow(dat) > 0) {
    #     dat$METRIC[dat$Variable %in% fMetric] <- "F"
    #     dat$METRIC[dat$Variable %in% bMetric] <- "SSB"
    #
    # Calculate mean
    #     anb-8c9a  and pan-sknd- Note that for these two stocks to calculate
    # the plot of F/Fmsy no additional division should be made but to calculate
    # the SSB/MSY Btrigger you should divide by 0.5 (because MSY Btrigger is 0.5 of BMSY).
    
    # Specify the ECOREGION, GUILD, 
    #     dat <- df[df$ECOREGION == "North Sea"  &
    #                 df$GUILD == "Demersal",]
    #
    #     mean(dat$VALUE[dat$STOCKID == "nep-3-4" & dat$METRIC == "FMSY"])
#     datMean <- ddply(dat, .(METRIC, GUILD), mutate,
#                      meanVal = VALUE/ mean(VALUE, na.rm = T))
#     #
#     unique(datMean$METRIC)
#     plotDat <- data.frame(YEAR = datMean$YEAR,
#                           STOCKID = datMean$STOCKID,
#                           meanVal = datMean$meanVal,
#                           METRIC = datMean$METRIC)
#     # Overall mean
#     overallMean <- ddply(datMean, .(METRIC, YEAR), summarize,
#                          overallMean = mean(meanVal, na.rm = T))
#     # plot(overallMean$overallMean[overallMean$VARIABLE == "FMSY"], type = "b")
#     plotDat <- rbind(data.frame(YEAR = overallMean$YEAR,
#                                 STOCKID = "MEAN",
#                                 meanVal = overallMean$overallMean,
#                                 METRIC = overallMean$METRIC),
#                      plotDat)
#     
#     #     datMean <- ddply(dat, .(Stock.code, METRIC), mutate,
#     #                      meanVal = VALUE/ mean(VALUE, na.rm = T))
#     #     #
#     #     plotDat <- data.frame(YEAR = datMean$YEAR,
#     #                           Stock.code = datMean$Stock.code,
#     #                           meanVal = datMean$meanVal,
#     #                           Metric = datMean$METRIC)
#     #     # Overall mean
#     #     overallMean <- ddply(datMean, .(YEAR, METRIC), summarize,
#     #                          overallMean = mean(meanVal, na.rm = T))
#     #     plotDat <- rbind(data.frame(YEAR = overallMean$YEAR,
#     #                                 Stock.code = "MEAN",
#     #                                 meanVal = overallMean$overallMean,
#     #                                 Metric = overallMean$METRIC),
#     #                      plotDat)
#     #
#     stocks.avg <- data.frame("CODE" = unique(plotDat$STOCKID),
#                              "COLOR" = c("grey40", colList[1:length(unique(plotDat$STOCKID)) - 1]),
#                              "LWD" = c(4, rep(2, length(unique(plotDat$STOCKID)) - 1)))
#     stocks.avg <- lapply(stocks.avg, as.character)
#     #
#     METRICList_AVG <- c("F", "SSB")
#     METRICFactor_AVG <- factor(unique(plotDat$METRIC[!is.na(plotDat$meanVal)]),
#                                levels = c("F", "SSB"))
#     METRICList_AVG <- METRICList_AVG[METRICList_AVG %in% METRICFactor_AVG]
#     #
#     # PLOT AVGs
#     png(filename = paste0(plotDir, ecoregion,"_", guild, "_AVG_v03.png"),
#         width = 172.4,
#         height = 81.3 * length(METRICList_AVG),
#         units = "mm",
#         res = 600)
#     #
#     par(mfrow = c(length(METRICList_AVG), 1),
#         mar=c(2.15, 2.25, 0.45, 0.25),
#         oma = c(0, 0, 1.25, 0),
#         usr = c(0,1,0,1),
#         mgp=c(3, .35, 0),
#         tck=-0.01,
#         family = "Calibri")
#     #
#     for(i in 1:length(METRICList_AVG)) {
#       ## PLOT F Avg ##
#       plotDat.i <- plotDat[plotDat$METRIC == METRICList_AVG[i],]
#       plotDat.i <- plotDat.i[sort(order(plotDat.i$STOCKID == "MEAN")),]
#       stocks.avg.i <- stocks.avg
#       # Identify plotting parameters
#       yRange.i <- c(0, max(plotDat.i$meanVal, na.rm =T) + max(plotDat.i$meanVal, na.rm = T) * .15)
#       xRange.i <- c(min(plotDat.i$YEAR[!is.na(plotDat.i$meanVal)]),
#                     max(plotDat.i$YEAR[!is.na(plotDat.i$meanVal)]))
#       stocks.i <- factor(unique(plotDat.i$STOCKID), ordered = T)
#       #
#       if(length(stocks.i) >= 10) {
#         levels(stocks.avg.i$COLOR) <- c(levels(stocks.avg.i$COLOR), "grey80")
#         stocks.avg.i$COLOR[stocks.avg.i$CODE != "MEAN"] <- "grey80"
#         stocks.i <- rev(levels(stocks.i))
#       }
#       #
#       if(length(stocks.i) < 10) {
#         stocks.i <- levels(stocks.i)
#       }
#       #
#       if(length(stocks.i) <= 2) {
#         stocks.i <- stocks.i[stocks.i != "MEAN"]
#         plotDat.i <- plotDat.i[plotDat.i$STOCKID %in% stocks.i,]
#       }
#       #
#       lab.i <- METRICList_AVG[i]
#       #
#       plot(NA,
#            type = "l",
#            ylim = yRange.i,
#            xlim = xRange.i,
#            yaxt = "n",
#            xaxt = "n",
#            ann = FALSE)
#       abline(h = 1.0, lty = 2, col = "black", lwd = 1)
#       #
#       for(j in 1:length(stocks.i)) {
#         if(all(is.na(plotDat.i$meanVal[plotDat.i$STOCKID == stocks.i[j]]))) {
#           stocks.i[j] <- NA
#           next
#         }
#         if(!all(is.na(plotDat.i$meanVal[plotDat.i$STOCKID == stocks.i[j]]))) {
#           d <- data.frame(plotDat.i$meanVal[plotDat.i$STOCKID == stocks.i[j]],
#                           plotDat.i$YEAR[plotDat.i$STOCKID == stocks.i[j]])
#           d <- d[order(d[,2]),]
#           col.d <- as.character(stocks.avg.i$COLOR[stocks.avg.i$CODE == stocks.i[j]])
#           lin.d <- as.numeric(as.character(stocks.avg.i$LWD[stocks.avg.i$CODE == stocks.i[j]]))
#           lines(d[,2], d[,1], col = col.d, lwd = lin.d)
#         }
#       } # close j loop
#       #
#       axis(1, at = pretty(xRange.i), cex.axis = .85)
#       mtext("Year", side = 1, line = 1.25, cex= 1)
#       axis(2, at = pretty(yRange.i), cex.axis = .75, las = 1)
#       #
#       if(lab.i == "F") {
#         mtext(expression("F/F"["average"]), side = 2, line = 1, cex= 1)
#       }
#       if(lab.i == "SSB") {
#         mtext(expression("SSB/SSB"["average"]), side = 2, line = 1, cex= 1)
#       }
#       # Legend
#       if(length(stocks.i) <= 9) {
#         legend("topright",
#                legend = stocks.avg.i$CODE[stocks.avg.i$CODE %in% stocks.i],
#                fill = stocks.avg.i$COLOR[stocks.avg.i$CODE %in% stocks.i],
#                bty="n",
#                ncol = 3,
#                cex = .85)
#       }
#       if(length(stocks.i) >= 10) {
#         legend("topright",
#                legend = "MEAN",
#                fill = "grey40",
#                bty="n",
#                ncol = 1,
#                cex = .85)
#       }
#     } # Close average loop
#     mtext(paste0(ecoregion, ", ", guild), side = 3, outer = T, cex= 1.5, font = 2)
#     dev.off()
}

unique(df$ECOREGION)
ecoregion = "Celtic Sea and West of Scotland"
guildAvg = F
guild = "All guilds"

# sapply(unique(df$ECOREGION), plotMSY, dat = df)
# #
# 
# summaryOut <- ddply(df, .(ECOREGION, GUILD), summarize,
#                     out = unique(STOCKID))
# 
# write.csv(summaryOut, file= paste0(plotDir, "ADGEco_plotSummary.csv"))

plotMSY <- function(dat, ecoregion) {
    ############
    # PLOT msy #
    ############
    guild = "All guilds"
    dat <- df[df$ECOREGION == ecoregion,]
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
    msyDat <- msyDat[,c("YEAR", "STOCKID", "GUILD", "VALUE", "METRIC")]

    if(nrow(msyDat) > 0) {

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
      
      METRICFactor_MSY <- factor(unique(msyDat$METRIC[!is.na(msyDat$VALUE)]),
                                 levels = c("F.Fmsy", "SSB.Btrigger"))
      #
      METRICList_MSY <- METRICList_MSY[METRICList_MSY %in% METRICFactor_MSY]
      #
      png(filename = paste0(plotDir, ecoregion, "_", guild, "_MSY_v04.png"),
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
#       k = 1
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
        xRange.k <- c(min(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]),
                      max(msyDat.k$YEAR[!is.na(msyDat.k$VALUE)]))
        stocks.k <- factor(unique(msyDat.k$STOCKID), ordered = T)
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
             xlim = xRange.k,
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
        axis(1, at = pretty(xRange.k), cex.axis = .85)
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

# Close no guild data "if" statement
  #
# } 
