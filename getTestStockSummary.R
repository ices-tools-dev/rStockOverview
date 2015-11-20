#' getStockSummary.R

#' ICES standard graph data extraction
#' @param year
#' @return none
#' @seealso none
#' @details none
#' @keywords none
#' @examples \dontrun{
#' tt <- getSummaryTable()
#' }
#' @export
#
#
getTestSummaryTable <- function(year = 2015) {
  # If you want all stocks for all years, then make year == 0
  library(XML)
  library(plyr)
  #
  keys <- data.frame(t(xmlSApply(xmlRoot(xmlTreeParse(paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getListStocks?year=",
                                                             year),
                                                      isURL = T,
                                                      options = HUGE,
                                                      useInternalNodes =  T)),
                                 function(x) xmlSApply(x, xmlValue))), row.names = NULL)
  #
  colnames(keys)[colnames(keys) == "FishStockName"] <- "STOCKID"
  refList <- paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getFishStockReferencePoints?key=",
                    unique(keys$key))
  #
  allRefs <- data.frame()
  for(i in 1:length(refList)) { # Loop over all reference points tables and extract data
    refNames.i <-  xmlRoot(xmlTreeParse(refList[i], isURL = T))
    refDat <- xmlSApply(refNames.i[["FishSettingsList"]], xmlValue)
    refDat[sapply(refDat, function(x) length(x) == 0)] <- NA
    allRefs <- rbind.fill(allRefs, data.frame(t(refDat)))
  } # Close i loop
  #
  # Clean up data
  allRefs <- data.frame(sapply(allRefs, function(x) ifelse(x == "NULL", NA, x)))
  numColsRefs <- colnames(allRefs)[!colnames(allRefs) %in% c("FishStockName")]
  allRefs[, numColsRefs] <- sapply(allRefs[, numColsRefs], function(x) as.numeric(x))
  allRefs[, c("FishStockName")] <- sapply(allRefs[, c("FishStockName")], function(x) as.character(x))
  colnames(allRefs)[colnames(allRefs) == "FishStockName"] <- "STOCKID"
  #
  summaryList <- data.frame(key = keys$key[keys$Status == " Published "],
                            URL = paste0("http://standardgraphs.ices.dk/StandardGraphsWebServices.asmx/getSummaryTable?key=",
                                          keys$key[keys$Status == " Published "]))
  #
  summaryDat <- data.frame()
  for(j in 1:nrow(summaryList)) { # Loop over all published summary tables and extract data
    summaryNames <-  xmlRoot(xmlTreeParse(summaryList$URL[j], isURL = T))
    # Parse XML data and convert into a data frame
    xmlDat <- xmlSApply(summaryNames[["lines"]], function(x) xmlSApply(x, xmlValue))
    xmlDat[sapply(xmlDat, function(x) length(x) == 0)] <- NA
    dimnames(xmlDat)[2] <- NULL
    summaryInfo <- data.frame(lapply(data.frame(t(xmlDat)), function(x) as.numeric(x)))
    #
    stockList <- names(summaryNames[names(summaryNames) != "lines"])
    stockValue <-  rbind(lapply(stockList, function(x) xmlSApply(summaryNames[[x]], xmlValue)))
    stockValue[sapply(stockValue, function(x) length(x) == 0)] <- NA
    dimnames(stockValue)[2] <- NULL
    stockValue <- data.frame(lapply(stockValue, function(x) as.character(x)), stringsAsFactors = F)
    colnames(stockValue) <- stockList
    #
    summaryInfo <- cbind(summaryInfo, stockValue)
    #
    if(any(colnames(summaryDat) %in% colnames(summaryInfo) |
             any(colnames(summaryInfo) %in% colnames(summaryDat)))) {
      newDat <- colnames(summaryInfo)[!colnames(summaryInfo) %in% colnames(summaryDat)]
      summaryDat[,newDat] <- NA
      newInfo <- colnames(summaryDat)[!colnames(summaryDat) %in% colnames(summaryInfo)]
      summaryInfo[,newInfo] <- NA
    }
    summaryInfo$key <- as.numeric(as.character(summaryList$key[j]))
    summaryDat <- rbind(summaryDat, summaryInfo)
  } # close j loop
  #
# tt <- summaryDat
# summaryDat <- tt
  # Clean up data
  charCols <- c("fishstock", "units", "Fage", "stockSizeDescription", 
                "stockSizeUnits", "fishingPressureDescription", "fishingPressureUnits", "StockPublishNote")
  numCols <- colnames(summaryDat)[!colnames(summaryDat) %in% charCols]
  summaryDat[, numCols] <- lapply(summaryDat[, numCols], function(x) as.numeric(x))
  summaryDat[, charCols] <- lapply(summaryDat[, charCols], function(x) as.character(x))
  colnames(summaryDat)[colnames(summaryDat) == "fishstock"] <- "STOCKID"
  #
  # Create new list with all summary tables and reference points
  allRefs$STOCKID <- tolower(as.character(allRefs$STOCKID))
  summaryDat$STOCKID <- tolower(as.character(summaryDat$STOCKID))
  keys$STOCKID <- tolower(as.character(keys$STOCKID))  
  #
  sTable <- merge(allRefs, keys, c("STOCKID", "AssessmentYear", "key"))
  sTable <- merge(summaryDat, sTable, by = c("STOCKID", "AssessmentYear","key"))
  #
  # To facilitate adding guild information
  sTable$speciesID <- tolower(gsub( "-.*$", "", as.character(sTable$STOCKID)))
  #   
  # Remove the extra turbot...
  sTable <- sTable[sTable$SpeciesName != "Psetta maxima (historic name)", ]
  # 
  # modify these stocks to all ecoregions (per ADGECO)
  stockMod <- sTable[sTable$STOCKID %in% c("hom-west", "mac-nea", "whb-comb"),]
  sTable <- sTable[!sTable$STOCKID %in% c("hom-west", "mac-nea", "whb-comb"),]
  stockMod$EcoRegion <- NA
  # 
  tn <- NULL
  td <- unique(as.character(sTable$EcoRegion[!sTable$EcoRegion %in% c("Widely distributed and migratory stocks", NA)]))
  for(i in td){
    stockMod$EcoRegion <- as.character(i)
    sTable <- rbind(sTable, stockMod)
    stockMod$EcoRegion <- NA
  }
  # 
  # Modify this stock to BoB and CS (per ADGECO)
  stockMod <- sTable[sTable$STOCKID %in% c("hke-nrtn"),]
  sTable <- sTable[!sTable$STOCKID %in% c("hke-nrtn"),]
  stockMod$EcoRegion <- NA
  # 
  td <- c("Bay of Biscay and Iberian Sea", "Celtic Sea and West of Scotland")
  for(i in td){
    #   stockMod <- stockMod[!colnames(stockMod) == "EcoRegion"]
    stockMod$EcoRegion <- as.character(i)
    sTable <- rbind(sTable, stockMod)
    stockMod$EcoRegion <- NA
  }
  return(sTable)
  #   newList <- list("summaryTable" = summaryDat, 
  #     "referencePoints" = allRefs, "keys" = keys)
  #
#   return(newList)
} # Close function
