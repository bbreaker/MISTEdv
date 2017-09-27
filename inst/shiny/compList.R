output$compList <- DT::renderDataTable ({

  siteList <- whatNWISsites (bbox = c(input$long1,input$lat1,input$long2,input$lat2), siteTypeCd = "ST", dataTypeCd = "dv")

  datList <- whatNWISdata(siteNumbers = siteList$site_no, service = "dv", parameterCd = "00060", statCd = "00003")

  siteInfo <- readNWISsite(datList$site_no)

  siteInfo <- siteInfo[,c(2,30)]

  datList <- datList[,c(2,3,4,5,6,7,17,22,23)]

  newList <- merge(x = datList, y = siteInfo, by = "site_no", all = TRUE)

  newList <- subset(newList, begin_date <= as.character(Sys.Date() - 1095) & end_date >= as.character(Sys.Date() - 372))

  newList <- subset(newList, is.na(loc_web_ds))

  newList <- subset(newList, site_no != input$ID3)

  complist <- list()

  for (i in newList$site_no) {

    dummyDF <- data.frame(Date = seq(Sys.Date() - 730, Sys.Date() - 365, 1),
                          agency_cd.x = "NoUSGS",
                          site_no.x = "dummy",
                          X_00060_00003.x = rnorm(366),
                          X_00060_00003_cd.x = "D",
                          agency_cd.y = "noUSGS",
                          site_no.y = "dummy",
                          X_00060_00003.y = rnorm(366),
                          X_00060_00003_cd.y = "D")

    name <- paste0("corr_", i)

    print(i)

    datInd <- readNWISdv(site = i,
                         startDate = as.character(Sys.Date() - 730),
                         endDate = as.character(Sys.Date() - 365),
                         parameterCd = "00060", statCd = "00003")

    datRef <- readNWISdv(site = input$ID3,
                         startDate = as.character(Sys.Date() - 730),
                         endDate = as.character(Sys.Date() - 365),
                         parameterCd = "00060", statCd = "00003")

    info <- subset(siteInfo, site_no == i)

    drnArea <- info[,2]

    if (nrow(datInd) >= 365) {

      datAll <- merge(x = datInd, y = datRef, by = "Date", all = TRUE)}

    else if (nrow(datInd) < 365) {

      datAll <- dummyDF

    }
    newCor <- c(site_no = i, drainageArea = drnArea,
                min5Day = round(cor((lag(datAll$X_00060_00003.x, 5))[6:366], datAll$X_00060_00003.y[6:366], method = "pearson"), 2),
                min4Day = round(cor((lag(datAll$X_00060_00003.x, 4))[5:366], datAll$X_00060_00003.y[5:366], method = "pearson"), 2),
                min3Day = round(cor((lag(datAll$X_00060_00003.x, 3))[4:366], datAll$X_00060_00003.y[4:366], method = "pearson"), 2),
                min2Day = round(cor((lag(datAll$X_00060_00003.x, 2))[3:366], datAll$X_00060_00003.y[3:366], method = "pearson"), 2),
                min1Day = round(cor((lag(datAll$X_00060_00003.x, 1))[2:366], datAll$X_00060_00003.y[2:366], method = "pearson"), 2),
                noLag = round(cor(datAll$X_00060_00003.x, datAll$X_00060_00003.y, method = "pearson"), 2),
                plus1Day = round(cor((lead(datAll$X_00060_00003.x, 1))[1:365], datAll$X_00060_00003.y[1:365], method = "pearson"), 2),
                plus2Day = round(cor((lead(datAll$X_00060_00003.x, 2))[1:364], datAll$X_00060_00003.y[1:364], method = "pearson"), 2),
                plus3Day = round(cor((lead(datAll$X_00060_00003.x, 3))[1:363], datAll$X_00060_00003.y[1:363], method = "pearson"), 2),
                plus4Day = round(cor((lead(datAll$X_00060_00003.x, 4))[1:362], datAll$X_00060_00003.y[1:362], method = "pearson"), 2),
                plus5Day = round(cor((lead(datAll$X_00060_00003.x, 5))[1:361], datAll$X_00060_00003.y[1:361], method = "pearson"), 2))

    complist[[name]] <- newCor

  }

  compDF <- as.data.frame(complist)

  compDF <- t(compDF)

  compDF <- data.frame(compDF)

  unfactorize <- colnames(compDF[,2:13])

  compDF[,unfactorize] <- lapply(unfactorize, function(x) as.numeric(as.character(compDF[,x])))

  compDF <- na.omit(compDF)

  compNew <- compDF[,c(3:13)]

  bestLag <- colnames(compNew)[apply(compNew,1,which.max)]

  bestCor <- apply(compNew, 1, function(x) max(x, na.rm = TRUE))

  newDF <- data.frame(siteNo = compDF$site_no, drainageArea = compDF$drainageArea, bestCor = bestCor, Lag = bestLag)

  newDF <- newDF[with(newDF, order(-bestCor)), ]

  newDF <- newDF[c(1:20),]

  DT::datatable(newDF, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)

})
