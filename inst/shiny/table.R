output$table <- DT::renderDataTable ({

dat <- getDat()

datP <- getDatP()

if (input$use2 == FALSE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datP <- cbind(datP, datPred)

        if (input$smooth == TRUE) {

          startSm <- input$dates4[1] - as.difftime(1, units = "days")

          endSm <- input$dates4[2] + as.difftime(1, units = "days")

          smPeriod <- subset(datP, Date >= startSm & Date <= endSm)

          allResids <- smPeriod$X_00060_00003.y - smPeriod$Estimated

          leftResid <- allResids[1]

          rightResid <- allResids[(length(allResids))]

          diffDates <- as.numeric(endSm) - as.numeric(startSm)

          slopeResid <- (rightResid - leftResid) / diffDates

          intercept <- leftResid

          smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$Date) - as.numeric(startSm))

          smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid

          smPeriod <- data.frame(Date = smPeriod$Date, Smoothed = smPeriod$Smoothed, adjResid = smPeriod$adjResid)

          if (smPeriod$Smoothed < 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 2)
          }

          else if (smPeriod$Smoothed >= 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 3)
          }

          datP <- merge(x = datP, y = smPeriod, by = "Date", all = TRUE)

        }

        if (datP$Estimated < 10) {
          datP$Estimated <- signif(datP$Estimated, 2)
        }

        else if (datP$Estimated >= 10) {
          datP$Estimated <- signif(datP$Estimated, 3)
        }

        if (datP$fitUpper < 10) {
          datP$fitUpper <- signif(datP$fitUpper, 2)
        }

        else if (datP$fitUpper >= 10) {
          datP$fitUpper <- signif(datP$fitUpper, 3)
        }

        if (datP$fitLower < 10) {
          datP$fitLower <- signif(datP$fitLower, 2)
        }

        else if (datP$fitLower >= 10) {
          datP$fitLower <- signif(datP$fitLower, 3)
        }

        datP$standardError <- signif(datP$standardError, 3)

        DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs"), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datP <- cbind(datP, datPred)

        if (input$smooth == TRUE) {

          startSm <- input$dates4[1] - as.difftime(1, units = "days")

          endSm <- input$dates4[2] + as.difftime(1, units = "days")

          smPeriod <- subset(datP, Date >= startSm & Date <= endSm)

          allResids <- smPeriod$X_00060_00003.y - smPeriod$Estimated

          leftResid <- allResids[1]

          rightResid <- allResids[(length(allResids))]

          diffDates <- as.numeric(endSm) - as.numeric(startSm)

          slopeResid <- (rightResid - leftResid) / diffDates

          intercept <- leftResid

          smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$Date) - as.numeric(startSm))

          smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid

          smPeriod <- data.frame(Date = smPeriod$Date, Smoothed = smPeriod$Smoothed, adjResid = smPeriod$adjResid)

          if (smPeriod$Smoothed < 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 2)
          }

          else if (smPeriod$Smoothed >= 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 3)
          }

          datP <- merge(x = datP, y = smPeriod, by = "Date", all = TRUE)

        }

        if (datP$Estimated < 10) {
          datP$Estimated <- signif(datP$Estimated, 2)
        }

        else if (datP$Estimated >= 10) {
          datP$Estimated <- signif(datP$Estimated, 3)
        }

        if (datP$fitUpper < 10) {
          datP$fitUpper <- signif(datP$fitUpper, 2)
        }

        else if (datP$fitUpper >= 10) {
          datP$fitUpper <- signif(datP$fitUpper, 3)
        }

        if (datP$fitLower < 10) {
          datP$fitLower <- signif(datP$fitLower, 2)
        }

        else if (datP$fitLower >= 10) {
          datP$fitLower <- signif(datP$fitLower, 3)
        }

        datP$standardError <- signif(datP$standardError, 3)

        DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)

      }

    }

  else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2), data = datP)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2) + fourier(Date), data = datP)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datP <- cbind(datP, datPred)

        if (input$smooth == TRUE) {

          startSm <- input$dates4[1] - as.difftime(1, units = "days")

          endSm <- input$dates4[2] + as.difftime(1, units = "days")

          smPeriod <- subset(datP, Date >= startSm & Date <= endSm)

          allResids <- smPeriod$X_00060_00003.y - smPeriod$Estimated

          leftResid <- allResids[1]

          rightResid <- allResids[(length(allResids))]

          diffDates <- as.numeric(endSm) - as.numeric(startSm)

          slopeResid <- (rightResid - leftResid) / diffDates

          intercept <- leftResid

          smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$Date) - as.numeric(startSm))

          smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid

          smPeriod <- data.frame(Date = smPeriod$Date, Smoothed = smPeriod$Smoothed, adjResid = smPeriod$adjResid)

          if (smPeriod$Smoothed < 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 2)
          }

          else if (smPeriod$Smoothed >= 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 3)
          }

          datP <- merge(x = datP, y = smPeriod, by = "Date", all = TRUE)

        }

        if (datP$Estimated < 10) {
          datP$Estimated <- signif(datP$Estimated, 2)
        }

        else if (datP$Estimated >= 10) {
          datP$Estimated <- signif(datP$Estimated, 3)
        }

        if (datP$fitUpper < 10) {
          datP$fitUpper <- signif(datP$fitUpper, 2)
        }

        else if (datP$fitUpper >= 10) {
          datP$fitUpper <- signif(datP$fitUpper, 3)
        }

        if (datP$fitLower < 10) {
          datP$fitLower <- signif(datP$fitLower, 2)
        }

        else if (datP$fitLower >= 10) {
          datP$fitLower <- signif(datP$fitLower, 3)
        }

        datP$standardError <- signif(datP$standardError, 3)

        DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs"), data = datP)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs") + fourier(Date), data = datP)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datP <- cbind(datP, datPred)

        if (input$smooth == TRUE) {

          startSm <- input$dates4[1] - as.difftime(1, units = "days")

          endSm <- input$dates4[2] + as.difftime(1, units = "days")

          smPeriod <- subset(datP, Date >= startSm & Date <= endSm)

          allResids <- smPeriod$X_00060_00003.y - smPeriod$Estimated

          leftResid <- allResids[1]

          rightResid <- allResids[(length(allResids))]

          diffDates <- as.numeric(endSm) - as.numeric(startSm)

          slopeResid <- (rightResid - leftResid) / diffDates

          intercept <- leftResid

          smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$Date) - as.numeric(startSm))

          smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid

          smPeriod <- data.frame(Date = smPeriod$Date, Smoothed = smPeriod$Smoothed, adjResid = smPeriod$adjResid)

          if (smPeriod$Smoothed < 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 2)
          }

          else if (smPeriod$Smoothed >= 10) {
            smPeriod$Smoothed <- signif(smPeriod$Smoothed, 3)
          }

          datP <- merge(x = datP, y = smPeriod, by = "Date", all = TRUE)

        }

        if (datP$Estimated < 10) {
          datP$Estimated <- signif(datP$Estimated, 2)
        }

        else if (datP$Estimated >= 10) {
          datP$Estimated <- signif(datP$Estimated, 3)
        }

        if (datP$fitUpper < 10) {
          datP$fitUpper <- signif(datP$fitUpper, 2)
        }

        else if (datP$fitUpper >= 10) {
          datP$fitUpper <- signif(datP$fitUpper, 3)
        }

        if (datP$fitLower < 10) {
          datP$fitLower <- signif(datP$fitLower, 2)
        }

        else if (datP$fitLower >= 10) {
          datP$fitLower <- signif(datP$fitLower, 3)
        }

        datP$standardError <- signif(datP$standardError, 3)

        DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)

      }

    }

})
