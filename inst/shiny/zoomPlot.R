output$zoomPlot <- renderPlot({

dat <- getDat()

datP <- getDatP()

if (!is.null(ranges2$x)) {
  ranges2$x <- as.Date(ranges2$x, origin = "1970-01-01")
  }

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

        datPlot <- cbind(datP, datPred)

        datPlot$Date <- as.Date(datPlot$Date)

        if (input$log10 == FALSE) {

          p <- ggplot(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            #scale_y_log10() +
            #annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())

        }

        else if (input$log10 == TRUE) {

          p <- ggplot(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

        }

        p

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

        estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)

        datPred <- data.frame(estVals)

        upr <- gamIntervals(estVals, regObj, interval = "prediction")$upr

        lwr <- gamIntervals(estVals, regObj, interval = "prediction")$lwr

        datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3),
                              fitUpper = signif(10^(upr), 3),
                              fitLower = signif(10^(lwr), 3),
                              standardError = signif(datPred$se.fit, 3))

        datPlot <- cbind(datP, datPred)

        datPlot$Date <- as.Date(datPlot$Date)

        if (input$log10 == FALSE) {

          p <- ggplot() +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            #scale_y_log10() +
            #annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())

        }

        else if (input$log10 == TRUE) {

          p <- ggplot() +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

        }

        p

      }

  }

  else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2) + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction", na.action = na.pass)

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datPlot <- cbind(datP, datPred)

        datPlot$Date <- as.Date(datPlot$Date)

        if (input$log10 == FALSE) {

          p <- ggplot(data = dat) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())

        }

        else if (input$log10 == TRUE) {

          p <- ggplot(data = dat) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

        }

        p

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs"), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs") + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.x2 = datP$X_00060_00003.x2, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)

        datPred <- data.frame(estVals)

        upr <- gamIntervals(estVals, regObj, interval = "prediction")$upr

        lwr <- gamIntervals(estVals, regObj, interval = "prediction")$lwr

        datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3),
                              fitUpper = signif(10^(upr), 3),
                              fitLower = signif(10^(lwr), 3),
                              standardError = signif(datPred$se.fit, 3))

        datPlot <- cbind(datP, datPred)

        datPlot$Date <- as.Date(datPlot$Date)

        if (input$log10 == FALSE) {

          p <- ggplot(data = dat) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())

        }

        else if (input$log10 == TRUE) {

          p <- ggplot(data = dat) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
            geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
            geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

        }

        p

      }

    }

})
