output$mnthRsd <- renderPlot({

dat <- getDat()

datP <- getDatP()

  if (input$use2 == FALSE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.y = datP$X_00060_00003.y)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.y = datP$X_00060_00003.y, fourier(datP$Date))

        }

        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")

        datPred <- data.frame(estVals)

        datPred[,(1:4)] <- 10^datPred[,(1:4)]

        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, fitLower = datPred$fit.lwr, standardError = datPred$se.fit)

        datPlot <- cbind(datP, datPred)

        datPlot$Residuals <- datPlot$X_00060_00003.y - datPlot$Estimated

        datPlot$Date <- as.Date(datPlot$Date)

        p <- ggplot() +
          geom_line(data = datPlot, aes(x = Date, y = Residuals)) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        o <- ggplot() +
          geom_boxplot(data = datPlot, aes(x = Date, y = Residuals, group = format(Date, "%Y-%m")), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(datPlot$Residuals, c(0.1, 0.9))) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        print(grid.arrange (p, o, ncol = 1))

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

        datPlot$Residuals <- datPlot$X_00060_00003.y - datPlot$Estimated

        datPlot$Date <- as.Date(datPlot$Date)

        p <- ggplot() +
          geom_line(data = datPlot, aes(x = Date, y = Residuals)) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        o <- ggplot() +
          geom_boxplot(data = datPlot, aes(x = Date, y = Residuals, group = format(Date, "%Y-%m")), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(datPlot$Residuals, c(0.1, 0.9))) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        print(grid.arrange (p, o, ncol = 1))

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

        datPlot$Residuals <- datPlot$X_00060_00003.y - datPlot$Estimated

        datPlot$Date <- as.Date(datPlot$Date)

        p <- ggplot() +
          geom_line(data = datPlot, aes(x = Date, y = Residuals)) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        o <- ggplot() +
          geom_boxplot(data = datPlot, aes(x = Date, y = Residuals, group = format(Date, "%Y-%m")), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(datPlot$Residuals, c(0.1, 0.9))) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        print(grid.arrange (p, o, ncol = 1))

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

        datPlot$Residuals <- datPlot$X_00060_00003.y - datPlot$Estimated

        datPlot$Date <- as.Date(datPlot$Date)

        p <- ggplot() +
          geom_line(data = datPlot, aes(x = Date, y = Residuals)) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        o <- ggplot() +
          geom_boxplot(data = datPlot, aes(x = Date, y = Residuals, group = format(Date, "%Y-%m")), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(datPlot$Residuals, c(0.1, 0.9))) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Residuals") +
          theme_bw() + theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_blank())

        print(grid.arrange (p, o, ncol = 1))

      }

    }

  })
