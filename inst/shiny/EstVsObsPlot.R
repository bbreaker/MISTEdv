output$EstVsObsPlot <- renderPlot ({

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

        datP$Predicted <- 10^(predict(regObj, predSet))

        datP$pred <- predict(regObj, predSet)

        datP$Date <- as.Date(datP$Date, format = "%Y-%m-%d")

        p <- ggplot(data = datP, aes(x = X_00060_00003.y, y = Predicted)) +
          geom_point(size = 3) +
          stat_smooth(method = "lm") +
          labs(x = "Observed discharge, in cubic feet per second", y = "Estimated discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())

        p

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs"), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.y = datP$X_00060_00003.y)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + fourier(Date), data = dat)

          predSet <- data.frame(Date = datP$Date, X_00060_00003.x = datP$X_00060_00003.x, X_00060_00003.y = datP$X_00060_00003.y, fourier(datP$Date))

        }

        datP$Predicted <- 10^(predict(regObj, predSet))

        datP$pred <- predict(regObj, predSet)

        datP$Date <- as.Date(datP$Date, format = "%Y-%m-%d")

        p <- ggplot(data = datP, aes(x = X_00060_00003.y, y = Predicted)) +
          geom_point(size = 3) +
          stat_smooth(method = "lm") +
          labs(x = "Observed discharge, in cubic feet per second", y = "Estimated discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())

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

        datP$Predicted <- 10^(predict(regObj, predSet))

        datP$pred <- predict(regObj, predSet)

        p <- ggplot(data = datP, aes(x = X_00060_00003.y, y = Predicted)) +
          geom_point(size = 3) +
          stat_smooth(method = "lm") +
          labs(x = "Observed discharge, in cubic feet per second", y = "Estimated discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())

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

        datP$Predicted <- 10^(predict(regObj, predSet))

        datP$pred <- predict(regObj, predSet)

        p <- ggplot(data = datP, aes(x = X_00060_00003.y, y = Predicted)) +
          geom_point(size = 3) +
          stat_smooth(method = "lm") +
          labs(x = "Observed discharge, in cubic feet per second", y = "Estimated discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())

        p

      }

    }

 })
