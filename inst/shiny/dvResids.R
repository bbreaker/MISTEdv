output$dvResids <- renderPlot({

dat <- getDat()

if (input$use2 == FALSE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + fourier(Date), data = dat)

        }

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs"), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + fourier(Date), data = dat)

        }

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

    }

  }

  else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2), data = datP)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2) + fourier(Date), data = datP)

        }

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs"), data = datP)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs") + fourier(Date), data = datP)

        }

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

    }

  }

})
