output$regSum <- renderPrint ({

dat <- getDat()

datP <- getDatP()

if (input$use2 == FALSE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + fourier(Date), data = dat)

        }

        summary (regObj)

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs"), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + fourier(Date), data = dat)

        }

        summary (regObj)

      }

    }

  else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        if (input$useSeas == FALSE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- lm(log10(X_00060_00003.y) ~ log10(X_00060_00003.x) + log10(X_00060_00003.x2) + fourier(Date), data = dat)

        }

        summary (regObj)

      }

      else if (input$Method == 2) {

        if (input$useSeas == FALSE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs"), data = dat)

        }

        else if (input$useSeas == TRUE) {

          regObj <- gam(log10(X_00060_00003.y) ~ s(log10(X_00060_00003.x), bs = "cs") + s(log10(X_00060_00003.x2), bs = "cs") + fourier(Date), data = dat)

        }

        summary (regObj)

      }

    }

})
