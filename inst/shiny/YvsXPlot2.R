output$YvsXPlot2 <- renderPlot({

  dat <- getDat()

  datP <- getDatP()

    if (input$use2 == TRUE) {

      if (input$Method == 1) {

        p <- ggplot(data = dat, aes(x = X_00060_00003.x2, y = X_00060_00003.y)) +
            geom_point() +
            scale_y_log10(labels = comma) +
            scale_x_log10(labels = comma) +
            annotation_logticks(sides = "trbl") +
            stat_smooth(method = "lm") +
            labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2),
                 y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) +
            theme_bw() + theme(legend.title = element_blank())

        p

      }

      else if (input$Method == 2) {

         p <- ggplot(data = dat, aes(x = X_00060_00003.x2, y = X_00060_00003.y)) +
            geom_point() +
            scale_y_log10(labels = comma) +
            scale_x_log10(labels = comma) +
            annotation_logticks(sides = "trbl") +
            stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
            labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2),
                 y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) +
            theme_bw() + theme(legend.title = element_blank())

        p

      }

    }

 })
