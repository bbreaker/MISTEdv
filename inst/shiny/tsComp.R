output$tsComp <- renderPlot({

 if (input$timeStep == 2) {

   dat <- readNWISdata(siteNumber = input$ID,
                       startDate = input$dates[1],
                       endDate = input$dates[2],
                       service = "dv",
                       parameterCd = "00060")

   dat <- ddply(dat, .((format(dateTime, "%Y-%m")), site_no), summarize, X_00060_00003 = mean(X_00060_00003))

   colnames(dat)[1] <- "Date"

   dat$Date <- as.yearmon(dat$Date, "%Y-%m")

   if (input$log10 == TRUE){

     p <- ggplot(data = dat, aes(x = Date, y = X_00060_00003)) +
       geom_point(aes(color = factor(site_no), shape = factor(site_no)), size = 3) +
       scale_y_log10() +
       scale_x_yearmon() +
       annotation_logticks(sides = "rl") +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())


     p

   }

   else if(input$log10 == FALSE){

     p <- ggplot(data = dat, aes(x = Date, y = X_00060_00003)) +
       geom_point(aes(color = factor(site_no), shape = factor(site_no)), size = 3) +
       scale_x_yearmon() +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank())

     p

   }

 }

 else if (input$timeStep == 1) {

   dat <- readNWISdata(siteNumber = input$ID,
                       startDate = input$dates[1],
                       endDate = input$dates[2],
                       service = "dv",
                       parameterCd = "00060")

   if (input$log10 == TRUE){

     p <- ggplot(data = dat, aes(x = dateTime, y = X_00060_00003)) +
       geom_line(aes(color = factor(site_no), linetype = factor(site_no)), size = 1) +
       scale_y_log10() +
       annotation_logticks(sides = "rl") +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

     p

   }

   else if(input$log10 == FALSE){

     p <- ggplot(data = dat, aes(x = dateTime, y = X_00060_00003)) +
       geom_line(aes(color = factor(site_no), linetype = factor(site_no)), size = 1) +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank())

     p

   }

 }

 else if (input$timeStep == 3) {

   dat <- readNWISdata(siteNumber = input$ID,
                       startDate = input$dates[1],
                       endDate = input$dates[2],
                       service = "dv",
                       parameterCd = "00060")

   dat <- ddply(dat, .((format(dateTime, "%Y")), site_no), summarize, X_00060_00003 = mean(X_00060_00003))

   colnames(dat)[1] <- "Date"

   dat$Date <- as.numeric(dat$Date)

   if (input$log10 == TRUE){

     p <- ggplot(data = dat, aes(x = Date, y = X_00060_00003)) +
       geom_point(aes(color = factor(site_no), shape = factor(site_no)), size = 3) +
       scale_y_log10() +
       annotation_logticks(sides = "rl") +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

     p

   }

   else if(input$log10 == FALSE){

     p <- ggplot(data = dat, aes(x = Date, y = X_00060_00003)) +
       geom_point(aes(color = factor(site_no), shape = factor(site_no)), size = 3) +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank())

     p

   }

 }

 else if (input$timeStep == 4) {

   dat <- readNWISdata(site = input$ID,
                       startDate = input$dates[1],
                       endDate = input$dates[2],
                       service="iv",
                       parameterCd = "00060")

   if (input$log10 == TRUE){

     p <- ggplot(data = dat, aes(x = dateTime, y = X_00060_00011)) +
       geom_line(aes(color = factor(site_no), linetype = factor(site_no)), size = 1) +
       scale_y_log10() +
       annotation_logticks(sides = "rl") +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

     p

   }

   else if(input$log10 == FALSE){

     p <- ggplot(data = dat, aes(x = dateTime, y = X_00060_00011)) +
       geom_line(aes(color = factor(site_no), linetype = factor(site_no)), size = 1) +
       labs(x = "Date", y = "Discharge, in cubic feet per second") +
       theme_bw() + theme(legend.title = element_blank())

     p

   }

 }

})
