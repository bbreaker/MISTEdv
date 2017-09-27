library (shiny)
library (shinydashboard)
library (DT)
library (plyr)
library (dplyr)
library (ggplot2)
library (gridExtra)
library (gtable)
library (scales)
library (dataRetrieval)
library (zoo)
library (lubridate)
library (smwrStats)
library (mgcv)
library(broom)

source("global.R")

options(DT.options = list(pageLength = 7, language = list(search = 'Filter:')))

header <- dashboardHeader(title = "MISTEdv", titleWidth = 300,
                          tags$li(a(href = "https://www.usgs.gov/products/data-and-tools/real-time-data/water",
                                    img(src = "gsLogoBlack.png",
                                        title = "USGS home page", height = "47px"),
                                    style = "padding-top:3px; padding-bottom:0px;"),
                                  class = "dropdown"))

sidebar <- dashboardSidebar(width = 300,
   sidebarMenu(
     submitButton("Apply changes", icon("paper-plane")),
     menuItem("Find Correlated Stations", tabName = "tabset1",
              numericInput("long1", "Western most longitude", -94.9),
              numericInput("lat1", "Southern most latitude", 35.4),
              numericInput("long2", "Eastern most longitude", -91.5),
              numericInput("lat2", "Northern most latitude", 36.9)
	  ),
	  menuItem("Data Estimation", tabName = "tabset1",
	           checkboxInput("log10", "Plot y axis in log10 scale for time series plots",
	                         value = TRUE),
	           dateRangeInput("dates3",
	                          "Date range for regression",
	                          start = as.character(Sys.Date() - 761),
	                          end = as.character(Sys.Date() - 396)),
	           dateRangeInput("dates2",
	                          "Date range for estimation",
	                          start = as.character(Sys.Date() - 38),
	                          end = as.character(Sys.Date() - 31)),
	           textInput("yID", "Station ID for response station", "yID"),
	           textInput("xID", "Station ID for explanatory station 1", "xID"),
	           sliderInput("lag1", "Lag for explanatory station 1 (in days)", min = -8, max = 8, value = 0),
	           checkboxInput("use2", "Use a second explanatory station",
	                         value = FALSE),
	           textInput("xID2", "Station ID for explanatory station 2", "xID2"),
	           sliderInput("lag2", "Lag for explanatory station 2 (in days)", min = -8, max = 8, value = 0)
	           ),
	  menuItem("Regression Tuning",
	           dateRangeInput("dates4",
	                          "Date range for missing DVs to smooth",
	                          start = as.character(Sys.Date() - 38),
	                          end = as.character(Sys.Date() - 31)),
	           checkboxInput("smooth", "Apply smoothing to estimated data",
	                         value = FALSE),
	           radioButtons("Method", label = "Select regression method",
	                        choices = list("Linear" = 1, "GAM" = 2),
	                        selected = 1),
	           checkboxInput("adjQ", "Adjust range of discharge values for regression", value = FALSE),
	           numericInput("qRange1", "Lowest daily mean discharge", 0),
	           numericInput("qRange2", "Highest daily mean discharge", 50000),
	           checkboxInput("useSeas", "Use seasonality", value = FALSE)
	           ),
	  menuItem("Time Series Comparison", tabName = "tabset1",
	           textInput("ID", "Station ID(s) for time series tab", "ID"),
	           dateRangeInput("dates",
	                          "Date range for time series",
	                          start = as.character(Sys.Date() - 38),
	                          end = as.character(Sys.Date() - 31)),
	           textInput("tz", "Time zone to use", "America/Chicago"),
	           radioButtons("timeStep", label = "Select time step",
	                        choices = list("Daily" = 1, "Monthly" = 2, "Annual" = 3, "Unit" = 4),
	                        selected = 1)
	           ),
	  downloadButton('downloadData', 'Download')
	  )
   )

body <- dashboardBody(
  fluidRow(
    tabBox(id = "tabset1", height = "450px", width = "900px",
           tabPanel("Time Series Est",
                    plotOutput ("allPlot", height = 400,
                                brush = brushOpts(
                                  id = "allPlotBrush",
                                  resetOnNew = FALSE))),
           tabPanel("Residuals",
                    plotOutput ("mnthRsd", height = 400)),
           tabPanel("Data Table",
                    DT::dataTableOutput ("table")),
           tabPanel("Time Series Comp",
                    plotOutput ("tsComp", height = 400)))
    ),
  fluidRow(
    tabBox(id = "tabset2", side ="left", height = "450px",
           tabPanel("Estimated vs Observed",
                    plotOutput ("EstVsObsPlot", height = 400, width = 400)),
           tabPanel("Response vs Explanatory 1",
                    plotOutput ("YvsXplot1", height = 400, width = 400)),
           tabPanel("Response vs Explanatory 2",
                    plotOutput ("YvsXplot2", height = 400, width = 400)),
           tabPanel("Plot Zoom",
                    plotOutput("zoomPlot", height = 400)),
           tabPanel("Smoothed DVs",
                    plotOutput("smthPlot", height = 400))
           ),
    tabBox(id = "tabset3", side = "left", height = "465px",
           tabPanel("Regression Summary",
                    verbatimTextOutput ("regSum")),
           tabPanel("Correlation Table",
                    DT::dataTableOutput ("complist"), width = "300px"),
           tabPanel("Available DVs",
                    DT::dataTableOutput ("availDVs"))
           )
    )
  )

ui <- dashboardPage(skin = "black", header, sidebar, body)

server <- function(input, output) ({

  getDat <- reactive({

   if (input$use2 == FALSE) {

	    startDate <- input$dates3[1]

      endDate <- input$dates3[2]

      startDateLag <- startDate + as.difftime(input$lag1, units = "days")

      endDateLag <- endDate + as.difftime(input$lag1, units = "days")

      daty <- readNWISdv(siteNumber=input$yID,
                         startDate = startDate,
                         endDate = endDate,
                         parameterCd = "00060",
                         statCd = "00003")

      datx <- readNWISdv(siteNumber=input$xID,
                         startDate = startDateLag,
                         endDate = endDateLag,
                         parameterCd = "00060",
                         statCd = "00003")

      daty$X_00060_00003 <- ifelse(daty$X_00060_00003 == 0, NA, daty$X_00060_00003)

      datx$X_00060_00003 <- ifelse(datx$X_00060_00003 == 0, NA, datx$X_00060_00003)

      daty$Date <- as.Date(daty$Date, format = "%Y-%m-%d")

      datx$Date <- as.Date(datx$Date, format = "%Y-%m-%d")

      newDate <- data.frame(Date = seq(from = startDate, to = endDate, by = "day"))

      newDate2 <- data.frame(Date = seq(from = startDateLag, to = endDateLag, by = "day"))

      daty <- merge(x = newDate, y = daty, by = "Date", all = TRUE)

      datx <- merge(x = newDate2, y = datx, by = "Date", all = TRUE)

      datx$Date <- as.Date(daty$Date, format = "%Y-%m-%d")

      dat <- merge(x = datx, y = daty, by = "Date", all = TRUE)

      if (input$adjQ == TRUE) {

        dat <- subset(dat, X_00060_00003.y > input$qRange1 & X_00060_00003.y < input$qRange2)

      }

      return (dat)

	}

	else if (input$use2 == TRUE) {

	    startDate <- input$dates2[1]

      endDate <- input$dates2[2]

      startDateLag <- startDate + as.difftime(input$lag1, units = "days")

      endDateLag <- endDate + as.difftime(input$lag1, units = "days")

      startDateLag2 <- startDate + as.difftime(input$lag2, units = "days")

      endDateLag2 <- endDate + as.difftime(input$lag2, units = "days")

      daty <- readNWISdv(siteNumber=input$yID,
                         startDate = startDate,
                         endDate = endDate,
                         parameterCd = "00060",
                         statCd = "00003")

      datx <- readNWISdv(siteNumber=input$xID,
                         startDate = startDateLag,
                         endDate = endDateLag,
                         parameterCd = "00060",
                         statCd = "00003")

      datx2 <- readNWISdv(siteNumber=input$xID2,
                          startDate = startDateLag2,
                          endDate = endDateLag2,
                          parameterCd = "00060",
                          statCd = "00003")

      daty$X_00060_00003 <- ifelse(daty$X_00060_00003 == 0, NA, daty$X_00060_00003)

      datx$X_00060_00003 <- ifelse(datx$X_00060_00003 == 0, NA, datx$X_00060_00003)

      datx2$X_00060_00003 <- ifelse(datx2$X_00060_00003 == 0, NA, datx2$X_00060_00003)

      daty$Date <- as.Date(daty$Date, format = "%Y-%m-%d")

      datx$Date <- as.Date(datx$Date, format = "%Y-%m-%d")

      datx2$Date <- as.Date(datx2$Date, format = "%Y-%m-%d")

      newDate <- data.frame(Date = seq(from = startDate, to = endDate, by = "day"))

      newDate2 <- data.frame(Date = seq(from = startDateLag, to = endDateLag, by = "day"))

      newDate3 <- data.frame(Date = seq(from = startDateLag2, to = endDateLag2, by = "day"))

      daty <- merge(x = newDate, y = daty, by = "Date", all = TRUE)

      datx <- merge(x = newDate2, y = datx, by = "Date", all = TRUE)

      datx2 <- merge(x = newDate3, y = datx2, by = "Date", all = TRUE)

      datx$Date <- as.Date(daty$Date, format = "%Y-%m-%d")

      colnames(datx)[3:5] <- paste0(colnames(datx)[3:5], ".x")

      datx2 <- datx2[,-1]

      colnames(datx2)[2:4] <- paste0(colnames(datx2)[2:4], ".x2")

      daty <- daty[,-1]

      colnames(daty)[2:4] <- paste0(colnames(daty)[2:4], ".y")

      dat <- cbind(datx, datx2, daty)

      dat$Date <- as.Date(dat$Date)

      if (input$adjQ == TRUE) {

        dat <- subset(dat, X_00060_00003.y > input$qRange1 & X_00060_00003.y < input$qRange2)

      }

	  return (dat)

	}

  })

  getDatP <- reactive({

	 if (input$use2 == FALSE) {

      startDateP <- input$dates2[1]

      endDateP <- input$dates2[2]

      startDateLagP <- startDateP + as.difftime(input$lag1, units = "days")

      endDateLagP <- endDateP + as.difftime(input$lag1, units = "days")

      datyP <- readNWISdv(siteNumber=input$yID,
                          startDate = startDateP,
                          endDate = endDateP,
                          parameterCd = "00060",
                          statCd = "00003")

      datxP <- readNWISdv(siteNumber=input$xID,
                          startDate = startDateLagP,
                          endDate = endDateLagP,
                          parameterCd = "00060",
                          statCd = "00003")

      datyP$X_00060_00003 <- ifelse(datyP$X_00060_00003 == 0, NA, datyP$X_00060_00003)

      datxP$X_00060_00003 <- ifelse(datxP$X_00060_00003 == 0, NA, datxP$X_00060_00003)

      datyP$Date <- as.Date(datyP$Date, format = "%Y-%m-%d")

      datxP$Date <- as.Date(datxP$Date, format = "%Y-%m-%d")

      newDateP <- data.frame(Date = seq(from = startDateP, to = endDateP, by = "day"))

      newDateLagP <- data.frame(Date = seq(from = startDateLagP, to = endDateLagP, by = "day"))

      datyP <- merge(x = newDateP, y = datyP, by = "Date", all = TRUE)

      datxP <- merge(x = newDateLagP, y = datxP, by = "Date", all = TRUE)

      datxP$Date <- as.Date(datyP$Date, format = "%Y-%m-%d")

      datP <- merge(x = datxP, y = datyP, by = "Date", all = TRUE)

	  return (datP)

	 }

	 else if (input$use2 == TRUE) {

      startDateP <- input$dates2[1]

      endDateP <- input$dates2[2]

      startDateLagP <- startDateP + as.difftime(input$lag1, units = "days")

      endDateLagP <- endDateP + as.difftime(input$lag1, units = "days")

      startDateLagP2 <- startDateP + as.difftime(input$lag2, units = "days")

      endDateLagP2 <- endDateP + as.difftime(input$lag2, units = "days")

      datyP <- readNWISdv(siteNumber=input$yID,
                          startDate = startDateP,
                          endDate = endDateP,
                          parameterCd = "00060",
                          statCd = "00003")

      datxP <- readNWISdv(siteNumber=input$xID,
                          startDate = startDateLagP,
                          endDate = endDateLagP,
                          parameterCd = "00060",
                          statCd = "00003")

      datx2P <- readNWISdv(siteNumber=input$xID2,
                           startDate = startDateLagP2,
                           endDate = endDateLagP2,
                           parameterCd = "00060",
                           statCd = "00003")

      datyP$X_00060_00003 <- ifelse(datyP$X_00060_00003 == 0, NA, datyP$X_00060_00003)

      datxP$X_00060_00003 <- ifelse(datxP$X_00060_00003 == 0, NA, datxP$X_00060_00003)

      datx2P$X_00060_00003 <- ifelse(datx2P$X_00060_00003 == 0, NA, datx2P$X_00060_00003)

      datyP$Date <- as.Date(datyP$Date, format = "%Y-%m-%d")

      datxP$Date <- as.Date(datxP$Date, format = "%Y-%m-%d")

      datx2P$Date <- as.Date(datx2P$Date, format = "%Y-%m-%d")

      newDateP <- data.frame(Date = seq(from = startDateP, to = endDateP, by = "day"))

      newDateLagP <- data.frame(Date = seq(from = startDateLagP, to = endDateLagP, by = "day"))

      newDateLagP2 <- data.frame(Date = seq(from = startDateLagP2, to = endDateLagP2, by = "day"))

      datyP <- merge(x = newDateP, y = datyP, by = "Date", all = TRUE)

      datxP <- merge(x = newDateLagP, y = datxP, by = "Date", all = TRUE)

      datx2P <- merge(x = newDateLagP2, y = datx2P, by = "Date", all = TRUE)

      datxP$Date <- as.Date(datyP$Date, format = "%Y-%m-%d")

      colnames(datxP)[3:5] <- paste0(colnames(datxP)[3:5], ".x")

      datx2P <- datx2P[,-1]

      colnames(datx2P)[2:4] <- paste0(colnames(datx2P)[2:4], ".x2")

      datyP <- datyP[,-1]

      colnames(datyP)[2:4] <- paste0(colnames(datyP)[2:4], ".y")

      datP <- cbind(datxP, datx2P, datyP)

      datP$Date <- as.Date(datP$Date)

      return (datP)

      }

	})

  ##################################################################

  source("compList.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("tsComp.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("YvsXPlot1.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("EstVsObsPlot.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("regSum.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("table.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("allPlot.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("mnthRsd.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("YvsXPlot2.R",local=TRUE)$value

  ##################################################################

  ranges2 <- reactiveValues(x = NULL, y = NULL)

  observe({
   brush <- input$allPlotBrush
   if (!is.null(brush)) {
    ranges2$x <- c(brush$xmin, brush$xmax)
    ranges2$y <- c(brush$ymin, brush$ymax)
   }
   else {
    ranges2$x <- NULL
    ranges2$y <- NULL
   }
  })

  ##################################################################

  source("zoomPlot.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("smthPlot.R",local=TRUE)$value

  ##################################################################

  ##################################################################

  source("availDVs",local=TRUE)$value

  ##################################################################

  output$downloadData <- downloadHandler(

    filename = function() {paste0 ("reg", input$yID, ".csv")},

    content = function(file){

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

    write.csv(datP, file)

    }

  )

})

shinyApp(ui, server)
