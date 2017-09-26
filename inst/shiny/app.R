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

source("global.R")

options(DT.options = list(pageLength = 7, language = list(search = 'Filter:')))

#First stable version, 2016-02-22
#This vesion, 2017-09-26
#This set of tools is intended to assist in the review and estimation of daily discharge values
#Author: Brian Breaker

header <- dashboardHeader(title = "MISTEdv", titleWidth = 300,
                          tags$li(a(href = "https://www.usgs.gov/products/data-and-tools/real-time-data/water",
                                    img(src = "gsLogoBlack.png",
                                        title = "USGS home page", height = "47px"),
                                    style = "padding-top:3px; padding-bottom:0px;"),
                                  class = "dropdown"))

sidebar <- dashboardSidebar(width = 300,
   sidebarMenu(
     submitButton("Apply changes", icon("paper-plane")),
      #Tools for creating correlated stations table
	   menuItem("Find Correlated Stations", tabName = "tabset1",
          textInput("ID3", "Station ID for comparison", "ID"),
		  numericInput("long1", "Western most longitude", -94.9),
	      numericInput("lat1", "Southern most latitude", 35.4),
	      numericInput("long2", "Eastern most longitude", -91.5),
	      numericInput("lat2", "Northern most latitude", 36.9)
	  ),
	   #Tools for adding response and explanatory stations and date ranges for regression
	   #and estimation... also date range for smoothing and apply smoothing
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
		  sliderInput("lag2", "Lag for explanatory station 2 (in days)", min = -8, max = 8, value = 0),
		  dateRangeInput("dates4",
		                   "Date range for missing DVs to smooth",
		                   start = as.character(Sys.Date() - 38),
		                   end = as.character(Sys.Date() - 31)),
		  checkboxInput("smooth", "Apply smoothing to estimated data",
		                   value = FALSE)
	  ),
	  #Tools for changing regression type, setting Q range for regression,
	  #and adding seasonal constants for regression
	  menuItem("Regression Tuning",
	      radioButtons("Method", label = "Select regression method",
                         choices = list("Linear" = 1, "GAM" = 2),
                         selected = 1),
		    checkboxInput("adjQ", "Adjust range of discharge values for regression", value = FALSE),
		    numericInput("qRange1", "Lowest daily mean discharge", 0),
		    numericInput("qRange2", "Highest daily mean discharge", 50000),
		    checkboxInput("useSeas", "Use seasonality", value = FALSE)
	  ),
	   #Tools the the time series comparison graph
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
    #Time series graph of explanatory stations, response stations, conf intervals,
	  #and estimated data... with added zoom
	  tabPanel("Time Series Est",
	           plotOutput ("plot4", height = 400,
                     brush = brushOpts(
                       id = "plot4Brush",
                       resetOnNew = FALSE))),
	  #residuals graphs
      tabPanel("Residuals",
         plotOutput ("plot5", height = 400)),
      #data table
	  tabPanel("Data Table",
               DT::dataTableOutput ("table")),
      #general times series comparison graph
	  tabPanel("Time Series Comp",
         plotOutput ("plot1", height = 400)))
    ),
  fluidRow(
    tabBox(id = "tabset2", side ="left", height = "450px",
      tabPanel("Estimated vs Observed",
         plotOutput ("plot3", height = 400, width = 400)),
      tabPanel("Response vs Explanatory 1",
         plotOutput ("plot2", height = 400, width = 400)),
      tabPanel("Response vs Explanatory 2",
         plotOutput ("plot6", height = 400, width = 400)),
	  tabPanel("Plot Zoom",
         plotOutput("plot7", height = 400)),
	  tabPanel("Smoothed DVs",
         plotOutput("plot8", height = 400))
	),
    tabBox(id = "tabset3", side = "left", height = "465px",
      tabPanel("Regression Summary",
        verbatimTextOutput ("regSum")),
	  #correlation table
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

  output$complist <- DT::renderDataTable ({

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

  output$plot1 <- renderPlot({

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

  output$plot2 <- renderPlot ({

	  dat <- getDat()

	  datP <- getDatP()

      if (input$Method == 1) {

        p <- ggplot(data = dat, aes(x = X_00060_00003.x, y = X_00060_00003.y)) +
          geom_point(size = 3) +
		      scale_y_log10() +
		      scale_x_log10() +
		      annotation_logticks(sides = "trbl") +
          stat_smooth(method = "lm") +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID),
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) +
          theme_bw() + theme(legend.title = element_blank())

        p

      }

      else if (input$Method == 2) {

        p <- ggplot(data = dat, aes(x = X_00060_00003.x, y = X_00060_00003.y)) +
          geom_point(size = 3) +
		      scale_y_log10() +
		      scale_x_log10() +
		      annotation_logticks(sides = "trbl") +
          stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID),
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) +
          theme_bw() + theme(legend.title = element_blank())

        p

      }

  })

  output$plot3 <- renderPlot ({

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

  output$plot4 <- renderPlot({

	dat <- getDat()

	datP <- getDatP()

	datMeas <- readNWISmeas(siteNumber = input$yID, startDate = input$dates2[1], endDate = input$dates2[2], tz = Sys.timezone())

	datMeas$labelN <- paste0("Q = ", datMeas$discharge_va, ", shift = ", datMeas$shift_adj_va, ", %diff = ", datMeas$diff_from_rating_pc)

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

          datMeas$measurement_dt <- as.Date(datMeas$measurement_dt)

          if (input$log10 == FALSE) {

            p <- ggplot(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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
              scale_y_log10() +
              annotation_logticks(sides = "rl") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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

		      datMeas$measurement_dt <- as.Date(datMeas$measurement_dt)

          if (input$log10 == FALSE) {

            p <- ggplot(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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
              scale_y_log10() +
              annotation_logticks(sides = "rl") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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

		      datMeas$measurement_dt <- as.Date(datMeas$measurement_dt)

          if (input$log10 == FALSE) {

            p <- ggplot(data = dat) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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

		      datMeas$measurement_dt <- as.Date(datMeas$measurement_dt)

          if (input$log10 == FALSE) {

            p <- ggplot(data = dat) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
              geom_line(data = datPlot, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line(data = datPlot, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datPlot, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datPlot, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
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
			        geom_point(data = datMeas, aes(x = measurement_dt, y = discharge_va), color = "black", size = 2) +
			        geom_text(data = datMeas, aes(x = measurement_dt, y = discharge_va, label = labelN, vjust = "inward", hjust = "inward", check_overlap = TRUE), color = "medium blue") +
              scale_y_log10() +
              annotation_logticks(sides = "rl") +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

          }

          p

        }

      }

  })

  output$plot5 <- renderPlot({

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

  output$plot6 <- renderPlot({

	  dat <- getDat()

	  datP <- getDatP()

      if (input$use2 == TRUE) {

        if (input$Method == 1) {

          p <- ggplot(data = dat, aes(x = X_00060_00003.x2, y = X_00060_00003.y)) +
              geom_point() +
			        scale_y_log10() +
			        scale_x_log10() +
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
			  scale_y_log10() +
			  scale_x_log10() +
			  annotation_logticks(sides = "trbl") +
			  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
             labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2),
                  y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) +
             theme_bw() + theme(legend.title = element_blank())

          p

        }

      }

	 })

  ranges2 <- reactiveValues(x = NULL, y = NULL)

  observe({
   brush <- input$plot4Brush
   if (!is.null(brush)) {
    ranges2$x <- c(brush$xmin, brush$xmax)
    ranges2$y <- c(brush$ymin, brush$ymax)
   }
   else {
    ranges2$x <- NULL
    ranges2$y <- NULL
   }
  })

  output$plot7 <- renderPlot({

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

  output$plot8 <- renderPlot({

	if (input$smooth == TRUE) {

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

          datP <- merge(x = datP, y = smPeriod, by = "Date", all.x = FALSE)

          if (input$log10 == FALSE) {

            p <- ggplot(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
              geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
              #scale_y_log10() +
              #annotation_logticks(sides = "rl") +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank())

          }

          else if (input$log10 == TRUE) {

            p <- ggplot(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
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

          datP <- cbind(datP, datPred)

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

          datP <- merge(x = datP, y = smPeriod, by = "Date", all.x = FALSE)

          if (input$log10 == FALSE) {

            p <- ggplot(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
              geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
              #scale_y_log10() +
              #annotation_logticks(sides = "rl") +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank())

          }

          else if (input$log10 == TRUE) {

            p <- ggplot(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
              geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
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

          datP <- merge(x = datP, y = smPeriod, by = "Date", all.x = FALSE)

          if (input$log10 == FALSE) {

            p <- ggplot() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
			        geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank())

          }

          else if (input$log10 == TRUE) {

            p <- ggplot() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
              geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
              scale_y_log10() +
              annotation_logticks(sides = "rl") +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

          }

          p

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

          estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)

          datPred <- data.frame(estVals)

          upr <- gamIntervals(estVals, regObj, interval = "prediction")$upr

          lwr <- gamIntervals(estVals, regObj, interval = "prediction")$lwr

          datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3),
                                fitUpper = signif(10^(upr), 3),
                                fitLower = signif(10^(lwr), 3),
                                standardError = signif(datPred$se.fit, 3))

          datP <- cbind(datP, datPred)

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

          datP <- merge(x = datP, y = smPeriod, by = "Date", all.x = FALSE)

		      if (input$log10 == FALSE) {

		        p <- ggplot() +
		          geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
		          geom_line(data = datP, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
		          geom_line(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
		          geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
		          geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
		          geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
		          geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
		          labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank())

          }

          else if (input$log10 == TRUE) {

            p <- ggplot() +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x, color = paste0("x-", input$xID))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.x2, color = paste0("x2-", input$xID2))) +
              geom_line(data = datP, aes(x = Date, y = X_00060_00003.y, color = paste0("y-", input$yID))) +
              geom_line(data = datP, aes(x = Date, y = Estimated, color = "Estimated"), linetype = "longdash") +
              geom_line(data = datP, aes(x = Date, y = fitUpper), color = "grey", linetype = "dashed") +
              geom_line(data = datP, aes(x = Date, y = fitLower), color = "grey", linetype = "dashed") +
              geom_point(data = datP, aes(x = Date, y = Smoothed, color = "Smoothed"), shape = 8) +
              scale_y_log10() +
              annotation_logticks(sides = "rl") +
              labs(x = "Date", y = "Discharge, in cubic feet per second") +
              theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())

          }

          p

        }

      }

    }

  })

  output$availDVs <- DT::renderDataTable({

  if (input$use2 == FALSE) {

  sites <- c(input$yID, input$xID)

  info <- whatNWISdata(sites)

  info <- subset(info, parm_cd == "00060" & data_type_cd == "dv" & stat_cd == "00003")

  info <- info[,c(3,4,6,7,22,23)]

  info

  }

  else if (input$use2 == TRUE) {

  sites <- c(input$yID, input$xID, input$xID2)

  info <- whatNWISdata(sites)

  info <- subset(info, parm_cd == "00060" & data_type_cd == "dv" & stat_cd == "00003")

  info <- info[,c(3,4,6,7,22,23)]

  info

  }

  })

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
