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
