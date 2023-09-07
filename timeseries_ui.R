source('app.R')

output$timeseries = renderUI({
  req(analysis_dat())
  req(parameters())
  if (all(colnames(analysis_dat()) == c(parameters()[[2]],"Emissions"))) {
    
    units = parameters()[[6]]
    params = parameters()[[2]]
    unit_column = data.frame(UNITS = rep(units,dim(input_data())[2]))
    plot_dat = cbind(cbind(analysis_dat(), unit_column),
                     input_data() %>% select(-unlist(params)))
    box(title = paste(input$pollutant,
                      " Estimated Emissions Time Series"),
        status = "warning", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        renderPlot(ggplot(plot_dat,
                          aes(x = Days,y = as.numeric(Emissions))) + 
                     geom_line() + xlab("Date") + 
                     ylab(paste(input$pollutant," Emissions (",units,")",sep = ""))
        )
    )
  }
})