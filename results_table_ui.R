source('app.R')

output$analysis_data = renderUI({
  req(analysis_dat())
  req(parameters())

  params = parameters()[[2]]
  num = length(params)
  params = c(lapply(1:num, function(i) {
    input[[params[i]]]
  }))
  units = parameters()[[6]]
  row_count = dim(input_data())[1]
  user_selected_columns = data.frame(UNITS = rep(units,
                                                 row_count),
                                     PollutantType = rep(input$pollutant,
                                                         row_count),
                                     AnimalType = rep(input$animal_type,
                                                      row_count),
                                     FacilityType = rep(input$facility_type,
                                                        row_count),
                                     FacilityTypeDetail = rep(input$facility_type_detail,
                                                              row_count)
  )
  box(title = paste(input$pollutant,
                    " Estimated Emissions Table"),
      status = "warning", solidHeader = TRUE,
      collapsible = TRUE,width = 12,
      renderDT(
        cbind(cbind(analysis_dat(), user_selected_columns),
              input_data() %>% select(-unlist(params))),
        extensions = "Buttons",
        options = list(pageLength = 5,
                       dom = 'tpB',
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       scrollX = TRUE)))
  
})