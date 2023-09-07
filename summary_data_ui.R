source('app.R')

output$threshold_slider = renderUI({
  req(analysis_dat())
  req(parameters())
  if (all(colnames(analysis_dat()) == c(parameters()[[2]],"Emissions"))) {
    
    box(title = "Optional: Enter a value to count emission values in excess of:",
        background = "black", solidHeader = TRUE, width = 3,
        collapsible = TRUE,
        numericInput("threshold", "Enter an Emission Threshold value",
                     value = 50)
    )
  }
})

output$summary = renderUI({
  req(analysis_dat())
  req(parameters())
  if (all(colnames(analysis_dat()) == c(parameters()[[2]],"Emissions"))) {
    
    units = parameters()[[6]]
    K = parameters()[[7]]
    total_emissions = round(sum(analysis_dat()$Emissions),digits = 2)
    uncertainty = round(K/total_emissions,digits = 2)
    mean_emissions = round(mean(analysis_dat()$Emissions),digits = 2)
    max_emissions = round(max(analysis_dat()$Emissions),digits = 2)
    
    box(title = paste(input$pollutant,
                      " Emission Summary Data"),
        status = "warning", solidHeader = TRUE,
        collapsible = TRUE, width = 9,
        valueBox(paste(total_emissions,
                       gsub(pattern = "/day",
                       replacement = "",x = units)),
                 "Total Emissions",
                 width = 9,color = "yellow"),
        ####### uncertainty box #############
        #valueBox(paste(uncertainty,"%"),
        #         HTML(paste("Uncertainty",
        #                    "<br/>",
        #                    "Note: Uncertainy is only valid",
        #                    "for annual data")),
        #         width = 9,color = "yellow"),
        ####################################
        valueBox(paste(mean_emissions,
                       units,sep = " "), 
                 "Mean Emissions", 
                 width = 9,color = "yellow"),
        valueBox(paste(max_emissions,
                       units,sep = " "),
                 "Max Emissions", 
                 width = 9,color = "yellow"),
        valueBox(sum(analysis_dat()$Emissions > input$threshold),
                 "Rows greater than the threshold", 
                 width = 9,
                 color = "yellow")
    )
  }
})