source('app.R')

output$model_prep = renderUI({
  req(input$pollutant)
  req(input$facility_type)
  req(input$facility_type_detail)
  
  if (!is.null(parameters()) & !is.null(input_data())) {

   lag_message = ""
   
   box(title = "Step 3: Select Input Data",
       background = "black", solidHeader = TRUE,
       collapsible = TRUE,width = 12,
       h4(paste("Select the columns that correspond to the",
                "model parameters"),
          style="text-align:center"),
       column(width = 5),
       column(div(parameters()[[1]]),
         width = 2),
       column(width = 5)
    )
  }
  
  })

