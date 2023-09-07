source('app.R')

output$upload <- renderUI({
  if (!is.null(input$animal_type) & 
      !is.null(input$pollutant) & 
      !is.null(input$facility_type) &
      !is.null(input$facility_type_detail)) {
    
    eq_data = dropdown_data %>% 
      filter(ANIMAL == input$animal_type,
             POLLUTANT == input$pollutant,
             FACILITYTYPE == input$facility_type,
             FACILITY.TYPE.DETAIL == input$facility_type_detail) %>%
      select(EQUATION,EQFUNCT,PARAMETERSNEEDED,NOTES,ei,C)
    
    ei = eq_data$ei
    C = eq_data$C
    
    lag_message = ""
    equation = ""
    transform_message = ""
    if (dim(eq_data)[1] != 0 ) {
      equation = gsub(x = strsplit(x = eq_data$EQFUNCT,
                                   split = "\\{")[[1]][2],
                      pattern = "\\}",replacement = "")
      
      if (!is.na(eq_data$NOTES)) {
        lag_message = p(paste("Note: For this model, inventory data should be ", 
                              "supplied with a 5 day lag."))
      }
      if (!is.na(ei)) {
        transform_message = p(paste("Back-transformation Parameters: ",
                                     "Ei=", ei, " and C=",C,sep = ""),
                              style="text-align:center")
      } else {
        transform_message = p("No back-transformation needed for this equation",
                              style="text-align:center")
      }
    } 
    

    box(title = "Step 2: Upload Input Data", background = "black", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        column(width = 6,
               h4("For this EEM, you will need the following inputs:",
                  style="text-align:center"),
               p(eq_data$PARAMETERSNEEDED,style = "text-align:center"),
               checkboxInput("dummy_data",HTML(paste("Don't have data? Explore <br/>",
                                                     "the tool with sample data")),
                             value = TRUE),
               fileInput("input_file", "Upload a CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               ),
        
        column(width = 6,
               h4("Equation Details",style="text-align:center"),
               p(paste("EEM Equation Number:", eq_data$EQUATION), 
                 style="text-align:center"),
               p(paste("Formula:",equation),style="text-align:center"),
               transform_message,
               p(paste("Depending on the EEM selected, you will need to provide some ",
                       "combination of the following:"),
                 style="text-align:center"),
               HTML("<ul style='list-style-position:inside';>
                            <li>LAW, live animal weight in thousands of kilograms (Mg) </li>
                            <li>CycleDay, the day of the animal placement cycle</li>
                            <li>Inventory (thousand-heads)</li>
                            <li>Temperature, ambient temperature in celsius</li>
                            <li>AmbRH, average daily relative humidity (percent of water vapor in the air)</li>
                            <li>Windspeed, average daily wind speed in meters per second (m/s) at a height of 2.5 meters.</li>
                         </ul>"),
               p("See the Overview tab for additional information.",
                 style="text-align:center"),
               lag_message
               )
            )
        }
      })

output$uploaded_data = renderUI({
  if (!is.null(input_data())) {
    box(title = "Input Data Preview", status = "warning", solidHeader = TRUE,
        collapsible = TRUE,width = 12,
        renderDT(
          input_data(),
          options = list(pageLength = 5,
                         scrollX = TRUE,
                         lengthChange = FALSE,
                         dom = 'tp')))
  }
})