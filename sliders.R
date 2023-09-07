output$pollutant_slider <- renderUI({
  
  req(input$animal_type)
  if (!is.null(input$animal_type)){
    pollutant = dropdown_data %>% 
      filter(ANIMAL == input$animal_type) %>% 
      select(POLLUTANT) %>% pull()
    selectInput("pollutant","Select Pollutant",
                pollutant)
  }})

output$facility_type_slider <- renderUI({
  req(input$pollutant)
  
  if (!is.null(input$pollutant)){
    facility_type = dropdown_data %>% 
      filter(ANIMAL == input$animal_type,
             POLLUTANT == input$pollutant) %>%
      select(FACILITYTYPE) %>% pull()
    selectInput("facility_type","Select Facility Type",
                facility_type)
  }})

output$facility_type_detail_slider <- renderUI({
  req(input$facility_type)
  
  if (!is.null(input$facility_type)){
    facility_type_detail = dropdown_data %>% 
      filter(ANIMAL == input$animal_type,
             POLLUTANT == input$pollutant,
             FACILITYTYPE == input$facility_type) %>%
      select(FACILITY.TYPE.DETAIL) %>% pull()
    
    selectInput("facility_type_detail","Select Facility Type Detail",
                facility_type_detail)
  }})