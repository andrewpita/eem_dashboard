## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(purrr)
library(ggplot2)
library(zoo)
library(markdown)
library(knitr)
library(shinyjs)
  
############################ load required data ###########################

dropdown_data = read.csv("EEM.csv",stringsAsFactors = FALSE)
dummy_data = read.csv("EEM Example Data.csv",stringsAsFactors = FALSE)
animal_type = unique(dropdown_data$ANIMAL) #### creates the selections for 
                                            ### for the animal type dropdown

##########################################################################

##################### this header code is necessary to get ###############
##################### the dashboard title off the sidebar ################
##################### and onto the main panel ############################
header_img <- div(
  #img(src="https://earthjustice.org/wp-content/uploads/ej_logo_white.svg", height="40px"),
  div(
    class = "my-title",
    h4('Air Emissions Estimating Methodologies Dashboard'),
    tags$style(".my-title :is(h4){color: black; font-weight: bold;}")
  ),
  style = "display: flex;"
)

header <-  htmltools::tagQuery(dashboardHeader(title = ""))
header <- header$
  addAttrs(style = "position: relative")$ # add some styles to the header 
  find(".navbar.navbar-static-top")$ # find the header right side
  append(header_img)$ # inject our img
  allTags()

############################################################################

############################# UI ###########################################

ui <- dashboardPage(title = "EEM Equations",skin = "black",
                    header, ##calls the header html and css from above 
                    
                    ########################### sidebar #############################
                    dashboardSidebar(
                      useShinyjs(),
                      ############# Step 1 sliders and Sidebar #####################
                      ############# The code for these is contains in sliders.R ###
                      sidebarMenu(
                        menuItem("Overview", tabName = "overview",
                                 icon = icon("book")),
                        menuItem("Dashboard", tabName = "dashboard", 
                                 icon = icon("table-columns")),
                        selected = 1)
                    ),
                    
                    ########################## body ###############################
                    dashboardBody(
                      
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(title = "Step 1: Indicate Pollutant and Facility Details", background = "black", solidHeader = TRUE,
                                      collapsible = TRUE, width = 12,
                                         style="text-align:center",
                                      column(width = 5),
                                      column(width = 2,
                                        selectInput('animal_type', 'Select Animal Type',
                                                  animal_type, selected = "SWINE"),
                                        uiOutput("pollutant_slider"), ### uiOutput() corresponds to
                                        ### outputs created by renderUi()
                                        uiOutput("facility_type_slider"),
                                        uiOutput("facility_type_detail_slider")
                                      ),
                                      column(width = 5)
                                      ),
                                ),
                                fluidRow(
                                  uiOutput("upload")
                                ),
                                fluidRow(
                                  uiOutput("uploaded_data")
                                ),
                                fluidRow(
                                  tags$style(".nav-tabs {background-color: #FFA500;}
                                  .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                  background-color: white;
                                  border-color: white;
                                  }

                                  .nav-tabs-custom .nav-tabs li.active {
                                    border-top-color: #FFF;
                                  }"),
                                  uiOutput("model_prep")
                                  #uiOutput("api_prep"),
                                  #uiOutput("api_returned_data")
                                ),
                                fluidRow(
                                  uiOutput("analysis_data")
                                ),
                                fluidRow(
                                  uiOutput("threshold_slider"),
                                  uiOutput("summary")
                                ),
                                fluidRow(
                                  uiOutput("timeseries"),
                                  renderText(dim(api_data()))
                                )
                                
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "overview",
                                p(HTML(paste("See ",a("HERE",
                                href="https://docs.google.com/document/d/1M-t5lg_NJFsA17KsZieN6BvoGIHK3ZSLJqt4yp5C68g/edit?usp=sharing"),
                                "for background and instructions")))
                                #includeMarkdown("background.Rmd")
                                #HTML(markdownToHTML(knit('background.Rmd', quiet = TRUE)))
                                
                        )
                      )
                    )
)
##############################################################################

####################### server ###############################################
######## the server code chunks are numerous and lengthy #####################
######## so I split them into separate files.  The source() ##################
######## function imports the R file, and then the outputs are ###############
######## accessed with the $syntax just like they would normally #############
##############################################################################
server <- function(input, output,session) { 
  ############### step 1 sliders ########################
  #######################################################
  source('sliders.R', local = TRUE)$pollutant_slider
  source('sliders.R', local = TRUE)$facility_type_slider
  source('sliders.R', local = TRUE)$facility_type_detail_slider
  ############### step 2 upload ########################
  #####################################################
  source('upload_ui.R', local = TRUE)$upload
  source('upload_ui.R', local = TRUE)$uploaded_data
  ############### step 3 model prep ###################
  #####################################################
  source('model_prep_ui.R', local = TRUE)$model_prep
  ############### estimate results ####################
  #####################################################
  source('results_table_ui.R', local = TRUE)$analysis_data
  source('summary_data_ui.R', local = TRUE)$threshold_slider
  source('summary_data_ui.R', local = TRUE)$summary
  source('timeseries_ui.R', local = TRUE)$timeseries
  ###########################################################
  
  ###### the rest of the script contains reactive ########
  ###### objects They are reactive functions that ########
  ###### watch for changes to their inputs.  #############
  
  ###### input_data() reacts  to changes to the ##########
  ###### dummy_data slider and input_file  ###############
  ###### choose CSV upload button #######################
  
  ###### parameters() creates the dropdown ###########
  ###### sliders  to select columns from the #########
  ###### input data  in Step  3, and also  ###########
  ###### stores the EEM info used throughout #########
  ###### the app. They are stored in a list ##########
  
  ##### analysis_dat() reacts to the input ###########
  ##### columns selected to create the    ############
  ##### results data and input to the summary ########
  
      input_data = reactive({
        req(input$pollutant)
        req(input$facility_type)
        req(input$facility_type_detail)
        if (input$dummy_data == TRUE) {
          data = dummy_data
          return(data)
      } else if (is.null(input$input_file) & input$dummy_data == FALSE) {
          return(NULL)
      } else if (!is.null(input$input_file) & input$dummy_data == FALSE) {
        data = read.csv(input$input_file$datapath,
                       stringsAsFactors = FALSE)
        return(data) }
      })
      
      
      parameters = reactive({
        req(input$pollutant)
        req(input$facility_type)
        req(input$facility_type_detail)
        
        if (!is.null(input_data())) {
          
          equation = dropdown_data %>% 
            filter(ANIMAL == input$animal_type,
                   POLLUTANT == input$pollutant,
                   FACILITYTYPE == input$facility_type,
                   FACILITY.TYPE.DETAIL == input$facility_type_detail) %>%
            select(EQUATION,EQFUNCT,PARAMETERSNEEDED,ei,C,UNITS,k)
          
          ei = equation %>% select(ei) %>% pull()
          C = equation %>% select(ei) %>% pull()
          units = equation %>% select(UNITS) %>% pull()
          eq = equation %>% select(EQFUNCT) %>% pull()
          K = equation %>% select(k) %>% pull()
          parameters = equation %>% select(PARAMETERSNEEDED) %>% pull()
          parameters = strsplit(parameters,",")
          param_length = length(parameters[[1]])
          param_vec = rep(NA,param_length)
          params_as_inputs = rep(NA,param_length)
          dropdown_list = list()
          
          for (i in 1:param_length) {
            
            param = trimws(parameters[[1]][i])
            param_vec[i] = param
            dropdown = selectInput(param,paste("Choose ", param),
                                   colnames(input_data()))
            dropdown_list[[i]] = dropdown
            params_as_inputs[i] = paste("input$",param,sep="")
            
          }
          
          return(list(dropdown_list,param_vec,eq,ei,C,units,K))
        } else {
          return(NULL)
        }
        
      })

      analysis_dat = reactive({
        req(input$pollutant)
        req(input$facility_type)
        req(input$facility_type_detail)
        req(parameters())
        if (!is.null(parameters())) {
          eq = parameters()[[3]]
          ei = parameters()[[4]]
          C = parameters()[[5]]
          params = parameters()[[2]]
          num = length(params)
          params = c(lapply(1:num, function(i) {
            input[[params[i]]]
          }))
          eq = eval(parse(text = eq))
          eTransform = function(x,ei,C){exp(x)*ei-C}
          dat = input_data() %>% select(unlist(params))
          cdim = dim(dat)[2]
          colnames(dat) = parameters()[[2]][1:cdim]
          if (cdim == num) {

            tryCatch({
              
              dat$Emissions = unlist(pmap(dat,eq))
              
              if (!is.na(ei)) {
                dat$Emissions = eTransform(as.numeric(dat$Emissions),ei,C)
              }
             
              dat$Emissions = round(dat$Emissions, digits = 3)
              dat$Emissions = ifelse(dat$Emissions < 0, 0,
                                     dat$Emissions)
            })
          }
          
          return(dat)
        } else {return(NULL)}
        
      })
    
    
  ####### observeEvent() function to make sure ############
  ####### the sample data is not used after data ############
  ####### is uploaded by the user ###########################
  
  observeEvent(input$input_file, {
    if (input$dummy_data == TRUE) {
      
      updateCheckboxInput(session,"dummy_data",
                          value = FALSE)
    }
  })
  
  
}

shinyApp(ui, server)