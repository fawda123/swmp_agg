# packages to use
library(ggplot2)
library(dplyr)
library(SWMPr)
library(httr)
library(XML)
library(data.table)

# master data file
loc <- 'https://github.com/fawda123/swmp_comp/blob/master/data/all_dat.RData?raw=true'
load(url(loc))

source('R/funcs.R')

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  #### dynamic UI

  ## for first plot
  # parameter to plot
  output$parm1 <- renderUI({
  
    type1 <- input$type1
  
    vars <- list(
    
      wq = list(
        'Temperature (C)' = 'temp',
        'Specific conductivity (mS/cm)' = 'spcond',
        'Salinity (psu)' = 'sal',
        'Dissolved oxyxgen (%)' = 'do_pct',
        'Dissolved oxygen (mg/L)' = 'do_mgl',
        'Depth (m)' = 'depth',
        'Referenced depth (m)' = 'level',
        'pH' = 'ph',
        'Turbidity (NTU)' = 'turb',
        'Chl fluorescence (ug/L)' = 'chlfluor'
        ),
      
      met = list(
        'Air temperature (C)' = 'atemp',
        'Relative humidity (%)' = 'rh',
        'Barometric pressure (mb)' = 'bp',
        'Wind speed (m/s)' = 'wspd',
        'Max wind speed (m/s)' = 'maxwspd',
        'Wind direction (degrees)' = 'wdir',
        'Wind direction (sd, degrees)' = 'sdwdir',
        'Total PAR (mmol/m2)' = 'totpar',
        'Total precipitation (mm)' = 'totprcp',
        'Total solar radiation (watts/m2)' = 'totsorad'
        ), 
      
      nut = list(
        'Orthophosphate (mg/L)' = 'po4f', 
        'Ammonium (mg/L)' = 'nh4f', 
        'Nitrite (mg/L)' = 'no2f', 
        'Nitrate (mg/L)' = 'no3f', 
        'Nitrite + Nitrate (mg/L)' = 'no23f', 
        'Chlorophyll (ug/L)' = 'chla_n'
        )
        
      )
  
    # select appropriate type, then remove those w/ no data
    var_sub <- vars[[type1]]

    selectInput(inputId = "parm1", label = h6('Parameter'),  
      choices = var_sub,
      selected = var_sub[[1]]
    )
   
  })
  
  # select reserve
  output$resv1 <- renderUI({
        
    resv1 <- all_dat[[input$parm1]] %>% 
      .$stat %>%
      substr(., 1, 3) %>% 
      unique %>% 
      sort
    
    selectInput("resv1", label = h6('Reserve'), 
      choices = resv1,
      selected = 'del')  
    
  })
  
  # checkbox of stations at the reserve, given parameter
  output$stsl1 <- renderUI({

    # select the stations based on reserve, parameter inputs
    parm1 <- input$parm1
    resv1 <- input$resv1
    if(!is.null(resv1)){
      stsl1 <- all_dat[[parm1]] %>% 
        filter(grepl(paste0('^', resv1), stat)) %>% 
        .$stat %>% 
        unique
    }
    checkboxGroupInput('stsl1', label = h6('Stations'), choices = stsl1, selected = stsl1, inline = TRUE)
    
  })
  
  ## for second plot
  # parameter to plot
  output$parm2 <- renderUI({
  
    type2 <- input$type2
  
    vars <- list(
    
      wq = list(
        'Temperature (C)' = 'temp',
        'Specific conductivity (mS/cm)' = 'spcond',
        'Salinity (psu)' = 'sal',
        'Dissolved oxyxgen (%)' = 'do_pct',
        'Dissolved oxygen (mg/L)' = 'do_mgl',
        'Depth (m)' = 'depth',
        'Referenced depth (m)' = 'level',
        'pH' = 'ph',
        'Turbidity (NTU)' = 'turb',
        'Chl fluorescence (ug/L)' = 'chlfluor'
        ),
      
      met = list(
        'Air temperature (C)' = 'atemp',
        'Relative humidity (%)' = 'rh',
        'Barometric pressure (mb)' = 'bp',
        'Wind speed (m/s)' = 'wspd',
        'Max wind speed (m/s)' = 'maxwspd',
        'Wind direction (degrees)' = 'wdir',
        'Wind direction (sd, degrees)' = 'sdwdir',
        'Total PAR (mmol/m2)' = 'totpar',
        'Total precipitation (mm)' = 'totprcp',
        'Total solar radiation (watts/m2)' = 'totsorad'
        ), 
      
      nut = list(
        'Orthophosphate (mg/L)' = 'po4f', 
        'Ammonium (mg/L)' = 'nh4f', 
        'Nitrite (mg/L)' = 'no2f', 
        'Nitrate (mg/L)' = 'no3f', 
        'Nitrite + Nitrate (mg/L)' = 'no23f', 
        'Chlorophyll (ug/L)' = 'chla_n'
        )
        
      )
  
    # select appropriate type, then remove those w/ no data
    var_sub <- vars[[type2]]

    selectInput(inputId = "parm2", label = h6('Parameter'),  
      choices = var_sub,
      selected = var_sub[[1]]
    )
   
  })
  
  # select reserve
  output$resv2 <- renderUI({
        
    resv2 <- all_dat[[input$parm2]] %>% 
      .$stat %>%
      substr(., 1, 3) %>% 
      unique %>% 
      sort
    
    selectInput("resv2", label = h6('Reserve'), 
      choices = resv2,
      selected = 'pdb')  
    
  })
  
  # checkbox of stations at the reserve, given parameter
  output$stsl2 <- renderUI({

    # select the stations based on reserve, parameter inputs
    parm2 <- input$parm2
    resv2 <- input$resv2
    if(!is.null(resv2)){
      stsl2 <- all_dat[[parm2]] %>% 
        filter(grepl(paste0('^', resv2), stat)) %>% 
        .$stat %>% 
        unique
    }
    checkboxGroupInput('stsl2', label = h6('Stations'), choices = stsl2, selected = stsl2, inline = TRUE)
    
  })

  ##### data
  
  ## for first plot
  dat1 <- reactive({
    
    toplo <- all_dat[[input$parm1]] %>% 
      filter(stat %in% input$stsl1)

    return(toplo)
    
  })
  dat2 <- reactive({
    
    toplo <- all_dat[[input$parm2]] %>% 
      filter(stat %in% input$stsl2)

    return(toplo)
    
  })

  #### plot, table
  
  ## first plot
  # whole plot 
  output$outplot1 <- renderPlot({

    plotInput1()
    
  }, height = 350, width = 1200)
  
  plotInput1 <- function(){

    # boolean points/lines
    pts <- !is.null(input$pts)
    lns <- !is.null(input$lns)
    
    # output
    plo_fun(dat1(), aggby = input$aggby, rng = input$years, pts = pts, lns = lns)
    
  }
  
  # second plot 
  output$outplot2 <- renderPlot({

    plotInput2()
    
  }, height = 350, width = 1200)
  
  plotInput2 <- function(){

    # boolean points/lines
    pts <- !is.null(input$pts)
    lns <- !is.null(input$lns)
    
    # output
    plo_fun(dat2(), aggby = input$aggby, rng = input$years, pts = pts, lns = lns)
    
  }
  
# 
#   # tabular data
#   tabInput <- function(){
#       
#     # input from ui
#     stat <- input$stat
#     var <- input$var
#     years <- input$years
#     
#     # output
#     plot_summary(dat(), var, years, sum_out = TRUE)
#     
#     }
#   
#   output$outplot <- renderPlot({
#   
#     plotInput()
#     
#     }, height = 600, width = 1100)
#   
#   # table 
#   output$outtab_sum_mo <- renderDataTable({
#     tabInput()$sum_mo
#     })
# 
#   ## downloads
#   
#   # plot
#   output$downloadplot <- downloadHandler(
#     filename = function() { paste(input$stat, '.pdf', sep='') },
#     content = function(file) {
#     
#       pdf(file, width = input$width, height =input$height, family = 'serif')
#       plotInput()
#       dev.off()
#       
#     }
#   )
# 
#   # table 
#   output$tab_mo <- downloadHandler(
#     filename = function() { paste(input$stat, '_tab.csv', sep='') },
#     content = function(file) {
#   
#       write.csv(tabInput()$sum_mo, file, quote = F, row.names = F)
#       
#    }
#   )

})