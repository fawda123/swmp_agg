# packages to use
library(ggplot2)
library(dplyr)
library(SWMPr)
library(httr)
library(XML)
library(data.table)
library(shinyBS)

# master data file
# this is the same file from swmp_comp app
load(file = 'data/all_dat.RData')

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
      selected = input$resv1)  
    
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
      selected = input$resv2)  
    
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

  # apply common axis?
  output$ylims <- renderUI({
    
    axs <- !is.null(input$axs)
    if(axs){
      
      rngs <- range(c(dat1()$value, dat2()$value), na.rm = TRUE)
      rngs <- round(rngs, 0.1)
      
      sliderInput("ylims", label = '',  
        min = min(rngs), max = max(rngs), 
        value = rngs,
        sep = '', ticks = TRUE
      )
      
    }
    
  })
  
  #### plot, table
  
  ## first plot
  # whole plot 
  output$outplot1 <- renderPlot({

    plotInput1()
    
  }, height = 350, width = 1200)
  
  plotInput1 <- function(){

    # boolean points/lines, ylims
    pts <- !is.null(input$pts)
    lns <- !is.null(input$lns)
    if(!is.null(input$axs))
      lims <- input$ylims
    else 
      lims <- NULL
    
    # output
    plo_fun(dat1(), aggby = input$aggby, rng = input$years, pts = pts, lns = lns, lims = lims)
    
  }
  
  # second plot 
  output$outplot2 <- renderPlot({

    plotInput2()
    
  }, height = 350, width = 1200)
  
  plotInput2 <- function(){

    # boolean points/lines, ylims
    pts <- !is.null(input$pts)
    lns <- !is.null(input$lns)
    if(!is.null(input$axs))
      lims <- input$ylims
    else 
      lims <- NULL
    
    # output
    plo_fun(dat2(), aggby = input$aggby, rng = input$years, pts = pts, lns = lns, lims = lims)
    
  }
  
  # output table 1
  tabInput1 <- function(){
    out <- plo_fun(dat1(), aggby = input$aggby, rng = input$years, tab = TRUE) %>% 
      select(-max, -min)
    names(out)[names(out) %in% 'ave'] <- input$parm1
    return(out)
  }
  output$tab1 <- renderDataTable({
    tabInput1()
  })
  
  # output table 2
  tabInput2 <- function(){
    out <- plo_fun(dat2(), aggby = input$aggby, rng = input$years, tab = TRUE) %>% 
      select(-max, -min)
    names(out)[names(out) %in% 'ave'] <- input$parm2
    return(out)
  }
  output$tab2 <- renderDataTable({
    tabInput2()
  })
  
  #### downloads
  
  # plot 1
  output$downloadplot1 <- downloadHandler(
    filename = function() { paste(input$resv1, '_plt.pdf', sep='') },
    content = function(file) {
    
      pdf(file, width = input$width1, height =input$height1, family = 'serif')
      plotInput1()
      dev.off()
      
   }
  )
  
  # table 1
  output$tabsv1 <- downloadHandler(
    filename = function() { paste(input$resv1, '_tab.csv', sep='') },
    content = function(file) {
      
      write.csv(tabInput1(), file, quote = F, row.names = F)
      
    }
  )
  
  # plot 2
  output$downloadplot2 <- downloadHandler(
    filename = function() { paste(input$resv2, '_plt.pdf', sep='') },
    content = function(file) {
    
      pdf(file, width = input$width2, height =input$height2, family = 'serif')
      plotInput2()
      dev.off()
      
    }
  )

  # table 2
  output$tabsv2 <- downloadHandler(
    filename = function() { paste(input$resv2, '_tab.csv', sep='') },
    content = function(file) {
  
      write.csv(tabInput2(), file, quote = F, row.names = F)
      
    }
  )

})