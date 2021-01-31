library(shiny)
library(shinyBS)
library(dplyr)
library(shinyCustom)

# max year from data
data(all_dat)
yrmax <- bind_rows(all_dat) %>%
  .$year %>%
  max(na.rm = TRUE) %>%
  as.numeric

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyCustom(slider_delay = '10'),
  
  # Application title
  h2("Aggregation of SWMP parameters within/between reserves"),
  
  h4('Created by Marcus W. Beck,', a('mbeck@tbep.org', href = 'mailto:mbeck@tbep.org'), "Todd O'Brien,", a('todd.obrien@noaa.gov', href = 'mailto:todd.obrien@noaa.gov')),
  
  p('This interactive widget can be used to compare time series of site data within and between reserves from the System Wide Monitoring Program of the National Estuarine Research Reserve System ', a('(NERRS).', href = 'http://www.nerrs.noaa.gov/', target = '_blank'), 'Data are based on monthly averages of raw observations through December 2020 and are current as of January 30, 2021. Two plots are shown for selected parameters and reserves that include time series of all sites at each location.  The monthly averages are shown by default. Data can also be viewed as quarterly (every three months) or annual aggregations based on averages of the monthly summaries. Tabular data for each plot can be viewed on the tables tab and downloads of the plots and tables are available on the downloads tab. See the', a('GitHub repository', href='https://github.com/fawda123/swmp_agg', target = '_blank'), 'for source code or to post', a('issues', href='https://github.com/fawda123/swmp_agg/issues', target = '_blank'), 'if problems occur.'),
  
  # buttons on top
  fluidRow(
    
    column(2,
      selectInput('aggby', h6('Aggregate by'), 
        choices = c('month', 'quarters', 'year'),
        selected = 'month'
      )

    ),
    
    column(3,
      customSliderInput("years", label = h6('Date range'),  
        min = 1995, max = yrmax, 
        value = c(1995, yrmax),
        sep = '', ticks = TRUE
      )
      
    ), 
    
    column(1, 
      h6('Aesthetics'),
      checkboxGroupInput('lns', label = NULL, choices = 'Lines', selected = 'Lines', inline = TRUE),
      checkboxGroupInput('pts', label = NULL, choices = 'Points', selected = 'Points', inline = TRUE)
    ), 
    
    column(1, 
      h6('Y-axis'),
      checkboxGroupInput('axs', label = NULL, choices = 'Common', inline = TRUE)
    ), 
    
    column(3, 
      h6(''),
      uiOutput('ylims')
    )
    
  ),
  
  fluidRow(
    
    tabsetPanel(
      
      tabPanel('Plots',
    
        ## first
        column(2, 
          selectInput('type1', label = h6('Parameter type'), 
            choices = c('Water quality' = 'wq', 'Nutrients' = 'nut', 'Meteorology' = 'met'),
            selected = 'wq')  
        ),
        
        column(3, 
          uiOutput("parm1")
        ),
        
        column(1, 
          uiOutput("resv1")
        ),
        
        column(6, 
          uiOutput("stsl1")
        ),
    
        column(12, 
          plotOutput("outplot1", width = "100%")
        ),

        ## second    
        column(2, 
          selectInput('type2', label = h6('Parameter type'), 
            choices = c('Water quality' = 'wq', 'Nutrients' = 'nut', 'Meteorology' = 'met'),
            selected = 'wq')  
        ),
        
        column(3, 
          uiOutput("parm2")
        ),
        
        column(1, 
          uiOutput("resv2")
        ),
        
        column(6, 
          uiOutput("stsl2")
        ),
        
        column(12, 
          plotOutput("outplot2", width = "100%")
        ),
        
        tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }")
        
      ), 

      tabPanel('Tables',
        
        column(6, 
          h6('First reserve'),
          dataTableOutput('tab1')
        ), 
        
        column(6, 
          h6('Second reserve'),
          dataTableOutput('tab2')
        )
        
      ),
      
      tabPanel('Downloads',
        
        column(6, 
          h6('First reserve'), 
          numericInput('height1', 'Plot height (in)', value = 4, min = 0, step = 1),
          numericInput('width1', 'Plot width (in)', value = 10, min = 0, step = 1),
          p(),
          downloadButton('downloadplot1', 'Download plot'),
          p(),
          downloadButton('tabsv1', 'Download table')
        ), 
        
        column(6, 
          h6('Second reserve'), 
          numericInput('height2', 'Plot height (in)', value = 4, min = 0, step = 1),
          numericInput('width2', 'Plot width (in)', value = 10, min = 0, step = 1),
          p(),
          downloadButton('downloadplot2', 'Download plot'),
          p(),
          downloadButton('tabsv2', 'Download table')
        )

      )
      
  ))
    
))