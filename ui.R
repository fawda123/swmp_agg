library(shiny)
library(shinyBS)

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # Application title
  h2("Agregation of SWMP parameters within reserves"),
  
  h4('Created by Marcus W. Beck,', a('beck.marcus@epa.gov', href = 'mailto:beck.marcus@epa.gov'), "Todd O'Brien,", a('todd.obrien@noaa.gov', href = 'mailto:todd.obrien@noaa.gov')),
  
  p('This interactive widget provides .... from the System Wide Monitoring Program of the National Estuarine Research Reserve System ', a('(NERRS).', href = 'http://www.nerrs.noaa.gov/', target = '_blank'), '....The data include observations through December 2015 and are current as of January 2016.  Plots are based on daily averages for each parameter. See the', a('GitHub repository', href='https://github.com/fawda123/swmp_agg', target = '_blank'), 'for source code or to post', a('issues.', href='https://github.com/fawda123/swmp_agg/issues', target = '_blank')),
  
  # buttons on top
  fluidRow(
    
    column(2,
      selectInput('aggby', h5('Aggregation'), 
        choices = c('month', 'quarters', 'year'),
        selected = 'year'
      )

    ),
    
    column(3,
      sliderInput("years", label = h6('Range'),  
        min = 1995, max = 2015, 
        value = c(1995, 2015),
        sep = '', ticks = TRUE
      )
      
    ), 
    
    column(1, 
      h6(''),
      checkboxGroupInput('lns', label = '', choices = 'Lines', selected = 'Lines', inline = TRUE)
    ),
    
    column(1, 
      h6(''),
      checkboxGroupInput('pts', label = '', choices = 'Points', selected = 'Points', inline = TRUE)
    )
    
  ),
  
  fluidRow(
    
    tabsetPanel(
      
      tabPanel('Plot',
    
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
        
      )#, 
# 
#       tabPanel('Tabular output',
#         
#         column(12, 
#           p('Tabular output.'),
#           dataTableOutput('outtab')
#         )
#         
#       ), 
# 
#       tabPanel('Downloads',
#         
#           column(7, 
#             h4('Figure'), 
#             column(6, 
#               numericInput('height', 'Plot height (in)', value = 8, min = 0, step = 1),
#               numericInput('width', 'Plot width (in)', value = 13, min = 0, step = 1)
#               ),
#             column(6, 
#               HTML('<p></p>'),
#               downloadButton('downloadplot', 'Plot')
#               )
#             ),
#           column(4, h4('Table'),
#             column(4, 
#               p(), 
#               downloadButton('tab', 'Table')
#             )
#           )
#         
#       )
      
  ))
    
))