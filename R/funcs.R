# summarize input data, create plot
plo_fun <- function(dat_in, aggby = 'year', rng = NULL, lims = NULL, lns = TRUE, pts = TRUE, tab = FALSE){

  # label lookup
  lab_look <- list(
    temp = 'Temperature (C)', 
    spcond = 'Specific conductivity (mS/cm)',
    sal = 'Salinity (psu)',
    do_pct = 'Dissolved oxyxgen (%)',
    do_mgl = 'Dissolved oxygen (mg/L)',
    depth = 'Depth (m)',
    cdepth = 'Depth (nonvented, m)',
    level = 'Referenced depth (m)',
    clevel = 'Referenced depth (nonvented, m)',
    ph = 'pH',
    turb = 'Turbidity (NTU)',
    chlfluor = 'Chl fluorescence (ug/L)',
    atemp = 'Air temperature (C)',
    rh = 'Relative humidity (%)',
    bp = 'Barometric pressure (mb)',
    wspd = 'Wind speed (m/s)',
    maxwspd = 'Max wind speed (m/s)',
    wdir = 'Wind direction (degrees)',
    sdwdir = 'Wind direction (sd, degrees)',
    totpar = 'Total PAR (mmol/m2)',
    totprcp = 'Total precipitation (mm)',
    totsorad = 'Total solar radiation (watts/m2)',
    po4f = 'Orthophosphate (mg/L)', 
    nh4f = 'Ammonium (mg/L)',
    no2f = 'Nitrite (mg/L)',
    no3f = 'Nitrate (mg/L)',
    no23f = 'Nitrite + Nitrate (mg/L)',
    chla_n = 'Chlorophyll (ug/L)'
  )
  ylab <- lab_look[[as.character(unique(dat_in$variable))]]

  # get summaries by station given agg period
  to_plo <- mutate(dat_in, 
      datetimestamp = lubridate::round_date(datetimestamp, unit = aggby)
    ) %>% 
    dplyr::group_by(stat, datetimestamp) %>%
    dplyr::summarise(
      ave = mean(value, na.rm = TRUE), 
      max = max(value), 
      min = min(value), 
      .groups = 'drop'
      ) 

  # create date rng limits if provided
  if(!is.null(rng))
    rng <- as.Date(c(paste0(rng[1], '-01-01'), paste0(rng[2], '-12-31')))
  else
    rng <- range(to_plo$datetimestamp, na.rm = TRUE)
  
  to_plo <- to_plo[to_plo$datetimestamp >= rng[1] & to_plo$datetimestamp <= rng[2], ]
  
  # return table if T
  if(tab)
    return(data.frame(to_plo, stringsAsFactors = FALSE))
           
  # Initialize empty plotly object
  p <- plot_ly()
  
  # Get unique groups
  groups <- sort(unique(to_plo[['stat']]))
  
  if(pts)
    md <- 'markers'
  if(lns)
    md <- 'lines'
  if(pts & lns)
    md <- 'lines+markers'
  
  # Add traces for each group
  for(i in seq_along(groups)) {
    group_data <- to_plo[to_plo[['stat']] == groups[i], ]

    p <- p %>% add_trace(
      data = group_data,
      x = ~datetimestamp,
      y = ~ave,
      name = groups[i],
      type = 'scatter',
      mode = md
    )

  }

  if(!is.null(lims))
    p <- p %>%  
      layout(
        yaxis = list(range = list(lims[1], lims[2]))
      )

  # disable legend click
  p <- p %>%  
    layout(
      legend = list(
        itemclick = FALSE,
        itemdoubleclick = FALSE,
        groupclick = FALSE,
        orientation = 'h', 
        xanchor = 'center', 
        x = 0.5, 
        y = 1.1
      ), 
      xaxis = list(title = '', range = list(rng[1], rng[2])),
      yaxis = list(title = ylab)
    )
    
  print(p)
  
}