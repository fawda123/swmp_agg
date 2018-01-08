# summarize input data, create plot
plo_fun <- function(dat_in, aggby = 'year', rng = NULL, errbar = FALSE, lims = NULL, lns = TRUE, pts = TRUE, tab = FALSE){

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
  if(aggby != 'day'){
    to_plo <- mutate(dat_in, 
      datetimestamp = as.IDate(datetimestamp), 
      datetimestamp = round(datetimestamp, digits = aggby)
      )
  } else {
    to_plo <- dat_in
  }
  to_plo <- group_by(to_plo, stat, datetimestamp) %>%
    summarize(
      ave = mean(value, na.rm = TRUE), 
      max = max(value, na.rm = TRUE), 
      min = min(value, na.rm = TRUE)
      ) %>% 
    ungroup 

  # create date rng limits if provided
  if(!is.null(rng))
    rng <- as.Date(c(paste0(rng[1], '-01-01'), paste0(rng[2], '-12-31')))
  else
    rng <- range(to_plo$datetimestamp, na.rm = TRUE)
  
  # return table if T
  if(tab){
    to_plo <- to_plo[to_plo$datetimestamp >= rng[1] & to_plo$datetimestamp <= rng[2], ]
    return(data.frame(to_plo, stringsAsFactors = FALSE))
  } 

  # plot
  p <- ggplot(to_plo, aes(x = datetimestamp, y = ave, colour = stat)) +
    scale_y_continuous(ylab) +
    scale_x_date(limits = rng) +
    theme_minimal() + 
    theme(
      legend.title = element_blank(), 
      legend.position = 'top', 
      axis.title.x = element_blank(), 
      axis.line.x = element_line(size = 0.5),
      axis.line.y = element_line(size = 0.5) 
      )
  
  # other aesthetic exceptions
  if(lns)
    p <- p + geom_line()
  if(pts)
    p <- p + geom_point(size = 2.5)
  if(pts & errbar) 
    p <- p + geom_errorbar(aes(ymin = min, ymax = max), width = 40)
  if(!is.null(lims))
    p <- p + scale_y_continuous(ylab, limits = lims)
  
  print(p)
  
}