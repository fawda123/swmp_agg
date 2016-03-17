# summarize input data, create plot
plo_fun <- function(dat_in, aggby = 'year', rng = NULL, errbar = FALSE, lns = TRUE, pts = TRUE){

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
    cumprcp = 'Cumulative precipitation (mm)',
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
      med = median(value, na.rm = TRUE), 
      his = quantile(value, 0.95, na.rm = TRUE), 
      los = quantile(value, 0.05, na.rm = TRUE)
      ) %>% 
    ungroup 
  
  if(!is.null(rng))
    rng <- as.Date(c(paste0(rng[1], '-01-01'), paste0(rng[2], '-12-31')))

  # plot
  p <- ggplot(to_plo, aes(x = datetimestamp, y = med, colour = stat)) +
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
  if(lns)
    p <- p + geom_line()
  if(pts)
    p <- p + geom_point(size = 2.5)
  if(pts & errbar) 
    p <- p + geom_errorbar(aes(ymin = los, ymax = his), width = 40)
  
  return(p)
  
}