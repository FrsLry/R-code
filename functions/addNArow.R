## Create function to insert row with NA when missing year
## Data need to be already in the format
## # Site = site name, TimeSeries_id = unique identifier for the time series, Year = survey year, Taxon = taxon name, Density = total density or biomass or number of individual of that taxon for that year.
addNArow <- function(tmp_data){
  
  tmp_data <- tmp_data %>% arrange(Year)
  
  missing_years <- seq(from = min(tmp_data$Year),
                       to = max(tmp_data$Year))[!seq(from = min(tmp_data$Year),to = max(tmp_data$Year)) %in% unique(tmp_data$Year)]
  
  for(ii in 1:length(missing_years)){
    
    
    tmp_data <-
      rbind(tmp_data[1:max(which(tmp_data$Year == missing_years[ii]- 1)),],
            c(NA, paste(tmp_data$TimeSeries_id[1]), missing_years[ii], NA, NA),
            tmp_data[(max(which(tmp_data$Year == missing_years[ii]-1))+1):nrow(tmp_data),])
    
  }
  
  tmp_data <- 
    tmp_data %>% mutate(TimeSeries_id = TimeSeries_id,
                    Year = as.numeric(Year),
                    Density = as.numeric(Density))
  
  return(tmp_data)

}
