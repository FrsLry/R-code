### Data management
rm(list = ls())
library(readxl)
library(stringr)

####### S004 #####
S004 <- read_xlsx("data/bird_data/S004.xlsx", sheet = 1, skip = 2)
S004 <-
data.frame(Site = S004$Site, 
           TimeSeries_id = 4,
           ## as there are several sampling per year, I create a temporal index finer than the year
           Year = str_split(S004$`Sampling date`, "/", simplify = T) %>% as_tibble() %>% mutate_all(as.numeric) %>% mutate(V2 = V2/12) %>% mutate(V1 = V1+V2) %>% pull(var=1),
           Taxon = S004$Taxon,
           Density = S004$`Density (ind/m²)`)


### S011-S019 #####
id <- c(14, 15, 16, 17, 12, 18, 19, 11, 13)


for(i in id){
  assign(paste0("S", 0, i),
         read_xlsx("data/bird_data/S011-S019.xlsx", sheet = which(i == id)+1, skip = 2))
}

for(i in id){
  
  tmp_data <- get(paste0("S0", i))
  
  tmp_data <- 
  assign(paste0("S0", i),
         data.frame(Site = tmp_data$Site,
                    TimeSeries_id = paste0("S0", i),
                    Year = as.numeric(format(tmp_data$`Sampling date`, format = "%Y")),
                    Taxon = tmp_data$Taxon,
                    Density = tmp_data$Counts))
  
  if(all.equal(seq(from = min(tmp_data$Year),
                   to = max(tmp_data$Year)),unique(tmp_data$Year)) == TRUE){
    
    next
    
  } else {
    
    missing_years <- seq(from = min(tmp_data$Year),
                         to = max(tmp_data$Year))[!seq(from = min(tmp_data$Year),to = max(tmp_data$Year)) %in% unique(tmp_data$Year)]
    
    for(ii in 1:length(missing_years)){
      
      tmp_data <- 
        rbind(tmp_data[1:max(which(tmp_data$Year == missing_years[ii]- 1)),],
              c(NA, paste0("S0", i), missing_years[ii], NA, NA),
              tmp_data[(max(which(tmp_data$Year == missing_years[ii]-1))+1):nrow(tmp_data),])
      
      tmp_data <- 
        tmp_data %>% mutate(TimeSeries_id = TimeSeries_id,
                            Year = as.numeric(Year),
                            Density = as.numeric(Density))
      
      assign(paste0("S0", i),
             tmp_data)
    }
    
    
  }
}

# Save everything in one file ####
save.image(file = "data/modified_data.Rdata")








                                                                             