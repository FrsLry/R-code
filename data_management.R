### Data management
rm(list = ls())
library(readxl)
library(stringr)
library(dplyr)
library(janitor)
source("functions/addNArow.R", local=T)

####### S004 #####
S004 <- read_xlsx("data/bird_data/S004.xlsx", sheet = 1, skip = 2)
S004 <-
data.frame(Site = S004$Site, 
           TimeSeries_id = "S004",
           ## as there are several sampling per year, I create a temporal index finer than the year
           Year = str_split(S004$`Sampling date`, "/", simplify = T) %>% as_tibble() %>% mutate_all(as.numeric) %>% mutate(V2 = V2/12) %>% mutate(V1 = V1+V2) %>% pull(var=1),
           Taxon = S004$Taxon,
           Density = S004$`Density (ind/m²)`)


### S011-S019 #####
id <- c(14, 15, 16, 17, 12, 18, 19, 11, 13)

## Load the different sheets
for(i in id){
  assign(paste0("S", 0, i),
         read_xlsx("data/bird_data/S011-S019.xlsx", sheet = which(i == id)+1, skip = 2))
}

## When no data for a year, create a row with the Year + NAs
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



### S047 #####
S047 <- read_excel("data/bird_data/S047.xlsx", sheet = 1, skip = 2)

## Fixing date format problem
S047[1:97, 2] <-
  as.numeric(format(convert_to_date(S047$`Sampling date`[1:97], character_fun = lubridate::my),
                                   format = '%Y'))


## Now format the final dataset
S047 <- 
data.frame(Site = S047$Site, 
           TimeSeries_id = "S047",
           Year = S047$`Sampling date`,
           Taxon = S047$Taxon,
           Density = S047$`Abundance (nest = breeding female)`)

S047 <- addNArow(S047)

### S074: not usable because only abundance data of 2 species ####

### S076 #####
S076 <- read_xlsx("data/bird_data/S076.xlsx", sheet = 1, skip = 3)

## Format the data into a long version
S076 <-
S076 %>% 
  select(-TOTAL) %>% 
  filter(YEAR != "TOTAL") %>% 
  pivot_longer(2:101, names_to = "Taxon", values_to = "Density") %>% 
  mutate(Year = as.numeric(YEAR),
         TimeSeries_id = "S076",
         Site = "LTSER Zone Atelier Pyrénées Garonne - France") %>% 
  select(-YEAR)


S076 <- addNArow(S076)

# Save everything in one file ####
save.image(file = "data/modified_data.Rdata")








                                                                             