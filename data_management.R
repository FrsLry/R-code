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
           Year = str_split(S004$`Sampling date`, "/", simplify = T) %>% as_tibble() %>% mutate_all(as.numeric) %>% mutate(V2 = V2/12) %>% mutate(V1 = V1+V2-1) %>% pull(var=1),
           Taxon = S004$Taxon,
           Density = S004$`Density (ind/m²)`)


### S011-S019 #####
for(i in 2:10){
  assign(paste0("S", 0, i+9),
         read_xlsx("data/bird_data/S011-S019.xlsx", sheet = i, skip = 2))
}



## Save everything in one file ####
save.image(file = "data/modified_data.Rdata")




