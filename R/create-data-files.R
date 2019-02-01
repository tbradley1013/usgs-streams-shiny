#===============================================================================
# Query meta data for PA Streams data shiny app 
# 
# Tyler Bradley 
# 2019-01-11
#===============================================================================

library(tidyverse)
library(dataRetrieval)
library(feather)

pa_streams <- whatNWISsites(stateCd = "PA", siteType = "ST", site_output = "basic")

safe_nwis_sites <- purrr::possibly(whatNWISsites, otherwise = NULL)

all_stream_sites <- stateCd$STUSAB %>% 
  map_dfr(~{
    
    output <- safe_nwis_sites(stateCd = .x, siteType = "ST", site_output = "basic", 
                              hasDataType = "dv", siteStatus = "active")
    
    if (is.null(output)){
      output <- tibble(state = .x)
      cat("Failed: ", .x, "\n")
    } else {
      output <- output %>% 
        mutate(state = .x)
      cat("Success: ", .x, "\n")
    }
    
    
    return(output)
  
      
  })


all_stream_sites %>% 
  as_tibble() %>% 
  filter(!is.na(site_no)) %>% 
  write_feather("data/usgs-stream-sites.feather")




## create a parameter reference list - from master file downloaded from 
## https://help.waterdata.usgs.gov/codes-and-parameters/parameters
parameter_df <- read_tsv("data/parameter_cd_query-raw.txt", skip = 7) %>% 
  select(parm_cd, parm_nm) %>% 
  filter(row_number() > 1)


write_feather(parameter_df, "data/usgs-parameters.feather")
