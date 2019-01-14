#==============================================================================
# This script contains the global.R code for the USGS River Data Web App
#
# Tyler Bradley
# 2018-04-06
#==============================================================================

suppressWarnings({
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinyalert)
    library(dataRetrieval)
    library(tidyverse)
    library(plotly)
    library(leaflet)
    library(sp)
  })
})


usgs_site_info <- read_rds("data/usgs-stream-sites.rds")
# usgs_site_info <- pa_streams
usgs_parameter_list <- read_rds("data/usgs-parameters.rds")


site_list <- usgs_site_info$site_no
names(site_list) <- usgs_site_info$station_nm



usgs_site_info$hover_text <- map2(
  usgs_site_info$site_no,
  usgs_site_info$station_nm,
  ~HTML(
    sprintf(
      "<div style='font-size:12px;width:%spx;float:left'>" %p%
        "<span style = 'font-size:18px;font-weight:bold;float:none;margin:0 40%%'>%s</span><br/>" %p%
        "<div style = 'width:95%%'>" %p%
        "<span style = 'font-weight:bold'>Site Name:</span><br/>" %p%
        "<span style = 'font-size:11px;font-style:italic;float:right'>%s</span><br/>" %p%
        # "<span>Facility:</span><br/>" %p%
        # "<span style = 'font-size:11px;font-style:italic;float:right'>%s</span><br/>" %p%
        # "<span>Service Area:</span><br/>" %p%
        # "<span style = 'font-size:11px;font-style:italic;float:right'>%s</span><br/>" %p%
        # "<span>Pressure District:</span><br/>" %p%
        # "<span style = 'font-size:11px;font-style:italic;float:right'>%s</span><br/>" %p%
        # "<span>Sample Days:</span><br/>" %p%
        # "<span style = 'font-size:11px;font-style:italic;float:right'>%s</span><br/>" %p%
        "</div>" %p%
        "</div>",
      if_else(nchar(.y) > 40, "325", "275"),
      .x, 
      .y
    )
  )
) 


