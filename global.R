#==============================================================================
# This script contains the global.R code for the USGS River Data Web App
#
# Tyler Bradley
# 2019-01-14
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

`%p%` <- function(x, y) paste0(x, y)

usgs_site_info <- read_feather("data/usgs-stream-sites.feather")
# usgs_site_info <- pa_streams
parameter_df <- read_feather("data/usgs-parameters.feather")


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
        "</div>" %p%
        "</div>",
      if_else(nchar(.y) > 40, "325", "275"),
      .x, 
      .y
    )
  )
) 


