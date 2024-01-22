#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(forcats)
library(thematic)
library(gtsummary)
library(paletteer)
library(scales)
library(reshape2)
library(heatmaply)




# Define server logic required to draw a table
shinyServer(function(input, output) {
  thematic::thematic_shiny()

  # Import csvFile Module for AMR Profile dataset
  dataframe <- csvFileServer("amr_dataset", obs_num = obs_num)
  
  
  # Import dataPlot Module for AMR Profile dataset
  filter_data <- dataPlotServer("amr_dataPlot", dataframe = dataframe)
  
  
  # Import dataStats Module for AMR Profile dataset
  dataStatsServer("amr_dataStats", filter_data = filter_data)
  
  
 # Input csvFile Module for Poland Biodiversity datasets
 datafile <- csvFileServer("poland_biodata")
 
 # Input csvFileServer Module for MDR-hvKp dataSet
 dataframe <- csvFileServer("MDR_hvKp_dataSet", obs_num = obs_num)
 
 # Input MDRdataPlotServer Module for MDR-hvKp dataSet
 filter_data <- MDRdataPlotServer("MDR_hvKp_dataPlot", dataframe = dataframe)
 
 # input MDRdataStatsServer Module for MDR-hvKp dataSet
 MDRdataStatsServer("MDR_hvKp_dataStat", filter_data = filter_data)
 
 
 
})



