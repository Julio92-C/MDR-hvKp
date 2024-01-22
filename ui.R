#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loaded packages
library(shiny)
library(shinythemes)
library(plotly)
library(gt)
library(shinydashboard)
library(shinydashboardPlus)


# Project Path: C:/Users/peros/Documents/MDR-hvKp/MDR-hvKp

theme = shinythemes::shinytheme("united")  # <--- Specify theme here

# Global variables
source("modules/input_csvFile_Module.R", local = T)
source("modules/dataPlot_Module.R", local = T)
source("modules/dataStats_Module.R", local = T)

source("modules/mdr_dataPlot_Module.R", local = T)
source("modules/mdr_dataStats_Module.R", local = T)


# Define UI for application that draws a histogram
dashboardPage(
  skin = "blue-light",
  dashboardHeader(title = "MDR-hvkp"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Skill set", tabName = "skill", icon = icon("user")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope")),
      menuItem("Blog", tabName = "blog", icon = icon("book")),
      menuItem("Resume", tabName = "resume", icon = icon("file-pdf")),
      menuItem("Social media", tabName = "social", icon = icon("users")),
      menuItem("Tutorials", tabName = "tutorials", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
   dashboardBody(
    tags$style(".content-wrapper {overflow-y: hidden !important; }"),
    tabItems(
      tabItem(tabName = "home",
              h1("Welcome to MDR-hvkp"),
              h2("This is a web application for the MDR-hvkp package"),
              h3("You can use this application to explore the package"),
              h4("Please select a tab to begin")
      ),
       
       tabItem(tabName = "dashboard",
               tabBox(
                 # title = "Welcome to dashboad section",
                 width = 12,
                 side = "right", 
                 height = "250px",
                 selected = "MDR-hvkp",
                 
                 # <--- AMR Profile Shiny App --->
                 tabPanel("AMR Profile", 
                            # includeHTML("www/tabs_mainPanel.html"),
                                         tabsetPanel(
                                           tabPanel("Description",
                                                    fluidRow(
                                                      column(4,
                                                             br(),
                                                              tags$img(src="arg_environment.png", alt= "ARG Environment", width = "100%", height= "auto")
                                                            
                                                            ),

                                                      column(8,
                                                             br(),
                                                             box(
                                                               width = 12,
                                                               status = "teal",
                                                               solidHeader = TRUE,
                                                               title = "Shiny Web Application to Explore AMR profile",
                                                               tags$p("It is an interactive and user-friendly web application using the Shiny
                                                                     framework to explore the antimicrobial resistance (AMR) profiles
                                                                     of various microorganisms. The application should allow users to
                                                                     input data and generate visualizations that help them understand
                                                                     the resistance patterns of different antibiotics and how they vary
                                                                     across different strains of microorganisms.")
                                                                 ),
                                                               br(),
                                                               box(width = 12, 
                                                                   solidHeader = TRUE, 
                                                                   status = "teal",
                                                                 title = "ARMA alignment for pathogen identification and resistance gene detection",
                                                                 tags$ul(
                                                                   tags$li("The ONT 'What's in my pot?' (WIMP)1 Metrichor application identifies the pathogen in real-time, using a reference database and centrifuge"),
                                                                   tags$li("Metrichor's Antimicrobial Resistance Mapping Application (ARMA)2 for real-time detection of antibiotic resistance genes using CARD database."),
  
                                                                 )
                                                               )
                                                      )
                                                    )
                                       
                                                  ),
                                           
                                          # <--- Tab Data Start ---> 
                                          tabPanel("Data",
                                                   br(),
                                                   # Import csvFileUI Module
                                                   csvFileUI("amr_dataset")
                                                  ), 
                                          # <--- Tab Data End --->
                                          
                                          # <--- Tab Data Start --->
                                          tabPanel("Plots",
                                                   br(),
                                                   dataPlotUI("amr_dataPlot")
                                                   
                                               ),
                                                
                                      tabPanel("Stats Analysis",
                                              br(),
                                              dataStatsUI("amr_dataStats")
                                              
                                       
                                          )
                                       
                                      

                                  )

                           
                          ), # <--- AMR Profile Shiny App End --->
                 
                 
                 # <--- Biodiversity Shiny App start --->
                 
                 tabPanel("Poland Biodiversity",
                          
                          tabsetPanel(
                            tabPanel("Description",
                                     fluidRow(
                                       column(4,
                                              br(),
                                              
                                              tags$img(src = "Biodiversity_poland.png", alt = "Biodiversity", width = "100%", height= "auto")
                                              
                                       ),
                                       
                                       column(8,
                                              br(),
                                              box(
                                                width = 12,
                                                status = "teal",
                                                solidHeader = TRUE,
                                                title = "Biodiversity of species occurrences in Poland",
                                                tags$p("The demo application is designed to provide a comprehensive visual representation of observed species on a map. The data used in the application is sourced from the Global Biodiversity Information Facility, which is known for its extensive and reliable datasets covering the entire world. However, for this particular application, only observations from Poland have been used to create the visualizations."),
                                                #br(),
                                                tags$h5("Features:"),
                                                tags$ul(
                                                  tags$li("The application offers a range of features that allow users to explore the data in detail. For instance, users can search for species by their vernacular name or scientific name, making it easier to find specific species of interest. Additionally, the application provides a timeline visualization that shows when selected species were observed, allowing users to track the changes in the population of a particular species over time."),
                                                  tags$li("Overall, the application provides a user-friendly and interactive way to explore the biodiversity of Poland, with graphical representations of the data in the form of maps and charts.")
                                                )
                                              )
                                       )
                                  )
                              
                            ),
                            
                            tabPanel("Data",
                                             br(),
                                            # Import csvFileUI Module for Poland Biodiversity Data
                                            csvFileUI("poland_biodata")
                                           
                                 ),
                            
                            
                            tabPanel("Plots",
                                     
                                     ),
                            
                            tabPanel("Stats Analysis",
                                     
                                     
                                     )
                          )
                            
                            

                            # <--- Biodiversity Shiny App End--->
                          
                          ),
                 
                 # <--- MDR-hvkp start Session--->
                 tabPanel("MDR-hvkp", 
                          tabsetPanel(
                            tabPanel("Description",
                                       fluidRow(
                                         column(6,
                                                br(),
                                                tags$img(src = "AMR_Pathogens.png", alt = "AMR-Pathogens", width = "100%", height= "auto")
                                                
                                         ),
                                         
                                         column(6,
                                                br(),
                                                box(
                                                  width = 12,
                                                  status = "teal",
                                                  solidHeader = TRUE,
                                                  title = "Genomic diversity and population structure analysis of MDR-hvkp",
                                                  tags$p("The spread of antimicrobial-resistant bacteria globally is a pressing issue that has captured my attention."),
                                                  tags$ul(
                                                    tags$li("It's concerning to see how certain bacteria are becoming resistant to the drugs we use to treat them, leading to the rise of superbugs that are difficult to control."),
                                                    tags$li("In 2017, WHO priority pathogens: A list of antibiotic-resistant bacteria assessed to be of highest priority for new antibiotic development."),
                                                    tags$li("In 2019, nearly 1.3 million deaths including 140 thousand newborns were caused by AMR. This is expected to rise to 10 million deaths by 2050. "),
                                                    tags$li("By understanding the patterns and drivers of antimicrobial resistance, we can develop strategies to prevent its spread and improve patient outcomes.")
                                                  ),
                                                  tags$p("Therefore, this web-based application has been developed to facilitate the exploration of
                                              genomic diversity and population structure analysis of multi-drug resistant
                                              hypervirulent Klebsiella pneumoniae (MDR-hvkp)."),
                                              
                                              tags$p("The application is designed
                                              to provide a user-friendly interface for researchers and clinicians to analyze
                                              and visualize the genomic data of MDR-hvkp strains."),
                                              
                                              tags$p("It offers a range of tools
                                              and features to help users identify genetic variations, track the spread of the pathogen,
                                              and understand the population structure of MDR-hvkp.")
                                                )
                                         )
                                       )
                                     
                                     
                                     ),
                            tabPanel("Data",
                                     br(),
                                     # Import csvFileUI Module for MDR-hvKp dataset
                                     csvFileUI("MDR_hvKp_dataSet")
                                     
                            ),
                            
                            tabPanel("Plot",
                                     br(),
                                     # Import MDR dataPlot Module UI
                                     MDRdataPlotUI("MDR_hvKp_dataPlot")
                                     
                                     ),
                            
                            tabPanel("Stats Analysis",
                                     br(),
                                     # Import MDR dataPlot Module UI
                                     MDRdataStatsUI("MDR_hvKp_dataStat")
                                     
                                     )
                          )
                          
                          
                  )
                 # <--- MDR-hvkp End--->
                 
                 
                 
              ),
                # <--- Project Section End --->


             # <---- Dashboard end section ---->
            ),
   
       
       
       tabItem(tabName = "tutorials",
               # <---Tutorials section start --->
                    box(
                        width = 12,
                        h3("List of the tutorials:"),
                        tags$ul(
                          tags$li("GPU Basecalling-Test on Debian Linux WSL 2"),
                          tags$li("Mass screening of ARM and VFs genes"),
                          tags$li("R Shiny Docker Demo Web APP")
                        ),
                        
                        fluidRow(
                          column(4,
                                 box(width = 12,
                                     title = "GPU Basecalling-Test",
                                     #collapsible=TRUE,
                                     status = "warning",
                                     solidHeader = TRUE,
                                     descriptionBlock(
                                       text = " ",
                                     ),
                                     
                                     tags$iframe(
                                       width= "100%", height="315", src="https://www.youtube.com/embed/EEN3NGA0mf8?si=zHYDJCSHXfjy7KRS", title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share; allowfullscreen"
                                       
                                     )
                                     
                                 )
                                 
                          ),
                          
                          column(4,
                                 box(width = 12,
                                     #collapsible=TRUE,
                                     status = "warning",
                                     solidHeader = TRUE,
                                     title = "Mass screening of ARM and VFs genes",
                                     descriptionBlock(
                                       text = " ",
                                     ),
                                     
                                     tags$iframe(
                                       width="100%", height="315", src="https://www.youtube.com/embed/YWg9_HkWwLQ?si=vQULSSgg-sK1D54u", title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share; allowfullscreen"
                                     )
                                     
                                 )
                                 
                          ),
                          
                          column(4,
                                 box(width = 12,
                                     #collapsible=TRUE,
                                     status = "warning",
                                     solidHeader = TRUE,
                                     title = "R Shiny Docker Demo Web APP",
                                     descriptionBlock(
                                       text = " ",
                                     ),
                                     
                                     tags$iframe(
                                       width="100%", height="315", src="https://www.youtube.com/embed/Y_OzVC-DClY?si=nvvjq2T35nsJKBZh", title="YouTube video player", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share; allowfullscreen",
                                     )
                                     
                                 )
                                 
                          )
                        )
                        
                 ) # <--- Tutorials section end --->

      ),

      tabItem(tabName = "about",
              h1("About MDR-hvkp"),
              h2("This is a web application for the MDR-hvkp package"),
              h3("You can use this application to explore the package"),
              h4("Please select a tab to begin")
      )
    )
  )
 )




  
  
  




    
