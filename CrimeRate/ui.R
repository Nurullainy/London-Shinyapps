#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(caTools)
library(magrittr)
library(ggplot2)
library(DT)
library(tmap)
library(tmaptools)
library(leaflet)
library(sf)
library(leaflet.extras)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)

# Define UI for application that draws a bar
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Righteous|Cabin:400,700');

      h1 {
        font-family: 'Righteous', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: blue;
        text-align:center;

      }

      body{
        background-color:#636363;
      }

      img{
        max-width:100%;
 
      }
      
      #map{

        display:block;
        align:center;
        

      } 

      #crimePlot, #cityPlot{
      #   border-style:solid;
      #   border-color:white;
      # }

      #cityResults, #crimeResults{
        border-color:white;
        border-style:solid;
        color:black;
        background-color:white;
      }

      #controls{
        border-color:grey;
        border-style:solid;
        color:black;
        border-radius: 5px;
        padding:3px;
        background-color:white;
        opacity: 0.8;
      }
      
      .well{
        overflow:auto !important;

      }

      hr#main{
        	border-top: 2px solid #8c8b8b;

      }
      
      hr#sub{
      	background-color: #fff;
	      border-top: 2px dashed #8c8b8b;
      }

    "))
  ),

#Create Navigations Panel
navbarPage("London Crime Watch",
           theme = "bootstrap.css",
           
  tabPanel("Map",
           div(class ="outer",
               leafletOutput("mymap", width = "100%", height = "650px"),
               absolutePanel(id = "controls", class = "panel panel-default",
                             fixed = FALSE, draggable = TRUE, 
                             top = "11%", left = "auto", right = 30, bottom = "auto",
                             width = 210, height = "auto", cursor = "move",
                             br(),
                             p(" Please allow the map to load"),
                             p(" Hover and click on the map to see more information"),
                             selectInput("mapyear","Which year?", c("2012","2013","2014","2015","2016"), selected = "2014"),
                             br(),
                             plotOutput("rankPlot", height = "250px"),
                             plotOutput("annualPlot", height = "150px"),
                             p("Map by Data Police")))
      ),
    
    navbarMenu("Report By",
      tabPanel("Name of City",
        sidebarLayout(
          sidebarPanel(
            uiOutput("cityOutput"),
            selectInput("cityyear","Select Year", c("2012","2013","2014","2015","2016"), selected = "2014"),
            width = 3,
            hr(id="main"),
            radioButtons(inputId = "cityD", label = "Select the file type to download graph", choices = list("png", "pdf")),
            downloadButton("cityPlot_download")),
                         
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("cityPlot"),
            
            br(),br(),
            dataTableOutput("cityResults")
            )
          )
        ),
      tabPanel("Type of Crime",
        sidebarLayout(
          sidebarPanel(
            uiOutput("majorOutput"),
            selectInput("crimeyear","Select Year", c("2012","2013","2014","2015","2016"), selected = "2014"),
            width = 3,
            hr(id="main"),
            radioButtons(inputId = "crimeD", label = "Select the file type to download graph", choices = list("png", "pdf")),
            downloadButton("crimePlot_download")
            ),
                         
        # Show a plot of the generated distribution
          mainPanel(
            
            plotOutput("crimePlot"),
            br(),br(),
            dataTableOutput("crimeResults")
            )
          )
       )
    ),
  
  tabPanel("Documentation",
           fluidRow(
             column(12,
                    wellPanel(
                      tags$h3(tags$b("About Us")),
                      tags$p("This Shiny application is developed by Data Police to fulfill the requirement of 
                        Principal of Data Science (WQD7001) course.
                        The purpose of this application is to visualise all the descriptive analysis done with
                        the dataset more efficiently. 
                        In this R shiny application, we included several features
                        like choropleth map, line chart and bar charts to visualise our result."),
                      tags$p("Title: Decsriptive Analysis of the London Crime Rate"),
                      tags$p("Developers: Mazlini Halini, Amira An-Nur, Aminah Sofia and Anis Afifi"),
                      tags$p("Tools: R Programming Language and RStudio"),
                      tags$p("Source Code: ", tags$a(href="https://www.facebook.com", "Click to get source code")),
                      tags$p("Data Source: ", br(), a("1. Kaggle Dataset: London Crime Data ", 
                                           href = "https://www.kaggle.com/LondonDataStore/london-crime",
                                           target ="_blank"), 
                        br(),
                        a("2. Data.uk.gov: Local Authority Districts (December 2016) Full Clipped Boundaries in the UK WGS84 ",
                          href = "https://data.gov.uk/dataset/759f6fcb-934a-464e-a45c-eced2f5fcf67/local-authority-districts-december-2016-full-clipped-boundaries-in-the-uk-wgs84",
                          target = "_blank"), 
                        br(),
                        a("3. London Datastore: Statistical GIS Boundary Files for London ",
                          href = "https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london",
                          target = "_blank")),
                      br(),
                      hr(id="main"),
                      tags$h3(tags$b("Documentation")),
                      br(),
                      column(12,
                        column(6,img(src = 'Map.png', id="map")),
                        column(6,h4("Map"),p("The choropleth map use different shading and coloring to 
                                              display the total number of crimes. The user able to select category of 
                                             crime and year to customize and display the result on the map.
                                             Number of crimes and percentage of crime are shown where the 
                                             user clicks the city on the map."))),
                      column(12,hr(id="sub"),
                            column(6,img(src = 'CityCrime.JPG', id="cityCrime")),
                            column(6,h4("Type of Crimes and Total Crimes Based on the Selected City"),p("Data of selected city and selected year is visualized in bar graph. The legends on the right side shows the type
                                        of crimes in UK. The graph is downloadable."))),
                      column(12,hr(id="sub"),
                            column(6,img(src = 'MajorCrime.JPG', id="majorCrime")),
                            column(6,h4("City and Total Crimes Based on the Selected Type of Crime"), p("Data of selected crime and selected year is visualized in bar graph. The legends on the right side shows the type 
                                        cities in UK. The graph is downloadable."))),
                      column(12,hr(id="sub"),
                            column(6,img(src = 'Table.JPG', id="table")),
                            column(6,h4("Data"),p("The data is displayed in the table and the user able to search within the table.")))

                    )
                  )
                )
      )
  
  ))
)
