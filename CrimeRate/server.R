#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

load("LondonCrime.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ukmap = reactive({
    if (input$mapyear == "2012"){
      ukmap2012
    }
    else if (input$mapyear == "2013") {
      ukmap2013
    }
    else if(input$mapyear == "2014"){
      ukmap2014
    } else if (input$mapyear == "2015") {
      ukmap2015
    } else {
      ukmap2016
    }
  }) 
  
  # Create Popup for each Categories of Crime--------------------------------------------------------------

  ukpopup = reactive({
    paste0("In the borough of </><b> ", ukmap()$City, "</b> there were <b>", ukmap()$Total, "</b> offences in the selected time range", 
           "<br /> The top crime in this borough is <b>", ukmap()$Top.Crime, "<br><br /></b>Number of Cases for Each Crime: ", 
           "<br /><b> Burglary: ", ukmap()$Burglary, "<br /><b> Drugs: ", ukmap()$Drugs, "<br /><b> Robbery: ", ukmap()$Robbery,
           "<br /><b> Criminal Damage: ", ukmap()$Vandalism, "<br /><b> Theft and Handling: ", ukmap()$Theft,
           "<br /><b> Violence Against the Person: ", ukmap()$Violence, "<br /><b> Other Notifiable Offences: ", ukmap()$Other) %>% lapply(htmltools::HTML)
  })
  
  rpopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Robbery <br />Number of Crime: ", ukmap()$Robbery, 
           "<br />Percentage of Crime: ", ukmap()$RobberyPct) %>% lapply(htmltools::HTML)
  }) 
  
  vpopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Criminal Damage <br />Number of Crime: ", ukmap()$Vandalism, 
           "<br />Percentage of Crime: ", ukmap()$VandalismPct) %>% lapply(htmltools::HTML)
  }) 
  
  dpopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Drugs <br />Number of Crime: ", ukmap()$Drugs, 
           "<br />Percentage of Crime: ", ukmap()$DrugsPct) %>% lapply(htmltools::HTML)
  }) 
  
  bpopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Burglary <br />Number of Crime: ", ukmap()$Burglary, 
           "<br />Percentage of Crime: ", ukmap()$BurglaryPct) %>% lapply(htmltools::HTML)
  }) 
  
  viopopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Violence Against the Person <br />Number of Crime: ", ukmap()$Violence, 
           "<br />Percentage of Crime: ", ukmap()$ViolencePct) %>% lapply(htmltools::HTML)
  }) 
  
  tpopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Theft and Handling <br />Number of Crime: ", ukmap()$Theft, 
           "<br />Percentage of Crime: ", ukmap()$TheftPct) %>% lapply(htmltools::HTML)
  }) 
  
  opopup = reactive({
    paste0("<b>", ukmap()$City, "<br /><b>Other Notifiable Offences <br />Number of Crime: ", ukmap()$Other, 
           "<br />Percentage of Crime: ", ukmap()$OtherPct) %>% lapply(htmltools::HTML)
  }) 
  
  
  # Create Palette for each Categories of Crime--------------------------------------------------------------
  
  crimepal = reactive({
    c("Burglary" = "#3182bd", "Drugs" = "#31a354", "Robbery" = "#c51b8a", "Criminal Damage" = "#1c9099",
      "Theft and Handling" = "#de2d26", "Violence Against the Person" = "#e6550d",
      "Other Notifiable Offences" = "#2c7fb8")
  }) 
  
  totpal = reactive({
    colorNumeric(palette ="Spectral", domain = ukmap()$Total)
  })
  
  bpal = reactive({
    colorNumeric(palette = "Blues", domain = ukmap()$BurglaryPct)
  })
  
  dpal = reactive({
    colorNumeric(palette = "Greens", domain = ukmap()$DrugsPct)
  })
  
  rpal = reactive({
    colorNumeric(palette = "RdPu", domain = ukmap()$RobberyPct)
  })
  
  vpal = reactive({
    colorNumeric(palette = "PuBuGn", domain = ukmap()$VandalismPct)
  })
  
  tpal = reactive({
    colorNumeric(palette = "Reds", domain = ukmap()$TheftPct)
  })
  
  viopal = reactive({
    colorNumeric(palette = "Oranges", domain = ukmap()$ViolencePct)
  })
  
  opal = reactive({
    colorNumeric(palette = "YlGnBu", domain = ukmap()$OtherPct)
  })
  
  # Create Interactive Map------------------------------------------------------------------------------------
  
  output$mymap = renderLeaflet({
    
    leaflet(data = ukmap()) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      fitBounds(min(ukmap()$long), min(ukmap()$lat), max(ukmap()$long), max(ukmap()$lat)) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup = ukpopup(), 
                  color = ~totpal()(ukmap()$Total),
                  group = "Overview"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= bpopup(), 
                  color = ~bpal()(ukmap()$BurglaryPct),
                  group = "Burglary"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= dpopup(), 
                  color = ~dpal()(ukmap()$DrugsPct),
                  group = "Drugs"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= rpopup(), 
                  color = ~rpal()(ukmap()$RobberyPct),
                  group = "Robbery"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= vpopup(), 
                  color = ~vpal()(ukmap()$VandalismPct),
                  group = "Criminal Damage"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= tpopup(), 
                  color = ~tpal()(ukmap()$TheftPct),
                  group = "Theft and Handling"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= viopopup(), 
                  color = ~viopal()(ukmap()$ViolencePct),
                  group = "Violence Against the Person"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 0.75, 
                  popup= opopup(),
                  color = ~opal()(ukmap()$OtherPct),
                  group = "Other Notifiable Offences"
      ) %>%
      addLayersControl(
        baseGroups=c("Overview","Burglary", "Robbery", "Drugs", "Criminal Damage", "Theft and Handling", 
                     "Violence Against the Person", "Other Notifiable Offences"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  observeEvent(input$mymap_groups, {
    mymap = leafletProxy("mymap", data = ukmap())
    mymap %>% clearControls()
    if (input$mymap_groups == "Overview"){
      mymap %>%
        addLegend(position = "bottomleft", title ="Number of Offences", pal = totpal(), values = ukmap()$Total, opacity = 0.5)
    }
    else if (input$mymap_groups == "Burglary") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Burglary", pal = bpal(), values = ukmap()$BurglaryPct, opacity = 0.5)
    }
    else if (input$mymap_groups == "Drugs") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Drugs", pal = dpal(), values = ukmap()$DrugsPct, opacity = 0.5)
    }
    else if (input$mymap_groups == "Robbery") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Robbery", pal = rpal(), values = ukmap()$RobberyPct, opacity = 0.5)
    } 
    else if (input$mymap_groups == "Criminal Damage") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Criminal Damage", pal = vpal(), values = ukmap()$VandalismPct, opacity = 0.5)
    }
    else if (input$mymap_groups == "Theft and Handling") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Theft and Handling", pal = tpal(), values = ukmap()$TheftPct, opacity = 0.5)
    }
    else if (input$mymap_groups == "Violence Against the Person") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Violence Against the Person", pal = viopal(), values = ukmap()$ViolencePct, opacity = 0.5)
    }
    else if(input$mymap_groups == "Other Notifiable Offences") {
      mymap %>%
        addLegend(position = "bottomleft", title = "% Other Notifiable Offences", pal = opal(), values = ukmap()$OtherPct, opacity = 0.5)
    }
    
  })
  
  output$cityOutput = renderUI({
    selectInput("cityInput", "Choose a City",
                sort(unique(crimefile$City)),
                selected = "Croydon")
  })
  
  output$yearsOutput = renderUI({
    selectInput("yearsInput", "Which Year?",
                sort(unique(crimefile$Year)),
                selected = "2014")
  })
  
  output$majorOutput = renderUI({
    selectInput("majorInput", "Type of Major Crime",
                choices = c("Burglary", "Drugs", "Robbery", "Criminal Damage", "Theft and Handling", 
                            "Violence Against the Person", "Other Notifiable Offences"),
                selected = "Drugs")
  })
  
  filtered1 = reactive({
    if(is.null(input$cityInput)){
      return(NULL)
    }
    
    crimefile %>%
      filter(City == input$cityInput,
             Year == input$cityyear,
             Value > 0) %>% 
      rename(AreaCode = LAD11CD)
  })
  
  filtered2 = reactive({
    if(is.null(input$majorInput)){
      return(NULL)
    }
    
    crimefile %>%
      filter(Major == input$majorInput,
             Year == input$crimeyear,
             Value > 0) %>% 
      rename(AreaCode = LAD11CD)
  })
  
  rank = reactive({
    if(is.null(input$mapyear)){
      return(NULL)
    }
    
    crimefile = crimefile %>% 
      filter(Year == input$mapyear, Major != "Sexual Offences", Major !="Fraud or Forgery") %>%
      group_by(Major) %>%
      summarise(Value = sum(Value)) %>%
      select(Major,Value)
     
    crimefile$Major <- factor(crimefile$Major, levels = crimefile$Major[order(crimefile$Value, decreasing = TRUE)])
    crimefile
  })
  
  annual = reactive({
    crimetype %>%
      filter(Year >= 2012) %>%
      group_by(Year) %>%
      summarise(Burglary = sum(Burglary), Vandalism = sum(Vandalism), Drugs = sum(Drugs), Robbery = sum(Robbery),
                Theft = sum(Theft), Violence = sum(Violence), Other = sum(Other), Total = sum(Total)) %>%
      select(Year, Burglary, Vandalism, Drugs, Robbery, Theft, Violence, Other, Total)
  })
  
  output$annualPlot = renderPlot({
    if(is.null(rank())) {
      return()
    }
    
    ggplot(annual(), aes(x = Year, y = Total, group = 1)) +
      geom_line(color = "red") +
      geom_point() +
      theme_bw() +
      ggtitle("Crime Rate Trend") +
      theme(legend.position = "none",
            text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
            axis.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1),
            plot.margin = unit(c(.5,.5,.5,.2), "cm"))
  })
  
  output$rankPlot = renderPlot({
    if(is.null(rank())) {
      return()
    }
    ggplot(rank(), aes(x=Major, y = Value, fill = Major)) +
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = crimepal()) +
      theme_classic() +
      ggtitle("Crime Rank for The Year") +
      theme(legend.position = "none",
            text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
            axis.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1),
            plot.margin = unit(c(.5,.5,.5,.2), "cm"))
  })
  
  output$cityPlot = renderPlot({
    if(is.null(filtered1())){
      return()
    }
    ggplot(filtered1(), aes(x = Major, y = Value, label=Value, fill=Major))+
      geom_bar(stat = "identity", color="black") +
      geom_text(position = position_stack(vjust = 0.5))+
      theme_bw() +
      ggtitle("Major Crime and Total Crimes") +
      theme(legend.position="top", plot.title = element_text(hjust = 0.5))+
      labs(x="Major Crime",y="Total Crimes") 
  })
  
  output$crimePlot = renderPlot({
    if(is.null(filtered2())){
      return()
    }
    ggplot(filtered2(), aes(x = City,y = Value, label=Value, fill=City))+
      geom_bar(stat = "identity", color="black")+coord_flip() +
      geom_text(    hjust = 1, 
                    position = position_dodge(width = 1),
                    inherit.aes = TRUE)+
      theme_bw() +
      ggtitle("City and Total Crimes") +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x="City",y="Total Crimes") 
  })
  
  output$crimePlot_download <- downloadHandler(
    filename =  function() {
      paste("Crime Plot", input$crimeD, sep=".")
    },
    content = function(file) {
      if(input$crimeD == "png")
        png(file)
      else
        pdf(file) 
      # p <- ggplot(filtered2(), aes(x = City,y = Value, fill=City))+
      #   geom_bar(stat = "identity", color="black")+coord_flip() +theme(legend.position="right") + ggtitle("Value and City")+
      #   theme(plot.title = element_text(hjust = 0.5))
      print(    ggplot(filtered2(), aes(x = City,y = Value, label=Value, fill=City))+
                  geom_bar(stat = "identity", color="black")+coord_flip() +
                  geom_text(    hjust = 1, 
                                position = position_dodge(width = 1),
                                inherit.aes = TRUE)+
                  theme_bw() +
                  ggtitle("City and Total Crimes") +
                  theme(legend.position="none",plot.title = element_text(hjust = 0.5))+
                  labs(x="Total Crimes",y="City") )
      dev.off()  
      
    } 
  )
  output$cityPlot_download <- downloadHandler(
    filename =  function() {
      paste("City Plot", input$cityD, sep=".")
    },
    content = function(file) {
      if(input$cityD == "png")
        png(file)
      else
        pdf(file) 
      print(ggplot(filtered1(), aes(x = Major, y = Value, label=Value, fill=Major))+
              geom_bar(stat = "identity", color="black") +
              geom_text(position = position_stack(vjust = 0.5))+
              theme_bw() +
              ggtitle("Major Crime and Total Crimes") +
              theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))+
              labs(x="Major Crime",y="Total Crimes"))
      dev.off()  
      
    } 
  )
  output$cityResults = renderDataTable({
    filtered1()
  }, width = '100%')
  
  output$crimeResults = renderDataTable({
    filtered2()
  }, width = '100%')
  
})
