#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
  
  # Update AU list ####
  updateSelectizeInput(inputId = "input_AU_choice"
                       , choices = c("", AU_list)
                       , server = TRUE
                       , options=list(maxOptions=2500))
  
  # modal dialog ####
  myModal <- modalDialog(
    title = "Welcome to the Massachusetts Natural Background Conditions App!"
    ,HTML("MassNBCtools was developed to improve the efficiency and consistency 
          of performing natural background condition determinations for 
          Massachusetts Assessment Units (AUs). This app was developed by Ben 
          Block of Tetra Tech (Ben.Block@tetratech.com), for use by the 
          Massachusetts Department of Environmental Protection (MassDEP).<br> 
          Please contact Laurie Kennedy (laurie.kennedy@mass.gov), Richard Chase 
          (richard.f.chase@mass.gov), or Bob Smith (robert.smith@mass.gov) with 
          any questions or comments on the application.<br>")
    ,HTML('<center><img src="MassDEPlogo.png" height="100"></center>')
    ,easyClose = T)
  
  # Show the model on start up
  showModal(myModal)
  
  # select AUs ####
  myData <- reactive({
    df_AU_Data %>%
      select(AU_ID, AU_TempClassQual, Area_SQ_MI, PERCAG, PERCDEV
             , PERCNAT, PERCWET, PERCAG_PR, PERCDEV_PR, PERCNAT_PR, PERCWET_PR
             , PERCAG_ST, PERCDEV_ST, PERCNAT_ST, PERCWET_ST, PERCAG_SP
             , PERCDEV_SP, PERCNAT_SP, PERCWET_SP, PercImp, nDams, nGWP
             , nNPDES, nPWS_SW, nSEMS, nTRIs, pctZone2)
  })#reactive ~ END
  
  # trim data table once AU is selected
  observeEvent(input$input_AU_choice, {
    if(input$input_AU_choice == ""){
      output$input_DT <- renderDT({
        return(myData())
      })#renderDT ~ END
    } else {
      output$input_DT <- renderDT({
        trim_data <- myData()
        trim_data2 <- trim_data %>% 
          filter(AU_ID == input$input_AU_choice)
        return(trim_data2)
      })#renderDT ~ END
    } # end 
  })#observeEvent ~END
  
  ## AU select data ####
  observeEvent(input$input_AU_choice, {
    req(input$input_AU_choice != "")
    
    myAU <- input$input_AU_choice
    AU_Data <- myData()
    AU_Data_trimmed <- AU_Data %>% 
      filter(AU_ID == myAU)
    
    ### stats ####
    # land cover stats
    Area_SQ_MI <- AU_Data_trimmed$Area_SQ_MI
    PERCNAT <- AU_Data_trimmed$PERCNAT
    PERCWET <- AU_Data_trimmed$PERCWET
    PERCAG <- AU_Data_trimmed$PERCAG
    PERCNAT_PR <- AU_Data_trimmed$PERCNAT_PR
    PERCWET_PR <- AU_Data_trimmed$PERCWET_PR
    PERCAG_PR <- AU_Data_trimmed$PERCAG_PR
    PERCNAT_ST <- AU_Data_trimmed$PERCNAT_ST
    PERCWET_ST <- AU_Data_trimmed$PERCWET_ST
    PERCAG_ST <- AU_Data_trimmed$PERCAG_ST
    PERCNAT_SP <- AU_Data_trimmed$PERCNAT_SP
    PERCWET_SP <- AU_Data_trimmed$PERCWET_SP
    PERCAG_SP <- AU_Data_trimmed$PERCAG_SP
    PercImp <- AU_Data_trimmed$PercImp
    
    # natural land stats
    PctNat_CompWs <- PERCNAT + PERCWET
    PctNat_ProxWs <- PERCNAT_PR + PERCWET_PR
    Min_PctNat_CompWs_ProxWs <- min(PctNat_CompWs, PctNat_ProxWs)
    
    PctNat_CompBuf <- PERCNAT_ST + PERCWET_ST
    PctNat_ProxBuf <- PERCNAT_SP + PERCWET_SP
    
    # ag land stats
    PctAg_CompWs <- PERCAG
    PctAg_ProxWs <- PERCAG_PR
    Max_PctAg_CompWs_ProxWs <- max(PctAg_CompWs, PctAg_ProxWs)
    
    PctAg_CompBuff <- PERCAG_ST
    PctAg_ProxBuff <- PERCAG_SP
    
    ## per Appendix A Table A1 (2022 CALM)
    if (Area_SQ_MI >= 25 & Min_PctNat_CompWs_ProxWs > 80
        & PctNat_ProxBuf >90) {
      NatResult <- paste("Natural land cover exceeds NBC thresholds. "
                         , "Recommended answer: No")
    } else if (Area_SQ_MI < 25 & Min_PctNat_CompWs_ProxWs > 80
               & PctNat_CompBuf >90) {
      NatResult <- paste("Natural land cover exceeds NBC thresholds. "
                         , "Recommended answer: No")
    } else {
      NatResult <- paste("Natural land cover does not meet NBC thresholds (i.e., anthropogenic influence is too great). "
                         , "Recommended answer: Yes")

    }# if/else ~ END
    
    ## per MassDEP updates to Appendix A
    #Max_PctAg_CompWs_ProxWs
    #PctAg_CompWs - complete watershed
    #PctAg_ProxWs - proximate watershed
    #PctAg_CompBuff - complete buffer
    #PctAg_ProxBuff - proximate buffer
    if (Area_SQ_MI >= 25 & Max_PctAg_CompWs_ProxWs < 10
        & PctAg_ProxBuff < 5) {
      AgResult <- paste("Agricultural land cover does not exceed NBC thresholds. "
                         , "Recommended answer: No")
    } else if (Area_SQ_MI < 25 & Max_PctAg_CompWs_ProxWs < 10
               & PctAg_CompBuff < 5) {
      AgResult <- paste("Agricultural land cover does not exceed NBC thresholds. "
                         , "Recommended answer: No")
    } else {
      AgResult <- paste("Agricultural land cover exceeds NBC thresholds (i.e., anthropogenic influence is too great). "
                         , "Recommended answer: Yes")
      
    }# if/else ~ END
  
    # point source counts
    nDams <- AU_Data_trimmed$nDams
    nNPDES <- AU_Data_trimmed$nNPDES
    nSEMS <- AU_Data_trimmed$nSEMS
    nTRIs <- AU_Data_trimmed$nTRIs
    
    # pct Zone II
    pctZone2 <- AU_Data_trimmed$pctZone2
    
    # temperature classes
    AU_TempClassQual <- AU_Data_trimmed$AU_TempClassQual
    
    ## outputs ####
    if (Area_SQ_MI >= 25) {
      output$output_LC_Results1 <- renderUI({ #Output: tab_Gen_input.R
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Proximate Stream Buffer): ",PctNat_ProxBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", AgResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_LC_Results2 <- renderUI({ #Output: tab_Gen_output.R
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Proximate Stream Buffer): ",PctNat_ProxBuf)
                   , paste0("% Impervious: ", PercImp)
                   # , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
    } else {
      output$output_LC_Results1 <- renderUI({ #Output: tab_Gen_input.R
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Complete Stream Buffer): ",PctNat_CompBuf)
                   , paste0("% Impervious: ", PercImp)
                   , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_LC_Results2 <- renderUI({ #Output: tab_Gen_output.R
        HTML(paste(paste0("% Natural (Minimum of Watershed Scales): ", Min_PctNat_CompWs_ProxWs)
                   , paste0("% Natural (Complete Stream Buffer): ",PctNat_CompBuf)
                   , paste0("% Impervious: ", PercImp)
                   # , paste0("Result: ", NatResult)
                   , sep="<br/>"))
      })#renderUI ~ END
    }# if/else ~ END

    if (Area_SQ_MI >= 25) {
      output$output_Ag_Results1 <- renderUI({ #Output: tab_Gen_input.R
        HTML(paste(paste0("% Ag (Maximum of Watershed Scales): ", Max_PctAg_CompWs_ProxWs)
                   , paste0("% Ag (Proximate Stream Buffer): ",PctAg_ProxBuff)
                   , paste0("Result: ", AgResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_Ag_Results2 <- renderUI({ #Output: tab_Gen_output.R
        HTML(paste(paste0("% Ag (Maximum of Watershed Scales): ", Max_PctAg_CompWs_ProxWs)
                   , paste0("% Ag (Proximate Stream Buffer): ",PctAg_ProxBuff)
                   , sep="<br/>"))
      })#renderUI ~ END
    } else {
      output$output_Ag_Results1 <- renderUI({ #Output: tab_Gen_input.R
        HTML(paste(paste0("% Ag (Maximum of Watershed Scales): ", Max_PctAg_CompWs_ProxWs)
                   , paste0("% Ag (Complete Stream Buffer): ",PctAg_CompBuff)
                   , paste0("Result: ", AgResult)
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_Ag_Results2 <- renderUI({ #Output: tab_Gen_output.R
        HTML(paste(paste0("% Ag (Maximum of Watershed Scales): ", Max_PctAg_CompWs_ProxWs)
                   , paste0("% Ag (Complete Stream Buffer): ",PctAg_CompBuff)
                   , sep="<br/>"))
      })#renderUI ~ END
    }# if/else ~ END
    
    if (PercImp >= 4) {
      output$output_Imperv1 <- renderUI({ #Output: tab_Gen_temp.R
        HTML(paste(paste0("% Impervious: ", PercImp)
                   , paste0("Result: Impervious land cover exceeds NBC thresholds. "
                            , "Recommended answer: Yes")
                   , sep="<br/>"))
      })#renderUI ~ END
    } else {
      output$output_Imperv1 <- renderUI({ #Output: tab_Gen_temp.R
        HTML(paste(paste0("% Impervious: ", PercImp)
                   , paste0("Result: Impervious land cover does not exceed NBC thresholds. "
                    , "Recommended answer: No")
                   , sep="<br/>"))
      })#renderUI ~ END
    }# if/else ~ END
    
    if (PERCWET_PR <= 7) {
      output$output_wetland1 <- renderUI({ #Output: tab_Gen_DO.R
        HTML(paste(paste0("% Wetlands (Proximal Watershed): ", PERCWET_PR)
                   , paste0("Result: Wetland land cover does not meet or exceed NBC thresholds. "
                            , "Recommended answer: Yes")
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_wetland2 <- renderUI({ #Output: tab_Gen_pH.R
        HTML(paste(paste0("% Wetlands (Proximal Watershed): ", PERCWET_PR)
                   , paste0("Result: Wetland land cover does not meet or exceed NBC thresholds. "
                            , "Recommended answer: Yes")
                   , sep="<br/>"))
      })#renderUI ~ END
    } else {
      output$output_wetland1 <- renderUI({ #Output: tab_Gen_DO.R
        HTML(paste(paste0("% Wetlands (Proximal Watershed): ", PERCWET_PR)
                   , paste0("Result: Wetland land cover meets or exceeds NBC thresholds. "
                            , "Recommended answer: No")
                   , sep="<br/>"))
      })#renderUI ~ END
      output$output_wetland2 <- renderUI({ #Output: tab_Gen_pH.R
        HTML(paste(paste0("% Wetlands (Proximal Watershed): ", PERCWET_PR)
                   , paste0("Result: Wetland land cover meets or exceeds NBC thresholds. "
                            , "Recommended answer: No")
                   , sep="<br/>"))
      })#renderUI ~ END
    }# if/else ~ END
    
    output$output_dam_count1 <- renderUI({ #Output: tab_Gen_input.R
      HTML(paste0("Number of Dams in AU: ", nDams))
      })#renderUI ~ END 
    output$output_dam_count2 <- renderUI({ #Output: tab_Gen_output.R
      HTML(paste0("Number of Dams in AU: ", nDams))
    })#renderUI ~ END
    
    output$output_ptsrc_counts1 <- renderUI({ #Output: tab_Gen_input.R
      HTML(paste(paste0("Number of NPDES in AU: ", nNPDES)
                 , paste0("Number of Superfunds in AU: ",nSEMS)
                 , paste0("Number of TRIs in AU: ", nTRIs)
                 , sep="<br/>"))
    })#renderUI ~ END
    output$output_ptsrc_counts2 <- renderUI({ #Output: tab_Gen_output.R
      HTML(paste(paste0("Number of NPDES in AU: ", nNPDES)
                 , paste0("Number of Superfunds in AU: ",nSEMS)
                 , paste0("Number of TRIs in AU: ", nTRIs)
                 , sep="<br/>"))
    })#renderUI ~ END
    
    output$output_pctZone2_1 <- renderUI({ #Output: tab_Gen_input.R
      HTML(paste0("% Zone II WPA in AU: ", pctZone2))
    })#renderUI ~ END
    output$output_pctZone2_2 <- renderUI({ #Output: tab_Gen_output.R
      HTML(paste0("% Zone II WPA in AU: ", pctZone2))
    })#renderUI ~ END
    
    output$output_TempClass <- renderUI({ #Output: tab_Gen_temp.R
      HTML(paste0(AU_TempClassQual))
      })#renderUI ~ END
    
    #Output: tab_Gen_output.R
    output$output_Area_SQ_MI <- renderText({round(Area_SQ_MI,3)}) 
    
  })#observeEvent ~ END
  
  # Mapping ####
  ## Base Map ####
  output$mymap <- renderLeaflet({
    leaflet("mymap") %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
      setView(lat = 42.3063, lng = -71.8046, zoom = 8) %>%
      addPolygons(data = GISlayer_AUpoly, color = "black", weight = 2, opacity = 1
                  , fill = FALSE, label = GISlayer_AUpoly$AU_ID, group = "AU Watersheds"
                  , popup = paste("<b> AU_ID:</b>", GISlayer_AUpoly$AU_ID, "<br>"
                                  ,"<b> Area_SQ_MI:</b>", GISlayer_AUpoly$Area_SQ_MI)) %>%
      addCircleMarkers(data = GISlayer_dams, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "red", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_dams$DAMNAME, group = "Dams"
                       , popup = paste("<b> Dam Name:</b>", GISlayer_dams$DAMNAME, "<br>"
                                       ,"<b> NATID:</b>", GISlayer_dams$NATID, "<br>"
                                       ,"<b> REGAUTH:</b>", GISlayer_dams$REGAUTH, "<br>"
                                       ,"<b> OWNTYPE1:</b>", GISlayer_dams$OWNTYPE1, "<br>"
                                       ,"<b> OWNTYPE2:</b>", GISlayer_dams$OWNTYPE2, "<br>"
                                       ,"<b> OWNTYPE3:</b>", GISlayer_dams$OWNTYPE3, "<br>"
                                       ,"<b> HAZCODE:</b>", GISlayer_dams$HAZCODE)) %>%
      addCircleMarkers(data = GISlayer_SEMS, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#beaed4", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_SEMS$PRIMARY_NA, group = "Superfunds"
                       , popup = paste("<b> Primary Name:</b>", GISlayer_SEMS$PRIMARY_NA, "<br>"
                                       ,"<b> Registry ID:</b>", GISlayer_SEMS$REGISTRY_I, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_SEMS$LATITUDE83, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_SEMS$LONGITUDE8)) %>%
      addCircleMarkers(data = GISlayer_NPDES, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "orange", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_NPDES$PRIMARY_NA, group = "NPDES"
                       , popup = paste("<b> Primary Name:</b>", GISlayer_NPDES$PRIMARY_NA, "<br>"
                                       ,"<b> Registry ID:</b>", GISlayer_NPDES$REGISTRY_I, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_NPDES$LATITUDE83, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_NPDES$LONGITUDE8)) %>%
      addCircleMarkers(data = GISlayer_TRIs, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#7fc97f", color = "black"
                       , opacity = 1, weight = 2
                       , label = GISlayer_TRIs$PRIMARY_NA, group = "TRIs"
                       , popup = paste("<b> Facility Name:</b>", GISlayer_TRIs$TRI_Faci_1, "<br>"
                                       ,"<b> Facility ID:</b>", GISlayer_TRIs$TRI_Facili, "<br>"
                                       ,"<b> FRSID:</b>", GISlayer_TRIs$FRSID, "<br>"
                                       ,"<b> Latest Year:</b>", GISlayer_TRIs$Latest_Yea, "<br>"
                                       ,"<b> Street Address:</b>", GISlayer_TRIs$Street_Add, "<br>"
                                       ,"<b> City:</b>", GISlayer_TRIs$City, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_TRIs$Latitude, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_TRIs$Longitude)) %>%
      addCircleMarkers(data = GISlayer_PWS_SW, radius = 7, stroke = TRUE
                       , fillOpacity = 1, fillColor = "#c51b7d", color = "black"
                         , opacity = 1, weight = 2
                       , label = GISlayer_PWS_SW$PWS_ID, group = "PWS SW"
                       , popup = paste("<b> Site Name:</b>", GISlayer_PWS_SW$SITE_NAME, "<br>"
                                       ,"<b> PWS ID:</b>", GISlayer_PWS_SW$PWS_ID, "<br>"
                                       ,"<b> Type:</b>", GISlayer_PWS_SW$TYPE, "<br>"
                                       ,"<b> Latitude:</b>", GISlayer_PWS_SW$LATITUDE, "<br>"
                                       ,"<b> Longitude:</b>", GISlayer_PWS_SW$LONGITUDE)) %>%
      addPolylines(data = GISlayer_AUflow, color = "blue", weight = 3
                   , label = GISlayer_AUflow$AU_ID, group = "AU River Arc"
                   , popup = paste("<b> AU_ID:</b>", GISlayer_AUflow$AU_ID, "<br>"
                                   ,"<b> AU_Name:</b>", GISlayer_AUflow$AU_Name, "<br>"
                                   ,"<b> AU_DESC1:</b>", GISlayer_AUflow$AU_DESC1, "<br>"
                                   ,"<b> AU_DESC2:</b>", GISlayer_AUflow$AU_DESC2, "<br>"
                                   ,"<b> AU_TYPE:</b>", GISlayer_AUflow$AU_TYPE, "<br>"
                                   ,"<b> AU_Size:</b>", GISlayer_AUflow$AU_Size, "<br>"
                                   ,"<b> AU_Unit:</b>", GISlayer_AUflow$AU_Unit, "<br>"
                                   ,"<b> AU_Class:</b>", GISlayer_AUflow$AU_Class, "<br>"
                                   ,"<b> AU_ClassQu:</b>", GISlayer_AUflow$AU_ClassQu, "<br>"
                                   ,"<b> AU_ClassRe:</b>", GISlayer_AUflow$AU_ClassRe)) %>%
      addPolygons(data = GISlayer_Zone2, color = "black", weight = 2, opacity = 1
                  , fillColor = "#df65b0", fillOpacity = 0.25, group = "Zone 2 WPA") %>%  
      addLayersControl(overlayGroups = c("AU River Arc", "AU Watersheds", "Dams"
                                         , "NPDES", "Superfunds", "TRIs"
                                         , "PWS SW", "Zone 2 WPA")
                       ,baseGroups = c("Esri WSM", "Esri Ortho")
                       ,options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("AU River Arc", "AU Watersheds", "Dams","NPDES", "Superfunds"
                  , "TRIs", "PWS SW", "Zone 2 WPA")) %>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                 , position = "bottomright")
  })#renderLeaflet ~ END

    ## Map Zoom ####
    # Map that filters output data to a single AU
    observeEvent(input$input_AU_choice, {
      req(input$input_AU_choice != "")
      
      myAU <- input$input_AU_choice
      
      df_AUCentroids_select <- df_AUCentroids %>% 
        filter(AU_ID == myAU)
      AU_long <- df_AUCentroids_select$Longitude # longitude
      AU_lat <- df_AUCentroids_select$Latitude # latitude

      GISlayer_AUpoly_select <- GISlayer_AUpoly[GISlayer_AUpoly$AU_ID == myAU,]
      GISlayer_AUflow_select <- GISlayer_AUflow[GISlayer_AUflow$AU_ID == myAU,]
      
      # modfiy map
      leafletProxy("mymap") %>%
        removeShape("layer_AU_selected")  %>%
        addPolygons(data = GISlayer_AUpoly_select, color = "black", weight = 2
                    , fillColor = "red", label = GISlayer_AUpoly_select$AU_ID
                    , group = "AU_selected", layerId = "layer_AU_selected") %>%
        addPolylines(data = GISlayer_AUflow_select, color = "green", weight = 3
                     , label = GISlayer_AUflow_select$AU_ID
                     , group = "AU_arc_selected", layerId = "layer_AU_arc_selected") %>%
        setView(lng = AU_long, lat = AU_lat, zoom = 12)
    })#observeEvent ~ END


    # Output info ####
    ## General Outputs ####
    # All outputs below directed to tab_Gen_output.R
    output$output_analyst <- renderText({input$input_analyst}) 
    output$output_AU_choice1 <- renderText({input$input_AU_choice})
    ### Nat Land ####
    output$output_Nat_Land_choice1 <- renderText({input$input_Nat_Land_choice})
    observeEvent(input$input_Nat_Land_choice, {
      req(input$input_Nat_Land_choice != "")
      
      if (input$input_Nat_Land_choice == "No") {
        output$output_Nat_Land_choice2 <- renderUI({
          paste("Natural land cover is above NBC thresholds; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_Nat_Land_choice2 <- renderUI({
          paste("Natural land cover is below NBC thresholds; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ### Ag Land ####
    output$output_Ag_Land_choice1 <- renderText({input$input_Ag_Land_choice})
    observeEvent(input$input_Ag_Land_choice, {
      req(input$input_Ag_Land_choice != "")
      
      if (input$input_Ag_Land_choice == "No") {
        output$output_Ag_Land_choice2 <- renderUI({
          paste("Agricultural land cover is below NBC thresholds; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_Ag_Land_choice2 <- renderUI({
          paste("Agricultural land cover is above NBC thresholds; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ### Dams ####
    output$output_Dam_choice1 <- renderText({input$input_Dam_choice})
    observeEvent(input$input_Dam_choice, {
      req(input$input_Dam_choice != "")
      
      if (input$input_Dam_choice == "No") {
        output$output_Dam_choice2 <- renderUI({
          paste("Dams are not present that likely alter conditions; continue NBC evaluation. ")
        })#renderUI ~ END
      } else {
        output$output_Dam_choice2 <- renderUI({
          paste("Dams are present that alter conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ### PtSrc ####
    output$output_PtSrc_choice1 <- renderText({input$input_PtSrc_choice})
    observeEvent(input$input_PtSrc_choice, {
      req(input$input_PtSrc_choice != "")
      
      if (input$input_PtSrc_choice == "No") {
        output$output_PtSrc_choice2 <- renderUI({
          paste("Point source discharges are not present; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_PtSrc_choice2 <- renderUI({
          paste("Point source discharges are present that alter conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ### WthDrwl ####
    output$output_Withdrawal_choice1 <- renderText({input$input_Withdrawal_choice})
    observeEvent(input$input_Withdrawal_choice, {
      req(input$input_Withdrawal_choice != "")
      
      if (input$input_Withdrawal_choice == "No") {
        output$output_Withdrawal_choice2 <- renderUI({
          paste("Water withdrawals are not present; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_Withdrawal_choice2 <- renderUI({
          paste("Water withdrawals are present that alter conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    output$output_notes <- renderText({input$input_notes})
    
    ## Temp Outputs ####
    output$output_AU_choice2 <- renderText({input$input_AU_choice}) # Output: tab_Gen_temp.R
    output$output_tempcrit_choice1 <- renderText({input$input_tempcrit_choice}) # Output: tab_Gen_output.R
    observeEvent(input$input_tempcrit_choice, {
      req(input$input_tempcrit_choice != "")
      
      if (input$input_tempcrit_choice == "Cold-Water" 
          |input$input_tempcrit_choice == "Existing Cold-Water") {
        output$output_tempcrit_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("The cold-water temperature exceedance may be due to natural conditions; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_tempcrit_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("The warm-water temperature exceedance is not due to natural conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
      })#observeEvent ~ END
    
    output$output_tempspike_choice1 <- renderText({input$input_tempspike_choice}) # Output: tab_Gen_output.R
    observeEvent(input$input_tempspike_choice, { 
      req(input$input_tempspike_choice != "")
      
      if (input$input_tempspike_choice == "No") {
        output$output_tempspike_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Isolated temperature spike(s) are not present; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_tempspike_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Isolated temperature spike(s) are present that indicate altered conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    output$output_imperv_choice1 <- renderText({input$input_imperv_choice}) # Output: tab_Gen_output.R
    observeEvent(input$input_imperv_choice, {
      req(input$input_imperv_choice != "")
      
      if (input$input_imperv_choice == "No") {
        output$output_imperv_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Impervious land cover likely does not alter conditions; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_imperv_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Impervious land cover alters conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ## DO Outputs ####
    output$output_AU_choice3 <- renderText({input$input_AU_choice}) # Output: tab_Gen_DO.R
    output$output_dospike_choice1 <- renderText({input$input_dospike_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_dospike_choice, {
      req(input$input_dospike_choice != "")
      
      if (input$input_dospike_choice == "No") {
        output$output_dospike_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Isolated DO spike(s) are not present; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_dospike_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Isolated DO spike(s) are present that indicate altered conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    output$output_dodiurnal_choice1 <- renderText({input$input_dodiurnal_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_dodiurnal_choice, {
      req(input$input_dodiurnal_choice != "")
      
      if (input$input_dodiurnal_choice == "No") {
        output$output_dodiurnal_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Diurnal DO concentration shifts are not present; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_dodiurnal_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Diurnal DO concentration shifts indicate altered conditions; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    output$output_wetland1_choice1 <- renderText({input$input_wetland1_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_wetland1_choice, {
      req(input$input_wetland1_choice != "")
      
      if (input$input_wetland1_choice == "No") {
        output$output_wetland1_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Wetland land cover is above NBC thresholds; continue NBC evaluation.")
        })#renderUI ~ END
      } else { 
        output$output_wetland1_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Wetland land cover is below NBC thresholds; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ## TP Outputs ####
    output$output_AU_choice4 <- renderText({input$input_AU_choice}) # Output: tab_Gen_TP.R
    output$output_geoTP_choice1 <- renderText({input$input_geoTP_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_geoTP_choice, {
      req(input$input_geoTP_choice != "")
      
      if (input$input_geoTP_choice == "No") {
        output$output_geoTP_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Lithology may be a potential source of TP exceedance; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_geoTP_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Lithology is not likely a source of TP exceedance; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ## pH Outputs ####
    output$output_AU_choice5 <- renderText({input$input_AU_choice}) # Output: tab_Gen_pH.R
    output$output_wetland2_choice1 <- renderText({input$input_wetland2_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_wetland2_choice, {
      req(input$input_wetland2_choice != "")
      
      if (input$input_wetland2_choice == "No") {
        output$output_wetland2_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Wetland land cover is above NBC thresholds; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_wetland2_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Wetland land cover is below NBC thresholds; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ## Metals Outputs ####
    output$output_AU_choice6 <- renderText({input$input_AU_choice}) # Output: tab_Gen_metal.R
    output$output_geoMetal_choice1 <- renderText({input$input_geoMetal_choice}) # Output: tab_Gen_output.R
    
    observeEvent(input$input_geoMetal_choice, {
      req(input$input_geoMetal_choice != "")
      
      if (input$input_geoMetal_choice == "No") {
        output$output_geoMetal_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Lithology may be a potential source of metal exceedance; continue NBC evaluation.")
        })#renderUI ~ END
      } else {
        output$output_geoMetal_choice2 <- renderUI({ # Output: tab_Gen_output.R
          paste("Lithology is not likely a source of metal exceedance; NBC determination excluded from further consideration.")
        })#renderUI ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
    ## NBC Determination ####
    
    observeEvent(ignoreInit = TRUE,c(
      input$input_Nat_Land_choice
      ,input$input_Ag_Land_choice
      ,input$input_Dam_choice
      ,input$input_PtSrc_choice
      ,input$input_Withdrawal_choice
      ,input$input_tempcrit_choice
      ,input$input_tempspike_choice
      ,input$input_imperv_choice
      ,input$input_dospike_choice
      ,input$input_dodiurnal_choice
      ,input$input_wetland1_choice
      ,input$input_geoTP_choice
      ,input$input_wetland2_choice
      ,input$input_geoMetal_choice
    ), {
      Question_Responses <- c(input$input_Nat_Land_choice
                              ,input$input_Ag_Land_choice
                              ,input$input_Dam_choice
                              ,input$input_PtSrc_choice
                              ,input$input_Withdrawal_choice
                              ,input$input_tempcrit_choice
                              ,input$input_tempspike_choice
                              ,input$input_imperv_choice
                              ,input$input_dospike_choice
                              ,input$input_dodiurnal_choice
                              ,input$input_wetland1_choice
                              ,input$input_geoTP_choice
                              ,input$input_wetland2_choice
                              ,input$input_geoMetal_choice)
      Yes_count <- sum(stringr::str_count(Question_Responses, "Yes"))
      Warm_count <- sum(stringr::str_count(Question_Responses, "Warm-Water"))
      Total_count <- sum(Yes_count,Warm_count)
      
      if (Total_count > 0) {
        output$output_NBC_auto <- renderText({
          paste("Based on responses, natural background conditions are unlikely to cause the SWQS exceedance(s).")
        })#renderText ~ END
      } else {
        output$output_NBC_auto <- renderText({
          paste("Based on responses, natural background conditions likely result in the SWQS excursion(s).")
        })#renderText ~ END
      }# if/else ~ END
      
    })
    
    
    
    
    observeEvent(input$input_NBC_choice, {
      req(input$input_NBC_choice != "")
      
      if (input$input_NBC_choice == "No") {
        output$output_NBC_choice <- renderText({
          paste("Natural background conditions are unlikely to cause the SWQS exceedance(s).")
        })#renderText ~ END
      } else {
        output$output_NBC_choice <- renderText({
          paste("Natural background conditions likely result in the SWQS excursion(s).")
        })#renderText ~ END
      }# if/else ~ END
    })#observeEvent ~ END
    
})##shinyServer ~ END