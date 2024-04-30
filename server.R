#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# nolint start
library(shiny)
# nolint end

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Misc Names----
  output$fn_input_display <- renderText({
    inFile <- input$fn_input
    
    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END
    
    return(paste0("'", inFile$name, "'"))
  }) ## renderText~END
  
  # Import ####
  
  ## File Watch ----
  file_watch <- reactive({
    # trigger for df_import()
    input$fn_input
  })## file_watch
  
  ## Import, df_import ####
  df_import <- eventReactive(file_watch(), {
    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$fn_input
    
    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END
    
    sep_user <- input$sep
    
    # Define file
    fn_inFile <- inFile$datapath
    
    #message(getwd())
    message(paste0("Import, separator: '", input$sep,"'"))
    message(paste0("Import, file name: ", input$fn_input$name))
    
    # Read user imported file
    df_input <- read.delim(fn_inFile
                           , header = TRUE
                           , sep = sep_user
                           , stringsAsFactors = FALSE
                           , na.strings = c("", "NA"))
    
    required_columns <- c("MonitoringLocationIdentifier", "LatitudeMeasure"
                          , "LongitudeMeasure")
    
    # QC Check for column names
    column_names <- colnames(df_input)
    col_req_match <- required_columns %in% column_names
    col_missing <- required_columns[!col_req_match]
    
    shiny::validate(
      need(all(required_columns %in% column_names)
           , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                    , paste("Required columns missing from the data:\n")
                    , paste("* ", col_missing, collapse = "\n")))
    )##END ~ validate() code
    # Add "Results" folder if missing
    boo_Results <- dir.exists(file.path(path_results))
    if (boo_Results == FALSE) {
      dir.create(file.path(path_results))
    }
    
    # Remove all files in "Results" folder
    fn_results <- list.files(file.path(path_results), full.names = TRUE)
    file.remove(fn_results)
    
    # Write to "Results" folder - Import as TSV
    fn_input <- file.path(path_results, "data_import.tsv")
    # write.table(df_input, fn_input, row.names = FALSE
    #             , col.names = TRUE, sep = "\t")
    
    # Copy to "Results" folder - Import "as is"
    file.copy(input$fn_input$datapath
              , file.path(path_results, input$fn_input$name))
    
    # button, disable, download
    # shinyjs::disable("b_download")
    
    return(df_input)
    
  })## df_import
  
  ## Import table ####
  output$df_import_DT <- DT::renderDT({
    
    df_data <- df_import()
    
  }##expression~END
  , filter = "top"
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100)
                   , autoWidth = TRUE)
  
  )##output$df_import_DT~END

# b_Calc, AUjoin ####
shiny::observeEvent(input$b_Calc, {
  shiny::withProgress({

    # Number of increments
    n_inc <- 10

    # sink output
    file_sink <- file(file.path(".", "Results", "results_Log.txt")
                      , open = "wt")
    sink(file_sink, type = "output", append = TRUE)
    sink(file_sink, type = "message", append = TRUE)

    # Log
    message("Results Log from AU Spatial Join App")
    message(Sys.time())
    inFile <- input$fn_input
    message(paste0("Imported file name: ", inFile$name))

    # Increment the progress bar, and update the detail text.
    incProgress(1/n_inc, detail = "Data, Initialize")
    Sys.sleep(0.25)

  # Read in saved file (known format)
  # df_MonLocData <- NULL  # set as null for IF QC check prior to import
  # fn_input <- file.path(".", "Results", "data_import.tsv")
  # df_MonLocData <- read.delim(fn_input, stringsAsFactors = FALSE, sep = "\t")
  # 
  # # QC, FAIL if TRUE
  # if (is.null(df_MonLocData)) {
  #   return(NULL)
  # }
  
  # Import data
  # data
  inFile <- input$fn_input
  df_MonLocData <- read.delim(inFile$datapath
                         , header = TRUE
                         , sep = input$sep
                         , stringsAsFactors = FALSE)
  # QC, FAIL if TRUE
  if (is.null(df_MonLocData)) {
    return(NULL)
  }

  ## 1. Join crosswalk ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Join, crosswalk")
  Sys.sleep(0.25)
  
  df_joinedMonLocAU <- dplyr::left_join(df_MonLocData, df_ML2AU
                               , by = "MonitoringLocationIdentifier")

  # 2. Filter matched MonLoc ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Filter")
  Sys.sleep(0.25)
  
  df_MonLocMatched <- df_joinedMonLocAU %>%
    filter(!is.na(AU_ID))

  # 3. Unmatched MonLoc by Type ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Prepare Unmatched")
  Sys.sleep(0.25)
  
  df_Lake_Unmatched <- df_joinedMonLocAU %>%
    filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Lake_types)) %>% 
    select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
              , FishConsumption_Use, Recreational_Use, Other_Use))

  df_Stream_Unmatched <- df_joinedMonLocAU %>%
    filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Stream_types)) %>% 
    select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
              , FishConsumption_Use, Recreational_Use, Other_Use))

  # 4. Spatial Joins ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Spatial Join, Lakes")
  Sys.sleep(0.25)
  
  ### 4a. Lake Spatial Join ####

  # QC check
  num_sites <- nrow(df_Lake_Unmatched)

  # only run spatial join if there are sites
  if(num_sites == 0){
    message(paste("There are NO lake monitoring locations missing AU data."))

    df_Lake_SpatJoin_Final <- NULL

  } else {
    message(paste("There are", num_sites, "lake monitoring locations missing AU data."))

    ### convert to geospatial layer (sf object)
    lakes_pts <- sf::st_as_sf(x = df_Lake_Unmatched, coords = c("LongitudeMeasure"
                                                              ,"LatitudeMeasure")
                              , crs = "+proj=longlat +datum=WGS84")%>%
      sf::st_transform(st_crs(GISlayer_lakes_transformed))

    ### spatial join
    lake_SpatJoin <- sf::st_join(lakes_pts, GISlayer_lakes_transformed
                                 , join = st_nearest_feature) # join points and AUs
      # select(MonitoringLocationIdentifier, MonitoringLocationName
      #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME, cultural_u
      #        , drinkingwa, ecological, fishconsum, recreation, other_use) %>%  # trim unneccessary columns
      # rename(Cultural_Use = cultural_u
      #        , DrinkingWater_Use = drinkingwa
      #        , Ecological_Use = ecological
      #        , FishConsumption_Use = fishconsum
      #        , Recreational_Use = recreation
      #        , Other_Use = other_use)

    ### determine distance (m) between points and nearest feature
    near_feat <- sf::st_nearest_feature(lakes_pts, GISlayer_lakes_transformed)
    Dist_to_AU_m <- sf::st_distance(lakes_pts, GISlayer_lakes_transformed[near_feat,]
                                    , by_element = TRUE)

    ### join distance measurements to join results
    lake_SpatJoin2 <- cbind(lake_SpatJoin, Dist_to_AU_m)

    ### results and export data
    df_Lake_SpatJoin_Final <- lake_SpatJoin2 %>%
      sf::st_transform(4326) %>%
      mutate(LongitudeMeasure = unlist(map(geometry,1)),
             LatitudeMeasure = unlist(map(geometry,2))) %>%
      sf::st_drop_geometry()

  }# end if/else statement


  ### 4b. Stream Spatial Join ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Spatial Join, Streams")
  Sys.sleep(0.25)
  
  # QC check
  num_sites <- nrow(df_Stream_Unmatched)
  
  # only run spatial join if there are sites
  if(num_sites == 0){
    message(paste("There are NO stream monitoring locations missing AU data."))
    
    df_Stream_SpatJoin_Final <- NULL
    
  } else {
    message(paste("There are", num_sites, "stream monitoring locations missing AU data."))
    
    ### convert to geospatial layer (sf object)
    streams_pts <- sf::st_as_sf(x = df_Stream_Unmatched, coords = c("LongitudeMeasure"
                                                                ,"LatitudeMeasure")
                              , crs = "+proj=longlat +datum=WGS84")%>%
      sf::st_transform(st_crs(GISlayer_streams_transformed))
    
    ### spatial join
    stream_SpatJoin <- sf::st_join(streams_pts, GISlayer_streams_transformed
                                 , join = st_nearest_feature) # join points and AUs
      # select(MonitoringLocationIdentifier, MonitoringLocationName
      #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns
    
    ### determine distance (m) between points and nearest feature
    near_feat <- sf::st_nearest_feature(streams_pts, GISlayer_streams_transformed)
    Dist_to_AU_m <- sf::st_distance(streams_pts, GISlayer_streams_transformed[near_feat,]
                                    , by_element = TRUE)
    
    ### join distance measurements to join results
    stream_SpatJoin2 <- cbind(stream_SpatJoin, Dist_to_AU_m)
    
    ### results and export data
    df_Stream_SpatJoin_Final <- stream_SpatJoin2 %>%
      sf::st_transform(4326) %>%
      mutate(LongitudeMeasure = unlist(map(geometry,1)),
             LatitudeMeasure = unlist(map(geometry,2))) %>%
      sf::st_drop_geometry()
    
  }# end if/else statement
  
  ### 4c. Join data ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Spatial Join, Combine")
  Sys.sleep(0.25)
  
  if (is.null(df_Lake_SpatJoin_Final) & is.null(df_Stream_SpatJoin_Final)){
    df_SpatJoin_Final <- NULL # both datasets are NULL
  } else if (!is.null(df_Lake_SpatJoin_Final) 
             & is.null(df_Stream_SpatJoin_Final)) {
    df_SpatJoin_Final <- df_Lake_SpatJoin_Final # only streams dataset is NULL
  } else if (is.null(df_Lake_SpatJoin_Final) 
             & !is.null(df_Stream_SpatJoin_Final)) {
    df_SpatJoin_Final <- df_Stream_SpatJoin_Final # only lakes dataset is NULL
  } else {
    df_SpatJoin_Final <- rbind(df_Lake_SpatJoin_Final, df_Stream_SpatJoin_Final)
    # both datasets have data
  }# END ~ IF/ELSE

  ### 4d. Join to matched data ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Combine all")
  Sys.sleep(0.25)
  
  df_MonLocMatched$Dist_to_AU_m <- 0
  
  if (is.null(df_SpatJoin_Final)){
    df_MonLocAU_4Map <- df_MonLocMatched %>% 
      mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                    , TRUE ~ "Matched")) %>%
      select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                , Recreational_Use, Other_Use))
  } else {
    df_MonLocAU_4Map <- rbind(df_MonLocMatched, df_SpatJoin_Final)%>% 
      mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                    , TRUE ~ "Matched")) %>%
      select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                , Recreational_Use, Other_Use))
  } # END ~ if/else
  
  ## 5. Display data ####
  ### 5a. Table ####
  output$df_results_DT <- DT::renderDT({
    
    if (is.null(df_MonLocAU_4Map)) {
      return(NULL)
    }##IF~is.null~END
    
    # Filter df_MonLocAU_4Map based on input_site_choice
    filtered_df <- df_MonLocAU_4Map
    
    if (input$input_site_choice != "") {
      mySite <- input$input_site_choice
      
      filtered_df <- df_MonLocAU_4Map %>% 
        filter(MonitoringLocationIdentifier == mySite)
      }##IF~END
    
    # Return the filtered dataframe
    return(filtered_df)
    
    # # datatable(df_MonLocAU_4Map, selection = "single")
    # return(df_MonLocAU_4Map)
    
  }##expression~END
  , selection = "single"
  , filter = "top"
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100)
                   , autoWidth = TRUE) 
  
  )##output$df_import_DT~END
  
  # browser()
  
  ### 5b. Counts ####
  # Define a reactive value for storing the row count
  row_count_matched <- reactiveVal(0)
  row_count_unmatched_G50 <- reactiveVal(0)
  row_count_unmatched_L50 <- reactiveVal(0)

  # Observe the changes in df_MonLocAU_4Map and update the row count
  observe({
    req(df_MonLocAU_4Map)
    nMatched <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
    nUnmatched_G50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                          & df_MonLocAU_4Map$Dist_to_AU_m >= 50)
    nUnmatched_L50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                          & df_MonLocAU_4Map$Dist_to_AU_m < 50)
    row_count_matched(nMatched)
    row_count_unmatched_G50(nUnmatched_G50)
    row_count_unmatched_L50(nUnmatched_L50)
  })# END ~ observe
  
  # Render the valueBox based on the row count
  output$MatchCount <- renderValueBox({
    valueBox(value = row_count_matched()
      , subtitle = "Number of Monitoring Locations matched to AU crosswalk table"
      , icon = icon("hashtag")
      , color = "aqua"
    )
  })# END ~ renderValueBox
  
  output$UnMatchCount_G50 <- renderValueBox({
    valueBox(value = row_count_unmatched_G50()
      , subtitle = "Number of Monitoring Locations with AU spatial join >= 50m"
      , icon = icon("hashtag")
      , color = "red"
    )
  })# END ~ renderValueBox
  
  output$UnMatchCount_L50 <- renderValueBox({
    valueBox(value = row_count_unmatched_L50()
      , subtitle = "Number of Monitoring Locations with AU spatial join < 50m"
      , icon = icon("hashtag")
      , color = "yellow"
    )
  })# END ~ renderValueBox
  
  ## 6. Map ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Display results, ~15 sec")
  Sys.sleep(0.25)
  
  output$mymap <- renderLeaflet({
    req(df_MonLocAU_4Map)
    
    # Subset data by Match group
    data_match <- df_MonLocAU_4Map %>% 
      filter(MatchGroup == "Matched")
    
    data_unmatch <- df_MonLocAU_4Map %>% 
      filter(MatchGroup == "UnMatched")
    
    leaflet("mymap") %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
      addCircleMarkers(data = data_match, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                       , group = "Matched Sites"
                       , popup = paste("SiteID:", data_match$MonitoringLocationIdentifier, "<br>"
                                       ,"Site Name:", data_match$MonitoringLocationName, "<br>"
                                       ,"Site Type:", data_match$MonitoringLocationTypeName, "<br>"
                                       ,"State:", data_match$State, "<br>"
                                       ,"AU_ID:", data_match$AU_ID, "<br>"
                                       ,"AU NAME:", data_match$AU_NAME)
                       , color = "black", fillColor = "blue", fillOpacity = 1, stroke = TRUE
                       # , clusterOptions = markerClusterOptions()
                       ) %>% 
      addCircleMarkers(data = data_unmatch, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                       , group = "Unmatched Sites"
                       , popup = paste("SiteID:", data_unmatch$MonitoringLocationIdentifier, "<br>"
                                       ,"Site Name:", data_unmatch$MonitoringLocationName, "<br>"
                                       ,"Site Type:", data_unmatch$MonitoringLocationTypeName, "<br>"
                                       ,"State:", data_unmatch$State, "<br>"
                                       ,"AU_ID:", data_unmatch$AU_ID, "<br>"
                                       ,"AU NAME:", data_unmatch$AU_NAME)
                       , color = "black", fillColor = "red", fillOpacity = 1, stroke = TRUE
                       # , clusterOptions = markerClusterOptions()
                       ) %>%
      addPolylines(data = streams_simp_shp, color = "blue", weight = 3
                   , label = streams_simp_shp$AU_ID, group = "River AUs"
                   , popup = paste("<b> AU_ID:</b>", streams_simp_shp$AU_ID, "<br>"
                                   ,"<b> AU_Name:</b>", streams_simp_shp$AU_NAME)) %>%
      addPolygons(data = lakes_shp, color = "black", weight = 2, opacity = 1
                  , fillColor = "#df65b0", fillOpacity = 0.25, group = "Lake AUs"
                  , popup = paste("<b> AU_ID:</b>", lakes_shp$AU_ID, "<br>"
                                  ,"<b> AU_Name:</b>", lakes_shp$AU_NAME)) %>%
      addLayersControl(overlayGroups = c("Matched Sites", "Unmatched Sites"
                                         , "River AUs", "Lake AUs")
                       ,baseGroups = c("Esri WSM", "Esri Ortho")
                       ,options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Matched Sites", "Unmatched Sites"
                  , "River AUs", "Lake AUs")) %>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                 , position = "bottomright")
    }) # END ~ renderLeaflet
  
  ## Map Zoom ####
  # Map that filters output data to a single location
  observeEvent(input$input_site_choice, {
    req(input$input_site_choice != "")
    
    mySite <- input$input_site_choice
    
    df_MonLocAU_select <- df_MonLocAU_4Map %>% 
      filter(MonitoringLocationIdentifier == mySite)
    
    site_long <- df_MonLocAU_select$Longitude # longitude
    site_lat <- df_MonLocAU_select$Latitude # latitude
    
    # modfiy map
    leafletProxy("mymap") %>%
      # setView(lng = site_long, lat = site_lat, zoom = 12)
      flyTo(lng = site_long, lat = site_lat, zoom = 16)
  })#observeEvent ~ END
  
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Process completed")
  Sys.sleep(0.25)
  
  # 7. Save results ####
  # Save, Matched
  fn_results <- "MonitoringLocations_AU_Matched.csv"
  dn_results <- path_results
  pn_results <- file.path(dn_results, fn_results)
  write.csv(df_MonLocMatched, pn_results, row.names = FALSE)
  
  # Save, SpatJoin
  fn_results <- "MonitoringLocations_AU_SpatialJoin.csv"
  dn_results <- path_results
  pn_results <- file.path(dn_results, fn_results)
  write.csv(df_SpatJoin_Final, pn_results, row.names = FALSE)
  
  # Create zip file of results
  fn_4zip <- list.files(path = path_results
                        , full.names = TRUE)
  zip::zip(file.path(path_results, "results.zip"), fn_4zip)
  
  # button, enable, download
  # shinyjs::enable("b_download")
  
  # Pop up ####
  n_match <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
  n_joinG50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                        & df_MonLocAU_4Map$Dist_to_AU_m >= 50)
  n_joinL50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
                        & df_MonLocAU_4Map$Dist_to_AU_m < 50)
  msg <- paste0("The AU join is complete, but results take ~30 seconds to render in the app!"
                , " Please be patient and wait for the map to render to use in QC review of spatial joins.", "\n\n"
                , "Number of sites matched to AUs = ", n_match, "\n\n"
                , "Number of sites spatially joined to AUs (<50m distance) = ", n_joinL50, "\n\n"
                , "Number of sites spatially joined to AUs (>=50m distance) = ", n_joinG50, "\n\n")
  shinyalert::shinyalert(title = "Task Complete"
                         , text = msg
                         , type = "success"
                         , closeOnEsc = TRUE
                         , closeOnClickOutside = TRUE
                         , size = "m")
  # validate(msg)

  }) #END ~ withProgress
}) # END ~ observeEvent
  
  ## Download data ####
  output$b_download <- downloadHandler(
    
    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      paste0(fn_input_base
             , "_results_"
             , format(Sys.Date(), "%Y%m%d")
             , ".zip")
    } ,
    content = function(fname) {##content~START
      
      file.copy(file.path(path_results, "results.zip"), fname)
      
    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans
  
  # UI interactions ####
  ## DataTable selection ####
  # React to DataTable selection
  # observeEvent(input$df_results_DT_rows_selected, {
  #   selected_row <- input$df_results_DT_rows_selected
  #   if (length(selected_row) > 0) {
  #     # Get the selected location's coordinates
  #     lat <- df_MonLocAU_4Map[selected_row, "LatitudeMeasure"]
  #     lng <- df_MonLocAU_4Map[selected_row, "LongitudeMeasure"]
  #     # Pan the map to the selected location
  #     leafletProxy("mymap") %>%
  #       setView(lng = lng, lat = lat, zoom = 15)
  #   }#END ~ if/else
  # })#END ~ observeEvent
  # 
   ## Map Click ####
  # # React to map marker click
  # observe({
  #   event <- input$mymap_marker_click
  #   if (!is.null(event)) {
  #     # Find the row corresponding to the clicked marker
  #     row <- df_MonLocAU_4Map$MonitoringLocationIdentifier == event$id
  #     # Highlight the corresponding row in the table
  #     selectRows = data.frame(row)
  #     proxy = dataTableProxy("df_results_DT")
  #     selectRows(proxy, selected = TRUE)
  #   }#END ~ if/else
  # })#END ~ observe
  
  
})##shinyServer ~ END