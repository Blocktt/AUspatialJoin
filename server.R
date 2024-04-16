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
  
  # MMI calc ----
  
  ## MMIcalc, FileWatch ----
  file_watch_mmicalc <- reactive({
    # trigger for df_import()
    input$fn_input
  })## file_watch
  
  ## change view
  # df_import for MMIcalc not set up the same as others
  # had to set its own watch for changing view
  df_import_mmicalc <- eventReactive(file_watch_mmicalc(), {
    # activate tab Panel with table of imported data
    updateTabsetPanel(session = getDefaultReactiveDomain()
                      , "tabs_MMIcalc_main"
                      , selected = "tab_MMIcalc_viewer")
  })## df_import_mmicalc
  
  ## df_import----
  output$df_import_DT <- DT::renderDT({
    # input$df_import will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    
    inFile <- input$fn_input
    
    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END
    
    # Read user imported file
    df_input <- read.csv(inFile$datapath, header = TRUE,
                         stringsAsFactors = FALSE)
    
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
    boo_Results <- dir.exists(file.path(".", "Results"))
    if (boo_Results == FALSE) {
      dir.create(file.path(".", "Results"))
    }
    
    # Remove all files in "Results" folder
    fn_results <- list.files(file.path(".", "Results"), full.names = TRUE)
    file.remove(fn_results)
    
    # Write to "Results" folder - Import as TSV
    fn_input <- file.path(".", "Results", "data_import.tsv")
    write.table(df_input, fn_input, row.names = FALSE
                , col.names = TRUE, sep = "\t")
    
    # Copy to "Results" folder - Import "as is"
    file.copy(input$fn_input$datapath
              , file.path(".", "Results", input$fn_input$name))
    
    return(df_input)
    
  }##expression~END
  , filter = "top", options = list(scrollX = TRUE)
  
  )##output$df_import_DT~END

## b_Calc, AUjoin ####
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
    message("Results Log from KDHEtools Shiny App")
    message(Sys.time())
    inFile <- input$fn_input
    message(paste0("file = ", inFile$name))

    # Increment the progress bar, and update the detail text.
    incProgress(1/n_inc, detail = "Data, Initialize")
    Sys.sleep(0.25)

  # Read in saved file (known format)
  df_MonLocData <- NULL  # set as null for IF QC check prior to import
  fn_input <- file.path(".", "Results", "data_import.tsv")
  df_MonLocData <- read.delim(fn_input, stringsAsFactors = FALSE, sep = "\t")

  # QC, FAIL if TRUE
  if (is.null(df_MonLocData)) {
    return(NULL)
  }

  ### 1. Join crosswalk ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Join, crosswalk")
  Sys.sleep(0.25)
  
  df_joinedMonLocAU <- dplyr::left_join(df_MonLocData, df_ML2AU
                               , by = "MonitoringLocationIdentifier")

  ## 2. Filter matched MonLoc ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Filter")
  Sys.sleep(0.25)
  
  df_MonLocMatched <- df_joinedMonLocAU %>%
    filter(!is.na(AU_ID))

  # Save
  fn_data_match <- file.path(".", "Results"
                             , "results_MonitoringLocations_AU_Matched.csv")
  write.csv(df_MonLocMatched, fn_data_match, row.names = FALSE)

  ## 3. Unmatched MonLoc by Type ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Prepare Unmatched")
  Sys.sleep(0.25)
  
  df_Lake_Unmatched <- df_joinedMonLocAU %>%
    filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Lake_types)) %>% 
    select(-c(AU_ID, AU_NAME))

  df_Stream_Unmatched <- df_joinedMonLocAU %>%
    filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Stream_types)) %>% 
    select(-c(AU_ID, AU_NAME))

  ## 4. Spatial Joins ####
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
    message(paste("There ARE", num_sites, "lake monitoring locations missing AU data."))

    ### convert to geospatial layer (sf object)
    lakes_pts <- sf::st_as_sf(x = df_Lake_Unmatched, coords = c("LongitudeMeasure"
                                                              ,"LatitudeMeasure")
                              , crs = "+proj=longlat +datum=WGS84")%>%
      sf::st_transform(st_crs(GISlayer_lakes_transformed))

    ### spatial join
    lake_SpatJoin <- sf::st_join(lakes_pts, GISlayer_lakes_transformed
                                 , join = st_nearest_feature) %>% # join points and AUs
      select(MonitoringLocationIdentifier, MonitoringLocationName
             , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns

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
    message(paste("There ARE", num_sites, "stream monitoring locations missing AU data."))
    
    ### convert to geospatial layer (sf object)
    streams_pts <- sf::st_as_sf(x = df_Stream_Unmatched, coords = c("LongitudeMeasure"
                                                                ,"LatitudeMeasure")
                              , crs = "+proj=longlat +datum=WGS84")%>%
      sf::st_transform(st_crs(GISlayer_streams_transformed))
    
    ### spatial join
    stream_SpatJoin <- sf::st_join(streams_pts, GISlayer_streams_transformed
                                 , join = st_nearest_feature) %>% # join points and AUs
      select(MonitoringLocationIdentifier, MonitoringLocationName
             , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns
    
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
  
  # Save
  fn_data_SpatJoin <- file.path(".", "Results"
                             , "results_MonitoringLocations_AU_SpatialJoin.csv")
  write.csv(df_SpatJoin_Final, fn_data_SpatJoin, row.names = FALSE)

  ### 4d. Join to matched data ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Combine all")
  Sys.sleep(0.25)
  
  df_MonLocMatched$Dist_to_AU_m <- 0
  
  if (is.null(df_SpatJoin_Final)){
    df_MonLocAU_4Map <- df_MonLocMatched %>% 
      mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                    , TRUE ~ "Matched"))
  } else {
    df_MonLocAU_4Map <- rbind(df_MonLocMatched, df_SpatJoin_Final)%>% 
      mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                    , TRUE ~ "Matched"))
  } # END ~ if/else
  
  ## 5. Display data ####
  ### 5a. Table ####
  output$df_results_DT <- DT::renderDT({
    
    if (is.null(df_MonLocAU_4Map)) {
      return(NULL)
    }##IF~is.null~END
    
    return(df_MonLocAU_4Map)
    
  }##expression~END
  , filter = "top", options = list(scrollX = TRUE)
  
  )##output$df_import_DT~END
  
  ### 5b. Counts ####
  # Define a reactive value for storing the row count
  row_count_matched <- reactiveVal(0)
  row_count_unmatched <- reactiveVal(0)

  # Observe the changes in df_MonLocAU_4Map and update the row count
  observe({
    req(df_MonLocAU_4Map)
    nMatched <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
    nUnmatched <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched")
    row_count_matched(nMatched)
    row_count_unmatched(nUnmatched)
  })# END ~ observe
  
  # Render the valueBox based on the row count
  output$MatchCount <- renderValueBox({
    valueBox(
      value = row_count_matched(),
      subtitle = "Number of Monitoring Locations matched to AU crosswalk table",
      icon = icon("hashtag")
    )
  })# END ~ renderValueBox
  
  output$UnMatchCount <- renderValueBox({
    valueBox(
      value = row_count_unmatched(),
      subtitle = "Number of Monitoring Locations requiring AU spatial join",
      icon = icon("hashtag")
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
  
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Process completed")
  Sys.sleep(0.25)
  
  ## 7. Create zip ####
  # Create zip file
  # fn_4zip <- list.files(path = file.path("Results")
  #                       , pattern = "^results_"
  #                       , full.names = TRUE)
  # zip::zip(file.path("Results", "results.zip"), fn_4zip)
  # 
  # # enable download button
  # shinyjs::enable("b_downloadData")

  }) #END ~ withProgress
}) # END ~ observeEvent
})##shinyServer ~ END