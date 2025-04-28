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
  df_WQData <- read.delim(inFile$datapath
                         , header = TRUE
                         , sep = input$sep
                         , stringsAsFactors = FALSE)
  # QC, FAIL if TRUE
  if (is.null(df_WQData)) {
    return(NULL)
  }
  
  ## 1. Join crosswalk ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Join, crosswalk")
  Sys.sleep(0.25)
  
  df_joinedMonLocAU <- df_WQData %>%
    dplyr::left_join(df_ML2AU, by = "MonitoringLocationIdentifier")

  # 2. Filter matched MonLoc ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Filter")
  Sys.sleep(0.25)
  
  df_WQ_Matched <- df_joinedMonLocAU %>%
    filter(!is.na(AU_ID))
  
  df_Matched_AUs <- df_WQ_Matched %>% 
    select(MonitoringLocationIdentifier, MonitoringLocationName
           , MonitoringLocationTypeName, AU_ID, AU_NAME
           , LatitudeMeasure, LongitudeMeasure) %>% 
    distinct() %>% 
    rename(AU_ID_CrosswalkMatch = AU_ID
           , AU_NAME_CrosswalkMatch = AU_NAME) %>% 
    mutate(ATTAINS.assessmentunitidentifier = NA
           , ATTAINS.assessmentunitname = NA
           , ATTAINS.waterTypeCode = NA
           , AU_Info_Source = "Regional Crosswalk Table"
           , Need_Review = "No") %>% 
    select(Need_Review, AU_Info_Source, everything())
  # from crosswalk table

  # 3. Unmatched MonLoc by Type ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Data, Prepare Unmatched")
  Sys.sleep(0.25)
  
  df_WQ_Unmatched <- df_joinedMonLocAU %>%
    filter(is.na(AU_ID))

  # 4. Spatial Joins ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "GetATTAINS")
  Sys.sleep(0.25)
  
  # TODO only need to get attains if there are unmatched (need an if else here)
  ### 4a. Get ATTAINS ####
  # https://usepa.github.io/EPATADA/articles/TADAModule2.html
  
  df_WQ_Spatial <- EPATADA::TADA_MakeSpatial(df_WQ_Unmatched)
  # df_WQ_ATTAINS <- EPATADA::TADA_GetATTAINS(.data = df_WQ_Spatial
  #                                           , return_sf = FALSE)
  
  site_ids <- unique(df_WQ_Spatial$MonitoringLocationIdentifier)
  num_site_ids <- length(site_ids)
  chunk_size <- 10
  results_list <- list()

  # if number of unique site ids is <= chunk size
  if (num_site_ids <= chunk_size) {
    
    # Retrieve data from the API for the current chunk
    df_chunk_data <- df_WQ_Spatial %>% 
      EPATADA::TADA_GetATTAINS(return_sf = FALSE)
    
    # Store the result in the list
    results_list[[length(results_list) + 1]] <- df_chunk_data
    
    # Print progress message
    print(paste(num_site_ids, "of", num_site_ids, "unique site ids complete"))
    
    # Show notification
    showNotification(paste(num_site_ids, "of", num_site_ids, "unique site ids complete"),
                     type = "message")
    
  }
  
  # else number of unique ids is > chunk size
  else {
    for (i in seq(1, num_site_ids, by = chunk_size)) {
      # Get the current chunk of SiteIDs
      current_chunk <- site_ids[i:min(i + chunk_size - 1, num_site_ids)]
      
      # Filter df_WQ_Spatial for the current chunk
      df_chunk <- df_WQ_Spatial %>%
        filter(MonitoringLocationIdentifier %in% current_chunk)
      
      # Retrieve data from the API for the current chunk
      df_chunk_data <- df_chunk %>% 
        EPATADA::TADA_GetATTAINS(return_sf = FALSE)
      
      # Store the result in the list
      results_list[[length(results_list) + 1]] <- df_chunk_data
      
      # Print progress message
      print(paste(min(i + chunk_size - 1, num_site_ids), "of", num_site_ids, "unique site ids complete"))
      
      # Show notification
      showNotification(paste(min(i + chunk_size - 1, num_site_ids), "of", num_site_ids, "unique site ids complete"),
                       type = "message")
    }
  }

  # Combine all the results into a single dataframe
  df_WQ_ATTAINS <- bind_rows(results_list)
  
  # check
  # unique(df_WQ_ATTAINS$MonitoringLocationIdentifier)
  # length(unique(df_WQ_ATTAINS$MonitoringLocationIdentifier))
  
  ### 4b. Pull joined AUs ####
  df_ATTAINS_AUs <- df_WQ_ATTAINS %>% 
    sf::st_drop_geometry() %>% 
    select(MonitoringLocationIdentifier, MonitoringLocationName
           , MonitoringLocationTypeName, ATTAINS.assessmentunitidentifier
           , ATTAINS.assessmentunitname, ATTAINS.waterTypeCode
           , LatitudeMeasure, LongitudeMeasure) %>% 
    distinct() %>% 
    mutate(AU_ID_CrosswalkMatch = NA,
           AU_NAME_CrosswalkMatch = NA,
           AU_Info_Source = case_when((!is.na(ATTAINS.assessmentunitidentifier)) ~ "TADA ATTAINS Geospatial",
                                      TRUE ~ "No Match; Manual Match Needed"),
           Need_Review = "Yes") %>% 
    select(Need_Review, AU_Info_Source, everything())
  
  ### 4c. Merge AU dfs ####
  df_ML2AU_4Review <- rbind(df_ATTAINS_AUs, df_Matched_AUs)
  
  ### 4d. QC checks ####
  dup_check <- df_ML2AU_4Review %>% 
    count(MonitoringLocationIdentifier) %>% 
    filter(n > 1) %>% 
    mutate(FLAG_Duplicate = "Duplicate from ATTAINS") %>% 
    select(-c(n))
  
  df_ML2AU_4Review_v2 <- df_ML2AU_4Review %>%
    left_join(., dup_check, by = "MonitoringLocationIdentifier") %>% 
    mutate(ML_Type = case_when((MonitoringLocationTypeName == "Lake, Reservoir, Impoundment"
                                | MonitoringLocationTypeName == "Lake")
                               ~ "LAKE"
                               , (MonitoringLocationTypeName == "Stream"
                                  | MonitoringLocationTypeName == "River/Stream")
                               ~ "STREAM"
                               , (is.na(MonitoringLocationTypeName)) ~ NA
                               , TRUE ~ "OTHER")
           , AU_Type = case_when((ATTAINS.waterTypeCode == "LAKE, FRESHWATER")
                                 ~ "LAKE"
                                 , (ATTAINS.waterTypeCode == "RIVER")
                                 ~ "STREAM"
                                 , (is.na(ATTAINS.waterTypeCode)) ~ NA
                                 , TRUE ~ "OTHER")
           , FLAG_WaterType = case_when((ML_Type == AU_Type) ~ NA
                                        , (ML_Type != AU_Type) ~ "Type Mismatch"
                                        , (is.na(AU_ID_CrosswalkMatch) 
                                           | is.na(ATTAINS.assessmentunitidentifier))
                                        ~ NA
                                        , (is.na(ML_Type) | is.na(AU_Type))
                                        ~ "Missing Info"
                                        , TRUE ~ "ERROR")
           , Need_Review = case_when((!is.na(FLAG_WaterType)) ~ "Yes"
                                     , (!is.na(FLAG_Duplicate)) ~ "Yes"
                                     , TRUE ~ Need_Review)) %>% 
    relocate(FLAG_WaterType, .after = AU_Info_Source) %>% 
    relocate(FLAG_Duplicate, .after = FLAG_WaterType) %>%
    select(-c(ML_Type, AU_Type))
  
  # duplicate check
  
  ### 4a. Lake Spatial Join

  # # QC check
  # num_sites <- nrow(df_Lake_Unmatched)
  # 
  # # only run spatial join if there are sites
  # if(num_sites == 0){
  #   message(paste("There are NO lake monitoring locations missing AU data."))
  # 
  #   df_Lake_SpatJoin_Final <- NULL
  # 
  # } else {
  #   message(paste("There are", num_sites, "lake monitoring locations missing AU data."))
  # 
  #   ### convert to geospatial layer (sf object)
  #   lakes_pts <- sf::st_as_sf(x = df_Lake_Unmatched, coords = c("LongitudeMeasure"
  #                                                             ,"LatitudeMeasure")
  #                             , crs = "+proj=longlat +datum=WGS84")%>%
  #     sf::st_transform(st_crs(GISlayer_lakes_transformed))
  # 
  #   ### spatial join
  #   lake_SpatJoin <- sf::st_join(lakes_pts, GISlayer_lakes_transformed
  #                                , join = st_nearest_feature) # join points and AUs
  #     # select(MonitoringLocationIdentifier, MonitoringLocationName
  #     #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME, cultural_u
  #     #        , drinkingwa, ecological, fishconsum, recreation, other_use) %>%  # trim unneccessary columns
  #     # rename(Cultural_Use = cultural_u
  #     #        , DrinkingWater_Use = drinkingwa
  #     #        , Ecological_Use = ecological
  #     #        , FishConsumption_Use = fishconsum
  #     #        , Recreational_Use = recreation
  #     #        , Other_Use = other_use)
  # 
  #   ### determine distance (m) between points and nearest feature
  #   near_feat <- sf::st_nearest_feature(lakes_pts, GISlayer_lakes_transformed)
  #   Dist_to_AU_m <- sf::st_distance(lakes_pts, GISlayer_lakes_transformed[near_feat,]
  #                                   , by_element = TRUE)
  # 
  #   ### join distance measurements to join results
  #   lake_SpatJoin2 <- cbind(lake_SpatJoin, Dist_to_AU_m)
  # 
  #   ### results and export data
  #   df_Lake_SpatJoin_Final <- lake_SpatJoin2 %>%
  #     sf::st_transform(4326) %>%
  #     mutate(LongitudeMeasure = unlist(map(geometry,1)),
  #            LatitudeMeasure = unlist(map(geometry,2))) %>%
  #     sf::st_drop_geometry()
  # 
  # }# end if/else statement


  ### 4b. Stream Spatial Join
  # Increment the progress bar, and update the detail text.
  # incProgress(1/n_inc, detail = "Spatial Join, Streams")
  # Sys.sleep(0.25)
  # 
  # # QC check
  # num_sites <- nrow(df_Stream_Unmatched)
  # 
  # # only run spatial join if there are sites
  # if(num_sites == 0){
  #   message(paste("There are NO stream monitoring locations missing AU data."))
  #   
  #   df_Stream_SpatJoin_Final <- NULL
  #   
  # } else {
  #   message(paste("There are", num_sites, "stream monitoring locations missing AU data."))
  #   
  #   ### convert to geospatial layer (sf object)
  #   streams_pts <- sf::st_as_sf(x = df_Stream_Unmatched, coords = c("LongitudeMeasure"
  #                                                               ,"LatitudeMeasure")
  #                             , crs = "+proj=longlat +datum=WGS84")%>%
  #     sf::st_transform(st_crs(GISlayer_streams_transformed))
  #   
  #   ### spatial join
  #   stream_SpatJoin <- sf::st_join(streams_pts, GISlayer_streams_transformed
  #                                , join = st_nearest_feature) # join points and AUs
  #     # select(MonitoringLocationIdentifier, MonitoringLocationName
  #     #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns
  #   
  #   ### determine distance (m) between points and nearest feature
  #   near_feat <- sf::st_nearest_feature(streams_pts, GISlayer_streams_transformed)
  #   Dist_to_AU_m <- sf::st_distance(streams_pts, GISlayer_streams_transformed[near_feat,]
  #                                   , by_element = TRUE)
  #   
  #   ### join distance measurements to join results
  #   stream_SpatJoin2 <- cbind(stream_SpatJoin, Dist_to_AU_m)
  #   
  #   ### results and export data
  #   df_Stream_SpatJoin_Final <- stream_SpatJoin2 %>%
  #     sf::st_transform(4326) %>%
  #     mutate(LongitudeMeasure = unlist(map(geometry,1)),
  #            LatitudeMeasure = unlist(map(geometry,2))) %>%
  #     sf::st_drop_geometry()
  #   
  # }# end if/else statement
  
  ### 4c. Join data 
  # # Increment the progress bar, and update the detail text.
  # incProgress(1/n_inc, detail = "Spatial Join, Combine")
  # Sys.sleep(0.25)
  # 
  # if (is.null(df_Lake_SpatJoin_Final) & is.null(df_Stream_SpatJoin_Final)){
  #   df_SpatJoin_Final <- NULL # both datasets are NULL
  # } else if (!is.null(df_Lake_SpatJoin_Final) 
  #            & is.null(df_Stream_SpatJoin_Final)) {
  #   df_SpatJoin_Final <- df_Lake_SpatJoin_Final # only streams dataset is NULL
  # } else if (is.null(df_Lake_SpatJoin_Final) 
  #            & !is.null(df_Stream_SpatJoin_Final)) {
  #   df_SpatJoin_Final <- df_Stream_SpatJoin_Final # only lakes dataset is NULL
  # } else {
  #   df_SpatJoin_Final <- rbind(df_Lake_SpatJoin_Final, df_Stream_SpatJoin_Final)
  #   # both datasets have data
  # }# END ~ IF/ELSE

  ### 4d. Join to matched data
  # Increment the progress bar, and update the detail text.
  # incProgress(1/n_inc, detail = "Data, Combine all")
  # Sys.sleep(0.25)
  # 
  # df_MonLocMatched$Dist_to_AU_m <- 0
  # 
  # if (is.null(df_SpatJoin_Final)){
  #   df_MonLocAU_4Map <- df_MonLocMatched %>% 
  #     mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
  #                                   , TRUE ~ "Matched")) %>%
  #     select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
  #               , Recreational_Use, Other_Use))
  # } else {
  #   df_MonLocAU_4Map <- rbind(df_MonLocMatched, df_SpatJoin_Final)%>% 
  #     mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
  #                                   , TRUE ~ "Matched")) %>%
  #     select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
  #               , Recreational_Use, Other_Use))
  # } # END ~ if/else
  
  ## 5. Display data ####
  ### 5a. Table ####
  output$df_results_DT <- DT::renderDT({
    
    if (is.null(df_ML2AU_4Review_v2)) {
      return(NULL)
    }##IF~is.null~END
    
    # Filter df_MonLocAU_4Map based on input_site_choice
    filtered_df <- df_ML2AU_4Review_v2
    
    if (input$input_site_choice != "") {
      mySite <- input$input_site_choice
      
      filtered_df <- df_ML2AU_4Review_v2 %>% 
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
  # # Define a reactive value for storing the row count
  # row_count_matched <- reactiveVal(0)
  # row_count_unmatched_G50 <- reactiveVal(0)
  # row_count_unmatched_L50 <- reactiveVal(0)
  # 
  # # Observe the changes in df_MonLocAU_4Map and update the row count
  # observe({
  #   req(df_MonLocAU_4Map)
  #   nMatched <- sum(df_MonLocAU_4Map$MatchGroup == "Matched")
  #   nUnmatched_G50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
  #                         & df_MonLocAU_4Map$Dist_to_AU_m >= 50)
  #   nUnmatched_L50 <- sum(df_MonLocAU_4Map$MatchGroup == "UnMatched"
  #                         & df_MonLocAU_4Map$Dist_to_AU_m < 50)
  #   row_count_matched(nMatched)
  #   row_count_unmatched_G50(nUnmatched_G50)
  #   row_count_unmatched_L50(nUnmatched_L50)
  # })# END ~ observe
  # 
  # # Render the valueBox based on the row count
  # output$MatchCount <- renderValueBox({
  #   valueBox(value = row_count_matched()
  #     , subtitle = "Number of Monitoring Locations matched to AU crosswalk table"
  #     , icon = icon("hashtag")
  #     , color = "aqua"
  #   )
  # })# END ~ renderValueBox
  # 
  # output$UnMatchCount_G50 <- renderValueBox({
  #   valueBox(value = row_count_unmatched_G50()
  #     , subtitle = "Number of Monitoring Locations with AU spatial join >= 50m"
  #     , icon = icon("hashtag")
  #     , color = "red"
  #   )
  # })# END ~ renderValueBox
  # 
  # output$UnMatchCount_L50 <- renderValueBox({
  #   valueBox(value = row_count_unmatched_L50()
  #     , subtitle = "Number of Monitoring Locations with AU spatial join < 50m"
  #     , icon = icon("hashtag")
  #     , color = "yellow"
  #   )
  # })# END ~ renderValueBox
  
  ## 6. Map ####
  # Increment the progress bar, and update the detail text.
  incProgress(1/n_inc, detail = "Display results")
  Sys.sleep(0.25)
  
  output$mymap <- renderLeaflet({
    req(df_ML2AU_4Review_v2)
    
    # Subset data by Match group
    data_Match <- df_ML2AU_4Review_v2 %>% 
      filter(AU_Info_Source == "Regional Crosswalk Table")
    
    data_ATTAINS <- df_ML2AU_4Review_v2 %>% 
      filter(AU_Info_Source == "TADA ATTAINS Geospatial") 
    
    data_NoMatch <- df_ML2AU_4Review_v2 %>% 
      filter(AU_Info_Source == "No Match; Manual Match Needed")
    
    leaflet("mymap", options = leafletOptions(attributionControl = FALSE)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
      addCircleMarkers(data = data_Match, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                       , group = "Matched Sites"
                       , popup = paste("SiteID:", data_Match$MonitoringLocationIdentifier, "<br>"
                                       # ,"Site Name:", data_match$MonitoringLocationName, "<br>"
                                       # ,"Site Type:", data_match$MonitoringLocationTypeName, "<br>"
                                       # ,"State:", data_match$State, "<br>"
                                       # ,"AU_ID:", data_match$AU_ID, "<br>"
                                       # ,"AU NAME:", data_match$AU_NAME
                                       )
                       , color = "black", fillColor = "blue", fillOpacity = 1, stroke = TRUE
                       # , clusterOptions = markerClusterOptions()
                       ) %>% 
      addCircleMarkers(data = data_ATTAINS, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                       , group = "ATTAINS Sites"
                       , popup = paste("SiteID:", data_ATTAINS$MonitoringLocationIdentifier, "<br>"
                                       # ,"Site Name:", data_unmatch$MonitoringLocationName, "<br>"
                                       # ,"Site Type:", data_unmatch$MonitoringLocationTypeName, "<br>"
                                       # ,"State:", data_unmatch$State, "<br>"
                                       # ,"AU_ID:", data_unmatch$AU_ID, "<br>"
                                       # ,"AU NAME:", data_unmatch$AU_NAME
                                       )
                       , color = "black", fillColor = "green", fillOpacity = 1, stroke = TRUE
                       # , clusterOptions = markerClusterOptions()
                       ) %>%
      addCircleMarkers(data = data_NoMatch, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                       , group = "No Match Sites"
                       , popup = paste("SiteID:", data_NoMatch$MonitoringLocationIdentifier, "<br>"
                                       # ,"Site Name:", data_unmatch$MonitoringLocationName, "<br>"
                                       # ,"Site Type:", data_unmatch$MonitoringLocationTypeName, "<br>"
                                       # ,"State:", data_unmatch$State, "<br>"
                                       # ,"AU_ID:", data_unmatch$AU_ID, "<br>"
                                       # ,"AU NAME:", data_unmatch$AU_NAME
                       )
                       , color = "black", fillColor = "red", fillOpacity = 1, stroke = TRUE
                       # , clusterOptions = markerClusterOptions()
                       ) %>%
      # addPolylines(data = streams_simp_shp, color = "blue", weight = 3
      #              , label = streams_simp_shp$AU_ID, group = "River AUs"
      #              , popup = paste("<b> AU_ID:</b>", streams_simp_shp$AU_ID, "<br>"
      #                              ,"<b> AU_Name:</b>", streams_simp_shp$AU_NAME)) %>%
      # addPolygons(data = lakes_shp, color = "black", weight = 2, opacity = 1
      #             , fillColor = "#df65b0", fillOpacity = 0.25, group = "Lake AUs"
      #             , popup = paste("<b> AU_ID:</b>", lakes_shp$AU_ID, "<br>"
      #                             ,"<b> AU_Name:</b>", lakes_shp$AU_NAME)) %>%
      addLayersControl(overlayGroups = c("Matched Sites", "ATTAINS Sites"
                                         , "No Match Sites")
                       ,baseGroups = c("Esri WSM", "Esri Ortho")
                       ,options = layersControlOptions(collapsed = TRUE)) %>%
      # hideGroup(c("Matched Sites", "Unmatched Sites"
      #             , "River AUs", "Lake AUs")) %>%
      addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                 , position = "bottomright") %>%
      addLegend(position = "bottomleft", 
                colors = c("blue", "green", "red"), 
                labels = c("Matched Sites", "ATTAINS Sites", "No Match Sites"), 
                title = "Site Types")
    
    }) # END ~ renderLeaflet
  
  ## Map Zoom ####
  # Map that filters output data to a single location
  observeEvent(input$input_site_choice, {
    req(input$input_site_choice != "")
    
    mySite <- input$input_site_choice
    
    df_MonLocAU_select <- df_ML2AU_4Review_v2 %>% 
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
  fn_results <- "MonLocations_to_AssessUnits_NeedsReview.csv"
  dn_results <- path_results
  pn_results <- file.path(dn_results, fn_results)
  write.csv(df_ML2AU_4Review_v2, pn_results, row.names = FALSE)
  
  # Save, SpatJoin
  # fn_results <- "MonitoringLocations_AU_SpatialJoin.csv"
  # dn_results <- path_results
  # pn_results <- file.path(dn_results, fn_results)
  # write.csv(df_SpatJoin_Final, pn_results, row.names = FALSE)
  
  # Create zip file of results
  fn_4zip <- list.files(path = path_results
                        , full.names = TRUE)
  zip::zip(file.path(path_results, "results.zip"), fn_4zip)
  
  # button, enable, download
  # shinyjs::enable("b_download")
  
  # Pop up ####
  
  data_Match <- df_ML2AU_4Review_v2 %>% 
    filter(AU_Info_Source == "Regional Crosswalk Table")
  
  data_ATTAINS <- df_ML2AU_4Review_v2 %>% 
    filter(AU_Info_Source == "TADA ATTAINS Geospatial") 
  
  data_NoMatch <- df_ML2AU_4Review_v2 %>% 
    filter(AU_Info_Source == "No Match; Manual Match Needed")
  
  
  n_match <- sum(df_ML2AU_4Review_v2$AU_Info_Source == "Regional Crosswalk Table")
  n_ATTAINS <- sum(df_ML2AU_4Review_v2$AU_Info_Source == "TADA ATTAINS Geospatial")
  n_noMatch <- sum(df_ML2AU_4Review_v2$AU_Info_Source == "No Match; Manual Match Needed")
  msg <- paste0("The AU join is complete, but results take ~30 seconds to render in the app!"
                , " Please be patient and wait for the map to render to use in QC review.", "\n\n"
                , "Number of sites matched to AUs via regional crosswalk table = ", n_match, "\n\n"
                , "Number of sites spatially joined to AUs via ATTAINS = ", n_ATTAINS, "\n\n"
                , "Number of sites with no match; manual review required = ", n_noMatch, "\n\n")
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
  )##download
  
})##shinyServer ~ END