# Import Page

function() {
  tabPanel("Join AUs"
           , fluidRow(
             box(width = 4, status = "info", solidHeader = TRUE,
                 title = "App Control"
            , p("Use the App Control to correctly assign sites to AUs."
              , "Sites will be joined to the existing "
              ,a("AU crosswalk table."
                 , href = "https://raw.githubusercontent.com/Blocktt/ShinyAppDocuments/main/AUSpatialJoin/MonLoc_to_AU_Crosswalk_20240415.xlsx"
                 , target="_blank")
              , "Sites not included in the table will be spatially joined"
              , "to the EPA ATTAINS spatial layers. These sites will need analyst review."
              , "Results will be shown via the table and map below once the join is complete.")
            , h4("1. Join Monitoring Locations to AUs")
            , actionButton("b_Calc", "Join AUs")
            , h4("2. Download Results")
            , p("All input and output files will be available in a single zip file.")
            , shinyjs::disabled(downloadButton("b_download"
                                               , "Download Results"))
            , h4("3. Select Monitoring Location")
            , p("Once map and table are loaded, enter a monitoring location identifier below"
                , "to zoom into that location.")
            , textInput(inputId = "input_site_choice"
                            , label = "MonitoringLocationIdentifier:"
                            , value = "")
             ) # END box
            # , valueBoxOutput("MatchCount", width = 3)
            # , valueBoxOutput("UnMatchCount_L50", width = 3)
            # , valueBoxOutput("UnMatchCount_G50", width = 3)
            , box(width = 8, status = "info", solidHeader = TRUE
                  , title = "Results Map"
                  , leafletOutput("mymap")
            ) # END box
           ) # END fluidRow
           , fluidRow(
             box(width = 6, status = "info", solidHeader = TRUE
                 , title = "Results Table"
                 , DT::dataTableOutput("df_results_DT")
             ) # END box
             , valueBoxOutput("MatchCount", width = 2)
             , valueBoxOutput("UnMatchCount_L50", width = 2)
             , valueBoxOutput("UnMatchCount_G50", width = 2)
             # , box(width = 6, status = "info", solidHeader = TRUE
             #       , title = "Results Map"
             #       , leafletOutput("mymap")
             # ) # END box
           )# END fluidRow
  )##tabPanel ~ END
}## FUNCTION ~ END
