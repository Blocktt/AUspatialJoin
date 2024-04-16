# Import Page

function() {
  tabPanel("Join AUs"
           , fluidRow(
             box(width = 4, status = "info", solidHeader = TRUE,
                 title = "App Control"
            , h4("Join Monitoring Locations to AUs")
            , actionButton("b_Calc", "Join AUs")
             ) # END box
            , valueBoxOutput("MatchCount", width = 4)
            , valueBoxOutput("UnMatchCount", width = 4)
           ) # END fluidRow
           , fluidRow(
             box(width = 6, status = "info", solidHeader = TRUE
                 , title = "Results Table"
                 , DT::dataTableOutput("df_results_DT")
             ) # END box
             , box(width = 6, status = "info", solidHeader = TRUE
                   , title = "Results Map"
                   , leafletOutput("mymap")
             ) # END box
           )# END fluidRow
  )##tabPanel ~ END
}## FUNCTION ~ END
