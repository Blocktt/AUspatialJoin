# Import Page

function() {
  tabPanel("Import Data"
           , fluidRow(
             box(width = 4, status = "info", solidHeader = TRUE,
                 title = "App Control",
                 h4("Load File")
                 , p("Only comma-separated or tab-separated files.")
                 , h5("Select file parameters")
                 #, checkboxInput('header', 'Header', TRUE)
                 , radioButtons("sep", "Separator",
                                c(Comma = ",",
                                  # Semicolon = ";",
                                  Tab = "\t"),
                                ',')
                 , fileInput("fn_input"
                             , label = "Choose file to upload"
                             , multiple = FALSE
                             , accept = c("text/csv"
                                          , "text/comma-separated-values"
                                          , "text/tab-separated-values"
                                          , "text/plain"
                                          , ".csv"
                                          , ".tsv"
                                          , ".txt")
                 )##fileInput~END
                 , tags$hr()
                 , p("The 'separator' allows the user to upload different file formats
            (e.g., csv, tsv, or txt).")
            , p("Files for all operations will be uploaded through this interface.")
            , p(paste0("File uploads are limited to a maximum of "
                       , mb_limit
                       , " MB in size."))
             ) # END box
            , box(width = 8, status = "info", solidHeader = TRUE,
                  title = "Preview uploaded data"
              , DT::dataTableOutput("df_import_DT")
            ) # END box
           ) # END fluidRow
  )##tabPanel ~ END
}## FUNCTION ~ END
