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

  # INPUT Display Names ####

  output$fn_input_display_bcg <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_bcg

  output$fn_input_display_taxatrans <- renderText({
    inFile <- input$fn_input

    if (is.null(inFile)) {
      return("..No file uploaded yet...")
    }##IF~is.null~END

    return(paste0("'", inFile$name, "'"))

  })## fn_input_display_taxatrans

  # ~~~~IMPORT~~~~----
  # IMPORT ----
  file_watch <- reactive({
    # trigger for df_import()
    input$fn_input
  })## file_watch

  ## IMPORT, df_import ####
  df_import <- eventReactive(file_watch(), {
    # use a multi-item reactive so keep on a single line (if needed later)

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

    # # Add "Results" folder if missing
    # boo_Results <- dir.exists(file.path(".", "results"))
    # if(boo_Results==FALSE){
    #   dir.create(file.path(".", "Results"))
    # }

    # Remove existing files in "results"
    clean_results()

    ### Mod, BCG_ATTR----
    # Read user imported file
    # Add extra colClasses parameter for BCG_Attr
    # the "i" values default to complex numbers
    # many permutations of BCG_Attr so check for it first then import

    df_header <- read.delim(fn_inFile
                            , header = TRUE
                            , sep = sep_user
                            , stringsAsFactors = FALSE
                            , na.strings = c("", "NA")
                            , nrows = 0)
    col_num_bcgattr <- grep("BCG_ATTR", toupper(names(df_header)))
    classes_df <- sapply(df_header, class)
    col_name_bcgattr <- names(df_header)[col_num_bcgattr]

    # 2023-12-04, modify for MN with multiple BCG_ATTR fields

    if (length(col_num_bcgattr) == 0) {
      # BCG_Attr present = FALSE
      # define classes = FALSE
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
    } else if (!"complex" %in%  as.vector(classes_df[col_num_bcgattr])) {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = FALSE
      # define classes on import = FALSE (change to text after import)
      df_input <- read.delim(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA"))
      #df_input[, col_num_bcgattr] <- as.character(df_input[, col_num_bcgattr])
      # doesn't work for multiple columns
      # df_input[, col_num_bcgattr] <- lapply(df_input[, col_num_bcgattr], as.character)
      # error for some files
      # use a loop :(
      for (b in col_num_bcgattr) {
        df_input[, b] <- as.character(df_input[, b])
      }## FOR ~ b
      #
    } else {
      # BCG_Attr present = TRUE
      # BCG_Attr Class is complex = TRUE
      # define classes on import = TRUE
      #classes_df <- sapply(df_header, class)
      classes_df[col_num_bcgattr] <- "character"
      df_input <- read.table(fn_inFile
                             , header = TRUE
                             , sep = sep_user
                             , stringsAsFactors = FALSE
                             , na.strings = c("", "NA")
                             #, colClasses = c(col_name_bcgattr = "character"))
                             # , colClasses = classes_df)
                             , colClasses = classes_df[col_name_bcgattr])
    }## IF ~ col_num_bcgattr



    # OLD
    # Will get a 'warning' for unknown columns but harmless
    # df_input <- read.delim(fn_inFile
    #                        , header = TRUE
    #                        , sep = ","
    #                        , stringsAsFactors = FALSE
    #                        , colClasses = c("BCG_Attr" = "character"
    #                                         , "BCG_ATTR" = "character"
    #                                         , "bcg_attr" = "character"
    #                                         , "BCG_attr" = "character"))

    # Copy user files to results sub-folder
    copy_import_file(import_file = input$fn_input)

    ## button, enable, calc ----
    shinyjs::enable("b_calc_taxatrans")
    shinyjs::enable("b_calc_bcg")

    # update cb_taxatrans_sum
    # doesn't work here as timing is after the file is created

    return(df_input)

  })##output$df_import ~ END

  ## IMPORT, df_import_DT ----
  output$df_import_DT <- DT::renderDT({
    df_data <- df_import()
  }##expression~END
  , filter = "top"
  , caption = "Table. Imported data."
  , options = list(scrollX = TRUE
                   , pageLength = 5
                   , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                   , autoWidth = TRUE)
  )##df_import_DT~END

  ## IMPORT, col names ----
  col_import <- eventReactive(file_watch(), {

    inFile <- input$fn_input

    if (is.null(inFile)) {
      return(NULL)
    }##IF~is.null~END

    # temp df
    df_temp <- df_import()
    # Column Names
    input_colnames <- names(df_temp)
    #
    return(input_colnames)

  })## col_import

  # ~~~~FILE BUILDER~~~~ ----
  # FB, TAXATRANS ----
  ## TaxaTrans, UI ----

  output$UI_taxatrans_pick_official <- renderUI({
    str_col <- "Calculation"
    selectInput("taxatrans_pick_official"
                , label = str_col
                , choices = c("", df_pick_taxoff[, "project"])
                , multiple = FALSE)
  })## UI_colnames

  # output$UI_taxatrans_pick_official_project <- renderUI({
  #   str_col <- "Official Taxa Data, Column Taxa_ID"
  #   selectInput("taxatrans_pick_official_project"
  #               , label = str_col
  #               , choices = names(df_pick_taxoff)
  #               , multiple = FALSE)
  # })## UI_colnames

  output$UI_taxatrans_user_col_taxaid <- renderUI({
    str_col <- "Column, TaxaID"
    selectInput("taxatrans_user_col_taxaid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "TaxaID"
                , multiple = FALSE)
  })## UI_colnames

  output$UI_taxatrans_user_col_drop <- renderUI({
    str_col <- "Columns to Drop"
    selectInput("taxatrans_user_col_drop"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_groupby <- renderUI({
    str_col <- "Columns to Keep in Output"
    selectInput("taxatrans_user_col_groupby"
                , label = str_col
                , choices = c("", names(df_import()))
                , multiple = TRUE)
  })## UI_colnames

  output$UI_taxatrans_user_col_sampid <- renderUI({
    str_col <- "Column, Unique Sample Identifier (e.g., SampleID)"
    selectInput("taxatrans_user_col_sampid"
                , label = str_col
                , choices = c("", names(df_import()))
                , selected = "SampleID"
                , multiple = FALSE)
  })## UI_colnames

  # ## TaxaTrans, combine ----
  ## b_Calc_TaxaTrans ----
  observeEvent(input$b_calc_taxatrans, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 00, Initialize ----
      prog_detail <- "Calculation, Taxa Translator..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 6
      prog_sleep <- 0.25

      ## Calc, 01, Import User Data ----
      prog_detail <- "Import Data, User"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      # path_results_sub <- file.path(path_results
      #                               , paste(abr_results, fn_abr, sep = "_"))
      # # Add "Results" folder if missing
      # boo_Results <- dir.exists(file.path(path_results_sub))
      # if (boo_Results == FALSE) {
      #   dir.create(file.path(path_results_sub))
      # }
      # Add "reference" folder if missing
      path_results_ref <- file.path(path_results, dn_files_ref)
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }
      # Add "Results" folder based on user selection later in this step

      # button, disable, download
      shinyjs::disable("b_download_taxatrans")

      # Import data
      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      ## Calc, 02, Gather and Test Inputs  ----
      prog_detail <- "QC Inputs"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Fun Param, Define
      sel_proj <- input$taxatrans_pick_official
      sel_user_taxaid <- input$taxatrans_user_col_taxaid
      #sel_col_drop <- unlist(input$taxatrans_user_col_drop)
      # sel_user_ntaxa <- input$taxatrans_user_col_n_taxa
      sel_user_groupby <- unlist(input$taxatrans_user_col_groupby)
      sel_summ <- input$cb_TaxaTrans_Summ
      # sel_user_indexclass <- input$taxatrans_user_col_indexclass
      # sel_user_gprr <- input$taxatrans_user_col_gprr

      fn_taxoff <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                  , "filename"]
      fn_taxoff_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "metadata_filename"]
      col_taxaid_official_match <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                  , "taxaid"]
      col_taxaid_official_project <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                                    , "calc_taxaid"]
      # col_drop_project <- unlist(strsplit(df_pick_taxoff[df_pick_taxoff$project == sel_proj
      #                                    , "col_drop"], ","))
      fn_taxoff_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_filename"]
      fn_taxoff_attr_meta <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                       , "attributes_metadata_filename"]
      col_taxaid_attr <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                        , "attributes_taxaid"]
      sel_user_sampid <- input$taxatrans_user_col_sampid

      sel_taxaid_drop <-  df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                     , "taxaid_drop"]
      dir_proj_results <- df_pick_taxoff[df_pick_taxoff$project == sel_proj
                                         , "dir_results"]

      # include = yes; unique(sel_user_groupby)
      # include sampid, taxaid, and n_taxa so not dropped
      user_col_keep <- names(df_input)[names(df_input) %in% c(sel_user_groupby
                                                              , sel_user_sampid
                                                              , sel_user_taxaid
                                                              # , sel_user_ntaxa
                                                              )]
      # flip to col_drop
      user_col_drop <- names(df_input)[!names(df_input) %in% user_col_keep]

      # Fun Param, Test

      if (sel_proj == "") {
        # end process with pop up
        msg <- "'Calculation' is missing!"
        shinyalert::shinyalert(title = "Taxa Translate"
                               , text = msg
                               , type = "error"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE)
        # validate(msg)
      }## IF ~ sel_proj

      if (is.na(fn_taxoff_meta) | fn_taxoff_meta == "") {
        # set value to NULL
        df_official_metadata <- NULL
      }## IF ~ fn_taxaoff_meta

      # if (is.na(sel_user_ntaxa) | sel_user_ntaxa == "") {
      #   sel_user_ntaxa <- NULL
      # }## IF ~ fn_taxaoff_meta

      if (is.null(sel_summ)) {
        sel_summ <- FALSE
      }## IF ~ sel_summ

      if (sel_taxaid_drop == "NULL") {
        sel_taxaid_drop <- NULL
      }## IF ~ sel_taxaid_drop

      # if (is.na(sel_user_indexclass) | sel_user_indexclass == "") {
      #   sel_user_indexclass <- NULL
      # }## IF ~ sel_user_indexclass

      message(paste0("User response to summarize duplicate sample taxa = "
               , sel_summ))

      dir_proj_results <- paste("FLCoral_BCG", dir_proj_results, sep = "_")

      dn_files <- paste(abr_results, dir_proj_results, sep = "_")

      # Add "Results" folder if missing
      path_results_sub <- file.path(path_results, dn_files)
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      ## Calc, 03, Import Official Data (and Metadata)  ----
      prog_detail <- "Import Data, Official and Metadata"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      ## Data,  Official Taxa----
      url_taxoff <- file.path(url_bmt_base
                              , "taxa_official"
                              , fn_taxoff)
      temp_taxoff <- tempfile(fileext = ".csv")
      httr::GET(url_taxoff, write_disk(temp_taxoff))

      df_taxoff <- read.csv(temp_taxoff)

      ## Data, Official Taxa, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_meta <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , fn_taxoff_meta)
        temp_taxoff_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_meta, write_disk(temp_taxoff_meta))

        df_taxoff_meta <- read.csv(temp_taxoff_meta)
      }## IF ~ fn_taxaoff_meta

      ## Data, Official Attributes----
      if (!is.null(fn_taxoff_attr)) {
        url_taxoff_attr <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , fn_taxoff_attr)
        temp_taxoff_attr <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr, write_disk(temp_taxoff_attr))

        df_taxoff_attr <- read.csv(temp_taxoff_attr)
      }## IF ~ fn_taxoff_attr

      ## Data, Official Attributes, Meta Data----
      if (!is.null(fn_taxoff_meta)) {
        url_taxoff_attr_meta <- file.path(url_bmt_base
                                     , "taxa_official"
                                     , fn_taxoff_attr_meta)
        temp_taxoff_attr_meta <- tempfile(fileext = ".csv")
        httr::GET(url_taxoff_attr_meta, write_disk(temp_taxoff_attr_meta))

        df_taxoff_attr_meta <- read.csv(temp_taxoff_attr_meta)
      }## IF ~ fn_taxaoff_meta


      ## Calc, 03, Run Function ----
      prog_detail <- "Calculate, Taxa Trans"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # function parameters
      df_user                 <- df_input
      df_user$N_TAXA <- 1
      df_official             <- df_taxoff
      df_official_metadata    <- df_taxoff_meta
      taxaid_user             <- sel_user_taxaid
      taxaid_official_match   <- col_taxaid_official_match
      taxaid_official_project <- col_taxaid_official_project
      taxaid_drop             <- sel_taxaid_drop
      col_drop                <- user_col_drop #NULL #sel_col_drop
      sum_n_taxa_boo          <- TRUE
      sum_n_taxa_col          <- "N_TAXA"
      sum_n_taxa_group_by     <- c(sel_user_sampid
                                   , sel_user_taxaid
                                   , sel_user_groupby)

      ## run the function ----
      taxatrans_results <- BioMonTools::taxa_translate(df_user
                                                       , df_official
                                                       , df_official_metadata
                                                       , taxaid_user
                                                       , taxaid_official_match
                                                       , taxaid_official_project
                                                       , taxaid_drop
                                                       , col_drop
                                                       , sum_n_taxa_boo
                                                       , sum_n_taxa_col
                                                       , sum_n_taxa_group_by)

      ## Munge ----

      # Remove non-project taxaID cols
      # Specific to shiny project, not a part of the taxa_translate function
      # col_keep <- !names(taxatrans_results$merge) %in% col_drop_project
      # taxatrans_results$merge <- taxatrans_results$merge[, col_keep]

      # Attributes if have 2nd file
      if (!is.na(fn_taxoff_attr)) {

        df_ttrm <- taxatrans_results$merge
        # drop translation file columns
        col_keep_ttrm <- names(df_ttrm)[names(df_ttrm) %in% c(sel_user_sampid
                                                            , sel_user_taxaid
                                                            # , sel_user_ntaxa
                                                            , "Match_Official"
                                                            , sel_user_groupby)]
        df_ttrm <- df_ttrm[, col_keep_ttrm]

        # merge with attributes
        df_merge_attr <- merge(df_ttrm
                               , df_taxoff_attr
                               , by.x = taxaid_user
                               , by.y = col_taxaid_attr
                               , all.x = TRUE
                               , sort = FALSE
                               , suffixes = c("_xDROP", "_yKEEP"))
        # Drop duplicate names from Trans file (x)
        col_keep <- names(df_merge_attr)[!grepl("_xDROP$"
                                                , names(df_merge_attr))]
        df_merge_attr <- df_merge_attr[, col_keep]
        # KEEP and rename duplicate names from Attribute file (y)
        names(df_merge_attr) <- gsub("_yKEEP$", "", names(df_merge_attr))
        # Save back to results list
        taxatrans_results$merge <- df_merge_attr

        # QC check
        # testthat::expect_equal(nrow(df_merge_attr), nrow(df_ttrm))
        # testthat::expect_equal(sum(df_merge_attr[, sel_user_ntaxa], na.rm = TRUE)
        #                        , sum(df_ttrm[, sel_user_ntaxa], na.rm = TRUE))
      }## IF ~ !is.na(fn_taxoff_attr)

      # Reorder by SampID and TaxaID
      taxatrans_results$merge <- taxatrans_results$merge[
           order(taxatrans_results$merge[, sel_user_sampid]
                   , taxatrans_results$merge[, sel_user_taxaid]), ]

      # Add input filenames
      taxatrans_results$merge[, "file_taxatrans"] <- fn_taxoff
      taxatrans_results$merge[, "file_attributes"] <- fn_taxoff_attr


      # Resort columns
      col_start <- c(sel_user_sampid
                     , sel_user_taxaid
                     # , sel_user_ntaxa
                     , "file_taxatrans"
                     , "file_attributes")
      col_other <- names(taxatrans_results$merge)[!names(taxatrans_results$merge)
                                                  %in% col_start]
      taxatrans_results$merge <- taxatrans_results$merge[, c(col_start
                                                             , col_other)]

      # Convert required file names to standard
      ## do at end so don't have to modify any other variables
      boo_req_names <- TRUE
      if (boo_req_names == TRUE) {
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_sampid] <- "SampleID"
        names(taxatrans_results$merge)[names(taxatrans_results$merge)
                                       %in% sel_user_taxaid] <- "TaxaID"
        # names(taxatrans_results$merge)[names(taxatrans_results$merge)
        #                                %in% sel_user_ntaxa] <- "N_Taxa"
      }## IF ~ boo_req_names

      # Hack/Fix
      # Noteworthy NA causing issue later in Shiny app
      # 20231201, only if have Noteworthy
      # if ("NOTEWORTHY" %in% toupper(taxatrans_results$merge)) {
      #   taxatrans_results$merge$Noteworthy <- ifelse(is.na(taxatrans_results$merge$Noteworthy)
      #                                                , FALSE
      #                                                , TRUE)
      # }## IF ~ Noteworthy

      # need index class brought through


      ## Calc, 04, Save Results ----
      prog_detail <- "Save Results"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Save files

      ## File version names
      df_save <- data.frame(Calculation = sel_proj
                            , OperationalTaxonomicUnit = col_taxaid_official_project
                            , TranslationTable = fn_taxoff
                            , AttributeTable = fn_taxoff_attr)
      # fn_part <- paste0("_", abr_filebuilder, "_0taxasource", ".csv")
      fn_part <- "BCG_TaxaTranslator_source.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa User
      # saved when imported

      # 2023-11-03, save original filenames
      # add taxatrans metadata

      ## Taxa Official
      # df_save <- df_official
      # fn_part <- paste0(fn_abr_save, "1official", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff
                , file.path(path_results_ref, fn_taxoff))

      ## Taxa Official, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_meta
                , file.path(path_results_ref, fn_taxoff_meta))

      ## Taxa Official, Attributes
      # df_save <- df_taxoff_attr
      # fn_part <- paste0(path_results_ref, "1attributes", ".csv")
      # write.csv(df_save
      #           , file.path(path_results, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr
                , file.path(path_results_ref, fn_taxoff_attr))

      ## Taxa Official, Attributes, meta data
      # df_save <- taxatrans_results$official_metadata # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "1metadata", ".csv")
      # write.csv(df_save
      #           , file.path(path_results_ref, paste0(fn_input_base, fn_part))
      #           , row.names = FALSE)
      # rm(df_save, fn_part)
      file.copy(temp_taxoff_attr_meta
                , file.path(path_results_ref, fn_taxoff_attr_meta))

      ## translate - crosswalk
      df_save <- taxatrans_results$taxatrans_unique # df_taxoff_meta
      # fn_part <- paste0(fn_abr_save, "2taxamatch", ".csv")
      fn_part <- "BCG_TaxaTranslator_modify.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Non Match
      df_save <- data.frame(taxatrans_results$nonmatch)
      # fn_part <- paste0(fn_abr_save, "3nonmatch", ".csv")
      fn_part <- "BCG_TaxaTranslator_nonmatch.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Taxa Trans
      df_save <- taxatrans_results$merge
      # fn_part <- paste0(fn_abr_save, "4taxaattr", ".csv")
      fn_part <- "BCG_TaxaTranslator_TAXAATTR.csv"
      write.csv(df_save
                , file.path(path_results_sub, fn_part)
                , row.names = FALSE)
      rm(df_save, fn_part)

      ## Calc, 05, Create Zip ----
      prog_detail <- "Create Zip File For Download"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Create zip file for download
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)


      ## Calc, 06, Clean Up ----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # button, enable, download
      shinyjs::enable("b_download_taxatrans")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      # Inform user about number of taxa mismatches
      ## calc number of mismatch
      df_mismatch <- data.frame(taxatrans_results$nonmatch)
      n_taxa_mismatch <- nrow(df_mismatch)
      msg <- paste0("Number of mismatch taxa = ", n_taxa_mismatch, "\n\n"
                    , "Any mismatched taxa in 'mismatch' file in results download.")
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress

  }##expr ~ ObserveEvent

  )##observeEvent ~ b_taxatrans_calc

  ## b_download_TaxaTrans ----
  output$b_download_taxatrans <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_taxatrans
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans

  #~~~~CALC~~~~----

  # Calc, BCG ----
  ## b_Calc_BCG ----
  observeEvent(input$b_calc_bcg, {
    shiny::withProgress({

      # time, start
      tic <- Sys.time()

      ### Calc, 0, Set Up Shiny Code ----

      prog_detail <- "Calculation, BCG..."
      message(paste0("\n", prog_detail))

      # Number of increments
      prog_n <- 12
      prog_sleep <- 0.25

      ## Calc, 1, Initialize ----
      prog_detail <- "Initialize Data"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Remove existing files in "results"
      clean_results()

      # Copy user files to results sub-folder
      copy_import_file(import_file = input$fn_input)

      # result folder and files
      # 2023-12-14, add community
      fn_comm <- input$si_community
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      path_results_sub <- file.path(path_results
                                    , paste(abr_results, fn_comm, fn_abr, sep = "_"))
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_sub))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_sub))
      }

      # reference folder
      path_results_ref <- file.path(path_results, dn_files_ref)
      # Add "Results" folder if missing
      boo_Results <- dir.exists(file.path(path_results_ref))
      if (boo_Results == FALSE) {
        dir.create(file.path(path_results_ref))
      }

      # button, disable, download
      shinyjs::disable("b_download_bcg")

      # data
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      message(paste0("Import, file name, base: ", fn_input_base))
      df_input <- read.delim(inFile$datapath
                             , header = TRUE
                             , sep = input$sep
                             , stringsAsFactors = FALSE)
      # QC, FAIL if TRUE
      if (is.null(df_input)) {
        return(NULL)
      }

      # QC, names to upper case
      names(df_input) <- toupper(names(df_input))

      # QC, Index_Name & Index_Class
      my_comm <- input$si_community
      if ((!"INDEX_CLASS" %in% toupper(names(df_input)))
          & (my_comm == "CREMP_Keys")) {
        df_input[, "INDEX_CLASS"] <- "CREMP_Keys"
      } else if((!"INDEX_CLASS" %in% toupper(names(df_input)))
                & (my_comm == "NOT_CREMP_Keys")){
        df_input[, "INDEX_CLASS"] <- "NOT_CREMP_Keys"
      }## IF ~ INDEX_NAME

      if (!"INDEX_NAME" %in% toupper(names(df_input))) {
        df_input[, "INDEX_NAME"] <- "FL_Coral_BCG"
      }## IF ~ INDEX_NAME

      ## Calc, 2, Exclude Taxa ----
      prog_detail <- "Calculate, Exclude Taxa"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # # Calc
      # message(paste0("User response to generate ExclTaxa = ", input$ExclTaxa))
      #
      # if (input$ExclTaxa) {
      #   ## Get TaxaLevel names present in user file
      #   phylo_all <- c("Kingdom"
      #                  , "Phylum"
      #                  , "SubPhylum"
      #                  , "Class"
      #                  , "SubClass"
      #                  , "Order"
      #                  , "SubOrder"
      #                  , "InfraOrder"
      #                  , "SuperFamily"
      #                  , "Family"
      #                  , "SubFamily"
      #                  , "Tribe"
      #                  , "Genus"
      #                  , "SubGenus"
      #                  , "Species"
      #                  , "Variety")
      #   phylo_all <- toupper(phylo_all) # so matches rest of file
      #
      #   # case and matching of taxa levels handled inside of markExluded
      #
      #   # Overwrite existing column if present
      #   # ok since user checked the box to calculate
      #   if ("EXCLUDE" %in% toupper(names(df_input))) {
      #     # save original user input
      #     df_input[, "EXCLUDE_USER"] <- df_input[, "EXCLUDE"]
      #     # drop column
      #     df_input <- df_input[, !names(df_input) %in% "EXCLUDE"]
      #   }## IF ~ Exclude
      #
      #   # overwrite current data frame
      #   df_input <- BioMonTools::markExcluded(df_samptax = df_input
      #                                         , SampID = "SAMPLEID"
      #                                         , TaxaID = "TAXAID"
      #                                         , TaxaCount = "N_TAXA"
      #                                         , Exclude = "EXCLUDE"
      #                                         , TaxaLevels = phylo_all
      #                                         , Exceptions = NA)
      #
      #   # Save Results
      #   # fn_excl <- paste0(fn_input_base, fn_abr_save, "1markexcl.csv")
      #   fn_excl <- "BCG_1markexcl.csv"
      #   dn_excl <- path_results_sub
      #   pn_excl <- file.path(dn_excl, fn_excl)
      #   write.csv(df_input, pn_excl, row.names = FALSE)
      #
      # }## IF ~ input$ExclTaxa


      ## Calc, 3, BCG Flag Cols ----
      # get columns from Flags (non-metrics) to carry through
      prog_detail <- "Calculate, Keep BCG Model Columns"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Rules - should all be metrics but leaving here just in case
      # Flags - not always metrics,
      # Index Name for import data
      import_IndexName <- unique(df_input$INDEX_NAME)
      # QC Flags for chosen BCG model (non-metrics)
      cols_flags <- unique(df_checks[df_checks$Index_Name == import_IndexName
                                     , "Metric_Name"])
      # can also add other columns to keep if feel so inclined
      cols_flags_keep <- cols_flags[cols_flags %in% names(df_input)]


      ## Calc, 3b, Rules ----
      prog_detail <- "Calculate, BCG Rules"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # filter for data Index_Name in data (drop 2 extra columns)
      df_rules <- df_bcg_models[df_bcg_models$Index_Name == import_IndexName
                                , !names(df_bcg_models) %in% c("SITE_TYPE", "INDEX_REGION")]
      # Save
      # fn_rules <- paste0(fn_input_base, fn_abr_save, "3metrules.csv")
      fn_rules <- "BCG_3metrules.csv"
      dn_rules <- path_results_sub
      pn_rules <- file.path(dn_rules, fn_rules)
      write.csv(df_rules, pn_rules, row.names = FALSE)

      ## Calc, 4, MetVal----
      prog_detail <- "Calculate, Metric, Values"
      message(paste0("\n", prog_detail))
      message(paste0("Community = ", input$si_community))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Add INDEX_NAME and INDEX_CLASS
      df_input$INDEX_NAME <- "FL_Coral_BCG"

      if (my_comm == "CREMP_KEYS") {
        df_input$INDEX_CLASS <- "CREMP_Keys"
      } else {
        df_input$INDEX_CLASS <- "NOT_CREMP_Keys"
      } # end if/else

      # Add default value for Juveniles
      df_input <- df_input %>%
        mutate(DIAMMAX_CM = case_when((JUVENILE == TRUE & is.na(DIAMMAX_CM) ~ 3)
                                       , TRUE ~ DIAMMAX_CM)
               , DIAMPERP_CM = case_when((JUVENILE == TRUE & is.na(DIAMPERP_CM) ~ 2)
                                        , TRUE ~ DIAMPERP_CM)
               , HEIGHT_CM = case_when((JUVENILE == TRUE & is.na(HEIGHT_CM) ~ 1)
                                         , TRUE ~ HEIGHT_CM))

      # Calc
      if (length(cols_flags_keep) > 0) {
        # keep extra cols from Flags (non-metric)
        df_metval <- BioMonTools::metric.values(df_input
                                                , fun.Community = "CORAL"
                                                , fun.cols2keep = cols_flags_keep
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      } else {
        df_metval <- BioMonTools::metric.values(df_input
                                                , fun.Community = "CORAL"
                                                , boo.Shiny = TRUE
                                                , verbose = TRUE
                                                , taxaid_dni = "DNI")
      }## IF ~ length(col_rules_keep)

      #df_metval$INDEX_CLASS <- df_metval$INDEX_CLASS

      ### Save Results ----

      # fn_metval <- paste0(fn_input_base, fn_abr_save, "2metval_all.csv")
      fn_metval <- "BCG_2metvall_all.csv"
      dn_metval <- path_results_sub
      pn_metval <- file.path(dn_metval, fn_metval)
      write.csv(df_metval, pn_metval, row.names = FALSE)

      ### Save Results (BCG) ----
      # Munge
      ## Model and QC Flag metrics only
      # cols_flags defined above
      cols_model_metrics <- unique(df_bcg_models[
        df_bcg_models$Index_Name == import_IndexName, "Metric_Name"])
      cols_req <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                    , "ncol_total", "nt_total")
      cols_metrics_flags_keep <- unique(c(cols_req
                                          , cols_flags
                                          , cols_model_metrics))
      df_metval_slim <- df_metval[, names(df_metval) %in% cols_metrics_flags_keep]
      # Save
      # fn_metval_slim <- paste0(fn_input_base, fn_abr_save, "2metval_BCG.csv")
      fn_metval_slim <- "BCG_2metval_BCG.csv"
      dn_metval_slim <- path_results_sub
      pn_metval_slim <- file.path(dn_metval_slim, fn_metval_slim)
      write.csv(df_metval_slim, pn_metval_slim, row.names = FALSE)


      ## Calc, 5, MetMemb----
      prog_detail <- "Calculate, Metric, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_metmemb <- BCGcalc::BCG.Metric.Membership(df_metval, df_bcg_models)
      # Save Results
      # fn_metmemb <- paste0(fn_input_base, fn_abr_save, "3metmemb.csv")
      fn_metmemb <- "BCG_3metmemb.csv"
      dn_metmemb <- path_results_sub
      pn_metmemb <- file.path(dn_metmemb, fn_metmemb)
      write.csv(df_metmemb, pn_metmemb, row.names = FALSE)


      ## Calc, 6, LevMemb----
      prog_detail <- "Calculate, Level, Membership"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levmemb <- BCGcalc::BCG.Level.Membership(df_metmemb, df_bcg_models)
      # Save Results
      # fn_levmemb <- paste0(fn_input_base, fn_abr_save, "4levmemb.csv")
      fn_levmemb <- "BCG_4levmemb.csv"
      dn_levmemb <- path_results_sub
      pn_levmemb <- file.path(dn_levmemb, fn_levmemb)
      write.csv(df_levmemb, pn_levmemb, row.names = FALSE)


      ## Calc, 7, LevAssign----
      prog_detail <- "Calculate, Level, Assignment"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # Calc
      df_levassign <- BCGcalc::BCG.Level.Assignment(df_levmemb)

      # Save Results
      # fn_levassign <- paste0(fn_input_base, fn_abr_save, "5levassign.csv")
      fn_levassign <- "BCG_5levassign.csv"
      dn_levassign <- path_results_sub
      pn_levassign <- file.path(dn_levassign, fn_levassign)
      write.csv(df_levassign, pn_levassign, row.names = FALSE)


      ## Calc, 8, QC Flags----
      prog_detail <- "Calculate, QC Flags"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(prog_sleep)

      # 2023-12-06
      # Split if no flags so doesn't crash

      # Check if Flags exist for data
      col_index_metval <- c("INDEX_NAME", "INDEX_CLASS")
      col_index_checks <- c("Index_Name", "INDEX_CLASS")
      index_metval <- unique(df_metval[, col_index_metval])
      index_checks <- unique(df_checks[, col_index_checks])
      index_merge <- merge(index_metval, index_checks
                           , by.x = col_index_metval
                           , by.y = col_index_checks)

      if (nrow(index_merge) == 0) {

        # create dummy files
        str_nodata <- "No flags for the Index Name/Class combinations present in data"
        # Flags
        df_flags <- data.frame(x = str_nodata
                                   , CHECKNAME = "No Flags"
                                   , FLAG = NA)
        df_lev_flags <- df_levassign
        # Flags Summary
        df_lev_flags_summ <- data.frame(x = str_nodata)
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        # Flag Metrics
        df_metflags <- data.frame(x = str_nodata)

      } else {

        # Calc
        # df_checks loaded in global.R
        df_flags <- BioMonTools::qc.checks(df_metval, df_checks)
        # Change terminology; PASS/FAIL to NA/flag
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "FAIL"] <- "flag"
        df_flags[, "FLAG"][df_flags[, "FLAG"] == "PASS"] <- NA
        # long to wide format
        df_flags_wide <- reshape2::dcast(df_flags
                                         , SAMPLEID ~ CHECKNAME
                                         , value.var = "FLAG")
        # Calc number of "flag"s by row.
        df_flags_wide$NumFlags <- rowSums(df_flags_wide == "flag", na.rm = TRUE)
        # Rearrange columns
        NumCols <- ncol(df_flags_wide)
        df_flags_wide <- df_flags_wide[, c(1, NumCols, 2:(NumCols - 1))]
        # Merge Levels and Flags
        df_lev_flags <- merge(df_levassign
                              , df_flags_wide
                              , by.x = "SampleID"
                              , by.y = "SAMPLEID"
                              , all.x = TRUE)
        # Flags Summary
        df_lev_flags_summ <- as.data.frame.matrix(table(df_flags[, "CHECKNAME"]
                                                        , df_flags[, "FLAG"]
                                                        , useNA = "ifany"))
        # Results
        df_results <- df_lev_flags[, !names(df_lev_flags) %in% c(paste0("L", 1:6))]
        ## remove L1:6

        # Flag Metrics
        col2keep_metflags <- c("SAMPLEID", "INDEX_NAME", "INDEX_CLASS"
                               , "METRIC_NAME", "CHECKNAME", "METRIC_VALUE"
                               , "SYMBOL", "VALUE", "FLAG")
        df_metflags <- df_flags[, col2keep_metflags]

      }## IF ~ check for matching index name and class


      # Save, Flags Summary
      # fn_levflags <- paste0(fn_input_base, fn_abr_save, "6levflags.csv")
      fn_levflags <- "BCG_6levflags.csv"
      dn_levflags <- path_results_sub
      pn_levflags <- file.path(dn_levflags, fn_levflags)
      write.csv(df_lev_flags_summ, pn_levflags, row.names = TRUE)

      # Save, Results
      # fn_results <- paste0(fn_input_base, fn_abr_save, "RESULTS.csv")
      fn_results <- "_BCG_RESULTS.csv"
      dn_results <- path_results_sub
      pn_results <- file.path(dn_results, fn_results)
      write.csv(df_results, pn_results, row.names = FALSE)

      # Save, Flag Metrics
      # fn_metflags <- paste0(fn_input_base, fn_abr_save, "6metflags.csv")
      fn_metflags <- "BCG_6metflags.csv"
      dn_metflags <- path_results_sub
      pn_metflags <- file.path(dn_metflags, fn_metflags)
      write.csv(df_metflags, pn_metflags, row.names = FALSE)


      ## Calc, 9, RMD----
      prog_detail <- "Calculate, Create Report"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      strFile.RMD <- file.path("external"
                               , "RMD_Results"
                               , "Results_BCG_Summary.Rmd")
      strFile.RMD.format <- "html_document"
      # strFile.out <- paste0(fn_input_base, fn_abr_save, "RESULTS.html")
      strFile.out <- "_BCG_RESULTS.html"
      dir.export <- path_results_sub
      rmarkdown::render(strFile.RMD
                        , output_format = strFile.RMD.format
                        , output_file = strFile.out
                        , output_dir = dir.export
                        , quiet = TRUE)

      ## Calc, 10, Save, Reference----
      prog_detail <- "Calculate, Save, Reference"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      path_results_ref <- file.path(path_results, dn_files_ref)

      # ## Index Class
      # fn_save <- "IndexClass.xlsx"
      # file_from <- temp_indexclass_crit
      # file_to <- file.path(path_results_ref, fn_save)
      # file.copy(file_from, file_to)

      ## Metric Flags
      fn_save <- "MetricFlags.xlsx"
      file_from <- temp_bcg_checks
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)

      ## Metric Names
      fn_save <- "MetricNames.xlsx"
      file_from <- temp_metricnames
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)

      ## Metric Scoring
      fn_save <- "MetricScoring.xlsx"
      file_from <- temp_metricscoring
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)

      ## BCG Rules
      fn_save <- "Rules.xlsx"
      file_from <- temp_bcg_models
      file_to <- file.path(path_results_ref, fn_save)
      file.copy(file_from, file_to)


      ## Calc, 11, Clean Up----
      prog_detail <- "Calculate, Clean Up"
      message(paste0("\n", prog_detail))
      # Increment the progress bar, and update the detail text.
      incProgress(1/prog_n, detail = prog_detail)
      Sys.sleep(2 * prog_sleep)

      # Create zip file of results
      fn_4zip <- list.files(path = path_results
                            , full.names = TRUE)
      zip::zip(file.path(path_results, "results.zip"), fn_4zip)

      # button, enable, download
      shinyjs::enable("b_download_bcg")

      # time, end
      toc <- Sys.time()
      duration <- difftime(toc, tic)

      # pop up
      msg <- paste0("Total Records (Input) = ", nrow(df_input)
                    , "\n\n"
                    , "Elapse Time (", units(duration), ") = ", round(duration, 2))
      shinyalert::shinyalert(title = "Task Complete"
                             , text = msg
                             , type = "success"
                             , closeOnEsc = TRUE
                             , closeOnClickOutside = TRUE)
      # validate(msg)

    }## expr ~ withProgress ~ END
    , message = "Calculating BCG"
    )## withProgress ~ END
  }##expr ~ ObserveEvent ~ END
  )##observeEvent ~ b_calc_bcg ~ END

  ## b_download_BCG ----
  output$b_download_bcg <- downloadHandler(

    filename = function() {
      inFile <- input$fn_input
      fn_input_base <- tools::file_path_sans_ext(inFile$name)
      fn_abr <- abr_bcg
      fn_abr_save <- paste0("_", fn_abr, "_")
      paste0(fn_input_base
             , fn_abr_save
             , format(Sys.time(), "%Y%m%d_%H%M%S")
             , ".zip")
    } ,
    content = function(fname) {##content~START

      file.copy(file.path(path_results, "results.zip"), fname)

    }##content~END
    #, contentType = "application/zip"
  )##download ~ BCG

})##shinyServer ~ END
