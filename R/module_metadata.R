
metadataServer <- function(id, rawfiles, tasks, rawfile_rm_handlers, sidebar, future) {
  stopifnot(is.reactivevalues(rawfiles))
  stopifnot(is.reactivevalues(tasks))
  stopifnot(is.reactive(rawfile_rm_handlers))
  stopifnot(is.reactive(sidebar))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rm_by_nm <- function(x, nm) x[names(x) != nm]

    rawfiles$metadata <- list()

    rawfile_rm_handlers(append(isolate(rawfile_rm_handlers()), function(id) {
      rawfiles$metadata <- rm_by_nm(rawfiles$metadata, id)
    }))

    extract_metadata <- function(filepath) {
      tibble::lst(
        File = filepath,
        FileName = basename(filepath),
        !!!MsRawAccess::extract_info(filepath),
        # !!!discard(MsRawAccess::extract_info(filepath), \(x) inherits(x, "POSIXlt")),
        DistinctActivationTypes =  paste0(MsRawAccess::extract_distinct_activation_types(filepath), collapse = ","),
        DistinctIonizationModes = paste0(MsRawAccess::extract_distinct_ionization_modes(filepath), collapse = ","),
        DistinctMs1MassAnalyzers = paste0(MsRawAccess::extract_distinct_ms1_mass_analyzers(filepath), collapse = ","),
        DistinctMs2MassAnalyzers = paste0(MsRawAccess::extract_distinct_ms2_mass_analyzers(filepath), collapse = ","),
        MethodName = basename(MethodFile)
      )
    }

    future_metadata <- function(future_uuid, rawfile_uuid) {
      force(future_uuid)
      force(rawfile_uuid)

      function() {
        if (!is.element(rawfile_uuid, rawfiles$ids))
          return()
        tasks$future_active <- tasks$future_active + 1L

        filepath <- rawfiles$filepaths[rawfile_uuid]
        f <- future(extract_metadata(filepath), globals = c("extract_metadata", "filepath", "a"), seed = TRUE)

        self <- observe(priority = 9, {
          if (resolved(f)) {
            tasks$future_active <- tasks$future_active - 1L
            tasks$future_log <- append(tasks$future_log, future_uuid)

            if (rawfile_uuid %in% rawfiles$ids)
              rawfiles$metadata[[rawfile_uuid]] <- as_tibble(value(f))

            self$destroy()
          } else {
            invalidateLater(1000)
          }
        })

        message("Extracting Metadata ", filepath)
      }
    }

    # new rawfiles readin
    observeEvent(rawfiles$to_process, priority = 8, {
      new_rawfile_uuids <- req(rawfiles$to_process)

      future_ids <- uuid::UUIDgenerate(n = length(new_rawfile_uuids))

      task <- list(
        type = "metadata",
        future_ids = future_ids
      )

      tasks$log <- append(tasks$log, task)

      tasks$future_todo_list <- c(tasks$future_todo_list, map2(future_ids, new_rawfile_uuids, future_metadata))
    })

    metadata_columns_sorted <- c(
      "Name",
      "FileName",
      "FileDate",
      "InstrumentModel",
      "ExpectedRuntime",
      "SpectraCount",
      "MethodName",
      "DistinctActivationTypes",
      "DistinctIonizationModes",
      "DistinctMs1MassAnalyzers",
      "DistinctMs2MassAnalyzers"
      # "SampleType",
      # "SampleId",
      # "SampleName",
      # "Comment",
      # "StartTime",
      # "EndTime",
      # "FirstScanNumber",
      # "LastScanNumber",
    )
    metadata_template <- as_tibble(extract_metadata(MsRawAccess::get_rawfile_sample_path()))[NULL, ]
    metadata_columns_extra <- setdiff(names(metadata_template), metadata_columns_sorted)
    metadata_columns_selected <- reactiveVal(c(
      "Name",
      "FileDate",
      "InstrumentModel",
      "ExpectedRuntime",
      "SpectraCount",
      "MethodName",
      "DistinctActivationTypes",
      "DistinctIonizationModes",
      "DistinctMs1MassAnalyzers",
      "DistinctMs2MassAnalyzers"
    ))

    # enforce metadata column selection
    observeEvent(input$tablecols, {
      metadata_columns_selected(input$tablecols)
    })

    # render the sidebar options
    output$metadataoptions <- renderUI({
      req(sidebar() == "metadata")
      checkboxGroupInput(ns("tablecols"), label = "Table columns", choices = c(metadata_columns_sorted, metadata_columns_extra), selected = isolate(metadata_columns_selected()))
    })

    # render the metadata table
    output$metadata_table <- DT::renderDT(
      options = list(autoWidth = FALSE, scrollX = TRUE),
      {
        ids <- rawfiles$ids[rawfiles$current_order]
        labels <- rawfiles$labels
        metadata <- rawfiles$metadata
        metadata_available <- intersect(ids, names(metadata))

        # https://stackoverflow.com/questions/55165477/r-shiny-dashboard-datatable-column-width
        df <- vctrs::vec_rbind(
          metadata_template,
          !!!unname(metadata[metadata_available])
        )

        df$Name <- labels[metadata_available] %||% character()

        df <- df[metadata_columns_selected()]

        # fix an error that occurs when rendering an empty DT with some column types
        if (nrow(df) == 0)
          df <- mutate(df, across(where(is.list), ~ character()))

        df
      }
    )
  })
}
