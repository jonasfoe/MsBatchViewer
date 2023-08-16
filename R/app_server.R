
shinyAppServer <- function(input, output, session) {
  cl_count <- availableCores()
  future <- tweak(multisession, workers = cl_count)

  rawfiles <- reactiveValues(
    to_process = character(),
    ids = character(),
    current_order = numeric(),
    filepaths = character(),
    labels = character()
  )

  # functions to handle removal of files
  # function(id)
  rawfile_rm_handlers = reactiveVal(list())

  tasks <- reactiveValues(
    future_todo_list = list(),
    future_active = 0L,
    log = list(),
    future_log = character(),
    force_output_counter = 0L
  )

  processingServer("processing", rawfiles, tasks, rawfile_rm_handlers, future, cl_count)
  metadataServer("metadata", rawfiles, tasks, rawfile_rm_handlers, reactive(input$sidebar), future)
  plotsServer("plots", rawfiles, tasks, rawfile_rm_handlers, reactive(input$sidebar), future)

  message("running")
}
