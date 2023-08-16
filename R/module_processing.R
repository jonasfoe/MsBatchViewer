
processingServer <- function(id, rawfiles, tasks, rawfile_rm_handlers, future, cl_count) {
  stopifnot(is.reactivevalues(rawfiles))
  stopifnot(is.reactivevalues(tasks))
  stopifnot(is.reactive(rawfile_rm_handlers))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rm_by_nm <- function(x, nm) x[names(x) != nm]

    # general
    default_root <- "."
    filesys_roots <- function() c("." = ".", getVolumes()())
    # # my local laptop
    # default_root <- "examples"
    # filesys_roots <- function() c("." = ".", "data" = "../../../data", "examples" = "C:/Users/foerstjo/Documents/riemergrp/tools_own/example_rawfiles", getVolumes()())
    # # the workstations
    # default_root <- "data"
    # filesys_roots <- function() c("data" = "E:/OE Raw data", getVolumes()())

    shinyFileChoose(input, id = "rawfiles_new", session = session, roots = filesys_roots(), filetypes = c("", "raw"), defaultRoot = default_root, defaultPath = "")

    flushed <- FALSE

    observe({message("==Active: ", tasks$future_active)})
    observe(priority = 999, {
      invalidateLater(500)
      if (flushed) {
        tasks$force_output_counter <- 0L
        # message("==Counter reset")
        flushed <<- FALSE
      }
    })
    observe(priority = -999, {
      invalidateLater(500)
      flushed <<- TRUE
      # message("==Flushed")
    })
    observe({
      invalidateLater(500)
      # message("==Counter check")
      output_counter <- tasks$force_output_counter
      req(output_counter < 4L)

      todo <- tasks$future_todo_list
      cl_avail <- cl_count - tasks$future_active

      todo_count <- length(todo)
      do_count <- min(cl_avail, todo_count, 4L - output_counter)

      if (do_count == 0)
        return()

      # walk(todo[1:do_count], ~message(.x))
      walk(todo[1:do_count], ~.x())
      tasks$force_output_counter <- output_counter + do_count
      # message("==Counter: ", output_counter + do_count)
      if (do_count < todo_count)
        tasks$future_todo_list <- todo[(do_count + 1):todo_count]
      else
        tasks$future_todo_list <- list()
    })

    # new rawfiles readin
    observeEvent(input$rawfiles_new, {
      rawfiles_new <- req(unname(parseFilePaths(filesys_roots, input$rawfiles_new)$datapath))
      rawfiles_new_ids <- uuid::UUIDgenerate(n = length(rawfiles_new))

      names(rawfiles_new) <- rawfiles_new_ids

      rawfiles_ids <- rawfiles$ids[rawfiles$current_order]
      rawfiles_ids <- c(rawfiles_ids, rawfiles_new_ids)
      rawfiles$ids <- rawfiles_ids
      rawfiles$current_order <- seq_along(rawfiles_ids)

      rawfiles$filepaths <- c(rawfiles$filepaths, rawfiles_new)[rawfiles_ids]
      rawfiles$labels <- c(rawfiles$labels, map_chr(rawfiles_new, basename))[rawfiles_ids]

      message("==================================================================")
      walk(rawfiles_new, ~ message("Adding file ", .x))

      rawfiles$to_process <- ""
      rawfiles$to_process <- rawfiles_new_ids
    })

    # enforce new rawfiles order
    observeEvent(input$file_list_div_order, {
      rawfiles$current_order <- as.numeric(req(input$file_list_div_order)$id)
    })

    # render the tasks menu
    output$tasksMenu <- renderMenu({
      tasks <- list(
        taskItem(value = 90, color = "green", "Documentation")
      )

      dropdownMenu(type = "tasks", badgeStatus = "success", .list = tasks)
    })

    filelist_observers <- list()

    # render the rawfiles list
    output$file_list <- renderUI({
      ids <- req(rawfiles$ids[isolate(rawfiles$current_order)])

      message("Rendering filelist with ", length(ids), " files.")

      filepaths <- rawfiles$filepaths
      labels <- isolate(rawfiles$labels)

      walk(filelist_observers, ~ .x$destroy())

      box_list <- imap(ids, function(id, i) {
        input_id_label <- paste0("text_label_", id)
        input_id_close <- paste0("button_close_", id)
        list(
          obs_label = observeEvent(input[[input_id_label]], ignoreInit = TRUE, {
            rawfiles$labels[str_sub(input_id_label, start = 12)] <- input[[input_id_label]]
          }),
          obs_close = observeEvent(input[[input_id_close]], ignoreInit = TRUE, {
            ids <- setdiff(rawfiles$ids[rawfiles$current_order], id)
            rawfiles$ids <- ids
            rawfiles$current_order <- seq_along(ids)
            rawfiles$filepaths <- rawfiles$filepaths[ids]
            rawfiles$labels <- rawfiles$labels[ids]

            rawfiles$metadata <- rm_by_nm(rawfiles$metadata, id)
            walk(rawfile_rm_handlers(), ~ .x(id))
          }),
          box = box(
            # h4(basename(.x)),
            # strong(basename(.x)),
            # column(width = 7, textInput(input_id_label, label = NULL, value = labels[id])),
            # column(width = 1, offset = 4, actionButton(input_id_close, label = NULL, icon = tags$i(class = "fas fa-window-close", style = "color: rgb(255,0,0)"))),
            # footer = filepaths[id],
            # filepaths[id], br(),
            # column(width = 11, textInput(input_id_label, label = NULL, value = labels[id])),
            # column(width = 1, actionButton(input_id_close, label = NULL, icon = tags$i(class = "fas fa-window-close", style = "color: rgb(255,0,0)"))),
            # br(),
            column(width = 11, textInput(ns(input_id_label), label = NULL, value = labels[id])),
            column(width = 1, actionButton(ns(input_id_close), label = NULL, icon = tags$i(class = "fas fa-window-close", style = "color: rgb(255,0,0)"))),
            filepaths[id],
            # div(style="display: inline-block",
            #     strong(style="display: inline-block", "Label: "), div(style="display: inline-block", textInput(input_id_label, label = NULL, value = labels[id])),
            #     div(filepaths[id])
            # ),
            # div(style="display:inline-block;vertical-align:center", actionButton(input_id_close, label = NULL, icon = tags$i(class = "fas fa-window-close", style = "color: rgb(255,0,0)"))),
            # # title = basename(.x),
            width = 12
            # collapsible = TRUE,
            # collapsed = TRUE
          ) %>%
            tagAppendAttributes(id = i)
        )
      })

      filelist_observers <- c(map(box_list, "obs_label"), map(box_list, "obs_close"))

      jqui_sortable(exec(div, id = ns("file_list_div"), !!!map(box_list, "box")))
    })
  })
}
