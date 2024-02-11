
plotsServer <- function(id, rawfiles, tasks, rawfile_rm_handlers, sidebar, future) {
  stopifnot(is.reactivevalues(rawfiles))
  stopifnot(is.reactivevalues(tasks))
  stopifnot(is.reactive(rawfile_rm_handlers))
  stopifnot(is.reactive(sidebar))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rm_by_nm <- function(x, nm) x[names(x) != nm]

    rawfiles$plotdata <- list()
    rawfiles$ticdata <- list()
    rawfiles$integraldata <- list()

    rawfile_rm_handlers(append(isolate(rawfile_rm_handlers()), function(id) {
      rawfiles$ticdata <- rm_by_nm(rawfiles$ticdata, id)
      rawfiles$plotdata <- rm_by_nm(rawfiles$plotdata, id)
      rawfiles$integraldata <- rm_by_nm(rawfiles$integraldata, id)
    }))

    integrate_timeseries <- function(x, y) {
      x_diff <- diff(x)
      x_weights <- c(x_diff, 0) + c(0, x_diff)

      sum(x_weights * y) / 2
    }

    extract_integrals_by_charge <- function(data) {
      positions <- arrange(distinct(data, scan_number, rt), scan_number)
      data <- group_by(select(data, scan_number, charge, intensity), charge)
      charges <- group_keys(data)$charge
      dfs <- group_split(data, .keep = FALSE)

      integrals <- map_dbl(dfs, ~
        tibble(scan_number = positions$scan_number, intensity = 0) %>%
          rows_update(.x, by = "scan_number", unmatched = "error") %>%
          pull(intensity) %>%
          integrate_timeseries(x = positions$rt, y = .)
      )

      # charge = NA is the TIC but I need it to be the unidentified charge states
      charge_is_na <- which(is.na(charges))
      if (length(charge_is_na) == 1) {
        integrals[charge_is_na] <- integrals[charge_is_na] - sum(integrals[-charge_is_na])
      } else if (length(charge_is_na) > 1) {
        stop()
      }

      tibble(charge = charges, intensity = integrals)
    }

    future_ticdata <- function(future_uuid, rawfile_uuid) {
      force(future_uuid)
      force(rawfile_uuid)

      function() {
        if (!is.element(rawfile_uuid, rawfiles$ids))
          return()
        tasks$future_active <- tasks$future_active + 1L

        filepath <- rawfiles$filepaths[rawfile_uuid]
        f <- future(MsRawAccess::extract_ms1_charge_states(filepath), globals = "filepath", seed = TRUE)

        self <- observe(priority = 9, {
          if (resolved(f)) {
            tasks$future_active <- tasks$future_active - 1L
            tasks$future_log <- append(tasks$future_log, future_uuid)

            if (rawfile_uuid %in% rawfiles$ids) {
              df <- as_tibble(value(f))

              rawfiles$ticdata[[rawfile_uuid]] <- df
              rawfiles$integraldata[[rawfile_uuid]] <- extract_integrals_by_charge(df)
            }


            self$destroy()
          } else {
            invalidateLater(3000)
          }
        })

        message("Extracting Plot Data ", filepath)
      }
    }

    # new rawfiles readin
    observeEvent(rawfiles$to_process, priority = 4, {
      new_rawfile_uuids <- req(rawfiles$to_process)

      future_ids <- uuid::UUIDgenerate(n = length(new_rawfile_uuids))

      task <- list(
        type = "ticdata",
        future_ids = future_ids
      )

      tasks$log <- append(tasks$log, task)

      tasks$future_todo_list <- c(tasks$future_todo_list, map2(future_ids, new_rawfile_uuids, future_ticdata))
    })

    plotsettings <- reactiveValues(
      max_charge = 8,
      invert_stacking = FALSE,
      unify_rt_range = FALSE,
      use_custom_rt_range = FALSE,
      custom_rt_range = c(0, 60),
      unify_y_limit = FALSE,
      use_custom_y_limit = FALSE,
      custom_y_limit = 3e8,
      annotate_charge_ranges = list()
    )

    generate_label <- function(integraldata, charge_groups) {
      charge_groups %>%
        map_dbl(~ sum(filter(integraldata, charge %in% .x)$intensity)) %>%
        imap_chr(~ paste0(.y, ": ", scales::scientific(.x))) %>%
        {paste0(c("z: Total Intensity", .), collapse = "\n")}
    }

    make_plot <- function(ticdata, integraldata, max_charge, invert_stacking, annotate_charge_ranges) {
      p <- tictools::plot_charge_data_gggrob(ticdata, max_charge = max_charge, color_log = FALSE, invert_stacking = invert_stacking)

      if (length(annotate_charge_ranges) > 0) {
        label <- paste0("\n", str_replace_all(generate_label(integraldata, annotate_charge_ranges), "\\n", "    \n"), "    ")
        p$plot <- p$plot +
          ggpp::annotate("text_npc", label = label, npcx = 1, npcy = 1, vjust = 1, hjust = 1)
      }

      p$digest <- digest::digest(list(ticdata, integraldata, max_charge, invert_stacking, annotate_charge_ranges))
      p
    }

    # fill plotdata
    observeEvent({rawfiles$ticdata; rawfiles$integraldata}, {
      ticdata <- rawfiles$ticdata
      integraldata <- rawfiles$integraldata
      plotdata <- rawfiles$plotdata
      plots_todo <- setdiff(names(ticdata), names(plotdata))

      for (id in plots_todo) {
        rawfiles$plotdata[[id]] <- make_plot(ticdata[[id]], integraldata[[id]], plotsettings$max_charge, plotsettings$invert_stacking, plotsettings$annotate_charge_ranges)
      }
    })

    plotdata_rt_range <- eventReactive(rawfiles$plotdata, {
      plotdata <- rawfiles$plotdata

      if (length(plotdata) > 0) {
        rt_ranges <- map(plotdata, "rt_range")
        rt_mins <- map_dbl(rt_ranges, 1)
        rt_maxs <- map_dbl(rt_ranges, 2)
        c(min(rt_mins), max(rt_maxs))
      } else {
        c(NA_real_, NA_real_)
      }
    })

    plotdata_tic_max <- eventReactive(rawfiles$plotdata, {
      plotdata <- rawfiles$plotdata

      if (length(plotdata) > 0)
        max(map_dbl(plotdata, "tic_max"))
      else
        NA_real_
    })

    interpret_charge_groups <- function(x) {
      if (x == "")
        return(list(name = "", charges = integer()))

      vals <- unique(sort(suppressWarnings(as.integer(str_split_1(x, "\\s*-\\s*"))), na.last = FALSE))

      if (length(vals) == 1)
        return(list(name = as.character(vals), charges = vals))

      if (is.na(vals[1])) {
        seq_start <- vals[2]
        seq_end <- last(vals)
        na_val <- vals[1]
      } else {
        seq_start <- vals[1]
        seq_end <- last(vals)
        na_val <- NULL
      }

      val_sequence <- c(na_val, seq_start:seq_end)
      names_val_sequence <- paste0(c(na_val, unique(c(seq_start, seq_end))), collapse = "-")

      list(name = names_val_sequence, charges = val_sequence)
    }

    interpret_charge_ranges <- function(x) {
      if (x == "")
        return(list())

      groups <- transpose(map(str_split_1(x, "\\s*,\\s*"), interpret_charge_groups))

      groups_out <- groups$charges
      duplicated <- duplicated(groups_out)
      empty <- lengths(groups_out) == 0
      names(groups_out) <- list_c(groups$name)

      groups_out[duplicated == 0 & !empty]
    }

    observeEvent(input$apply_plotsettings, {
      plotsettings$max_charge <- input$slider_max_charge
      plotsettings$invert_stacking <- input$checkbox_invert_stacking
      plotsettings$unify_rt_range <- input$checkbox_unify_rt_range
      plotsettings$use_custom_rt_range <- input$checkbox_use_custom_rt_range
      plotsettings$custom_rt_range <- input$slider_custom_rt_range
      plotsettings$unify_y_limit <- input$checkbox_unify_y_limit
      plotsettings$use_custom_y_limit <- input$checkbox_use_custom_y_limit
      plotsettings$custom_y_limit <- input$numeric_custom_y_limit

      charge_ranges <- interpret_charge_ranges(input$text_annotate_charge_ranges)
      plotsettings$annotate_charge_ranges <- charge_ranges
      updateTextInput(inputId = "text_annotate_charge_ranges", value = paste0(names(charge_ranges), collapse = ", "))
    })

    observeEvent(input$checkbox_unify_rt_range, {
      if (!input$checkbox_unify_rt_range && input$checkbox_use_custom_rt_range)
        updateCheckboxInput(inputId = "checkbox_use_custom_rt_range", value = FALSE)
    })
    observeEvent(input$checkbox_use_custom_rt_range, {
      if (input$checkbox_use_custom_rt_range && !input$checkbox_unify_rt_range)
        updateCheckboxInput(inputId = "checkbox_unify_rt_range", value = TRUE)
    })

    observeEvent(plotdata_rt_range(), {
      rt_max <- max(plotdata_rt_range()[2], 60, na.rm = TRUE)

      custom_rt_range <- plotsettings$custom_rt_range
      plotsettings$custom_rt_range <- c(min(custom_rt_range[1], rt_max), min(custom_rt_range[2], rt_max))

      custom_rt_range <- input$slider_custom_rt_range
      updateSliderInput(inputId = "slider_custom_rt_range", max = rt_max, value = c(min(custom_rt_range[1], rt_max), min(custom_rt_range[2], rt_max)))
    })

    observeEvent(input$checkbox_unify_y_limit, {
      if (!input$checkbox_unify_y_limit && input$checkbox_use_custom_y_limit)
        updateCheckboxInput(inputId = "checkbox_use_custom_y_limit", value = FALSE)
    })
    observeEvent(input$checkbox_use_custom_y_limit, {
      if (input$checkbox_use_custom_y_limit && !input$checkbox_unify_y_limit)
        updateCheckboxInput(inputId = "checkbox_unify_y_limit", value = TRUE)
    })

    # fill plotdata
    observeEvent({plotsettings$max_charge; plotsettings$invert_stacking; plotsettings$annotate_charge_ranges}, {
      rawfiles$plotdata <- map2(rawfiles$ticdata, rawfiles$integraldata, ~ make_plot(.x, .y, plotsettings$max_charge, plotsettings$invert_stacking, plotsettings$annotate_charge_ranges))
    })

    # render the sidebar options
    output$plotoptions <- renderUI({
      req(sidebar() == "plots")
      list(
        sliderInput(ns("slider_max_charge"), label = "Max Charge State", min = 1, max = 100, value = plotsettings$max_charge, round = TRUE),
        checkboxInput(ns("checkbox_invert_stacking"), label = "Invert Charge Stacking", value = plotsettings$invert_stacking),
        checkboxInput(ns("checkbox_unify_rt_range"), label = "Unify RT Range", value = plotsettings$unify_rt_range),
        checkboxInput(ns("checkbox_use_custom_rt_range"), label = "Use Custom RT Range", value = plotsettings$use_custom_rt_range),
        sliderInput(ns("slider_custom_rt_range"), label = "Custom RT Range", value = plotsettings$custom_rt_range, min = 0, max = max(plotdata_rt_range()[2], 60, na.rm = TRUE)),
        checkboxInput(ns("checkbox_unify_y_limit"), label = "Unify Y-Limit", value = plotsettings$unify_y_limit),
        checkboxInput(ns("checkbox_use_custom_y_limit"), label = "Use Custom Y-Limit", value = plotsettings$use_custom_y_limit),
        numericInput(ns("numeric_custom_y_limit"), label = "Custom Y-Limit", value = plotsettings$custom_y_limit, min = 0),
        textInput(ns("text_annotate_charge_ranges"), label = "Annotate Charge Ranges", value = paste0(names(plotsettings$annotate_charge_ranges), collapse = ", "), placeholder = "1-3, NA"),
        actionButton(ns("apply_plotsettings"), label = "Apply")
      )
    })

    output$plots_boxes <- renderUI({
      ids <- rawfiles$ids[rawfiles$current_order]
      labels <- rawfiles$labels
      plotdata <- rawfiles$plotdata

      rt_range <- c(NA_real_, NA_real_)
      tic_max <- NA_real_

      if (plotsettings$unify_rt_range) {
        if (plotsettings$use_custom_rt_range) {
          rt_range <- plotsettings$custom_rt_range
        } else {
          rt_range <- plotdata_rt_range()
        }
      }

      if (plotsettings$unify_y_limit) {
        if (plotsettings$use_custom_y_limit)
          tic_max <- plotsettings$custom_y_limit
        else
          tic_max <- plotdata_tic_max()
      }

      map(ids, function(id) {
        box(
          title = labels[id],
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          {
            if (hasName(plotdata, id)) {
              p <- plotdata[[id]]

              if (all(is.na(rt_range)))
                p_rt_range <- p$rt_range
              else
                p_rt_range <- rt_range

              if (is.na(tic_max))
                p_tic_max <- p$tic_max
              else
                p_tic_max <- tic_max

              renderCachedPlot(
                {
                  p$plot +
                    scale_x_continuous(limits = p$rt_range, breaks = scales::extended_breaks(n = 14, Q = c(1, 5, 2, 4, 3))) +
                    scale_y_continuous(limits = c(0, p$tic_max)) +
                    coord_cartesian(xlim = p_rt_range, ylim = c(0, p_tic_max)) +
                    theme(legend.key.height = unit(4, "lines"))
                },
                { digest::digest(list(p$digest, p_tic_max, p_rt_range)) },
                sizePolicy = sizeGrowthRatio(width = 300, height = 400, growthRate = 1.2),
                outputArgs = list(height = 400)
              )
            } else {
              renderText(paste("Not available yet."))
            }
          }
        )
      })
    })
  })
}
