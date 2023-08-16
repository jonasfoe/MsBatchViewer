
shinyAppUI <- function() {
  # Create a namespace function using the provided id
  ns_processing <- NS("processing")
  ns_metadata <- NS("metadata")
  ns_plots <- NS("plots")

  shinyAppHeader <- dashboardHeader(
    title = "MsBatchViewer",
    dropdownMenuOutput("tasksMenu"),
    dropdownMenu(
      type = c("messages"),
      badgeStatus = NULL,
      icon = icon("circle-info"),
      headerText = "Info",
      messageItem("Author", icon = icon("user"),
        "Jonas D. Förster"
      ),
      notificationItem(icon = icon(NULL), tags$small(
        "MsBatchViewer uses:", br(),
        "RawFileReader reading tool.", br(),
        "Copyright © 2016 by Thermo Fisher Scientific, Inc.", br(),
        "All rights reserved."
      ))
    )
  )

  shinyAppSidebar <- dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem(
        "Files", icon = icon("list"), tabName = "input"
        # badgeLabel = "new", badgeColor = "green"
      ),
      menuItem(
        "Metadata", icon = icon("table-list"), tabName = "metadata"
        # badgeLabel = "new", badgeColor = "green"
      ),
      menuItem(
        "Plots", icon = icon("chart-area"), tabName = "plots"
        # badgeLabel = "new", badgeColor = "green"
      )
    ),
    uiOutput(outputId = ns_metadata("metadataoptions")),
    uiOutput(outputId = ns_plots("plotoptions"))
  )

  shinyAppBody <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "input",
        fixedRow(column(12,
          h2("Manage Files"), br(),
          div("Here you can import rawfiles, assign custom titles and drag&drop to sort the list."), br(),
          div("Note: There is currently no error handling so importing broken / incomplete rawfiles will crash the app."), br(),
          shinyFilesButton(id = ns_processing("rawfiles_new"), label = "Select Raw File(s)", title = "Select Raw File(s)", multiple = TRUE), br(),
        )),
        fluidRow(
          uiOutput(outputId = ns_processing("file_list"))
        )
      ),
      tabItem(tabName = "metadata",
        fluidRow(column(width = 12,
          h2("Metadata")
        )),
        fluidRow(column(width = 12,
          DT::dataTableOutput(outputId = ns_metadata("metadata_table"))
        ))
      ),
      tabItem(tabName = "plots",
        fluidRow(column(width = 12,
          h2("Charge State Plots")
        )),
        fluidRow(
          uiOutput(outputId = ns_plots("plots_boxes"))
        )
      )
    )
  )

  dashboardPage(
    shinyAppHeader,
    shinyAppSidebar,
    shinyAppBody
  )
}
