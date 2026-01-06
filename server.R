# library(shiny)
# library(readr)
# library(shinyjs)
library(tidyverse)

options(shiny.usecairo = TRUE)

hyetograph_file <- "www/js/hyetograph.js"
hyetograph_js <- readChar(
  con = hyetograph_file,
  nchars = file.info(hyetograph_file)$size
)

server <- function(input, output, session) {
  guide1a$init()
  guide1b$init()
  guide2a$init()
  guide2b$init()

  # #hacky way to specify div height by % with js
  # height50 = reactive({
  #     ifelse(is.null(input$height50), 0, input$height50)
  # })
  # js$getHeight50()

  init_vals <- reactiveValues()
  init_vals$enable_unitconvert <- FALSE
  init_vals$recent_domain <- "hbef"
  init_vals$basedata_change_reloads_plots <- FALSE
  init_vals$initial_plots_loaded <- FALSE
  init_vals$ts_tab_is_pristine <- TRUE

  session$onFlushed(function() {
    init_vals$enable_unitconvert <- TRUE
  }, once = TRUE)

  observeEvent(input$COLLAPSE_SIDEBAR, {
    shinyjs::toggleClass(
      selector = ".sidebar-sub",
      class = "sidebar-sub-gone"
    )
    shinyjs::toggleClass(
      selector = ".content-wrapper",
      class = "content-wrapper-wide"
    )
  })

  observeEvent(input$COLLAPSE_DATA, {
    shinyjs::toggleClass(
      selector = ".data-sub",
      class = "data-sub-gone"
    )
    shinyjs::toggleClass(
      selector = ".content-wrapper",
      class = "data-wrapper-wide"
    )
    shinyjs::toggleClass(
      selector = ".main-sidebar",
      class = "main-sidebar-wide"
    )
    shinyjs::toggleClass(
      selector = "#data-toggler",
      class = "data-toggled"
    )
  })

  observeEvent(input$COLLAPSE_ATTRIBUTES, {
    # shinyjs::toggleClass(
    #     selector = "#attribute-content",
    #     class = "hidden"
    # )
    shinyjs::toggleClass(
      selector = "#MAP",
      class = "fullmap"
    )
  })


  stream_gauge_buttons <- read_file("ui/stream_gauge_buttons.html")
  rain_gauge_buttons <- read_file("ui/rain_gauge_buttons.html")
  loading_dots <- HTML(read_file("ui/loading_dots.html"))

  show_loading_dots <- function(id, duration = NULL, message = NULL) {
    msg <- if (is.null(message)) "" else message

    # shinyjs::show("loading-start")

    showNotification(loading_dots,
      ui = message,
      id = id,
      duration = duration,
      closeButton = FALSE,
      type = "message"
    )
  }

  # source("ui/loading_dots_ui.R", local = TRUE)
  source("server/summary_biplot_server.R", local = TRUE)
  # source('server/oneSiteNVar_server.R', local=TRUE)
  source("server/nSiteNVar_server.R", local = TRUE)
  source("server/map_server.R", local = TRUE)
  source("ui/data_ui.R", local = TRUE)
  source("server/data_server.R", local = TRUE)

  # # register clicking of map popup links
  # observeEvent(input$SITE_EXPLORE, {
  #     updateTabsetPanel(session,
  #         "right_tabs",
  #         selected = "multisite_exploration"
  #     )
  # })
  # register clicking of map popup links
  observeEvent(input$SITE_EXPLORE, {
    sel <- input$active_tab

    if (sel == "multisite_exploration") {
      updateTabsetPanel(session,
        "right_tabs",
        selected = "multisite_exploration"
      )
    } else if (sel == "biplot") {
      updateTabsetPanel(session,
        "right_tabs",
        selected = "biplot"
      )
    } else {
      print("inconclusive right tab focal data, default to TS")
      updateTabsetPanel(session,
        "right_tabs",
        selected = "multisite_exploration"
      )
    }
  })
  output$NSTREAMS <- renderText({
    nrow(unique(site_data[, c("stream", "domain")]))
  })

  output$NSITES <- renderText({
    sum(site_data$site_type %in% c("stream_gauge", "stream_sampling_point"),
      na.rm = TRUE
    )
  })

  output$NOBS <- renderText({
    readr::read_file("data/general/total_nonspatial_observations.txt") %>%
      as.numeric() %>%
      round(-4) %>%
      format(
        scientific = FALSE,
        big.mark = ","
      )
  })

  # observeEvent(
  #     once = TRUE,
  #     ignoreNULL = FALSE,
  #     ignoreInit = FALSE,
  #     eventExpr = TRUE,
  #     handler.quoted = TRUE,
  #     handlerExpr = {
  #         show_loading_dots("LOADING_POPUP",
  #             message = "loading"
  #         )
  #     }
  # )

  observeEvent(
    eventExpr = input$MAPDATA,
    priority = 110,
    handlerExpr = {
      init_vals$basedata_change_reloads_plots <- FALSE

      spatial_list <- lapply(rapply(input$MAPDATA, enquote, how = "unlist"), eval)
      sites_list <- c()

      for (x in names(spatial_list)) {
        sites_list <- c(sites_list, spatial_list[[x]])
      }

      site_pairs <- c()
      i <- 0
      for (item in sites_list) {
        i <- i + 1
        site <- str_match(item, "(.+?)__(.+?)_goto.*$")[1, 3]
        domain <- str_match(item, "(.+?)__(.+?)_goto.*$")[1, 2]
        pair <- c(site, domain)
        site_pairs[[i]] <- pair
      }

      site_all <- c()
      dmns <- c()
      dmn_all <- c()

      for (unit in site_pairs) {
        # get domain and site
        this_dmn <- unit[2]
        this_site <- unit[1]

        # append domain and site to master lists
        dmns <- c(dmns, this_dmn)
        site_all <- c(site_all, this_site)

        # get sitelist for domain, and append
        this_dmn_sitelist <- get_sitelist(
          domain = this_dmn,
          type = c(
            "stream_gauge",
            "stream_sampling_point"
          )
        )
        dmn_all <- c(dmn_all, this_dmn_sitelist)
      }

      updateSelectizeInput(
        session = session,
        inputId = "DOMAINS3",
        label = NULL,
        selected = dmns,
        choices = domains_pretty,
        options = list(maxItems = 3)
      )

      updateSelectizeInput(
        session = session,
        inputId = "SITES3",
        label = NULL,
        selected = site_all,
        choices = dmn_all,
        options = list(maxItems = 3)
      )

      # update biplot selectizers too
      updateSelectizeInput(
        session = session,
        inputId = "DOMAINS2_B",
        label = NULL,
        selected = dmns,
        choices = domains_pretty,
        # options = list()
      )

      updateSelectizeInput(
        session = session,
        inputId = "SITES2_B",
        label = NULL,
        selected = site_all,
        choices = dmn_all,
        # options = list()
      )
      print("MAPDATA")
    }
  )

  observeEvent(
    eventExpr = {
      input$DISMISS_MODAL
    },
    handlerExpr = {
      removeModal(session)
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$TAKE_TOUR,
    handlerExpr = {
      print("TAKE_TOUR, MODERN: INITIATED")
      guide1a$start()
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )
  observeEvent(
    eventExpr = input$CONTINUE_TOUR,
    handlerExpr = {
      guide1b$start()
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = init_vals$loading_modal,
    handlerExpr = {
      # showModal(p('oi'))
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$START_DATA_TOUR,
    handlerExpr = {
      if (init_vals$ts_tab_is_pristine) {
        guide2a$start()
      } else {
        click("GEN_PLOTS3")
        show_loading_dots("LOADING_POPUP",
          message = "loading"
        )
      }
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$TRIGGER_LOADING_DOTS,
    handlerExpr = show_loading_dots("LOADING_POPUP",
      message = input$TRIGGER_LOADING_DOTS
    )
  )

  observeEvent(
    eventExpr = input$CONTINUE_DATA_TOUR,
    handlerExpr = {
      removeNotification("LOADING_POPUP")
      if (input$CONTINUE_DATA_TOUR == "a") {
        guide2a$start()
      } else if (input$CONTINUE_DATA_TOUR == "b") {
        guide2b$start()
      }
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  # observeEvent(
  #     eventExpr = input$CONTINUE_DATA_TOUR,
  #     handlerExpr = {
  #         guide2b$start()
  #     },
  #     autoDestroy = FALSE,
  #     ignoreInit = TRUE
  # )

  observeEvent(
    eventExpr = input$BACK_TO_VARIABLE_CATALOG,
    handlerExpr = {
      removeModal(session)
      shinyjs::click("VARIABLE_CATALOG_BUTTON")
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$BACK_TO_SITE_CATALOG,
    handlerExpr = {
      removeModal(session)
      shinyjs::click("SITE_CATALOG_BUTTON")
    },
    autoDestroy = FALSE,
    ignoreInit = TRUE
  )
}
