#
# output$SITE_CATALOG <- renderRHandsontable({
#
#     site_table <- readr::read_csv('data/general/catalog_files/all_sites.csv')
#
#     rhandsontable::rhandsontable(data = site_table,
#                                  rowHeaders = NULL,
#                                  # search = TRUE,
#                                  height = 700,
#                                  readOnly = TRUE) %>%
#         rhandsontable::hot_cols(colWidths = 100) %>%
#         rhandsontable::hot_rows(rowHeights = 50) %>%
#         rhandsontable::hot_table(overflow = 'hidden')
# })

output$SITE_CATALOG <- DT::renderDataTable({

    d <- sm(readr::read_csv('data/general/catalog_files/all_sites.csv'))
    DT::datatable(d,
                  options = list(scrollX = 'true'),
                  escape = FALSE)
                                    # autoWidth = 'true'
                                    # columnDefs = list(list(width = '50px',
                                    #                        targets = 1))
        # '[{ targets: [0, 1], width: 250},{ targets: "_all", width: 50}]'
})

output$VARIABLE_CATALOG <- DT::renderDataTable(
    expr = {
        d <- sm(readr::read_csv('data/general/catalog_files/all_variables.csv'))
        DT::datatable(d,
                      options = list(scrollX = 'true'),
                      escape = FALSE)
    }
    # server = FALSE
)

output$VARIABLE_SUBCATALOG <- DT::renderDataTable({

    req(input$VARIABLE_SUBCATALOG_BUTTON_LISTENER)
    d <- sm(readr::read_csv(glue('data/general/catalog_files/indiv_variables/{v}.csv',
                         v = input$VARIABLE_SUBCATALOG_BUTTON_LISTENER)))
    DT::datatable(d, options = list(scrollX = 'true'))
})

output$SITE_SUBCATALOG <- DT::renderDataTable({

    req(input$SITE_SUBCATALOG_BUTTON_LISTENER)
    d <- sm(readr::read_csv(glue('data/general/catalog_files/indiv_sites/{s}.csv',
                         s = input$SITE_SUBCATALOG_BUTTON_LISTENER)))
    DT::datatable(d, options = list(scrollX = 'true'))
})

# download_clicks <- reactiveValues(ts_download = 0)

# observeEvent(input$NO_SELECTIONS_WARNING, {
# # observeEvent(input$DL_TS_SELECTIONS, {
#
#     # if(length(input$DL_TS_SELECTIONS) == 1 && is.numeric(input$DL_TS_SELECTIONS)){
#     showNotification(input$NO_SELECTIONS_WARNING[2],
#                      duration = 0.75,
#                      closeButton = FALSE,
#                      type = 'warning')
#     # } else {
#     #     # download_clicks$ts_download <- download_clicks$ts_download + 1
#     #     print(download_clicks$ts_download)
#     # }
# })

output$DL_SUBMIT_SITE <- downloadHandler(
    filename = 'macrosheds_sitedata.csv',
    content = function(file){

        dlset <- select(site_data_copy,
                        network, pretty_network, domain, pretty_domain, site_code,
                        epsg_code = CRS,
                        timezone_olson = local_time_zone) %>%
            right_join(read_csv('data/general/catalog_files/all_sites.csv',
                       col_types = cols()),
                      by = c(pretty_network = 'Network',
                             pretty_domain = 'Domain',
                             site_code = 'SiteCode')) %>%
            select(network,
                   network_fullname = pretty_network,
                   domain,
                   domain_fullname = pretty_domain,
                   site_code,
                   site_fullname = SiteName,
                   stream_name = StreamName,
                   site_type = SiteType,
                   ws_status = WatershedStatus,
                   latitude = Latitude,
                   longitude = Longitude,
                   epsg_code,
                   ws_area_ha = AreaHectares,
                   n_observations = Observations,
                   n_variables = Variables,
                   first_record_utc = FirstRecordUTC,
                   last_record_utc = LastRecordUTC,
                   timezone_olson)
            # mutate(Q_data_available = paste(domain, site_code, sep = '_') %in%
            #            sites_with_Q)

        write_csv(dlset, file)
    },
    contentType = 'text/csv')

output$DL_SUBMIT_VAR <- downloadHandler(
    filename = 'macrosheds_vardata.csv',
    content = function(file){

        dlset <- read_csv('data/general/catalog_files/all_variables.csv',
                          col_types = cols()) %>%
            # mutate(method = 'pending') %>% #TODO: include method info?
            select(variable_code = VariableCode,
                   variable_name = VariableName,
                   chem_category = ChemCategory,
                   unit = Unit,
                   # method
                   observations = Observations,
                   n_sites = Sites,
                   # mean_obs_per_site = MeanObsPerSite,
                   first_record_utc = FirstRecordUTC,
                   last_record_utc = LastRecordUTC)
                   # type = variable_type,
                   # subtype = variable_subtype)

        write_csv(dlset, file)
    },
    contentType = 'text/csv')

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    autoDestroy = FALSE,
    handler.quoted = TRUE,
    # suspended = TRUE,
    eventExpr = input$SITE_CATALOG_BUTTON,
    handlerExpr = site_catalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$VARIABLE_CATALOG_BUTTON,
    handlerExpr = variable_catalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$VARIABLE_SUBCATALOG_BUTTON_LISTENER,
    handlerExpr = variable_subcatalog
)

observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handler.quoted = TRUE,
    autoDestroy = FALSE,
    eventExpr = input$SITE_SUBCATALOG_BUTTON_LISTENER,
    handlerExpr = site_subcatalog
)

# observeEvent(
#     ignoreNULL = TRUE,
#     ignoreInit = TRUE,
#     handler.quoted = TRUE,
#     autoDestroy = FALSE,
#     eventExpr = input$TIMESERIES_DL_BUTTON,
#     handlerExpr = timeseries_dl
# )
#
# observeEvent(
#     ignoreNULL = TRUE,
#     ignoreInit = TRUE,
#     handler.quoted = TRUE,
#     autoDestroy = FALSE,
#     eventExpr = input$SPATIAL_DL_BUTTON,
#     handlerExpr = spatial_dl
# )
