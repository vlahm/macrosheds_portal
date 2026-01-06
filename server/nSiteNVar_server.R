reactive_vals <- reactiveValues()
reactive_vals$update_basedata <- 0
reactive_vals$basedata <- list()

## reactivity flow control ####

# when domain(s) change, site options change (but not site selections)
observeEvent(
    eventExpr = input$DOMAINS3,
    ignoreNULL = FALSE,
    handlerExpr = {
        print("domain change (update sitelist)")

        dmns <- input$DOMAINS3
        sites <- input$SITES3

        site_opts <- generate_dropdown_sitelist(dmns)
        selection <- if (is.null(site_opts)) "" else sites

        updateSelectizeInput(session,
            "SITES3",
            choices = site_opts,
            selected = selection,
            options = list(maxItems = 3)
        )
    }
)

siteChanged <- eventReactive(
    {
        input$SITES3
    },
    {
        shinyjs::disable("GEN_PLOTS3")
        print("debouncing basedata reload")
    }
) %>% debounce(700)

observeEvent(
    eventExpr = siteChanged(),
    handlerExpr = {
        print("site change (update basedata)")
        shinyjs::enable("GEN_PLOTS3")

        time_scheme <- input$TIME_SCHEME3
        agg <- input$AGG3
        dmns <- input$DOMAINS3

        if (is.null(dmns)) { # for empty domain dropdown
            dmns <- init_vals$recent_domain
            sites <- get_default_site(domain = dmns[1])
        } else {
            sites <- input$SITES3
        }

        Q <- read_combine_feathers("discharge",
            dmns = dmns,
            sites = sites
        )
        chem <- read_combine_feathers("stream_chemistry",
            dmns = dmns,
            sites = sites
        )
        flux <- read_combine_feathers("stream_flux_inst_scaled",
            dmns = dmns,
            sites = sites
        )
        P <- read_combine_feathers("precipitation",
            dmns = dmns,
            sites = sites
        )
        pchem <- read_combine_feathers("precip_chemistry",
            dmns = dmns,
            sites = sites
        )
        pflux <- read_combine_feathers("precip_flux_inst_scaled",
            dmns = dmns,
            sites = sites
        )

        init_vals$recent_domain <- dmns[1] # needed?

        basedata <- list(
            Q = Q,
            chem = chem,
            flux = flux,
            P = P,
            pchem = pchem,
            pflux = pflux
        )

        if (time_scheme != "UTC" && agg == "Instantaneous") {
            basedata <- purrr::modify2(basedata,
                get_local_solar_time,
                .y = time_scheme
            )
        }

        reactive_vals$basedata <- basedata
    }
)

## on selectizer input change, update plot button
tsSelectizers <- reactive({
  list(input$DOMAINS3,
       input$SITES3,
       input$VARS3)
})

observeEvent(
    eventExpr = tsSelectizers(),
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    handlerExpr = {
        # change PLOT button to orange 'new data' status
        print('data input altered, ready to update plot')
        shinyjs::addClass("GEN_PLOTS3", "btn-warning")
    }
)

# when basedata changes, variable list changes, but not variable selections,
# unless the previous selections are not available for the newly selected sites(s).
# if basedata is changing as a result of clicking a map "Go to" link
# or advancing the data tour, Update Plots is triggered AFTER variables update
observeEvent(
    eventExpr = reactive_vals$basedata,
    handlerExpr = {
        print("basedata change (update varlist)")

        # DISABLE var dropdown until basedata is loaded
        basedata <- reactive_vals$basedata
        vars_ <- input$VARS3

        # chemvars_display_subset <- generate_dropdown_varlist(
        #     chemvars = chemvars,
        #     filter_set = drop_var_prefix(unique(basedata$chem$var))
        # )
        chemvars_display_subset <- filter_dropdown_varlist(basedata$chem)
        chemvars_vec <- unlist(chemvars_display_subset,
            recursive = TRUE,
            use.names = FALSE
        )
        vars_ <- vars_[vars_ %in% chemvars_vec]

        if (!length(vars_)) vars_ <- chemvars_vec[1]

        bcrp1 <- (!is.null(input$basedata_change_reloads_plots) &&
            input$basedata_change_reloads_plots)
        bcrp2 <- init_vals$basedata_change_reloads_plots

        if (!bcrp1) {

            # this will always occur unless we're here because of set_tour_location
            updateSelectizeInput(
                session = session,
                inputId = "VARS3",
                choices = chemvars_display_subset,
                selected = vars_
            )
        }

        if (bcrp1 || bcrp2) {

            # only possible to get here from input$MAPDATA or set_tour_location

            # probably unnecessary for this to be an updateSelectizeInput, when all it's
            # doing is invalidating to ensure GEN_PLOTS3 fires after VARS3
            # has updated. but i wanted to ensure VARS_INVISIBLE3 would have
            # the same priority as VARS3, to ensure no timing funny business
            # would be nice if we could directly invalidate VARS3
            updateSelectizeInput(
                session = session,
                inputId = "VARS_INVISIBLE3",
                choices = letters,
                selected = sample(letters, 3, TRUE)
            )
        }
    }
)

# disable site dropdown during basedata reload after variable change
observeEvent(input$SITES3, {
    shinyjs::disable("VARS3")
})

# genplots will always automatically disable itself
observeEvent(input$GEN_PLOTS3, {
    shinyjs::disable("GEN_PLOTS3")
})

observeEvent(reactive_vals$basedata, {
    shinyjs::enable("VARS3")
})

# disable timeslider on click 'update plots'
# re-enabling handled in generaljs
observeEvent(input$GEN_PLOTS3, {
    shinyjs::disable("DATES3")
})

observeEvent(
    eventExpr = input$GEN_PLOTS3,
    handlerExpr = {
        shinyjs::disable("DATES3")

        if (init_vals$initial_plots_loaded) {
            init_vals$ts_tab_is_pristine <- FALSE
        } else {
            init_vals$initial_plots_loaded <- TRUE
        }
    }
)

# for triggering Update Plots click from map go-to buttons AFTER updating var selection
observeEvent(input$VARS_INVISIBLE3,
             ignoreInit = TRUE,
             handlerExpr = {
               print("change to VARS_INVISIBLE3")
               ## print("---- currently does not fire plots")
               shinyjs::click("GEN_PLOTS3")
               init_vals$basedata_change_reloads_plots <- FALSE
             })

# update timeslider when Update Plots is clicked
observeEvent(
    eventExpr = input$GEN_PLOTS3,
    priority = 90,
    ignoreNULL = FALSE,
    handlerExpr = {
        basedata <- reactive_vals$basedata
        if (length(basedata) == 0) {
            return()
        }
        agg <- input$AGG3
        vars_ <- input$VARS3
        dates <- input$DATES3
        sites <- input$SITES3

        print("Update Plots")
        shinyjs::disable("GEN_PLOTS3")

        dtrng <- get_timeslider_extent(basedata, dates)

        if (nrow(basedata$chem)) {
            vardates <- filter(basedata$chem, drop_var_prefix(var) %in% vars_)$datetime
        } else {
            vardates <- dtrng
        }

        if (length(sites) == 1 && !dt_ranges_overlap(vardates, dates)) {
            if (agg == "Yearly") {
                selected_dtrng <- dtrng
            } else {
                selected_dtrng <- as.Date(most_recent_year(range(vardates)))
            }
        } else {
            selected_dtrng <- dates
        }

        print("plot update triggering from update button slider change")
        updateSliderInput(
            session = session,
            inputId = "DATES3",
            min = dtrng[1],
            max = dtrng[2],
            value = selected_dtrng,
            timeFormat = "%b %Y"
        )

        # probably unnecessary for this to be an updateSliderInput, when all it's
        # doing is invalidating to ensure timeSliderChanged fires after DATES3
        # has updated., but i wanted to ensure DATES_INVISIBLE3 would have
        # the same priority as DATES3, to ensure no timing funny business.
        # would be nice if we could directly invalidate DATES3
        invdate <- as.Date("1900-01-01")
        invdate2 <- invdate + runif(1, 1, 100000)
        updateSliderInput(
            session = session,
            inputId = "DATES_INVISIBLE3",
            min = invdate,
            max = invdate2,
            value = c(invdate2 - 1, invdate2),
            timeFormat = "%b %Y"
        )

        # session$sendCustomMessage('flash_plot',
        #                           jsonlite::toJSON('placeholder'))
    }
)

# reduce the reactivity sensitivity of the timeslider, so that intermediate inputs
# don't trigger plot updates
timeSliderChanged <- eventReactive(
    {
        input$DATES_INVISIBLE3
        input$DATES3
    },
    {

        # this is the ultimate gatekeeper for plot rendering. it can be triggered by
        # the Update Plots button or by the user directly. to ensure that the plots
        # still update if the dates selected don't change, a random date is appended
        # to input$DATES3. this random date is then ignored in all the data preppers

        print("slider update for plots.")

        datevec_rando_append <- c(input$DATES3, as.Date(runif(1, 0, 10000)))
        return(datevec_rando_append)
    }
) %>%
    debounce(1000)

## data preppers ####

dataChem <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataChem")

        # vars_ <<- input$VARS3
        # conc_flux <<- input$CONC_FLUX3
        # conc_unit <<- input$CONC_UNIT3
        # flux_unit <<- input$FLUX_UNIT3
        # agg <<- input$AGG3
        # sites <<- input$SITES3
        # time_scheme <<- input$TIME_SCHEME3
        # igsn <<- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        # show_uncert <<- input$SHOW_UNCERT3
        # show_flagged <<- input$FLAGS3
        # show_imputed <<- input$INTERP3
        # dates <<- input$DATES3
        # enable_unitconvert <<- init_vals$enable_unitconvert
        # basedata <<- reactive_vals$basedata

        vars_ <- input$VARS3
        conc_flux <- input$CONC_FLUX3
        conc_unit <- input$CONC_UNIT3
        flux_unit <- input$FLUX_UNIT3
        agg <- input$AGG3
        sites <- input$SITES3
        time_scheme <- input$TIME_SCHEME3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        dates <- timeSliderChanged()[1:2]

        enable_unitconvert <- init_vals$enable_unitconvert
        basedata <- reactive_vals$basedata


        datachem <- if (conc_flux == "Flux") basedata$flux else basedata$chem

        datachem <- filter_and_unprefix(
            d = datachem,
            selected_vars = vars_,
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        datachem <- ms_aggregate(
            d = datachem,
            agg_selection = agg
        )

        if(length(datachem) == 0){
          return(tibble())
        }

        if (nrow(datachem) == 0) {
            return(datachem)
        }

        datachem <- pivot_wider(datachem,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        datachem <- convert_portal_units(
            d = datachem,
            conversion_enabled = enable_unitconvert,
            conc_flux_selection = conc_flux,
            conc_unit = conc_unit,
            flux_unit = flux_unit
        )

        return(datachem)
    }
)

dataVWC <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataVWC")

        vars_ <- input$VARS3
        conc_flux <- input$CONC_FLUX3
        conc_unit <- input$CONC_UNIT3
        flux_unit <- input$FLUX_UNIT3
        agg <- input$AGG3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        dates <- timeSliderChanged()[1:2]

        enable_unitconvert <- init_vals$enable_unitconvert
        basedata <- reactive_vals$basedata

        agg_unit <- ifelse(agg == "Monthly", "month", "year") # this won't run if agg < monthly

        dataq <- filter_and_unprefix(
            d = basedata$Q,
            selected_vars = "discharge",
            selected_datebounds = dates,
            # selected_agg = agg,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )
        # conc_or_flux = conc_flux)

        first_two_rows <- dataq[1:2, ]
        if (first_two_rows$site_code[1] == first_two_rows$site_code[2]) {
            dtdiff <- diff(first_two_rows$datetime)
            units(dtdiff) <- "mins"
            if (as.numeric(dtdiff) == 15) {
                stop("are we in high-res mode? if so, time to write pre-aggregated (by day) flux, P, and Q datasets to portal/data, so that vwc (and maybe other stuff) can be calculated without having to do a daily groupby
    the two VWC functions should only ever receive P, Q, and flux data in daily increments")
            }
        }

        # NOTE: the "traditional" way to compute VWC is:
        # VWC = sum(concentrations * volumes) / sum(volumes)
        # over a given period. Because we have discharge (a rate), rather than volume,
        # we could scale it to the sample interval and then use the equation
        # above. For simplicity, I use this instead, which works out the same:
        # VWC = mean(concentrations * discharges) / mean(discharges)
        # Note also that we're starting from flux here, for computational
        # efficiency, so the "concentrations * volumes" part is already computed,
        # and it's a rate too. That means the modified equation is:
        # VWC = mean(fluxes) / mean(discharges). There are two minor introductions of
        # error. The first is that we don't omit discharge values from the denominator
        # that lack a corresponding flux value (this is expensive, and not doing it
        # should rarely skew the results). The second is that a "month" is considered
        # 30 days here, which only affects coverage filtering.

        if (agg == "Monthly") {
            agg_unit <- "month"
            period_complete_n <- 30 # for filtering below. see justification in comment below
        } else if (agg == "Yearly") {
            agg_unit <- "year"
            period_complete_n <- 365
        } else {
            stop("this should never run when agg < monthly")
        }

        dataflux <- filter_and_unprefix(
            d = basedata$flux,
            selected_vars = vars_,
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        period_mean_Q <- dataq %>%
            mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
            group_by(site_code, datetime) %>%
            # divisor of vwc is MEAN(Q) (unlike Pvwc), because Q is expressed as a rate (L/s)
            summarize(
                val = mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status),
                ms_interp = numeric_any(ms_interp),
                .groups = "drop"
            )

        datavwc <- dataflux %>%
            mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
            group_by(site_code, var, datetime) %>%
            summarize(
                nday = sum(!is.na(val)),
                # vwc dividend is a mean, not a sum, because flux is also a rate
                val = mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status),
                ms_interp = numeric_any(ms_interp),
                .groups = "drop"
            ) %>%
            filter(nday > period_complete_n * 0.1) %>%
            left_join(period_mean_Q,
                by = c("datetime", "site_code"),
                suffix = c(".flux", ".Q")
            ) %>%
            left_join(site_data[c("site_code", "ws_area_ha")],
                by = "site_code"
            ) %>%
            #     mg/L = kg/ha/d  /  L/s  *  ha          ...
            mutate(
                val = val.flux / val.Q * ws_area_ha * 1e6 / 86400,
                ms_status = numeric_any_v(ms_status.flux, ms_status.Q),
                ms_interp = numeric_any_v(ms_interp.flux, ms_interp.Q)
            ) %>%
            select(datetime, site_code, var, val, ms_status, ms_interp)

        if (nrow(datavwc) == 0) {
            return(datavwc)
        }

        datavwc <- pivot_wider(datavwc,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        datavwc <- convert_portal_units(
            d = datavwc,
            conversion_enabled = enable_unitconvert,
            conc_flux_selection = conc_flux,
            conc_unit = conc_unit,
            flux_unit = flux_unit
        )

        return(datavwc)
    }
)

dataPchem <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataPchem")

        vars_ <- input$VARS3
        conc_flux <- input$CONC_FLUX3
        conc_unit <- input$CONC_UNIT3
        flux_unit <- input$FLUX_UNIT3
        agg <- input$AGG3
        agg <- input$AGG3
        sites <- input$SITES3
        time_scheme <- input$TIME_SCHEME3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        dates <- timeSliderChanged()[1:2]

        enable_unitconvert <- init_vals$enable_unitconvert
        basedata <- reactive_vals$basedata

        dataPchem <- if (conc_flux == "Flux") basedata$pflux else basedata$pchem

        dataPchem <- filter_and_unprefix(
            d = dataPchem,
            selected_vars = vars_,
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        dataPchem <- ms_aggregate(
            d = dataPchem,
            agg_selection = agg
        )

        if (nrow(dataPchem) == 0) {
            return(dataPchem)
        }

        dataPchem <- pivot_wider(dataPchem,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        dataPchem <- convert_portal_units(
            d = dataPchem,
            conversion_enabled = enable_unitconvert,
            conc_flux_selection = conc_flux,
            conc_unit = conc_unit,
            flux_unit = flux_unit
        )


        return(dataPchem)
    }
)

dataPVWC <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataPVWC")

        vars_ <- input$VARS3
        conc_flux <- input$CONC_FLUX3
        conc_unit <- input$CONC_UNIT3
        flux_unit <- input$FLUX_UNIT3
        agg <- input$AGG3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        dates <- timeSliderChanged()[1:2]

        enable_unitconvert <- init_vals$enable_unitconvert
        basedata <- reactive_vals$basedata

        # NOTE: the "traditional" way to compute VWC is:
        # VWC = sum(concentrations * volumes) / sum(volumes)
        # over a given period. We're starting from flux here, for computational
        # efficiency, so the "concentrations * volumes" part is already computed,
        # and it's a rate, which means the modified equation is:
        # VWC = mean(fluxes) * ndays / sum(volumes). There are two minor introductions of
        # error. The first is that we don't omit precip volumes from the denominator
        # that lack a corresponding flux value (this is expensive, and not doing it
        # should rarely skew the results). The second is that a "month" is considered
        # 30 days here. So the mean daily flux for Feb will get multiplied by 30
        # instead of 28, etc. That avoids a slow dplyr conditional.

        if (agg == "Monthly") {
            agg_unit <- "month"
            period_complete_n <- 30 # for filtering below. not worth using actual n days per month
        } else if (agg == "Yearly") {
            agg_unit <- "year"
            period_complete_n <- 365
        } else {
            stop("this should never run when agg < monthly")
        }

        datap <- filter_and_unprefix(
            d = basedata$P,
            selected_vars = "precipitation",
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        dataPflux <- filter_and_unprefix(
            d = basedata$pflux,
            selected_vars = vars_,
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        period_sum_P <- datap %>%
            mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
            group_by(site_code, datetime) %>%
            # divisor of Pvwc is SUM(P) (unlike vwc), because P is expressed as a length (mm)
            summarize(
                val = sum(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status),
                ms_interp = numeric_any(ms_interp),
                .groups = "drop"
            )

        dataPvwc <- dataPflux %>%
            mutate(datetime = lubridate::floor_date(datetime, unit = agg_unit)) %>%
            group_by(site_code, var, datetime) %>%
            summarize(
                nday = sum(!is.na(val)),
                # Pvwc dividend is a mean, not a sum, because flux is a rate
                val = mean(val, na.rm = TRUE),
                ms_status = numeric_any(ms_status),
                ms_interp = numeric_any(ms_interp),
                .groups = "drop"
            ) %>%
            filter(nday > period_complete_n * 0.5) %>%
            left_join(period_sum_P,
                by = c("datetime", "site_code"),
                suffix = c(".flux", ".P")
            ) %>%
            # mg/L = kg/ha/d  /  mm (~ per month or per year) * 1e6 / 1e4 * d
            mutate(
                val = val.flux / val.P * 100 * period_complete_n,
                ms_status = numeric_any_v(ms_status.flux, ms_status.P),
                ms_interp = numeric_any_v(ms_interp.flux, ms_interp.P)
            ) %>%
            select(datetime, site_code, var, val, ms_status, ms_interp)

        if (nrow(dataPvwc) == 0) {
            return(dataPvwc)
        }

        dataPvwc <- pivot_wider(dataPvwc,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        dataPvwc <- convert_portal_units(
            d = dataPvwc,
            conversion_enabled = enable_unitconvert,
            conc_flux_selection = conc_flux,
            conc_unit = conc_unit,
            flux_unit = flux_unit
        )

        return(dataPvwc)
    }
)

dataPrecip <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataPrecip")

        agg <- input$AGG3
        conc_flux <- input$CONC_FLUX3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        dates <- timeSliderChanged()[1:2]

        basedata <- reactive_vals$basedata

        dataP <- filter_and_unprefix(
            d = basedata$P,
            selected_vars = "precipitation",
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        dataP <- ms_aggregate(
            d = dataP,
            agg_selection = agg
        )

        if(length(dataP) == 0){
          return(tibble())
        }

        if (nrow(dataP) == 0) {
            return(dataP)
        }

        dataP <- pivot_wider(dataP,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        return(dataP)
    }
)

dataQ <- eventReactive(
    {
        timeSliderChanged()
    },
    {
        print("dataQ")

        agg <- input$AGG3
        igsn <- c(input$INSTALLED_V_GRAB3, input$SENSOR_V_NONSENSOR3)
        show_uncert <- input$SHOW_UNCERT3
        show_flagged <- input$FLAGS3
        show_imputed <- input$INTERP3
        conc_flux <- input$CONC_FLUX3
        dates <- timeSliderChanged()[1:2]

        basedata <- reactive_vals$basedata

        dataQ <- filter_and_unprefix(
            d = basedata$Q,
            selected_vars = "discharge",
            selected_datebounds = dates,
            selected_prefixes = igsn,
            show_uncert = show_uncert,
            show_flagged = show_flagged,
            show_imputed = show_imputed
        )

        dataQ <- ms_aggregate(
            d = dataQ,
            agg_selection = agg
        )

        if(length(dataQ) == 0){
          return(tibble())
        }

        if (nrow(dataQ) == 0) {
            return(dataQ)
        }

        dataQ <- pivot_wider(dataQ,
            names_from = var,
            values_from = c("val", "ms_status", "ms_interp")
        )

        return(dataQ)
    }
)


## plot generators ####

output$GRAPH_PRECIP3 <- renderDygraph({
    dataP <- dataPrecip()
    dates <- isolate(timeSliderChanged()[1:2])
    sites <- isolate(input$SITES3)

    print("plot P")

    # if(reactive_vals$precip3 == 0) return()

    tryCatch(
        {
            dataP <- dataP %>%
                dplyr::rename_with(~ gsub("_precipitation", "", .x)) %>%
                tidyr::pivot_wider(
                    names_from = site_code,
                    values_from = c(
                        "val", "ms_status",
                        "ms_interp"
                    )
                ) %>%
                dplyr::rename_with(~ gsub("val_", "", .x))
        },
        error = function(e) NULL
    )

    if (nrow(dataP)) {
        dataP <- select(
            dataP,
            datetime, any_of(sites)
        ) # preserve order

        colnms <- colnames(dataP)

        displabs <- colnms[!colnms == "datetime"]

        dydat <- xts(dataP[, displabs],
            order.by = dataP$datetime,
            tzone = lubridate::tz(dataP$datetime[1])
        )

        dimnames(dydat) <- list(NULL, displabs)

        ymax <- max(dydat,
            na.rm = TRUE
        )

        dg <- dygraph(dydat,
            group = "nSiteNVar"
        ) %>%
            dyOptions(
                useDataTimezone = TRUE,
                fillGraph = TRUE,
                retainDateWindow = TRUE,
                labelsKMB = TRUE,
                # stackedGraph = TRUE,

                # #if precip panels are separated, use these specifications
                # fillAlpha = 1,
                # colors = raincolors[1],
                # strokeWidth = 3,
                # plotter = hyetograph_js,

                # if not showing points, use these
                drawPoints = FALSE,
                strokeWidth = 1,
                fillAlpha = 0.4,
                colors = selection_color_match(
                    sites_selected = sites,
                    sites_all = displabs[displabs %in% sites],
                    colorvec = pchemcolors
                ),
                drawGapEdgePoints = TRUE

                # #if showing points, use these (needs work)
                # drawPoints = TRUE,
                # strokeWidth = 0.01,
                # pointSize = 1,
                # strokeBorderWidth = 1,
                # colors = 'white',
                # fillAlpha = 0.4,
                # strokeBorderColors = selection_color_match(
                #     sites_selected = sites,
                #     sites_all = displabs,
                #     colorvec = linecolors
                # ),
            ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "P3"
            ) %>%
            dyAxis("y",
                label = "PPT (mm)",
                valueRange = c(
                    ymax + ymax * 0.1,
                    0
                ),
                labelWidth = 16,
                labelHeight = 10,
                pixelsPerLabel = 10,
                rangePad = 10
            )
        # # dyCSS('~/git/macrosheds/portal/www/dygraph.css') %>%
        # dyAnnotation(x = watermark_specs$dt,
        #              text = 'macrosheds.org',
        #              attachAtBottom = TRUE,
        #              # cssClass = 'dygraph-watermark',
        #              tickHeight = 0,
        #              width = 0,
        #              series = watermark_specs$series,
        #              tooltip = '')

        # alternative way to show points in different color? also needs work)
        # dg2 <- dySeries(dg,
        #                name = 'w1',
        #                # group = 'nSiteNVar',
        #                fillGraph = FALSE,
        #                color = 'red',
        #                axis = 'y',
        #                drawPoints = TRUE,
        #                strokeWidth = 0,
        #                pointSize = 1) %>%
        #     dyOptions(stackedGraph = TRUE)
        # dg2
    } else {
        dg <- plot_empty_dygraph(dates,
            plotgroup = "nSiteNVar",
            ylab = "P (mm)",
            px_per_lab = 10
        ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "P3"
            )
    }

    return(dg)
})

output$GRAPH_MAIN3a <- renderDygraph({
    sites <- na.omit(isolate(input$SITES3))
    varA <- isolate(input$VARS3[1]) #
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dmns <- isolate(input$DOMAINS3)
    dates <- isolate(timeSliderChanged()[1:2])

    if (conc_flux == "VWC") {
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if (show_pchem) {
        if (conc_flux == "VWC") {
            raindata <- dataPVWC()
        } else {
            raindata <- dataPchem()
        }
    } else {
        raindata <- tibble()
    }

    print(paste(
        "plot main A:",
        isolate(reactive_vals$basedata$chem$site_code[1]),
        Sys.time()
    ))

    alldata <- pad_widen_join(
        v = varA, #
        sites = sites,
        dates = dates,
        streamdata = streamdata,
        raindata = raindata,
        show_input_concentration = show_pchem
    )

    ylabel <- get_ylab(
        v = varA, #
        conc_flux = conc_flux,
        conc_unit = conc_unit,
        flux_unit = flux_unit
    )

    if (nrow(alldata)) {
        streamsites_rainsites <- c(sites, paste0("PPT_", sites))
        colnms <- str_replace(colnames(alldata), "P_", "PPT_")
        colnames(alldata) <- colnms

        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(
            alldata,
            datetime, any_of(streamsites_rainsites)
        ) # preserve order

        if (show_uncert) {
            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errhi = ~ (errors::drop_errors(.) +
                        errors::errors(.)))
                )) %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errlo = ~ (errors::drop_errors(.) -
                        errors::errors(.)))
                ))

            included_cols <- c(
                included_cols,
                paste0(included_cols, "_errhi"),
                paste0(included_cols, "_errlo")
            )
        }

        dydat <- xts(select(alldata, -datetime),
            order.by = alldata$datetime,
            tzone = lubridate::tz(alldata$datetime[1])
        )

        is_inst <- ifelse(agg == "Instantaneous",
            TRUE,
            FALSE
        )

        dg <- dygraph(dydat, # [,1:2],
            group = "nSiteNVar"
        ) %>%
            dyOptions(
                useDataTimezone = TRUE,
                retainDateWindow = TRUE,

                # if not showing all points, use these specifications.
                drawPoints = FALSE,
                colors = selection_color_match( # pchem NAs required here
                    sites_selected = sites,
                    sites_all = included_cols,
                    sites_missing = sites[!sites %in% colnms],
                    colorvec = linecolors
                ),
                strokeWidth = 2,
                pointSize = 2,
                drawGapEdgePoints = TRUE,
                labelsKMB = TRUE,

                # #if showing points, use these
                # drawPoints = TRUE,
                # strokeWidth = 0.01,
                # pointSize = 1,
                # strokeBorderWidth = 1,
                # colors = 'white',
                # strokeBorderColor = selection_color_match(
                #     sites_selected = sites,
                #     sites_all = included_cols,
                #     colorvec = linecolors
                # ),

                connectSeparatedPoints = is_inst
            ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3a"
            ) %>% #
            dyAxis("y",
                label = ylabel,
                labelWidth = 16,
                labelHeight = 10,
                pixelsPerLabel = 20,
                rangePad = 10
            )

        # # dyCSS('~/git/macrosheds/portal/www/dygraph.css') %>%
        # # dyCSS('www/dygraph.css') %>%
        # dyAnnotation(x = watermark_specs$dt,
        #              text = 'macrosheds.org',
        #              attachAtBottom = TRUE,
        #              # cssClass = 'dygraphDefaultAnnotation',
        #              # cssClass = 'dygraph-watermark',
        #              tickHeight = 0,
        #              width = 0,
        #              series = watermark_specs$series,
        #              tooltip = '')

        rainsites <- paste0("PPT_", sites)

        if (show_pchem && any(rainsites %in% colnms)) {
            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[!rainsites %in% colnms],
                colorvec = raincolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if (show_uncert) {
                rainsites <- lapply(
                    rainsites,
                    function(x) {
                        c(
                            paste0(x, "_errlo"),
                            x,
                            paste0(x, "_errhi")
                        )
                    }
                )
            } else {
                rainsites <- as.list(rainsites)
            }

            for (i in 1:length(rainsites)) {
                dg <- dySeries(dg,
                    name = rainsites[[i]],
                    color = rain_or_pchem_colors[i],
                    axis = "y",
                    drawPoints = FALSE,
                    strokeWidth = 1,
                    pointSize = 2,
                    strokePattern = "dashed"
                )
            }
        }

        if (show_uncert) {
            sites <- sites[sites %in% colnms]

            stream_names <- lapply(
                sites,
                function(x) {
                    c(
                        paste0(x, "_errlo"),
                        x,
                        paste0(x, "_errhi")
                    )
                }
            )

            for (i in 1:length(sites)) {
                dg <- dySeries(dg,
                    name = stream_names[[i]]
                )
            }
        }
    } else {
        dg <- plot_empty_dygraph(dates,
            mainlab = colnames(alldata)[-1],
            maindiv = "main3a", #
            plotgroup = "nSiteNVar",
            ylab = ylabel,
            px_per_lab = 20
        ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3a"
            )
    }

    return(dg)
})

output$GRAPH_QC3a <- renderPlot({
    show_qc <- isolate(input$SHOW_QC3)
    sites <- na.omit(isolate(input$SITES3[1:3]))
    vars_ <- isolate(input$VARS3)
    varA <- vars_[1]
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dates <- isolate(timeSliderChanged()[1:2])
    # input$REFRESH

    dataq <- dataQ()
    datachem <- if (conc_flux == "VWC") dataVWC() else dataChem()

    print(paste("plot QC A", Sys.time()))

    if (length(vars_) == 0 || !show_qc) {
        return()
    }

    alldata <- datachem %>%
        select(c("datetime", "site_code", ends_with(varA))) %>%
        inner_join(dataq,
            by = c("datetime", "site_code")
        )

    n_val_cols <- sum(grepl("^val_", colnames(alldata)))
    if (n_val_cols < 2) {
        return(plot_empty_qc(ylab = varA))
    }

    if (show_uncert) {
        alldata <- alldata %>%
            mutate(across(starts_with("val_"),
                .fns = list(errhi = ~ (errors::drop_errors(.) +
                    errors::errors(.)))
            )) %>%
            mutate(across(starts_with("val_") &
                !ends_with("_errhi"),
            .fns = list(errlo = ~ (errors::drop_errors(.) -
                errors::errors(.)))
            )) %>%
            mutate(across(
                starts_with("val_") &
                    !ends_with(c("_errhi", "_errlo")),
                errors::drop_errors
            ))
    }

    cq <- ggplot(
        alldata,
        aes(
            x = val_discharge,
            y = !!sym(paste0("val_", varA)),
            colour = site_code
        )
    ) +
        geom_point(
            na.rm = TRUE,
            size = 1
        )

    if (show_uncert) {
        cq <- cq +
            geom_linerange(aes(
                ymin = !!sym(paste0("val_", varA, "_errlo")),
                ymax = !!sym(paste0("val_", varA, "_errhi"))
            )) +
            geom_errorbarh(aes(
                xmin = val_discharge_errlo,
                xmax = val_discharge_errhi
            ))
    }

    cq <- cq +
        scale_colour_manual(
            values = linecolors,
            breaks = c(sites)
        ) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste("Q", "vs.", varA)) +
        theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            ),
            plot.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            )
        )

    return(cq)
})

output$GRAPH_MAIN3b <- renderDygraph({
    sites <- na.omit(isolate(input$SITES3))
    varB <- isolate(input$VARS3[2]) #
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    dates <- isolate(timeSliderChanged()[1:2])
    show_uncert <- isolate(input$SHOW_UNCERT3)

    if (conc_flux == "VWC") {
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if (show_pchem) {
        if (conc_flux == "VWC") {
            raindata <- dataPVWC()
        } else {
            raindata <- dataPchem()
        }
    } else {
        raindata <- tibble()
    }

    print("plot main B")

    alldata <- pad_widen_join(
        v = varB,
        sites = sites,
        dates = dates,
        streamdata = streamdata,
        raindata = raindata,
        show_input_concentration = show_pchem
    )

    ylabel <- get_ylab(
        v = varB,
        conc_flux = conc_flux,
        conc_unit = conc_unit,
        flux_unit = flux_unit
    )


    if (nrow(alldata)) {
        streamsites_rainsites <- c(sites, paste0("PPT_", sites))
        colnms <- str_replace(colnames(alldata), "P_", "PPT_")
        colnames(alldata) <- colnms

        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(
            alldata,
            datetime, any_of(streamsites_rainsites)
        ) # preserve order

        if (show_uncert) {
            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errhi = ~ (errors::drop_errors(.) +
                        errors::errors(.)))
                )) %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errlo = ~ (errors::drop_errors(.) -
                        errors::errors(.)))
                ))

            included_cols <- c(
                included_cols,
                paste0(included_cols, "_errhi"),
                paste0(included_cols, "_errlo")
            )
        }

        dydat <- xts(select(alldata, -datetime),
            order.by = alldata$datetime,
            tzone = lubridate::tz(alldata$datetime[1])
        )

        is_inst <- ifelse(agg == "Instantaneous",
            TRUE,
            FALSE
        )

        dg <- dygraph(dydat,
            group = "nSiteNVar"
        ) %>%
            dyOptions(
                useDataTimezone = TRUE,
                retainDateWindow = TRUE,
                drawPoints = FALSE,
                colors = selection_color_match( # pchem NAs required here
                    sites_selected = sites,
                    sites_all = included_cols,
                    sites_missing = sites[!sites %in% colnms],
                    colorvec = linecolors
                ),
                strokeWidth = 2,
                pointSize = 2,
                drawGapEdgePoints = TRUE,
                labelsKMB = TRUE,
                connectSeparatedPoints = is_inst
            ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3b"
            ) %>%
            dyAxis("y",
                label = ylabel,
                labelWidth = 16,
                labelHeight = 10,
                pixelsPerLabel = 20,
                rangePad = 10
            )

        rainsites <- paste0("PPT_", sites)

        if (show_pchem && any(rainsites %in% colnms)) {
            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[!rainsites %in% colnms],
                colorvec = raincolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if (show_uncert) {
                rainsites <- lapply(
                    rainsites,
                    function(x) {
                        c(
                            paste0(x, "_errlo"),
                            x,
                            paste0(x, "_errhi")
                        )
                    }
                )
            } else {
                rainsites <- as.list(rainsites)
            }

            for (i in 1:length(rainsites)) {
                dg <- dySeries(dg,
                    name = rainsites[[i]],
                    color = rain_or_pchem_colors[i],
                    axis = "y",
                    drawPoints = FALSE,
                    strokeWidth = 1,
                    pointSize = 2,
                    strokePattern = "dashed"
                )
            }
        }

        if (show_uncert) {
            sites <- sites[sites %in% colnms]

            stream_names <- lapply(
                sites,
                function(x) {
                    c(
                        paste0(x, "_errlo"),
                        x,
                        paste0(x, "_errhi")
                    )
                }
            )

            for (i in 1:length(sites)) {
                dg <- dySeries(dg,
                    name = stream_names[[i]]
                )
            }
        }
    } else {
        dg <- plot_empty_dygraph(dates,
            mainlab = colnames(alldata)[-1],
            maindiv = "main3b",
            plotgroup = "nSiteNVar",
            ylab = ylabel,
            px_per_lab = 20
        ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3b"
            )
    }

    return(dg)
})

output$GRAPH_QC3b <- renderPlot({
    show_qc <- isolate(input$SHOW_QC3)
    sites <- na.omit(isolate(input$SITES3[1:3]))
    vars_ <- isolate(input$VARS3)
    varB <- vars_[2]
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dates <- isolate(timeSliderChanged()[1:2])
    # input$REFRESH

    dataq <- dataQ()
    datachem <- if (conc_flux == "VWC") dataVWC() else dataChem()

    print("plot QC B")

    if (length(vars_) <= 1 || !show_qc) {
        return()
    }

    alldata <- datachem %>%
        select(c("datetime", "site_code", ends_with(varB))) %>%
        inner_join(dataq,
            by = c("datetime", "site_code")
        )

    n_val_cols <- sum(grepl("^val_", colnames(alldata)))
    if (n_val_cols < 2) {
        return(plot_empty_qc(ylab = varB))
    }

    if (show_uncert) {
        alldata <- alldata %>%
            mutate(across(starts_with("val_"),
                .fns = list(errhi = ~ (errors::drop_errors(.) +
                    errors::errors(.)))
            )) %>%
            mutate(across(starts_with("val_") &
                !ends_with("_errhi"),
            .fns = list(errlo = ~ (errors::drop_errors(.) -
                errors::errors(.)))
            )) %>%
            mutate(across(
                starts_with("val_") &
                    !ends_with(c("_errhi", "_errlo")),
                errors::drop_errors
            ))
    }

    cq <- ggplot(
        alldata,
        aes(
            x = val_discharge,
            y = !!sym(paste0("val_", varB)),
            colour = site_code
        )
    ) +
        geom_point(
            na.rm = TRUE,
            size = 1
        )

    if (show_uncert) {
        cq <- cq +
            geom_linerange(aes(
                ymin = !!sym(paste0("val_", varB, "_errlo")),
                ymax = !!sym(paste0("val_", varB, "_errhi"))
            )) +
            geom_errorbarh(aes(
                xmin = val_discharge_errlo,
                xmax = val_discharge_errhi
            ))
    }

    cq <- cq +
        scale_colour_manual(
            values = linecolors,
            breaks = c(sites)
        ) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste("Q", "vs.", varB)) +
        theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            ),
            plot.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            )
        )

    return(cq)
})

output$GRAPH_MAIN3c <- renderDygraph({
    sites <- na.omit(isolate(input$SITES3))
    varC <- isolate(input$VARS3[3]) #
    conc_flux <- isolate(input$CONC_FLUX3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    conc_unit <- isolate(input$CONC_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dates <- isolate(timeSliderChanged()[1:2])

    if (conc_flux == "VWC") {
        streamdata <- dataVWC()
    } else {
        streamdata <- dataChem()
    }

    if (show_pchem) {
        if (conc_flux == "VWC") {
            raindata <- dataPVWC()
        } else {
            raindata <- dataPchem()
        }
    } else {
        raindata <- tibble()
    }

    print("plot main C")

    alldata <- pad_widen_join(
        v = varC,
        sites = sites,
        dates = dates,
        streamdata = streamdata,
        raindata = raindata,
        show_input_concentration = show_pchem
    )

    ylabel <- get_ylab(
        v = varC,
        conc_flux = conc_flux,
        conc_unit = conc_unit,
        flux_unit = flux_unit
    )

    if (nrow(alldata)) {
        streamsites_rainsites <- c(sites, paste0("PPT_", sites))
        colnms <- str_replace(colnames(alldata), "P_", "PPT_")
        colnames(alldata) <- colnms

        included_cols <- streamsites_rainsites[streamsites_rainsites %in% colnms]
        alldata <- select(
            alldata,
            datetime, any_of(streamsites_rainsites)
        ) # preserve order

        if (show_uncert) {
            alldata <- alldata %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errhi = ~ (errors::drop_errors(.) +
                        errors::errors(.)))
                )) %>%
                mutate(across(any_of(included_cols),
                    .fns = list(errlo = ~ (errors::drop_errors(.) -
                        errors::errors(.)))
                ))

            included_cols <- c(
                included_cols,
                paste0(included_cols, "_errhi"),
                paste0(included_cols, "_errlo")
            )
        }

        dydat <- xts(select(alldata, -datetime),
            order.by = alldata$datetime,
            tzone = lubridate::tz(alldata$datetime[1])
        )

        is_inst <- ifelse(agg == "Instantaneous",
            TRUE,
            FALSE
        )

        dg <- dygraph(dydat,
            group = "nSiteNVar"
        ) %>%
            dyOptions(
                useDataTimezone = TRUE,
                retainDateWindow = TRUE,
                drawPoints = FALSE,
                colors = selection_color_match( # pchem NAs required here
                    sites_selected = sites,
                    sites_all = included_cols,
                    sites_missing = sites[!sites %in% colnms],
                    colorvec = linecolors
                ),
                strokeWidth = 2,
                pointSize = 2,
                drawGapEdgePoints = TRUE,
                labelsKMB = TRUE,
                connectSeparatedPoints = is_inst
            ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3c"
            ) %>%
            dyAxis("y",
                label = ylabel,
                labelWidth = 16,
                labelHeight = 10,
                pixelsPerLabel = 20,
                rangePad = 10
            )

        rainsites <- paste0("PPT_", sites)

        if (show_pchem && any(rainsites %in% colnms)) {
            rain_or_pchem_colors <- selection_color_match(
                sites_selected = rainsites,
                sites_all = rainsites,
                sites_missing = rainsites[!rainsites %in% colnms],
                colorvec = raincolors
            )

            rainsites <- rainsites[rainsites %in% colnms]

            if (show_uncert) {
                rainsites <- lapply(
                    rainsites,
                    function(x) {
                        c(
                            paste0(x, "_errlo"),
                            x,
                            paste0(x, "_errhi")
                        )
                    }
                )
            } else {
                rainsites <- as.list(rainsites)
            }

            for (i in 1:length(rainsites)) {
                dg <- dySeries(dg,
                    name = rainsites[[i]],
                    color = rain_or_pchem_colors[i],
                    axis = "y",
                    drawPoints = FALSE,
                    strokeWidth = 1,
                    pointSize = 2,
                    strokePattern = "dashed"
                )
            }
        }

        if (show_uncert) {
            sites <- sites[sites %in% colnms]

            stream_names <- lapply(
                sites,
                function(x) {
                    c(
                        paste0(x, "_errlo"),
                        x,
                        paste0(x, "_errhi")
                    )
                }
            )

            for (i in 1:length(sites)) {
                dg <- dySeries(dg,
                    name = stream_names[[i]]
                )
            }
        }
    } else {
        dg <- plot_empty_dygraph(dates,
            mainlab = colnames(alldata)[-1],
            maindiv = "main3c",
            plotgroup = "nSiteNVar",
            ylab = ylabel,
            px_per_lab = 20
        ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "main3c"
            )
    }

    return(dg)
})

output$GRAPH_QC3c <- renderPlot({
    show_qc <- isolate(input$SHOW_QC3)
    sites <- na.omit(isolate(input$SITES3[1:3]))
    vars_ <- isolate(input$VARS3)
    varC <- vars_[3]
    conc_flux <- isolate(input$CONC_FLUX3)
    conc_unit <- isolate(input$CONC_UNIT3)
    flux_unit <- isolate(input$FLUX_UNIT3)
    show_pchem <- isolate(input$SHOW_PCHEM3)
    agg <- isolate(input$AGG3)
    show_uncert <- isolate(input$SHOW_UNCERT3)
    dates <- isolate(timeSliderChanged()[1:2])
    # input$REFRESH

    dataq <- dataQ()
    datachem <- if (conc_flux == "VWC") dataVWC() else dataChem()

    print(paste("plot QC C", Sys.time()))

    if (length(vars_) <= 2 || !show_qc) {
        return()
    }

    alldata <- datachem %>%
        select(c("datetime", "site_code", ends_with(varC))) %>%
        inner_join(dataq,
            by = c("datetime", "site_code")
        )

    n_val_cols <- sum(grepl("^val_", colnames(alldata)))
    if (n_val_cols < 2) {
        return(plot_empty_qc(ylab = varC))
    }

    if (show_uncert) {
        alldata <- alldata %>%
            mutate(across(starts_with("val_"),
                .fns = list(errhi = ~ (errors::drop_errors(.) +
                    errors::errors(.)))
            )) %>%
            mutate(across(starts_with("val_") &
                !ends_with("_errhi"),
            .fns = list(errlo = ~ (errors::drop_errors(.) -
                errors::errors(.)))
            )) %>%
            mutate(across(
                starts_with("val_") &
                    !ends_with(c("_errhi", "_errlo")),
                errors::drop_errors
            ))
    }

    cq <- ggplot(
        alldata,
        aes(
            x = val_discharge,
            y = !!sym(paste0("val_", varC)),
            colour = site_code
        )
    ) +
        geom_point(
            na.rm = TRUE,
            size = 1
        )

    if (show_uncert) {
        cq <- cq +
            geom_linerange(aes(
                ymin = !!sym(paste0("val_", varC, "_errlo")),
                ymax = !!sym(paste0("val_", varC, "_errhi"))
            )) +
            geom_errorbarh(aes(
                xmin = val_discharge_errlo,
                xmax = val_discharge_errhi
            ))
    }

    cq <- cq +
        scale_colour_manual(
            values = linecolors,
            breaks = c(sites)
        ) +
        ggthemes::theme_few() +
        scale_y_continuous(position = "right") +
        ylab(paste("Q", "vs.", varC)) +
        theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            ),
            plot.background = element_rect(
                fill = "#f5f5f5",
                color = "#f5f5f5"
            )
        )

    return(cq)
})

output$GRAPH_Q3 <- renderDygraph({
    dataq <- dataQ()
    dates <- isolate(timeSliderChanged()[1:2])
    sites <- isolate(input$SITES3)

    print("plot Q")

    tryCatch(
        {
            dataq <- dataq %>%
                dplyr::rename_with(~ gsub("_discharge", "", .x)) %>%
                tidyr::pivot_wider(
                    names_from = site_code,
                    values_from = c(
                        "val", "ms_status",
                        "ms_interp"
                    )
                ) %>%
                dplyr::rename_with(~ gsub("val_", "", .x))
        },
        error = function(e) NULL
    )

    if (length(dataq) > 0 && nrow(dataq)) {
        dataq <- select(
            dataq,
            datetime, any_of(sites)
        ) # preserve order

        colnms <- colnames(dataq)


        displabs <- colnms[!colnms == "datetime"]

        dydat <- xts(dataq[, displabs],
            order.by = dataq$datetime,
            tzone = lubridate::tz(dataq$datetime[1])
        )

        dimnames(dydat) <- list(NULL, displabs)

        dg <- dygraph(dydat,
            group = "nSiteNVar"
        ) %>%
            dyOptions(
                useDataTimezone = TRUE,
                drawPoints = FALSE,
                fillGraph = TRUE,
                strokeWidth = 1,
                fillAlpha = 0.4,
                retainDateWindow = TRUE,
                colors = selection_color_match(
                    sites_selected = sites,
                    sites_all = displabs[displabs %in% sites],
                    colorvec = linecolors
                ),
                labelsKMB = TRUE,
                drawGapEdgePoints = TRUE
            ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "Q3"
            ) %>%
            dyAxis("y",
                label = "Q (L/s)",
                labelWidth = 16,
                labelHeight = 10,
                pixelsPerLabel = 10,
                rangePad = 10
            )
    } else {
        dg <- plot_empty_dygraph(dates,
            plotgroup = "nSiteNVar",
            ylab = "Q (L/s)",
            px_per_lab = 10
        ) %>%
            dyLegend(
                show = "always",
                labelsSeparateLines = FALSE,
                labelsDiv = "Q3"
            )
    }
    return(dg)
})

## other ####

# allows ui to control hide/show of plot facets
output$n_plots3 <- reactive({
    timeSliderChanged()

    n_plots <- length(isolate(input$VARS3))

    return(n_plots)
})

outputOptions(output,
    name = "n_plots3",
    suspendWhenHidden = FALSE,
    priority = 100
)


# allows ui to control hide/show of QC/QF plots
output$SHOW_QC_GEN3 <- reactive({
    timeSliderChanged()

    show_QC <- isolate(input$SHOW_QC3)

    return(show_QC)
})

outputOptions(output,
    name = "SHOW_QC_GEN3",
    suspendWhenHidden = FALSE,
    priority = 100
)
