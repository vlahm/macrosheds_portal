landing_page <- quote({
    showModal(
        modalDialog(
            title = NULL,
            footer = NULL,
            easyClose = TRUE,
            id = "landing",
            size = "l",
            fluidRow(
                class = "text-center",
                column(
                    12,
                    img(
                        src = "new_logo_full.png",
                        style = "height: 100px; width: 300px"
                    ),
                    br(),
                    br(),
                    br()
                )
            ),
            fluidRow(
                class = "text-center",
                column(
                    4,
                    img(
                        src = "water-solid-193d85.svg",
                        style = "height: 56px; width: 56px"
                    ),
                    h1(textOutput("NSTREAMS")),
                    p("STREAMS")
                ),
                column(
                    4,
                    icon("map-marker", class = "fa-4x"),
                    h1(textOutput("NSITES")),
                    p("SITES")
                ),
                column(
                    4,
                    icon("chart-bar", class = "fa-4x"),
                    h1(textOutput("NOBS")),
                    p("OBSERVATIONS")
                )
            ),
            fluidRow(
                class = "text-center",
                column(8,
                    offset = 2,
                    br(),
                    br(),
                    p(HTML(paste(
                        "This is MacroSheds portal v1.0 <span style='color: red'><strong>beta</strong></span>,",
                        "so all major components are in place, but there might still be some feature",
                        "bugs or data errors. Please <a href='mailto: mail@macrosheds.org'><u>let us know",
                        "</u></a> if you find any!"
                    ))),
                    # p(paste("We've hit the size limit for our hosting service, so two domains (Niwot Ridge and McMurdo) are not available for download or site inspection. You can still see summary data for these domains on the Site Comparison tab, and we'll be getting a dedicated server soon.")),
                    p(paste(
                        "Our documentation in a future release will include the date and time at which we retrieved each dataset",
                        "that comprises MacroSheds. For this release, every dataset was retrieved from its web source between",
                        "2021-07-01 and 2021-08-17."
                    ))
                )
            ),
            fluidRow(
                class = "text-center",
                column(
                    12,
                    br(),
                    br(),
                    br(),
                    br(),
                    div(
                        class = "loading-container",
                        div(
                            class = "loading",
                            div(class = "loading-letter", "L"),
                            div(class = "loading-letter", "O"),
                            div(class = "loading-letter", "A"),
                            div(class = "loading-letter", "D"),
                            div(class = "loading-letter", "I"),
                            div(class = "loading-letter", "N"),
                            div(class = "loading-letter", "G")
                        ),
                    )
                ),
                column(6,
                    class = "text-right",
                    actionButton("DISMISS_MODAL",
                        label = "Continue",
                        style = "display: none"
                    )
                ),
                column(6,
                    class = "text-left",
                    actionButton("TAKE_TOUR",
                        label = "Take a site tour",
                        class = "btn btn-info",
                        style = "display: none"
                    )
                )
            )
        )
    )
})
