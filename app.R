# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(ggplot2)
    library(dplyr)
    library(DT)
    library(faux)
    library(cowplot)
})

# setup ----


# functions ----
source("scripts/func.R") # helper functions

# user interface ----

## UI ----
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Within-subject t-test forensics",
        titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "custom.js")
        ),
        column(width = 4,
               ## data paramaters ----
               box(id = "input_box",
                   title = "Reported Data Parameters", width = 12, solidHeader = TRUE,
                   p("Check the possible p-values and t-values for the given N, means and SDs for correlations between -1 and +1."),
                   numericInput("n", "N", 20, min = 1),
                   numericInput("m1", "Mean 1", 0.2),
                   numericInput("sd1", "SD 1", 1, min = 0.0001),
                   numericInput("m2", "Mean 2", 0),
                   numericInput("sd2", "SD 2", 1, min = 0.0001)
               ),

               ## reported test parameters ----
               box(id = "test_box",
                   title = "Reported Test Parameters", width = 12, solidHeader = TRUE,
                   p("Given a t-value or p-value, get the possible correlations for one- and two-tailed tests."),
                   numericInput("reported_t", "t-value", NULL),
                   numericInput("reported_p", "p-value", NULL, min = 0, max = 1)
               ),
               tags$a(href="https://github.com/debruine/within", "Code & Citation")
        ),
        column(width = 8,
               box(title = "r-values", width = 6, solidHeader = TRUE,
                   plotOutput("r_plot")
               ),
               box(title = "t-values", width = 6, solidHeader = TRUE,
                   plotOutput("t_plot")
               ),
               DTOutput("r_table")
        )
    )
)


# server ----
server <- function(input, output, session) {
    ## r_table ----
    r_table <- reactive({
        check_values(
            n = input$n,
            m1 = input$m1,
            sd1 = input$sd1,
            m2 = input$m2,
            sd2 = input$sd2
        )
    })

    ## r_plot ----
    output$r_plot <- renderPlot({
        req(r_table())
        r_plot(r_table(), input$reported_p)
    })

    ## t_plot ----
    output$t_plot <- renderPlot({
        req(r_table())
        t_plot(r_table(), input$reported_t)
    })

    less_r <- reactive({
        solve_r(param = "p",
                  reported = input$reported_p,
                  n = input$n,
                  m1 = input$m1,
                  m2 = input$m2,
                  sd1 = input$sd1,
                  sd2 = input$sd2,
                  alternative = "less")
    })

    greater_r <- reactive({
        solve_r(param = "p",
                reported = input$reported_p,
                n = input$n,
                m1 = input$m1,
                m2 = input$m2,
                sd1 = input$sd1,
                sd2 = input$sd2,
                alternative = "greater")
    })

    two.sided_r <- reactive({
        solve_r(param = "p",
                reported = input$reported_p,
                n = input$n,
                m1 = input$m1,
                m2 = input$m2,
                sd1 = input$sd1,
                sd2 = input$sd2,
                alternative = "two.sided")
    })

    ## p_table ----
    p_table <- reactive({
        if(is.na(input$reported_p)) return(NULL)
        message("making p_table")

        r <- list(less_r(), greater_r(), two.sided_r())
        alt <- c("less", "greater", "two.sided")
        nosol <- sapply(r, is.null)

        if (length(nosol) == 0) return(NULL)

        tidyr::crossing(
            n = input$n,
            m1 = input$m1,
            m2 = input$m2,
            sd1 = input$sd1,
            sd2 = input$sd2,
            r = unlist(r)
        ) %>%
            mutate(alternative = alt[!nosol]) %>%
            dplyr::bind_cols(purrr::pmap_df(., within_t)) %>%
            select(r, diff_sd, t, p, alternative) %>%
            mutate_if(is.numeric, round, 3) %>%
            print()
    })

    ## t_table ----
    t_table <-  reactive({
        if (is.na(input$reported_t)) return(NULL)

        message("making t_table")

        r <- solve_r(param = "t",
                reported = input$reported_t,
                n = input$n,
                m1 = input$m1,
                m2 = input$m2,
                sd1 = input$sd1,
                sd2 = input$sd2,
                alternative = "two.sided")

        if (is.null(r)) return(NULL)

        alt <- c("less", "greater", "two.sided")

        tidyr::crossing(
            n = input$n,
            m1 = input$m1,
            m2 = input$m2,
            sd1 = input$sd1,
            sd2 = input$sd2,
            r = r,
            alternative = alt
        ) %>%
            # calculate p and t-values
            dplyr::bind_cols(purrr::pmap_df(., within_t)) %>%
            select(r, diff_sd, t, p, alternative) %>%
            mutate_if(is.numeric, round, 3) %>%
            print()
    })



    ## r_table ----
    output$r_table <- renderDT( {
        message("r_table")
        bind_rows(p_table(), t_table()) %>%
            print()
    }, options = list(dom = 't'))
}

shinyApp(ui, server)
