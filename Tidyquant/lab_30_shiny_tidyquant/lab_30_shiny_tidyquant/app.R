library(shiny)
library(htmlwidgets)
library(sortable)
library(tidyverse)
library(tidyquant)
library(shinythemes)
library(rlang)
library(shinyWidgets)
library(rhandsontable)
library(DT)
library(plotly)


# Setup Data & Params ----
stock_index    <- tq_index("SP500")
stock_symbols  <- stock_index %>% pull(symbol)
initial_stocks <- c("FB", "AMZN", "NFLX", "GOOG",
                    "ADBE", "MSFT", "NVDA", "CRM")
modification_variables <- "date"

function_choices_modify_variables <- c("DAY()", "WEEK()", "MONTH()", "QUARTER()", "YEAR()",
                                       "FLOOR_DAY()", "FLOOR_WEEK()", "FLOOR_MONTH()", "FLOOR_QUARTER()", "FLOOR_YEAR()")

function_choices_summarize_variable <- c("SUM", "COUNT", "AVERAGE", "MIN", "MAX", 
                                         "FIRST", "LAST", "CHANGE_FIRSTLAST", "PCT_CHANGE_FIRSTLAST")

# FUNCTIONS ----

colnames_to_tags <- function(df){
    
    names(df) %>%
        map(.f = function(nm) {
            tag(
                "p",
                list(
                    class = str_c(class(df[, nm][[1]]) %>% str_to_lower(), 
                                  " item-text"),
                    tags$span(class = "glyphicon glyphicon-move"),
                    tags$strong(nm)
                )
            )
        })
}

collapse_pivot_variables <- function(x) {
    x %>%
        map(.f = function(x) {
            if (str_detect(x, "\\(")) str_c("~ ", x)
            else x
        }) %>%
        str_c(collapse = ",") %>%
        str_c("c(", ., ")")
}



# 1.0 UI ----
ui <- navbarPage(
    title = "Financial App with tidyquant",
    theme = shinytheme("paper"),
    collapsible = TRUE,
    
    tabPanel(
        title = "Stock Dashboard",
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        fluidRow(
            class = "col-sm-12",
            div(
                class = "panel-heading",
                img(src = "tidyquant-logo.png", class = "navbar-logo pull-left", style="max-height:100px;margin-right:20px;"),
                h1(class = "text-primary", "Stock Dashboard in R", tags$small("with tidyquant"))
            )  
        ),
        fluidRow(
            class = "col-sm-12",
            fluidRow(
                class = "panel-body",
                column(
                    width = 4,
                    class = "",
                    
                    # 1.1 Stock Selections ----
                    div(
                        class = "panel panel-default",
                        div(
                            class = "panel-body",
                            column(
                                width = 8,
                                shiny::selectizeInput(
                                    inputId  = "stock_selections", 
                                    label    = NULL, 
                                    choices  = stock_symbols %>% sort(), 
                                    multiple = TRUE, 
                                    selected = initial_stocks, 
                                    width    = "100%"
                                )
                            ),
                            column(
                                width = 4,
                                actionButton("get_data", "Run")
                            ),
                            column(
                                width = 12,
                                materialSwitch(inputId = "heatmap", label = "Heatmap", status = "danger")
                            )
                        )
                    ),
                    
                    
                    # 1.2 Variables ----
                    div(
                        class = "panel panel-default",
                        div(
                            class = "panel-heading", 
                            tags$span(class = "glyphicon glyphicon-list"),
                            "Variables",
                            hr(),
                            
                            # 1.3 Modify Variables Button ----
                            fluidRow(
                                column(width=12, "Date Groups")
                            ),
                            fluidRow(
                                column(
                                    width = 4,
                                    shiny::selectizeInput(
                                        "modify_variable",
                                        label = NULL,
                                        choices = modification_variables
                                    )
                                ),
                                column(
                                    width = 4,
                                    shiny::selectizeInput(
                                        "modify_function", 
                                        label = NULL, 
                                        choices = function_choices_modify_variables)
                                ),
                                column(
                                    width = 4,
                                    shiny::actionButton(
                                        "modify_button",
                                        "Add"
                                    )
                                )
                            ),
                        ),
                        # 1.4 Variables Output ----
                        uiOutput("variables")
                    ),
                    fluidRow(
                        column(
                            width = 6,
                            # 1.5 Analyze Rows ----
                            tags$div(
                                class = "panel panel-default",
                                tags$div(
                                    class = "panel-heading",
                                    tags$span(class = "glyphicon glyphicon-option-vertical"),
                                    "Rows"
                                ),
                                tags$div(
                                    class = "panel-body",
                                    id = "sort2"
                                )
                            )
                        ),
                        column(
                            width = 6,
                            # 1.6 Analyze Columns ----
                            tags$div(
                                class = "panel panel-default",
                                tags$div(
                                    class = "panel-heading",
                                    tags$span(class = "glyphicon glyphicon-option-horizontal"),
                                    "Columns"
                                ),
                                tags$div(
                                    class = "panel-body",
                                    id = "sort3"
                                )
                            )
                        ),
                        column(
                            width = 12,
                            # 1.7 Analyze Values ----
                            div(
                                class = "panel panel-default",
                                div(
                                    class = "panel-heading", style = "min-height:60px;",
                                    
                                    tags$span(class = "glyphicon glyphicon-stats"),
                                    "Values",
                                    tags$span(
                                        class = "pull-right", 
                                        selectizeInput("summarize_fun", label = NULL, width = "200px", choices = function_choices_summarize_variable)
                                    )
                                ),
                                tags$div(
                                    class = "panel-body",
                                    id = "sort4"
                                )
                            )
                        )
                    )
                ),
                # 1.8 Pivot Table Output ----
                column(
                    width = 8,
                    tabsetPanel(
                        tabPanel(
                            title = "Stock Charts",
                            uiOutput(outputId = "stock_time_series")
                        ),
                        tabPanel(
                            title = "Raw Data",
                            dataTableOutput("raw_data")
                        ),
                        tabPanel(
                            title = "Pivot Table",
                            rHandsontableOutput("pivot_tbl")
                            # dataTableOutput("pivot_tbl")
                        )
                    )
                )
            )
        )
    )
)

# 2.0 SERVER ----
server <- function(input, output) {
    
    # 2.1 Collect Data ----
    rv <- reactiveValues()
    
    observeEvent(input$get_data, {
        rv$new_data <- tq_get(input$stock_selections)
    }, ignoreNULL = FALSE)
    
    # 2.1.1 Raw Data ----
    output$raw_data <- renderDataTable({
        req(rv$new_data)
        rv$new_data %>% 
            datatable(
                extensions = 'Buttons', 
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel')
                )
            )
    })
    
    # 2.2 New Variables ----
    observeEvent(input$modify_button, {
        
        mutate_var <- input$modify_variable
        mutate_fun <- input$modify_function %>% str_remove_all("\\(|\\)")
        
        function_text <- str_c(mutate_fun, "(", mutate_var, ")")
        function_expr <- sym(function_text)
        
        rv$new_data <- rv$new_data %>%
            mutate(
                !!function_expr := !! rlang::parse_quo(rlang::quo_name(function_expr), env = rlang::caller_env())
            )
    })
    
    # 2.3 Stock Visualization ----
    output$stock_time_series <- renderUI({
        req(rv$new_data)
        
        rv$new_data %>%
            group_by(symbol) %>%
            summarise_by_time(
                .date_var = date,
                .by       = "month",
                adjusted  = LAST(adjusted)
            ) %>%
            group_by(symbol) %>%
            group_split() %>%
            
            map(.f = function(.data) {
                g <- .data %>%
                    ggplot(aes(date, adjusted)) +
                    geom_line(color = palette_light()[[1]]) +
                    scale_color_tq() +
                    theme_tq() +
                    labs(title = "", x = "", y = "")
                
                column(
                    width = 3,
                    style = "",
                    div(
                        class = "panel",
                        div(
                            class = "panel-heading",
                            unique(.data$symbol) %>% VLOOKUP(stock_index, symbol, company) %>% str_to_title()
                        ),
                        div(
                            class = "panel-body",
                            ggplotly(g, height = 300) %>% layout(showlegend = FALSE)
                        )
                    )
                )
            }) %>% 
            tagList()
       
    })
    
    # 2.4 Sortable List ----
    output$variables <- renderUI({
        
        variables_names <- setdiff(names(rv$new_data), c(x(), y(), z()) )
        
        tagList(
            tags$div(
                class = "panel-body",
                id = "sort1",
                # colnames_to_tags(rv$new_data)
                colnames_to_tags(rv$new_data[,variables_names])
            ),
            sortable_js(
                "sort1",
                options = sortable_options(
                    group = list(
                        name = "sortGroup1",
                        put  = TRUE
                    ),
                    swap = TRUE,
                    swapClass = "sortable-swap-highlight",
                    sort = TRUE,
                    onSort = sortable_js_capture_input("sort_vars")
                    
                )
            ),
            sortable_js(
                "sort2",
                options = sortable_options(
                    group = list(
                        group = "sortGroup1",
                        put   = TRUE, # htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
                        pull  = TRUE
                    ),
                    swap = TRUE,
                    swapClass = "sortable-swap-highlight",
                    onSort = sortable_js_capture_input("sort_x")
                )
            ),
            sortable_js(
                "sort3",
                options = sortable_options(
                    group = list(
                        group = "sortGroup1",
                        put   = TRUE, # htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
                        pull  = TRUE
                    ),
                    swap = TRUE,
                    swapClass = "sortable-swap-highlight",
                    onSort = sortable_js_capture_input("sort_y")
                )
            ),
            sortable_js(
                "sort4",
                options = sortable_options(
                    group = list(
                        group = "sortGroup1",
                        put   = TRUE, # htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
                        pull  = TRUE
                    ),
                    swap = TRUE,
                    swapClass = "sortable-swap-highlight",
                    onSort = sortable_js_capture_input("sort_z")
                )
            )
        )
    })
    
    # 2.5 Sort Buckets ----
    x <- reactive({
        x <- input$sort_x
        if (is.character(x)) x %>% trimws()
    })
    
    y <- reactive({
        input$sort_y %>% trimws()
    })
    
    z <- reactive({
        input$sort_z
    })
    
    sum_fun <- reactive({
        input$summarize_fun
    })
    
    
    # 2.6 Hands-On Table Output ----
    output$pivot_tbl <- renderRHandsontable({
        
        validate(
            need(x(), "Drag a variable to Rows"),
            need(y(), "Drag a variable to Columns"),
            need(z(), "Drag a variable to Values")
        )
        
        
        # rv$new_data[, c(x(), y(), z()) ] %>% rhandsontable()
        
        x_collapsed <- x() %>% collapse_pivot_variables()
        y_collapsed <- y() %>% collapse_pivot_variables()
        z_collapsed <- str_glue("~ {sum_fun()}({z()})") %>% as.character()
        
        pivot_tbl <- rv$new_data %>%
            pivot_table(
                .rows    = !! x_collapsed,
                .columns = !! y_collapsed,
                .values  = !! z_collapsed
            )
        
        # * Testing ----
        # print(
        #     list(
        #         selections = list(
        #             x = x(), y = y(), z = z()
        #         ),
        #         collapsed = list(
        #             x_collapsed = x_collapsed, y_collapsed = y_collapsed, z_collapsed = z_collapsed)
        #         ,
        #         data        = rv$new_data,
        #         pivot_tbl   = pivot_tbl
        #     )
        # )
        
        

        hands_on_table <- pivot_tbl %>%
            set_names(str_trim(names(.))) %>%
            rhandsontable()

        if (input$heatmap) {
            # not_row_grouping <- !names(pivot_tbl) %in% x()
            # seq_num_cols     <- (1:ncol(pivot_tbl))[not_row_grouping]

            # hands_on_table <- hands_on_table %>%
            #     rhandsontable::hot_heatmap(cols = seq_num_cols)
            
            hands_on_table <- hands_on_table %>%
                rhandsontable::hot_heatmap()
        }

        return(hands_on_table)
        
        # datatable(pivot_tbl)
        
        
    })
    
}
shinyApp(ui, server)
