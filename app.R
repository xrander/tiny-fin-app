
# Load packages -----------------------------------------------------------
pacman::p_load(shiny, bslib, gt)
source("script/data-processing.R")


# App User Interface ------------------------------------------------------

ui <- page_sidebar(
    theme = bs_theme(
        version = "5",
        preset = "litera",
        bg = "#ffffff",
        fg = "#202124", 
        base_font = c("Inter", "sans-serif"),
        code_font = c("Courier", "monospace"),
        primary = "#ffffff",
        heading_font = "Plus Jakarta Sans"
    ),
    # App title
    title = "Distribution of Power Prices",
    
    # Side panel
    sidebar = sidebar(
        title = "Control",
        
        # Drop down 
        selectInput(
            inputId = "exchange",
            label = "Select Exchange:",
            choices = unique(tbl_processed$exchange)
        ),
        selectInput(
            inputId = "product",
            label = "Select Product:",
            choices = unique(tbl_processed$symbol)
        ),
        selectInput(
            inputId = "type",
            label = "Select Type:",
            choices = unique(tbl_processed$type)
        ),
        dateRangeInput(
            inputId = "date_range",
            label = "Select Date Range:",
            start = min(tbl_processed$date),
            end = max(tbl_processed$date)
        )
    ),
    
    # Main content
    card(
        plotlyOutput("plot"),
        height = 500, 
        full_screen = TRUE
    ),
    card(
        gt_output(outputId = "summary_table"),
        hr(),
        gt_output(outputId = "table_2")
    )
    
)

server <- function(input, output, session) {
    
    # Reactive component to filter data
    filtered_data <- reactive({
        tbl_processed |> 
            filter(
                between(date, min(input$date_range), max(input$date_range)),
                exchange == input$exchange,
                symbol == input$product,
                type == input$type
            )
    })
    
    # Reactive component to create a new table
    new_table <- reactive({
        tbl_long |> 
            mutate(
                across(where(is.double), \(x) round(x, 1))
            ) |> 
            filter(
                Exchange == input$exchange,
                symbol == input$product,
                Date == max(input$date_range)
            ) |> 
            rename(
                `3-day Return` = ret,
                `Volatility (in %)` = Volatility,
                `1 %` = l1pct,
                `5 %` = l5pct,
                `95 %` = l95pct,
                `99 %` = l99pct
            ) |> 
            select(
                `Close Price`, `Volatility (in %)`, `3-day Return`, `1 %`:`99 %`
            ) |> 
            mutate(
                `Volatility (in %)` = scales::percent(`Volatility (in %)`, scale = 100, accuracy = .01)
            )
    })
    
    # Render the summary table with gt
    output$summary_table <- render_gt({
        data <- filtered_data()
        
        # Summary calculation based on input$type
        summary_data <- if (input$type == "Close Price") {
            data |> 
                summarize(
                    start_date = min(date),
                    end_date = max(date),
                    min_value = round(min(types_value), 1),
                    mean_value = round(mean(types_value), 2),
                    max_value = round(max(types_value), 1)
                )
        } else {
            data |> 
                summarize(
                    start_date = min(date),
                    end_date = max(date),
                    min_value = min(types_value),
                    mean_value = mean(types_value),
                    max_value = max(types_value)
                ) |> 
                mutate(
                    min_value = scales::percent(min_value, scale = 100, accuracy = .01),
                    mean_value = scales::percent(mean_value, scale = 100, accuracy = .01),
                    max_value = scales::percent(max_value, scale = 100, accuracy = .01)
                )
        }
        
        # Generate gt table
        summary_data |> 
            gt() |> 
            cols_label(
                start_date = md("**Start Date**"),
                end_date = md("**End Date**"),
                min_value = md("**Min Value**"),
                mean_value = md("**Mean Value**"),
                max_value = md("**Max Value**")
            ) |> 
            cols_width(
                start_date:end_date ~ px(300),
                everything() ~ px(280)
            ) |> 
            opt_interactive(
                use_compact_mode = FALSE,
                use_pagination = FALSE
            )
    })
    
    output$table_2 <- render_gt({
        tbl <- new_table()
        
        expr = tbl |> 
            gt() |> 
            cols_label(
                `Close Price` = md("**Close Price**"),
                `Volatility (in %)` = md("**Volatility (in %)**"),
                `3-day Return` = md("**3-day Return**"),
                `1 %` = md("**1 %**"),
                `5 %` = md("**5 %**"),
                `95 %` = md("**95 %**"),
                `99 %` = md("**99 %**")
            ) |> 
            opt_interactive(
                use_compact_mode = FALSE,
                use_pagination = FALSE
            ) |> 
            tab_header(
                title = paste("As of", max(input$date_range),
                              "to Forecasted Price as of",
                              max(input$date_range) + 1, sep = " ")
            )
    })
    
    output$plot <- renderPlotly({
        plot_data <- filtered_data() 
        p <- plot_ats(
            data = plot_data, 
            exchange = input$exchange,
            types_value = types_value, type = input$type,
            product = input$product
        ) +
            scale_y_continuous(n.breaks = 7) +
            scale_x_date(
                date_breaks = "18 months",
                date_minor_breaks = "4 months",
                date_labels = "%b %Y"
            ) 
        # Make plot reactive
        ggplotly(p)
    })
}

shinyApp(ui, server)