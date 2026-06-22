library(shiny)
library(ggplot2)
library(DynamicBakeryPackageR)

# Load all S4 classes, methods, simulator, product_catalog
# source("data.R")
# source("classes.R")
# source("methods.R")
# source("objects.R")
# source("simulation.R")

css <- paste0(
  "* { box-sizing: border-box; margin: 0; padding: 0; }",
  "body { background-color: #1a2744; font-family: sans-serif; color: #e8d9b0; min-height: 100vh; }",
  ".outer-wrapper { display: flex; min-height: 100vh; }",
  ".sidebar { width: 260px; min-width: 260px; background: #111d36; border-right: 1px solid #c9b47a; padding: 32px 20px; display: flex; flex-direction: column; gap: 32px; }",
  ".sidebar-title { font-size: 1.5rem; font-weight: 600; letter-spacing: 0.2em; text-transform: uppercase; color: #c9b47a; margin-bottom: 12px; }",
  ".sidebar label { font-size: 1.3rem; color: #8a9bbf; display: block; margin-bottom: 6px; }",
  ".sidebar select { width: 100%; background: #1a2744; color: #e8d9b0; border: 1px solid #2a3f6e; border-radius: 6px; padding: 8px 10px; font-size: 0.85rem; outline: none; cursor: pointer; }",
  ".sidebar select:focus { border-color: #c9b47a; }",
  ".sidebar-section { border-top: 1px solid #1e2f52; padding-top: 24px; }",
  ".main-area { flex: 1; padding: 48px 40px; display: flex; flex-direction: column; gap: 32px; }",
  ".header { text-align: center; }",
  ".header h1 { font-size: 3.2rem; font-weight: 700; color: #ffffff; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 6px; }",
  ".header p { font-size: 1.5rem; color: #c9b47a; letter-spacing: 0.15em; text-transform: uppercase; }",
  ".chart-card { background: #1e2f52; border: 2px solid #c9b47a; border-radius: 12px; padding: 32px 32px 24px; box-shadow: 0 40px 80px rgba(0, 0, 0, 0.5); }",
  ".chart-label { font-size: 1.2rem; letter-spacing: 0.2em; text-transform: uppercase; color: #c9b47a; margin-bottom: 4px; }",
  ".chart-title { font-size: 1.5rem; color: #ffffff; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 24px; }",
  ".footer-note { text-align: center; font-size: 0.72rem; color: #c9b47a; letter-spacing: 0.15em; text-transform: uppercase; opacity: 0.5; }",
  ".shiny-output-error-validation { color: #e05555 !important; font-size: 1.2rem !important; font-weight: 600; }"
)

product_families <- c(
  "Sandwiches"  = "Sandwich",
  "Breads"      = "Bread",
  "Pizzas"      = "Pizza"
)

graphs <- c(
  "Daily revenue"   = "revenue",
  "Price evolution" = "evolution_prix"
)

ui <- fluidPage(
  tags$head(tags$style(css)),
  div(class = "outer-wrapper",

      div(class = "sidebar",
          div(
            div(class = "sidebar-title", "Category"),
            tags$label("Select a category:"),
            selectInput("category", label = NULL, choices = product_families, width = "100%"),
            selectInput("product", label = "Select a product:", choices = NULL, width = "100%")
          ),
          div(class = "sidebar-section",
              div(class = "sidebar-title", "Visualization"),
              tags$label("Select a chart:"),
              selectInput("graph", label = NULL, choices = graphs, width = "100%")
          ),
          # Simulation inputs in sidebar
          div(class = "sidebar-section",
              div(class = "sidebar-title", "Parameters"),
              tags$label("Number of purchases:"),
              sliderInput("nb_purchase", label = NULL, min = 10, max = 100, value = 30, step = 10),
              tags$label("Price update interval (min):"),
              sliderInput("tic_size", label = NULL, min = 1, max = 60, value = 10, step = 1),
              tags$label("Day of the week:"),
              selectInput("day", label = NULL,
                          choices = c("Monday"    = "mon",
                                      "Tuesday"   = "tue",
                                      "Wednesday" = "wed",
                                      "Thursday"  = "thu",
                                      "Friday"    = "fri",
                                      "Saturday"  = "sat",
                                      "Sunday"    = "sun")
                          )
          ),
          div(class = "sidebar-section",
            div(class = "sidebar-title", "Monte Carlo"),
            tags$label("Number of runs:"),
            sliderInput("n_runs", label = NULL, min = 50, max = 500, value = 100, step = 50),
            actionButton("run_mc", "Run simulation",
                 style = "width:100%; background:#c9b47a; color:#111d36;
                          font-weight:700; border:none; padding:10px;
                          border-radius:6px; cursor:pointer; margin-top:8px;")
          ),
      ),

      div(class = "main-area",
          div(class = "header",
              h1("Bakery Dynamic Pricing Simulator"),
              p("Demand-Driven Approach - Based on Real Warsaw Bakery")
          ),
          div(class = "chart-card",
              div(class = "chart-label", "Visualization"),
              div(class = "chart-title", textOutput("chart_title", inline = TRUE)),
              plotOutput("graphique", height = "420px")
          ),
          div(class = "chart-card",
              div(class = "chart-label", "Monte Carlo"),
              div(class = "chart-title", "Revenue distribution across simulations"),
              plotOutput("mc_plot", height = "320px")
          ),
          div(class = "chart-card",
              tableOutput("summary_table")
          )
      )
  )
)

server <- function(input, output, session) {

  # Update product choices when family changes
  observe({
    family <- input$category
    matching <- Filter(function(p) is(p, family), product_catalog)
    updateSelectInput(session, "product", choices = names(matching))
  })

  # Reactive simulation result (reruns when inputs change)
  sim_results <- reactive({
    req(input$product)

    if (input$day == "sun") {
      showNotification(
        "Lubaszka is closed on Sundays — no simulation available.",
        type = "warning",
        duration = NULL
      )
      return(NULL)
    }

    product <- product_catalog[[input$product]]

    if (input$nb_purchase > product@stock_initial * 2) {
      showNotification(
        paste0("Warning: ", input$nb_purchase, " purchases requested but '",
               input$product, "' only has ", product@stock_initial,
               " units in stock. Most demand will be unmet."),
        type = "warning",
        duration = 8
      )
    }

    tryCatch(
      simulator(
        product     = product,
        nb_purchase = input$nb_purchase,
        tic_size    = input$tic_size,
        bakery      = lubaszka_solec,
        day         = input$day,
        verbose     = FALSE
      ),
      error = function(e) {
        showNotification(paste("Simulation error:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      }
    )
  })

  # Monte Carlo simulation
  # Result stored in a reactiveVal so it can be invalidated
  mc_store <- reactiveVal(NULL)

  # Reset the stored MC result whenever a config input tied to it changes
  observeEvent(
    list(input$product, input$day, input$nb_purchase, input$tic_size, input$n_runs),
    {
      mc_store(NULL)
    },
    ignoreInit = TRUE
  )

  # Run Monte Carlo only on button click, then store the result
  observeEvent(input$run_mc, {
    req(input$product, input$day != "sun")
    product <- product_catalog[[input$product]]
    result <- tryCatch(
      monte_carlo_simulation(
        product     = product,
        bakery      = lubaszka_solec,
        day         = input$day,
        n_runs      = input$n_runs,
        nb_purchase = input$nb_purchase,
        tic_size    = input$tic_size
      ),
      error = function(e) {
        showNotification(paste("MC error:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      }
    )
    mc_store(result)
  })



  # Chart title
  output$chart_title <- renderText({
    graph_name <- names(graphs)[graphs == input$graph]
    paste(graph_name, "-", input$product)
  })

  # Main plot
  output$graphique <- renderPlot({
    res <- sim_results()
    validate(need(!is.null(res), "No data to display — the bakery is closed today."))

    time_hours <- (res$time_offset_min + res$time) / 60

    if (input$graph == "revenue") {
      df <- data.frame(time = time_hours, revenue = cumsum(res$revenue))

      ggplot(df, aes(x = time, y = revenue)) +
        geom_area(fill = "#c9b47a", alpha = 0.15) +
        geom_line(color = "#c9b47a", linewidth = 1.5) +
        labs(x = "Hour of day", y = "Cumulative revenue (PLN)") +
        scale_x_continuous(breaks = seq(floor(min(df$time)), ceiling(max(df$time)), by = 1)) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "#1e2f52", color = NA),
          panel.background = element_rect(fill = "#1e2f52", color = NA),
          panel.grid.major = element_line(color = "#243560", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          axis.text        = element_text(color = "#ffffff", size = 12),
          axis.title.y     = element_text(color = "#c9b47a", margin = margin(r = 30), size = 14),
          axis.title.x     = element_text(color = "#c9b47a", margin = margin(t = 30), size = 14)
        )

    } else {
      df <- data.frame(time = time_hours, price = res$price)

      subtitle_text <-
        if (is(product_catalog[[input$product]], "Sandwich")) {
          "Freshness: -15% to +15% (steep decay)"
        } else if (is(product_catalog[[input$product]], "Bread")) {
          "Freshness: -5% to +5% (slow decay)"
        } else if (is(product_catalog[[input$product]], "Pizza")) {
          "Freshness: -20% to +20% (steep decay)"
        } else {
          "Freshness: no adjustment"
        }

      ggplot(df, aes(x = time, y = price)) +
        geom_line(color = "#c9b47a", linewidth = 1.5) +
        geom_hline(yintercept = product_catalog[[input$product]]@price_ref,
                   color = "red", linetype = "dashed", linewidth = 1) +
        labs(x = "Hour of day", y = "Price (PLN)", subtitle = subtitle_text) +
        scale_x_continuous(breaks = seq(floor(min(df$time)), ceiling(max(df$time)), by = 1)) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "#1e2f52", color = NA),
          panel.background = element_rect(fill = "#1e2f52", color = NA),
          panel.grid.major = element_line(color = "#243560", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          axis.text        = element_text(color = "#ffffff", size = 12),
          axis.title.y     = element_text(color = "#c9b47a", margin = margin(r = 30), size = 14),
          axis.title.x     = element_text(color = "#c9b47a", margin = margin(t = 30), size = 14),
          plot.subtitle    = element_text(color = "#c9b47a", size = 15)
        )
    }
  }, bg = "#1e2f52")

  # Monte Carlo plot
  output$mc_plot <- renderPlot({
    # res <- mc_results()
    res <- mc_store()
    validate(need(!is.null(res), "Click 'Run simulation' to launch Monte Carlo."))

    df <- data.frame(total_revenue = res$total_revenue)

    ggplot(df, aes(x = total_revenue)) +
      geom_histogram(fill = "#c9b47a", color = "#111d36", bins = 30, alpha = 0.85) +
      geom_vline(xintercept = mean(df$total_revenue),
                 color = "white", linetype = "dashed", linewidth = 1) +
      labs(x = "Total revenue (PLN)", y = "Count",
           subtitle = paste0("Mean: ", round(mean(df$total_revenue), 2),
                             " PLN  |  SD: ", round(sd(df$total_revenue), 2), " PLN")) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#1e2f52", color = NA),
        panel.background = element_rect(fill = "#1e2f52", color = NA),
        panel.grid.major = element_line(color = "#243560", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color = "#ffffff", size = 12),
        axis.title.y     = element_text(color = "#c9b47a", margin = margin(r = 30), size = 14),
        axis.title.x     = element_text(color = "#c9b47a", margin = margin(t = 30), size = 14),
        plot.subtitle    = element_text(color = "#8a9bbf", size = 13)
      )
  }, bg = "#1e2f52")

  # Summary table
  output$summary_table <- renderTable({
    res <- sim_results()
    validate(need(!is.null(res), "No simulation results available."))
    p <- res$final_product
    data.frame(
      Metric = c("Initial stock", "Items sold", "Final stock",
                 "Reference price", "Min price", "Max price", "Avg price",
                 "Total revenue"),
      Value  = c(
        p@stock_initial,
        p@stock_initial - p@stock,
        p@stock,
        round(p@price_ref, 2),
        round(min(res$price), 2),
        round(max(res$price), 2),
        round(mean(res$price), 2),
        round(sum(res$revenue), 2)
      )
    )
  })

}

shinyApp(ui = ui, server = server)
