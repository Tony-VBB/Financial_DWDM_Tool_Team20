library(shiny)
library(bs4Dash)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(httr)
library(jsonlite)
library(ggcorrplot) 
library(plotly)    
library(arules)    
library(stringr) 

options(shiny.maxRequestSize = 5 * 1024^3)

# ================= AI FUNCTION =================

get_ai_code <- function(query, meta) {
  prompt <- paste0(
    "You are an R expert.\n",
    "Return ONLY executable R code.\n",
    "Do NOT explain anything.\n",
    "Always return a dataframe.\n",
    "Use dataframe df only.\n",
    "Columns: ", paste(meta$columns, collapse=", "), "\n",
    "Task: ", query
  )
  
  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=AIzaSyARsCBm7tKptG8aN4PVa65b_5DN7iGwbEI"
  
  res <- tryCatch({
    POST(
      url,
      add_headers(`Content-Type` = "application/json"),
      body = toJSON(list(
        contents = list(
          list(parts = list(list(text = prompt)))
        )
      ), auto_unbox = TRUE)
    )
  }, error = function(e) return(NULL))
  
  if (is.null(res) || status_code(res) != 200) return(NULL)
  
  parsed <- tryCatch(content(res, as = "parsed"), error = function(e) NULL)
  txt <- tryCatch({ parsed$candidates[[1]]$content$parts[[1]]$text }, error = function(e) NULL)
  
  if (is.null(txt)) return(NULL)
  
  txt <- gsub("```[rR]?", "", txt)
  txt <- gsub("```", "", txt)
  trimws(txt)
}

# ================= UI =================
ui <- bs4DashPage(
  title = "Advanced Financial Analytics",
  header = bs4DashNavbar(title = "Data Mining & Insights"),
  sidebar = bs4DashSidebar(
    status = "primary",
    elevation = 3,
    fileInput("file", "Step 1: Upload Data", accept = c(".csv", ".xlsx", ".xls")),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database")),
      menuItem("OLAP Cube", tabName = "olap", icon = icon("cube")),
      menuItem("Visual Analytics", tabName = "viz", icon = icon("chart-pie")),
      menuItem("Correlation Matrix", tabName = "corr", icon = icon("th")),
      menuItem("Customer Clustering", tabName = "cluster", icon = icon("users")),
      menuItem("Market Basket", tabName = "mining", icon = icon("shopping-cart")),
      menuItem("AI Assistant", tabName = "ai", icon = icon("brain"))
    ),
    uiOutput("controls")
  ),
  body = bs4DashBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rows", width = 6),
                valueBoxOutput("cols", width = 6)
              ),
              fluidRow(
                box(width = 12, title = "Data Summary", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("summary"))
              ),
              fluidRow(
                box(width = 12, title = "Automated Distribution Insights", status = "primary",
                    uiOutput("auto_plots"))
              )
      ),
      tabItem("data", 
              box(width = 12, title = "Dataset Viewer", DTOutput("table"))
      ),
      tabItem("olap",
              fluidRow(
                box(width = 4, title = "OLAP Dimensions", status = "primary",
                    uiOutput("olap_dims_ui")),
                box(width = 4, title = "OLAP Measures", status = "primary",
                    uiOutput("olap_meas_ui")),
                box(width = 4, title = "Operations", status = "primary",
                    actionButton("run_olap", "Apply OLAP Operations", class = "btn-block btn-success"))
              ),
              fluidRow(
                box(width = 12, title = "OLAP Result Table", status = "info",
                    DTOutput("olap_table")),
                box(width = 12, title = "OLAP Visual (Dice)", status = "info",
                    plotlyOutput("olap_plot"))
              )
      ),
      tabItem("viz",
              box(width = 12, title = "Interactive Plotting", status = "success",
                  plotlyOutput("plot", height = 500))
      ),
      tabItem("corr",
              box(width = 12, title = "Variable Correlation Heatmap", status = "danger",
                  plotOutput("heatmap", height = 600))
      ),
      tabItem("cluster",
              box(width = 12, title = "K-Means Customer Segmentation", status = "warning",
                  plotOutput("cluster_plot", height = 550))
      ),
      tabItem("mining",
              box(width = 12, title = "Association Rule Mining", status = "secondary",
                  verbatimTextOutput("rules_out"))
      ),
      tabItem("ai",
              box(width = 12, title = "Natural Language Query", 
                  textInput("query", "Ask a question about your data"),
                  actionButton("run_ai", "Execute Query", class = "btn-primary"),
                  hr(),
                  DTOutput("ai_table"))
      )
    )
  )
)

# ================= SERVER =================
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- if (ext == "csv") fread(input$file$datapath, data.table = FALSE) else read_excel(input$file$datapath)
    df <- na.omit(df)
    df <- df %>% mutate(across(where(is.character), ~ str_to_title(str_trim(.))))
    if ("Date" %in% colnames(df)) df$Date <- as.Date(df$Date)
    df
  })
  
  # Sidebar Controls for specific tabs
  output$controls <- renderUI({
    req(data())
    df <- data()
    cols <- colnames(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    switch(input$tabs,
           "viz" = tagList(
             selectInput("x", "X-Axis Variable", cols),
             selectInput("y", "Y-Axis Variable (Numeric)", num_cols),
             selectInput("type", "Plot Type", c("Scatter", "Bar", "Line", "Boxplot"))
           ),
           "cluster" = tagList(
             selectInput("cx", "X-Axis (Numeric)", num_cols),
             selectInput("cy", "Y-Axis (Numeric)", num_cols),
             numericInput("k", "Number of Clusters (k)", 3, min = 2)
           )
    )
  })
  
  # --- OLAP CATEGORIZED SELECTION ---
  output$olap_dims_ui <- renderUI({
    req(data())
    # Only allow Non-Numeric columns for Dimensions
    dim_cols <- names(data())[sapply(data(), function(x) !is.numeric(x))]
    if(length(dim_cols) == 0) return(p("No categorical columns available."))
    
    tagList(
      selectInput("group_cols", "Select Dimensions (Drill-down)", choices = dim_cols, multiple = TRUE),
      helpText("Slicing/Dicing: Select categories to group data.")
    )
  })
  
  output$olap_meas_ui <- renderUI({
    req(data())
    # Only allow Numeric columns for Measures
    num_cols <- names(data())[sapply(data(), is.numeric)]
    if(length(num_cols) == 0) return(p("No numeric columns available."))
    
    tagList(
      selectInput("meas_col", "Select Measure", choices = num_cols),
      selectInput("agg_func", "Aggregation (Roll-up)", choices = c("Sum", "Mean", "Count", "Max", "Min"))
    )
  })
  
  # --- OLAP LOGIC ---
  olap_data <- eventReactive(input$run_olap, {
    req(data(), input$group_cols, input$meas_col)
    df <- data()
    
    res <- df %>%
      group_by(across(all_of(input$group_cols))) %>%
      summarise(
        Value = match.fun(tolower(input$agg_func))(.data[[input$meas_col]], na.rm = TRUE), 
        .groups = 'drop'
      )
    
    res
  })
  
  output$olap_table <- renderDT({
    req(olap_data())
    datatable(olap_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$olap_plot <- renderPlotly({
    req(olap_data())
    res <- olap_data()
    
    # Dynamically handle multiple dimensions for plot fill
    p <- ggplot(res, aes(x = .data[[input$group_cols[1]]], y = Value))
    
    if(length(input$group_cols) > 1) {
      p <- p + geom_col(aes(fill = .data[[input$group_cols[2]]]), position = "dodge")
    } else {
      p <- p + geom_col(fill = "steelblue")
    }
    
    p <- p + theme_minimal() + labs(title = paste(input$agg_func, "of", input$meas_col))
    ggplotly(p)
  })
  
  # --- Standard Outputs ---
  output$rows <- renderValueBox({ req(data()); valueBox(nrow(data()), "Total Records", icon = icon("list"), color = "primary") })
  output$cols <- renderValueBox({ req(data()); valueBox(ncol(data()), "Total Features", icon = icon("columns"), color = "info") })
  output$table <- renderDT({ req(data()); datatable(data(), options = list(scrollX = TRUE)) })
  output$summary <- renderPrint({ req(data()); summary(data()) })
  
  output$auto_plots <- renderUI({
    req(data())
    df <- data()
    cols_to_plot <- names(df)[1:min(10, ncol(df))]
    plot_output_list <- lapply(cols_to_plot, function(col) {
      plotname <- paste0("auto_", col)
      output[[plotname]] <- renderPlot({
        p <- ggplot(df, aes(x = .data[[col]])) + theme_minimal()
        if (is.numeric(df[[col]])) {
          p + geom_histogram(fill = "#007bff", color = "white", alpha = 0.7)
        } else {
          p + geom_bar(fill = "#ffc107", color = "white") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      })
      column(6, plotOutput(plotname, height = 300))
    })
    fluidRow(plot_output_list)
  })
  
  output$plot <- renderPlotly({
    req(data(), input$x, input$y)
    df <- data()
    p <- ggplot(df, aes(x = .data[[input$x]], y = .data[[input$y]])) + theme_bw()
    if (input$type == "Scatter") p <- p + geom_point(alpha = 0.5, color = "midnightblue")
    if (input$type == "Bar") p <- p + geom_col(fill = "steelblue")
    if (input$type == "Line") p <- p + geom_line(color = "red")
    if (input$type == "Boxplot") p <- p + geom_boxplot(fill = "lightgrey")
    ggplotly(p)
  })
  
  output$heatmap <- renderPlot({
    req(data())
    num <- data()[sapply(data(), is.numeric)]
    if(ncol(num) < 2) return(NULL)
    corr <- round(cor(num, use = "complete.obs"), 2)
    ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, colors = c("#E46726", "white", "#6D9EC1"))
  })
  
  output$cluster_plot <- renderPlot({
    req(data(), input$cx, input$cy)
    df <- data(); df_sub <- df[, c(input$cx, input$cy)]
    df_scaled <- scale(df_sub)
    set.seed(123)
    k_res <- kmeans(df_scaled, centers = input$k)
    df$cluster <- as.factor(k_res$cluster)
    ggplot(df, aes(x = .data[[input$cx]], y = .data[[input$cy]], color = cluster)) +
      geom_point(size = 3) + stat_ellipse() + theme_minimal()
  })
  
  output$rules_out <- renderPrint({
    req(data())
    df_cat <- as.data.frame(lapply(data(), as.factor))
    rules <- tryCatch({ apriori(df_cat, parameter = list(supp = 0.1, conf = 0.8), control = list(verbose=FALSE)) }, error = function(e) NULL)
    if(!is.null(rules)) inspect(head(sort(rules, by = "lift"), 10)) else "No rules found."
  })
  
  observeEvent(input$run_ai, {
    req(data())
    meta <- list(columns = colnames(data()))
    code <- get_ai_code(input$query, meta)
    if (is.null(code)) { showNotification("AI Error", type = "error"); return() }
    df <- data()
    result <- tryCatch({ eval(parse(text = code)) }, error = function(e) NULL)
    if (!is.data.frame(result)) result <- data.frame(Result = result)
    output$ai_table <- renderDT(datatable(result))
  })
}

shinyApp(ui, server)