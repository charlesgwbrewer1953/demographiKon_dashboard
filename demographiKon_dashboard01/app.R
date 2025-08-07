# app.R
# -------------------------------------------------------------------
# Canvassing Analytics Dashboard  (v0.0.0.7)
#
# REMINDER (for developers only; not displayed in the UI):
# 1) Location → pick ED type & EDs; optionally pick OAs (default: all in EDs).
# 2) Click "Load location data" to fetch rows for selected OAs (mandatory step).
# 3) Time filters: optional date range; weekday & time of day (multi, default all).
# 4) Factors: Q1/Q2/Q3/Q4 populated from the loaded location subset.
# 5) SQL is built incrementally and shown before execution.
# 6) Click "Retrieve data" to run final query and show results.
# -------------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DBI)
library(pool)
library(RMariaDB)
library(glue)
library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)

APP_VERSION <- "0.0.0.7"

# ----- DB pool -----
pool <- dbPool(
  drv      = RMariaDB::MariaDB(),
  dbname   = Sys.getenv("MYSQL_DATABASE", "canvass_simulation"),
  host     = Sys.getenv("MYSQL_HOST", "10.0.0.21"),
  username = Sys.getenv("MYSQL_USER", "demographikon1"),
  password = Sys.getenv("MYSQL_PASSWORD", "liverbird_beard1"),
  port     = as.integer(Sys.getenv("MYSQL_PORT", "3306")),
  bigint   = "character"
)
onStop(function() poolClose(pool))

# ----- ED mapping -----
ed_map <- list(
  "Parliamentary constituency" = list(code = "pcon25cd",  name = "pcon25nm"),
  "Local Authority"            = list(code = "lad24cd",   name = "lad24nm"),
  "Mayoralty"                  = list(code = "mayr26cd",  name = "mayr26nm"),
  "Ward"                       = list(code = "wd24cd",    name = "wd24nm"),
  "Senedd"                     = list(code = "sened26cd", name = "sened26nm")
)

# helpers
is_all <- function(x) is.null(x) || length(x) == 0 || any(x == "*")
parse_ed_codes <- function(x) if (is.null(x) || length(x) == 0) character(0) else unique(sub("\\|.*$", "", x))

# ----- UI -----
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .header-row { display:flex; align-items:flex-start; justify-content:space-between; }
      .version-info { font-style:italic; font-size: 0.9em; text-align:right; opacity: 0.8; }
    "))
  ),
  fluidRow(
    column(
      12,
      div(class="header-row",
          h2("Canvassing Analytics Dashboard"),
          div(class="version-info", sprintf("v%s", APP_VERSION))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("use_training", "Use training table only (responses_t) and select by OA only", value = FALSE),
      hr(),
      h4("1) Location"),
      selectInput("ed_type", "Electoral Division type",
                  choices = c("*", names(ed_map)), selected = "*"),
      pickerInput("ed_names", "Electoral Division(s)",
                  choices = NULL, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      pickerInput("oa_codes", "Output Area(s) (OA21 codes)",
                  choices = c("*"="*"), multiple = TRUE, selected="*",
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      actionButton("load_location", "Load location data", class="btn-info"),
      tags$small(em("You must load location data before running the final query.")),
      hr(),
      
      h4("2) Time"),
      tabsetPanel(
        tabPanel("Date range",
                 dateInput("start_date", "Start date", value = NULL),
                 dateInput("end_date",   "End date",   value = NULL),
                 helpText("Blank = earliest/latest available in the loaded location data.")
        ),
        tabPanel("Weekday / Time of day",
                 pickerInput("weekday", "Weekday(s)", choices = c("*"="*"),
                             multiple = TRUE, selected = "*",
                             options = list(`actions-box` = TRUE)),
                 pickerInput("tod", "Time of day",
                             choices = c("*"="*", "morning", "afternoon", "evening"),
                             multiple = TRUE, selected = "*",
                             options = list(`actions-box` = TRUE))
        )
      ),
      hr(),
      h4("3) Factors"),
      pickerInput("q1", "Q1_Party", choices = c("*"="*"), multiple = TRUE, selected="*",
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      pickerInput("q2", "Q2_Support", choices = c("*"="*"), multiple = TRUE, selected="*",
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      pickerInput("q3", "Q3_Votelikelihood", choices = c("*"="*"), multiple = TRUE, selected="*",
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      pickerInput("q4", "Q4_Issue", choices = c("*"="*"), multiple = TRUE, selected="*",
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      hr(),
      actionButton("run_query", "Retrieve data", class = "btn-primary"),
      hr(),
      
      h4("4) Analysis"),
      pickerInput("analysis_party", "Party focus (Q1_Party)",
                  choices = c("(load data first)"=""), multiple = FALSE),
      pickerInput("analysis_support_levels", "Supportive Q2 levels",
                  choices = c("Very likely", "Likely", "Lean", "Unsure", "Unlikely", "Very unlikely"),
                  selected = c("Very likely","Likely","Lean"), multiple = TRUE,
                  options = list(`actions-box`=TRUE)),
      pickerInput("analysis_persuadable_levels", "Persuasion levels (Q2)",
                  choices = c("Unsure","Lean"), selected = c("Unsure","Lean"),
                  multiple = TRUE, options = list(`actions-box`=TRUE)),
      selectInput("analysis_metric", "Metric",
                  choices = c("Support rate (EB)"="rate_eb",
                              "Expected supporters (EB)"="expected_supporters",
                              "Persuasion pool"="persuasion_pool"),
                  selected = "rate_eb"),
      numericInput("analysis_min_n", "Minimum effective sample size per OA", value = 10, min = 0, step = 1),
      numericInput("analysis_half_life", "Recency half-life (days)", value = 30, min = 1, step = 1),
      actionButton("run_analysis", "Run analysis", class = "btn-success")
    ),
    mainPanel(
      h4("Results"),
      verbatimTextOutput("sql_preview"),
      tableOutput("summary"),
      DTOutput("rows"),
      hr(),
      h4("Analysis outputs"),
      DTOutput("oa_ranking"),
      plotOutput("plot_top_oas", height = 320),
      plotOutput("plot_heatmap", height = 320)
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {
  
  shinyjs::disable("run_query")
  location_data <- reactiveVal(NULL)
  location_scope_sql <- reactiveVal(NULL)
  
  base_table <- reactive({
    if (isTRUE(input$use_training)) "canvass_simulation.responses_t" else "canvass_simulation.responses"
  })
  
  ed_cols <- reactive({ ed_map[[input$ed_type]] })
  
  # Toggle: training mode
  observeEvent(input$use_training, {
    if (isTRUE(input$use_training)) {
      shinyjs::disable("ed_type"); shinyjs::disable("ed_names")
      sql <- glue("SELECT DISTINCT oa21cd FROM {base_table()} WHERE oa21cd IS NOT NULL ORDER BY oa21cd")
      oa_df <- dbGetQuery(pool, sql)
      updatePickerInput(session, "oa_codes",
                        choices = c("*"="*", setNames(oa_df$oa21cd, oa_df$oa21cd)),
                        selected="*")
    } else {
      shinyjs::enable("ed_type"); shinyjs::enable("ed_names")
      updatePickerInput(session, "oa_codes", choices = c("*"="*"), selected="*")
      if (input$ed_type != "*") {
        cols <- ed_cols()
        sql <- glue_sql("
          SELECT DISTINCT {`cols$name`} AS ed_name, {`cols$code`} AS ed_code
          FROM ed_complete_20250720
          WHERE {`cols$name`} IS NOT NULL AND {`cols$code`} IS NOT NULL
          ORDER BY {`cols$name`}
        ", .con = pool)
        df <- dbGetQuery(pool, sql)
        choices <- setNames(paste(df$ed_code, df$ed_name, sep="|"), df$ed_name)
        updatePickerInput(session, "ed_names", choices = choices, selected = NULL)
      }
    }
    location_data(NULL); location_scope_sql(NULL); shinyjs::disable("run_query")
  }, ignoreInit = TRUE)
  
  # Populate ED names when not in training mode
  observeEvent(input$ed_type, {
    req(!isTRUE(input$use_training))
    if (input$ed_type == "*") {
      updatePickerInput(session, "ed_names", choices = c("*"="*"), selected = "*")
      return()
    }
    cols <- ed_cols()
    sql <- glue_sql("
      SELECT DISTINCT {`cols$name`} AS ed_name, {`cols$code`} AS ed_code
      FROM ed_complete_20250720
      WHERE {`cols$name`} IS NOT NULL AND {`cols$code`} IS NOT NULL
      ORDER BY {`cols$name`}
    ", .con = pool)
    df <- dbGetQuery(pool, sql)
    choices <- setNames(paste(df$ed_code, df$ed_name, sep="|"), df$ed_name)
    updatePickerInput(session, "ed_names", choices = choices, selected = NULL)
    updatePickerInput(session, "oa_codes", choices = c("*"="*"), selected="*")
    location_data(NULL); location_scope_sql(NULL); shinyjs::disable("run_query")
  }, ignoreInit = TRUE)
  
  # OA list when EDs change
  observeEvent(input$ed_names, {
    req(!isTRUE(input$use_training))
    if (is_all(input$ed_names) || input$ed_type == "*") {
      updatePickerInput(session, "oa_codes", choices = c("*"="*"), selected="*")
      return()
    }
    cols <- ed_cols()
    ed_codes <- parse_ed_codes(input$ed_names)
    if (!length(ed_codes)) {
      updatePickerInput(session, "oa_codes", choices = c("*"="*"), selected="*")
      return()
    }
    sql <- glue_sql("
      SELECT DISTINCT oa21cd
      FROM ed_complete_20250720
      WHERE {`cols$code`} IN ({ed_codes*})
      ORDER BY oa21cd
    ", .con = pool)
    oa_df <- dbGetQuery(pool, sql)
    updatePickerInput(session, "oa_codes",
                      choices = c("*"="*", setNames(oa_df$oa21cd, oa_df$oa21cd)),
                      selected="*")
  }, ignoreInit = TRUE)
  
  # Load location data
  observeEvent(input$load_location, {
    clauses <- list()
    if (isTRUE(input$use_training)) {
      if (!is_all(input$oa_codes)) {
        clauses <- append(clauses, glue_sql("oa21cd IN ({input$oa_codes*})", .con = pool))
      }
    } else {
      if (input$ed_type != "*" && !is_all(input$ed_names)) {
        cols <- ed_cols()
        ed_codes <- parse_ed_codes(input$ed_names)
        if (length(ed_codes)) {
          oa_in_eds <- glue_sql("
            SELECT DISTINCT oa21cd
            FROM ed_complete_20250720
            WHERE {`cols$code`} IN ({ed_codes*})
          ", .con = pool)
          clauses <- append(clauses, glue("oa21cd IN ({oa_in_eds})"))
        }
      }
      if (!is_all(input$oa_codes)) {
        clauses <- append(clauses, glue_sql("oa21cd IN ({input$oa_codes*})", .con = pool))
      }
    }
    where_sql <- if (length(clauses)) paste("WHERE", paste(clauses, collapse = " AND ")) else ""
    location_scope_sql(where_sql)
    
    sql <- glue("SELECT * FROM {base_table()} {where_sql}")
    df <- dbGetQuery(pool, sql)
    location_data(df)
    
    if ("survey_date" %in% names(df) && nrow(df) > 0) {
      updateDateInput(session, "start_date", value = min(df$survey_date, na.rm = TRUE))
      updateDateInput(session, "end_date", value = max(df$survey_date, na.rm = TRUE))
    }
    
    # Update dropdowns from loaded data (guard for column existence)
    if ("weekday" %in% names(df)) {
      updatePickerInput(session, "weekday",
                        choices = c("*"="*", sort(unique(na.omit(df$weekday)))), selected="*")
    }
    # Factors (iterate over a named vector)
    factor_map <- c(q1 = "Q1_Party", q2 = "Q2_Support", q3 = "Q3_Votelikelihood", q4 = "Q4_Issue")
    for (id in names(factor_map)) {
      col <- factor_map[[id]]
      vals <- if (col %in% names(df)) sort(unique(na.omit(df[[col]]))) else character(0)
      updatePickerInput(session, id, choices = c("*"="*", vals), selected="*")
    }
    # Seed analysis party list
    if ("Q1_Party" %in% names(df)) {
      parties <- sort(unique(na.omit(df$Q1_Party)))
      updatePickerInput(session, "analysis_party", choices = parties,
                        selected = if (length(parties)) parties[1] else "")
    }
    
    shinyjs::enable("run_query")
    showNotification(sprintf("Loaded %s rows from %s.", format(nrow(df), big.mark = ","), base_table()),
                     type = "message", duration = 5)
  })
  
  # SQL builder
  final_sql <- eventReactive(input$run_query, {
    clauses <- list()
    if (!is.null(location_scope_sql()) && nzchar(location_scope_sql())) {
      clauses <- append(clauses, sub("^WHERE\\s+", "", location_scope_sql()))
    }
    if (!is.null(input$start_date)) clauses <- append(clauses, glue_sql("survey_date >= {input$start_date}", .con=pool))
    if (!is.null(input$end_date))   clauses <- append(clauses, glue_sql("survey_date <= {input$end_date}", .con=pool))
    if (!is_all(input$weekday))     clauses <- append(clauses, glue_sql("weekday IN ({input$weekday*})", .con=pool))
    if (!is_all(input$tod))         clauses <- append(clauses, glue_sql("timeofDay IN ({input$tod*})", .con=pool))
    if (!is_all(input$q1)) clauses <- append(clauses, glue_sql("Q1_Party IN ({input$q1*})", .con=pool))
    if (!is_all(input$q2)) clauses <- append(clauses, glue_sql("Q2_Support IN ({input$q2*})", .con=pool))
    if (!is_all(input$q3)) clauses <- append(clauses, glue_sql("Q3_Votelikelihood IN ({input$q3*})", .con=pool))
    if (!is_all(input$q4)) clauses <- append(clauses, glue_sql("Q4_Issue IN ({input$q4*})", .con=pool))
    where_sql <- if (length(clauses)) paste("WHERE", paste(clauses, collapse=" AND ")) else ""
    glue("SELECT oa21cd, survey_date, weekday, timeofDay, Q1_Party, Q2_Support, Q3_Votelikelihood, Q4_Issue FROM {base_table()} {where_sql}")
  })
  
  output$sql_preview <- renderText({ req(final_sql()); as.character(final_sql()) })
  results <- eventReactive(input$run_query, { dbGetQuery(pool, final_sql()) })
  
  output$rows <- DT::renderDT({
    req(results())
  }, options = list(pageLength = 25), filter = "top")
  
  output$summary <- renderTable({
    df <- results(); req(nrow(df) > 0)
    data.frame(
      n_rows     = nrow(df),
      from       = suppressWarnings(min(as.Date(df$survey_date), na.rm = TRUE)),
      to         = suppressWarnings(max(as.Date(df$survey_date), na.rm = TRUE)),
      unique_OAs = length(unique(df$oa21cd))
    )
  })
  
  # -------- Analysis --------
  recency_weight <- function(dates, half_life_days) {
    if (is.null(dates) || all(is.na(dates))) return(rep(1, length(dates)))
    age <- as.numeric(max(as.Date(dates), na.rm = TRUE) - as.Date(dates))
    exp(-pmax(age, 0) / pmax(half_life_days, 1))
  }
  
  analysis_df <- eventReactive(input$run_analysis, {
    df <- location_data()
    validate(need(!is.null(df) && nrow(df) > 0, "Load location data first."))
    
    if (!is.null(input$start_date)) df <- df %>% filter(as.Date(survey_date) >= as.Date(input$start_date))
    if (!is.null(input$end_date))   df <- df %>% filter(as.Date(survey_date) <= as.Date(input$end_date))
    if (!is_all(input$weekday) && "weekday" %in% names(df)) df <- df %>% filter(weekday %in% input$weekday)
    if (!is_all(input$tod)     && "timeofDay" %in% names(df)) df <- df %>% filter(timeofDay %in% input$tod)
    
    df$._w <- recency_weight(df$survey_date, input$analysis_half_life)
    df
  })
  
  compute_eb_prior <- function(s, n) {
    p0 <- if (sum(n) > 0) sum(s) / sum(n) else 0.5
    v  <- 0.01
    k  <- max(p0 * (1 - p0) / v - 1, 1)
    list(alpha0 = p0 * k, beta0 = (1 - p0) * k)
  }
  
  ranking <- eventReactive(input$run_analysis, {
    df <- analysis_df()
    validate(need(input$analysis_party != "", "Choose a party for analysis."))
    
    df <- df %>%
      mutate(
        success = ifelse(Q1_Party == input$analysis_party & Q2_Support %in% input$analysis_support_levels, 1, 0),
        pers    = ifelse(Q2_Support %in% input$analysis_persuadable_levels, 1, 0)
      )
    ##################
    browser()
    df <- df %>%
      mutate(
        success = ifelse(Q1_Party == input$analysis_party &
                           Q2_Support %in% input$analysis_support_levels, 1, 0),
        pers = ifelse(Q2_Support %in% input$analysis_persuadable_levels, 1, 0)
      )
    
    print(table(df$success))
    print(table(df$pers))
    
    ###################
    grp <- df %>%
      group_by(oa21cd) %>%
      summarise(
        n_eff = sum(._w, na.rm = TRUE),
        s_eff = sum(._w * success, na.rm = TRUE),
        pers_eff = sum(._w * pers, na.rm = TRUE),
        rate_raw = ifelse(n_eff > 0, s_eff / n_eff, NA_real_),
        .groups = "drop"
      ) %>%
      filter(n_eff >= input$analysis_min_n)
    
    validate(need(nrow(grp) > 0, "No OAs meet the minimum effective sample size."))
    
    prior <- compute_eb_prior(grp$s_eff, grp$n_eff)
    grp <- grp %>%
      mutate(
        rate_eb = (s_eff + prior$alpha0) / (n_eff + prior$alpha0 + prior$beta0),
        expected_supporters = rate_eb * n_eff
      )
    
    metric <- switch(input$analysis_metric,
                     rate_eb = grp$rate_eb,
                     expected_supporters = grp$expected_supporters,
                     persuasion_pool = grp$pers_eff)
    grp$metric <- metric
    
    grp %>%
      arrange(desc(metric)) %>%
      mutate(rank = row_number()) %>%
      select(rank, oa21cd, n_eff, s_eff, rate_raw, rate_eb, expected_supporters, pers_eff, metric)
  })
  
  output$oa_ranking <- DT::renderDT({
    req(ranking())
    datatable(
      ranking(),
      options = list(pageLength = 20, order = list(list(0, "asc"))),
      rownames = FALSE
    ) %>%
      formatRound(c("n_eff","s_eff","expected_supporters","pers_eff","metric"), digits = 2) %>%
      formatPercentage("rate_raw", 1) %>%
      formatPercentage("rate_eb", 1)
  })
  
  output$plot_top_oas <- renderPlot({
    req(ranking())
    topn <- head(ranking(), 15)
    ggplot(topn, aes(x = reorder(oa21cd, metric), y = metric)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "OA21 code",
        y = switch(input$analysis_metric,
                   rate_eb = "Support rate (EB)",
                   expected_supporters = "Expected supporters (EB)",
                   persuasion_pool = "Persuasion pool (weighted)"),
        title = "Top OAs"
      )
  })
  
  output$plot_heatmap <- renderPlot({
    df <- analysis_df()
    req(nrow(df) > 0, input$analysis_party != "")
    
    df2 <- df %>%
      mutate(success = ifelse(Q1_Party == input$analysis_party & Q2_Support %in% input$analysis_support_levels, 1, 0)) %>%
      filter(!is.na(weekday), !is.na(timeofDay)) %>%
      group_by(weekday, timeofDay) %>%
      summarise(
        n_eff = sum(._w, na.rm = TRUE),
        s_eff = sum(._w * success, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_eff > 0)
    
    if (nrow(df2) == 0) return(NULL)
    
    prior <- compute_eb_prior(df2$s_eff, df2$n_eff)
    df2 <- df2 %>%
      mutate(rate_eb = (s_eff + prior$alpha0) / (n_eff + prior$alpha0 + prior$beta0))

    ggplot(df2, aes(x = timeofDay, y = weekday, fill = rate_eb)) +
      geom_tile() +
      scale_fill_gradient(name = "Support rate (EB)", limits = c(0, 1)) +
      labs(x = "Time of day", y = "Weekday", title = "Best times to canvass (selected party)") +
      theme_minimal()
  })
}

shinyApp(ui, server)