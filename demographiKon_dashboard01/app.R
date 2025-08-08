# app.R
# -------------------------------------------------------------------
# Canvassing Analytics Dashboard  (v0.0.0.9)
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
library(rlang)

APP_VERSION <- "0.0.0.9"

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
      checkboxInput("use_training", "Use training table only (responses_q) and select by OA only", value = FALSE),
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
      # Standardized values for Q2, Q3, Q4
      pickerInput("q2", "Q2_Support",
                  choices = c("*"="*", "pledge", "strong", "lean_to", "none"),
                  selected = "*", multiple = TRUE,
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      pickerInput("q3", "Q3_Votelikelihood",
                  choices = c("*"="*", "definite", "probable", "unlikely", "no"),
                  selected = "*", multiple = TRUE,
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      pickerInput("q4", "Q4_Issue",
                  choices = c("*"="*", "Economy", "Immigration", "Housing", "Net zero", "NHS"),
                  selected = "*", multiple = TRUE,
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      hr(),
      actionButton("run_query", "Retrieve data", class = "btn-primary"),
      hr(),
      
      h4("4) Analysis"),
      pickerInput("analysis_party", "Party focus (Q1_Party)",
                  choices = c("(load data first)"=""), multiple = FALSE),
      pickerInput("analysis_support_levels", "Supportive Q2 levels",
                  choices = c("pledge", "strong", "lean_to", "none"),
                  selected = c("pledge","strong","lean_to"), multiple = TRUE,
                  options = list(`actions-box`=TRUE)),
      pickerInput("analysis_persuasion_levels_q3", "Persuasion = Q3_Votelikelihood in:",
                  choices = c("definite","probable","unlikely","no"),
                  selected = c("probable","unlikely"),
                  multiple = TRUE, options = list(`actions-box`=TRUE)),
      selectInput("analysis_metric", "Metric",
                  choices = c("Support rate (EB)"                  = "rate_eb",
                              "Expected supporters (EB, pop‑scaled)" = "expected_supporters",
                              "Expected supporters (raw, pop‑scaled)"= "expected_supporters_raw",
                              "Persuasion pool"                     = "persuasion_pool"),
                  selected = "expected_supporters"  # or "rate_eb" if you prefer
      ),
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
      plotOutput("plot_heatmap", height = 320),
      plotOutput("plot_response_heatmap", height = 320),
      plotOutput("plot_heatmap", height = 320),
      tabPanel("Debug",
               verbatimTextOutput("dbg_counts"),
               verbatimTextOutput("dbg_cols"),
               DT::DTOutput("dbg_head")
      )
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {
  
  shinyjs::disable("run_query")
  location_data <- reactiveVal(NULL)
  location_scope_sql <- reactiveVal(NULL)
  
  base_table <- reactive({
    if (isTRUE(input$use_training)) "canvass_simulation.responses_q" else "canvass_simulation.response"
  })
  
  ed_lookup_table <- reactive({
    if (isTRUE(input$use_training)) {
      "ed_complete_demo"
    } else {
      "ed_complete_20250720"
    }
  })
  
  ed_cols <- reactive({ ed_map[[input$ed_type]] })
  
  
  # Toggle hamburger
  observeEvent(input$toggle_sidebar, {
    # Toggle a CSS class on the <body> to hide/show the sidebar and stretch main
    session$sendCustomMessage("toggleSidebarClass", list())
  })
  
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
      FROM {`ed_lookup_table()`}
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
  FROM {`ed_lookup_table()`}
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
      FROM {`ed_lookup_table()`}
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
    
    # Auto-update date inputs
    if ("survey_date" %in% names(df) && nrow(df) > 0) {
      updateDateInput(session, "start_date", value = suppressWarnings(min(as.Date(df$survey_date), na.rm = TRUE)))
      updateDateInput(session, "end_date",   value = suppressWarnings(max(as.Date(df$survey_date), na.rm = TRUE)))
    }
    
    # Update weekday & time-of-day choices based on actual columns present
    if ("weekday" %in% names(df)) {
      updatePickerInput(session, "weekday",
                        choices = c("*"="*", sort(unique(na.omit(df$weekday)))), selected="*")
    }
    time_choices <- NULL
    if ("timeofDay" %in% names(df))    time_choices <- sort(unique(na.omit(df$timeofDay)))
    if ("timeofday" %in% names(df))    time_choices <- sort(unique(na.omit(df$timeofday)))
    if ("time_period" %in% names(df))  time_choices <- sort(unique(na.omit(df$time_period)))
    if ("timeOfDay" %in% names(df))    time_choices <- sort(unique(na.omit(df$timeOfDay)))
    if (!is.null(time_choices) && length(time_choices)) {
      updatePickerInput(session, "tod", choices = c("*"="*", time_choices), selected="*")
    }
    
    # Factors
    factor_map <- c(q1 = "Q1_Party", q2 = "Q2_Support", q3 = "Q3_Votelikelihood", q4 = "Q4_Issue")
    for (id in names(factor_map)) {
      col <- factor_map[[id]]
      vals <- if (col %in% names(df)) sort(unique(na.omit(df[[col]]))) else character(0)
      updatePickerInput(session, id, choices = c("*"="*", vals), selected="*")
    }
    if ("Q1_Party" %in% names(df)) {
      parties <- sort(unique(na.omit(df$Q1_Party)))
      updatePickerInput(session, "analysis_party", choices = parties,
                        selected = if (length(parties)) parties[1] else "")
    }
    
    shinyjs::enable("run_query")
    showNotification(sprintf("Loaded %s rows from %s.", format(nrow(df), big.mark = ","), base_table()),
                     type = "message", duration = 5)
  })
  
  # ========== FINAL SQL (schema-aware for weekday/time column) ==========
  final_sql <- eventReactive(input$run_query, {
    validate(need(!is.null(location_data()), "Load location data first."))
    
    # Detect available columns from current base table
    schema_df <- dbGetQuery(pool, glue("SELECT * FROM {base_table()} LIMIT 0"))
    cols_present <- names(schema_df)
    has <- function(x) x %in% cols_present
    
    # Column detection
    weekday_col <- if (has("weekday")) "weekday" else NA_character_
    time_col <- dplyr::case_when(
      has("timeofDay")   ~ "timeofDay",
      has("timeofday")   ~ "timeofday",
      has("time_period") ~ "time_period",
      has("timeOfDay")   ~ "timeOfDay",
      TRUE               ~ NA_character_
    )
    
    # WHERE clauses
    clauses <- list()
    loc_where <- location_scope_sql()
    if (!is.null(loc_where) && nzchar(loc_where)) {
      clauses <- append(clauses, sub("^WHERE\\s+", "", loc_where))
    }
    if (!is.null(input$start_date)) clauses <- append(clauses, glue_sql("survey_date >= {input$start_date}", .con = pool))
    if (!is.null(input$end_date))   clauses <- append(clauses, glue_sql("survey_date <= {input$end_date}", .con = pool))
    if (!is_all(input$weekday) && !is.na(weekday_col)) {
      clauses <- append(clauses, glue_sql(SQL(paste0(weekday_col, " IN ({input$weekday*})")), .con = pool))
    }
    if (!is_all(input$tod) && !is.na(time_col)) {
      clauses <- append(clauses, glue_sql(SQL(paste0(time_col, " IN ({input$tod*})")), .con = pool))
    }
    if (!is_all(input$q1) && has("Q1_Party"))           clauses <- append(clauses, glue_sql("Q1_Party IN ({input$q1*})", .con = pool))
    if (!is_all(input$q2) && has("Q2_Support"))         clauses <- append(clauses, glue_sql("Q2_Support IN ({input$q2*})", .con = pool))
    if (!is_all(input$q3) && has("Q3_Votelikelihood"))  clauses <- append(clauses, glue_sql("Q3_Votelikelihood IN ({input$q3*})", .con = pool))
    if (!is_all(input$q4) && has("Q4_Issue"))           clauses <- append(clauses, glue_sql("Q4_Issue IN ({input$q4*})", .con = pool))
    
    where_sql <- if (length(clauses)) paste("WHERE", paste(clauses, collapse = " AND ")) else ""

    # Build SELECT field list only with existing columns
    select_fields <- c("oa21cd", "survey_date")
    if (!is.na(weekday_col)) select_fields <- c(select_fields, weekday_col)
    if (!is.na(time_col))    select_fields <- c(select_fields, time_col)
    if (has("Q1_Party"))           select_fields <- c(select_fields, "Q1_Party")
    if (has("Q2_Support"))         select_fields <- c(select_fields, "Q2_Support")
    if (has("Q3_Votelikelihood"))  select_fields <- c(select_fields, "Q3_Votelikelihood")
    if (has("Q4_Issue"))           select_fields <- c(select_fields, "Q4_Issue")
    if (has("Response"))           select_fields <- c(select_fields, "Response")
    
    # 🔧 The key fix: compute the clause outside glue, no backticks
    select_clause <- paste(select_fields, collapse = ", ")
    
    glue("
    SELECT {select_clause}
    FROM {base_table()}
    {where_sql}
  ")
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
    
    # Apply time filters in-memory, using detected time column if present
    time_col <- dplyr::case_when(
      "timeofDay"   %in% names(df) ~ "timeofDay",
      "timeofday"   %in% names(df) ~ "timeofday",
      "time_period" %in% names(df) ~ "time_period",
      "timeOfDay"   %in% names(df) ~ "timeOfDay",
      TRUE ~ NA_character_
    )
    
    if (!is.null(input$start_date)) df <- df %>% filter(as.Date(survey_date) >= as.Date(input$start_date))
    if (!is.null(input$end_date))   df <- df %>% filter(as.Date(survey_date) <= as.Date(input$end_date))
    if (!is_all(input$weekday) && "weekday" %in% names(df)) df <- df %>% filter(weekday %in% input$weekday)
    if (!is_all(input$tod)     && !is.na(time_col))         df <- df %>% filter(.data[[time_col]] %in% input$tod)
    
    # Factor filters (optional & guarded)
    if (!is_all(input$q1) && "Q1_Party" %in% names(df))           df <- df %>% filter(Q1_Party %in% input$q1)
    if (!is_all(input$q2) && "Q2_Support" %in% names(df))         df <- df %>% filter(Q2_Support %in% input$q2)
    if (!is_all(input$q3) && "Q3_Votelikelihood" %in% names(df))  df <- df %>% filter(Q3_Votelikelihood %in% input$q3)
    if (!is_all(input$q4) && "Q4_Issue" %in% names(df))           df <- df %>% filter(Q4_Issue %in% input$q4)
    
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
    
    # Flags
    # success always uses Q2; pers prefers Q3, falls back to Q2 if Q3 missing
    df <- df %>%
      mutate(
        success = ifelse(
          Q1_Party == input$analysis_party &
            Q2_Support %in% input$analysis_support_levels, 1, 0
        ),
        # Use Q3 if present; otherwise fall back to Q2 ("lean_to","none")
        pers = if ("Q3_Votelikelihood" %in% names(df)) {
          ifelse(Q3_Votelikelihood %in% input$analysis_persuasion_levels_q3, 1, 0)
        } else {
          ifelse(Q2_Support %in% c("lean_to","none"), 1, 0)
        }
      )
    
    # OA-level summaries (time-decayed)
    grp <- df %>%
      group_by(oa21cd) %>%
      summarise(
        n_raw   = n(),                               # NEW: unweighted count
        n_eff   = sum(._w, na.rm = TRUE),
        s_eff   = sum(._w * success, na.rm = TRUE),
        pers_eff= sum(._w * pers,    na.rm = TRUE),
        rate_raw= ifelse(n_eff > 0, s_eff / n_eff, NA_real_),
        .groups = "drop"
      ) %>%
      filter(n_eff >= input$analysis_min_n)
    
    validate(need(nrow(grp) > 0, "No OAs meet the minimum effective sample size."))
    
    # EB prior & smoothed rate
    p0 <- if (sum(grp$n_eff) > 0) sum(grp$s_eff) / sum(grp$n_eff) else 0.5
    v  <- 0.01
    k  <- max(p0 * (1 - p0) / v - 1, 1)
    alpha0 <- p0 * k; beta0 <- (1 - p0) * k
    
    grp <- grp %>%
      mutate(
        rate_eb = (s_eff + alpha0) / (n_eff + alpha0 + beta0)
      )
    
    # ---- JOIN POPULATION (ed_complete_20250720.population) ----
    oa_pops <- dbGetQuery(pool, "SELECT oa21cd, population FROM ed_complete_20250720")
    # Ensure types & de-dup
    oa_pops <- dbGetQuery(pool, glue_sql("
          SELECT oa21cd, population
          FROM {`ed_lookup_table()`}
        ", .con = pool))
    grp <- left_join(grp %>% mutate(oa21cd = as.character(oa21cd)),
                     oa_pops %>% mutate(oa21cd = as.character(oa21cd)),
                     by = "oa21cd")
    
    # Notify if many missing populations
    missing_pop <- sum(is.na(grp$population))
    if (missing_pop > 0) {
      showNotification(sprintf("Note: %d OA(s) missing population; expected supporters will be NA for those.", missing_pop),
                       type = "warning", duration = 6)
    }
    
    # ---- EXPECTED SUPPORTERS (population-scaled) ----
    grp <- grp %>%
      mutate(
        expected_supporters_raw = ifelse(!is.na(population), rate_raw * population, NA_real_),
        expected_supporters_eb  = ifelse(!is.na(population), rate_eb  * population, NA_real_)
      )
    
    # Which metric to rank by
    metric <- switch(input$analysis_metric,
                     rate_eb                 = grp$rate_eb,
                     expected_supporters     = grp$expected_supporters_eb,  # EB pop‑scaled as default
                     persuasion_pool         = grp$pers_eff,
                     expected_supporters_raw = grp$expected_supporters_raw)  # optional new choice
    grp$metric <- metric
    
    grp %>%
      arrange(desc(metric)) %>%
      mutate(rank = row_number()) %>%
      select(rank, oa21cd, population, n_raw, n_eff, s_eff, rate_raw, rate_eb,
             expected_supporters_raw, expected_supporters_eb, pers_eff, metric)
  })
  
  output$oa_ranking <- DT::renderDT({
    req(ranking())
    datatable(
      ranking(),
      options = list(pageLength = 20, order = list(list(0, "asc"))),
      rownames = FALSE
    ) %>%
      formatRound(c("population","n_eff","s_eff","pers_eff",
                    "expected_supporters_raw","expected_supporters_eb","metric"), digits = 1) %>%
      formatPercentage("rate_raw", 1) %>%
      formatPercentage("rate_eb", 1)
  })
  
  output$plot_top_oas <- renderPlot({
    req(ranking())
    topn <- head(ranking(), 15)
    ylab <- switch(input$analysis_metric,
                   rate_eb                  = "Support rate (EB)",
                   expected_supporters      = "Expected supporters (EB, pop‑scaled)",
                   expected_supporters_raw  = "Expected supporters (raw, pop‑scaled)",
                   persuasion_pool          = "Persuasion pool (weighted)")
    ggplot(topn, aes(x = reorder(oa21cd, metric), y = metric)) +
      geom_col() +
      coord_flip() +
      labs(x = "OA21 code", y = ylab, title = "Top OAs")
  })
  
  # Heatmap: Support rate by weekday x time
  output$plot_heatmap <- renderPlot({
    df <- analysis_df()
    req(nrow(df) > 0, input$analysis_party != "")
    
    # Ensure weekdays in the right order
    weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    # Mark "success" based on Q1/Q2
    df2 <- df %>%
      mutate(
        responded = ifelse(!is.na(Q1_Party) & !is.na(Q2_Support), 1, 0),
        success = ifelse(
          responded == 1 &
            Q1_Party == input$analysis_party &
            Q2_Support %in% input$analysis_support_levels,
          1, 0
        )
      ) %>%
      filter(!is.na(weekday), !is.na(timeofDay)) %>%
      mutate(
        weekday = factor(weekday, levels = weekday_levels),
        timeofDay = factor(timeofDay, levels = c("morning", "afternoon", "evening"))
      ) %>%
      group_by(weekday, timeofDay) %>%
      summarise(
        total_ops = n(),
        n_responses = sum(responded, na.rm = TRUE),
        s_eff = sum(._w * success, na.rm = TRUE),
        n_eff = sum(._w * responded, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total_ops > 0)
    
    if (nrow(df2) == 0) return(NULL)
    
    # Bayesian prior
    prior <- compute_eb_prior(df2$s_eff, df2$n_eff)
    
    df2 <- df2 %>%
      mutate(
        rate_eb = (s_eff + prior$alpha0) / (n_eff + prior$alpha0 + prior$beta0),
        response_rate = n_responses / total_ops
      )
    
    ggplot(df2, aes(x = timeofDay, y = weekday, fill = rate_eb)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "red", mid = "white", high = "blue",
        midpoint = 0.5, limits = c(0, 1),
        name = "Support rate (EB)"
      ) +
      geom_text(aes(label = sprintf("%.0f%%\nResp: %.0f%%", rate_eb * 100, response_rate * 100)),
                size = 3) +
      labs(
        x = "Time of day",
        y = "Weekday",
        title = "Best times to canvass (selected party)",
        subtitle = "Values show EB support rate and overall response rate"
      ) +
      theme_minimal()
  })
  
  # Heatmap: Response rate by weekday x time (if Response column exists)
  output$plot_response_heatmap <- renderPlot({
    df <- analysis_df()
    req(nrow(df) > 0)
    
    # Need Response column to compute response rate
    if (!("Response" %in% names(df))) return(NULL)
    
    # Detect time-of-day column
    time_col <- dplyr::case_when(
      "timeofDay"   %in% names(df) ~ "timeofDay",
      "timeofday"   %in% names(df) ~ "timeofday",
      "time_period" %in% names(df) ~ "time_period",
      "timeOfDay"   %in% names(df) ~ "timeOfDay",
      TRUE ~ NA_character_
    )
    req(!is.na(time_col))
    req("weekday" %in% names(df))
    
    # Coerce Response to 0/1 robustly
    if (!is.numeric(df$Response)) {
      df$Response <- ifelse(
        tolower(as.character(df$Response)) %in% c("1","y","yes","true","t","responded"),
        1, 0
      )
    }
    
    # Order axes
    wk_levels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    df$weekday <- factor(as.character(df$weekday), levels = wk_levels, ordered = TRUE)
    if (all(unique(na.omit(df[[time_col]])) %in% c("morning","afternoon","evening"))) {
      df[[time_col]] <- factor(df[[time_col]],
                               levels = c("morning","afternoon","evening"),
                               ordered = TRUE)
    }
    
    # Aggregate to weekday x time with per-attempt denominators (weighted by recency)
    agg <- df %>%
      dplyr::filter(!is.na(weekday), !is.na(.data[[time_col]])) %>%
      dplyr::group_by(weekday, .data[[time_col]]) %>%
      dplyr::summarise(
        n_attempts = sum(._w, na.rm = TRUE),             # all attempts (response or not)
        n_resp     = sum(._w * Response, na.rm = TRUE),  # responded
        resp_rate  = dplyr::if_else(n_attempts > 0, n_resp / n_attempts, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(resp_rate))
    
    if (!nrow(agg)) return(NULL)
    
    ggplot(agg, aes(x = .data[[time_col]], y = weekday, fill = resp_rate)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(
        colours = c("#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf", "#2166ac"),
        limits = c(0, 1), name = "Response rate"
      ) +
      geom_text(aes(label = sprintf("%.0f%%", resp_rate * 100)), size = 3) +
      labs(
        x = "Time of day",
        y = "Weekday",
        title = "Response rate by weekday × time period"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)