# app.R
# -------------------------------------------------------------------
# Canvassing Analytics Dashboard  (v0.0.0.4)
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

APP_VERSION <- "0.0.0.4"

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

# ----- ED mapping (UPDATED) -----
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
      # NEW: training toggle
      checkboxInput("use_training", "Use training table only (responses_t) and select by OA only", value = FALSE),
      hr(),
      h4("1) Location"),
      # ED controls (disabled when training toggle is on)
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
      actionButton("run_query", "Retrieve data", class = "btn-primary")
    ),
    mainPanel(
      h4("Results"),
      verbatimTextOutput("sql_preview"),
      tableOutput("summary"),
      DTOutput("rows")
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
  
  # Toggle behavior: when training mode is ON, disable ED controls and load OA list from responses_t
  observeEvent(input$use_training, {
    if (isTRUE(input$use_training)) {
      shinyjs::disable("ed_type"); shinyjs::disable("ed_names")
      # OA list from responses_t
      sql <- glue("SELECT DISTINCT oa21cd FROM {base_table()} WHERE oa21cd IS NOT NULL ORDER BY oa21cd")
      oa_df <- dbGetQuery(pool, sql)
      updatePickerInput(session, "oa_codes",
                        choices = c("*"="*", setNames(oa_df$oa21cd, oa_df$oa21cd)),
                        selected="*")
    } else {
      shinyjs::enable("ed_type"); shinyjs::enable("ed_names")
      updatePickerInput(session, "oa_codes", choices = c("*"="*"), selected="*")
      # repopulate EDs for current type if not "*"
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
    cols <- ed_cols(); validate(need(!is.null(cols), "Unknown ED type"))
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
  
  # OA list when EDs change (normal mode)
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
  
  # Mandatory: load location data
  observeEvent(input$load_location, {
    clauses <- list()
    
    if (isTRUE(input$use_training)) {
      # Training mode: OA-only from responses_t
      if (!is_all(input$oa_codes)) {
        clauses <- append(clauses, glue_sql("oa21cd IN ({input$oa_codes*})", .con = pool))
      }
    } else {
      # Normal mode: OAs from EDs, optionally narrowed by explicit OAs
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
    
    sql <- glue("
      SELECT *
      FROM {base_table()}
      {where_sql}
    ")
    df <- dbGetQuery(pool, sql)
    location_data(df)
    
    # Auto-update date inputs
    if ("survey_date" %in% names(df) && nrow(df) > 0) {
      min_date <- suppressWarnings(min(df$survey_date, na.rm = TRUE))
      max_date <- suppressWarnings(max(df$survey_date, na.rm = TRUE))
      updateDateInput(session, "start_date", value = min_date)
      updateDateInput(session, "end_date", value = max_date)
    }
    
    # Update dependent dropdowns from loaded data
    wk <- sort(unique(na.omit(df$weekday)))
    updatePickerInput(session, "weekday", choices = c("*"="*", wk), selected="*")
    
    set_choices <- function(col, inputId) {
      vals <- sort(unique(na.omit(df[[col]])))
      updatePickerInput(session, inputId, choices = c("*"="*", vals), selected="*")
    }
    set_choices("Q1_Party", "q1")
    set_choices("Q2_Support", "q2")
    set_choices("Q3_Votelikelihood", "q3")
    set_choices("Q4_Issue", "q4")
    
    shinyjs::enable("run_query")
    showNotification(
      sprintf("Loaded %s rows from %s.", format(nrow(df), big.mark = ","), base_table()),
      type = "message", duration = 5
    )
  })
  
  # Final SQL (uses base_table + saved location scope + time + factors)
  final_sql <- eventReactive(input$run_query, {
    validate(need(!is.null(location_data()), "Load location data first."))
    clauses <- list()
    loc_where <- location_scope_sql()
    if (!is.null(loc_where) && nzchar(loc_where)) {
      clauses <- append(clauses, sub("^WHERE\\s+", "", loc_where))
    }
    if (!is.null(input$start_date)) clauses <- append(clauses, glue_sql("survey_date >= {input$start_date}", .con = pool))
    if (!is.null(input$end_date))   clauses <- append(clauses, glue_sql("survey_date <= {input$end_date}",   .con = pool))
    if (!is_all(input$weekday))     clauses <- append(clauses, glue_sql("weekday IN ({input$weekday*})", .con = pool))
    if (!is_all(input$tod))         clauses <- append(clauses, glue_sql("timeOfDay IN ({input$tod*})",   .con = pool))
    if (!is_all(input$q1)) clauses <- append(clauses, glue_sql("Q1_Party IN ({input$q1*})", .con = pool))
    if (!is_all(input$q2)) clauses <- append(clauses, glue_sql("Q2_Support IN ({input$q2*})", .con = pool))
    if (!is_all(input$q3)) clauses <- append(clauses, glue_sql("Q3_Votelikelihood IN ({input$q3*})", .con = pool))
    if (!is_all(input$q4)) clauses <- append(clauses, glue_sql("Q4_Issue IN ({input$q4*})", .con = pool))
    
    where_sql <- if (length(clauses)) paste("WHERE", paste(clauses, collapse = " AND ")) else ""
    glue("
      SELECT
        oa21cd, survey_date, weekday, timeOfDay,
        Q1_Party, Q2_Support, Q3_Votelikelihood, Q4_Issue
      FROM {base_table()}
      {where_sql}
    ")
  })
  
  output$sql_preview <- renderText({ req(final_sql()); as.character(final_sql()) })
  
  results <- eventReactive(input$run_query, {
    dbGetQuery(pool, final_sql())
  })
  
  output$rows <- DT::renderDT({
    req(results())
  }, options = list(pageLength = 25), filter = "top")
  
  output$summary <- renderTable({
    df <- results(); req(nrow(df) > 0)
    data.frame(
      n_rows     = nrow(df),
      from       = suppressWarnings(min(df$survey_date, na.rm = TRUE)),
      to         = suppressWarnings(max(df$survey_date, na.rm = TRUE)),
      unique_OAs = length(unique(df$oa21cd))
    )
  })
}

shinyApp(ui, server)