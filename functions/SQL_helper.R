library(DBI)
library(RSQLite)
library(pool)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
now_iso <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

ALLOWED_VIEWERS <- c("Adrian", "Prajna")

# --- Feature flag (as before) ---
sys <- tolower(Sys.info()[["sysname"]] %||% .Platform$OS.type)
is_linux <- identical(sys, "linux")
resolve_bool <- function(x) {
  xl <- tolower(x %||% "")
  if (xl %in% c("1","true","yes","on"))  return(TRUE)
  if (xl %in% c("0","false","no","off")) return(FALSE)
  NA
}
env_flag <- resolve_bool(Sys.getenv("NOTES_ENABLED", "auto"))
NOTES_ENABLED <- if (is.na(env_flag)) is_linux else env_flag

DB_PATH <- "/srv/shiny-app-data/user_feedback/cif_notes.db"

current_user <- function(session) {
  u <- tryCatch(session$user, error = function(e) NULL)
  if (!is.null(u) && !is.na(u) && nzchar(u)) return(u)
  NA_character_
}

pool <- NULL

# Utility to check if a column exists
has_column <- function(tbl, col) {
  info <- DBI::dbGetQuery(pool, sprintf("PRAGMA table_info(%s);", dbQuoteIdentifier(pool, tbl)))
  tolower(col) %in% tolower(info$name)
}

init_db <- function() {
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS notes (
      id           INTEGER PRIMARY KEY,
      submitted_at TEXT    NOT NULL,
      user_label   TEXT,
      note         TEXT    NOT NULL
    );
  ")
  # Add email column if it doesn't exist yet
  if (!has_column("notes", "email")) {
    dbExecute(pool, "ALTER TABLE notes ADD COLUMN email TEXT;")
  }
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_notes_submitted_at ON notes(submitted_at DESC);")
  dbExecute(pool, 'PRAGMA journal_mode=WAL;')
  dbExecute(pool, 'PRAGMA synchronous=NORMAL;')
}

if (NOTES_ENABLED) {
  pool <- dbPool(drv = RSQLite::SQLite(), dbname = DB_PATH)
  init_db()
  onStop(function() poolClose(pool))
}

# --- Thin wrappers so the app stays clean ---
db_insert_note <- function(submitted_at, user_label, email, note) {
  if (!NOTES_ENABLED) return(invisible(FALSE))
  DBI::dbExecute(
    pool,
    "INSERT INTO notes (submitted_at, user_label, email, note) VALUES (?, ?, ?, ?);",
    params = list(submitted_at, user_label, email %||% "", note)
  )
  invisible(TRUE)
}

db_get_all_notes <- function() {
  if (!NOTES_ENABLED) return(data.frame())
  DBI::dbGetQuery(pool, "SELECT id, submitted_at, user_label, email, note
                         FROM notes
                         ORDER BY submitted_at DESC;")
}

db_count_notes <- function() {
  if (!NOTES_ENABLED) return(0L)
  res <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM notes;")
  as.integer(res$n[[1]])
}
