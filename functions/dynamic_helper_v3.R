# ============================================================================ #
#  dynamic-helper.R – build UI elements from ASP_input_parameters xlsx rows    #
#  v3: unified “show-all only when both groups empty”; future-ready rotation   #
# ============================================================================ #

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

# Small JS helper builders ----------------------------------------------------
js_show_none <- function(prefix) {
  # true iff no input starting with prefix is checked
  sprintf(
    "Object.keys(input).filter(k => k.startsWith('%s')).every(k => input[k] === false)",
    prefix
  )
}
js_any_checked <- function(prefix) {
  # true iff at least one input starting with prefix is checked
  sprintf(
    "Object.keys(input).some(k => k.startsWith('%s') && input[k] === true)",
    prefix
  )
}

# Build per-row JS condition for a tag group ---------------------------------
# Rule:
# - If the group has no selections globally -> wildcard (passes).
# - Else (group has selections) -> row must match at least one selected tag in that group.
# - If row has no tags for that group -> it fails when group has selections.
group_condition <- function(row_tags, prefix) {
  row_tags <- trimws(row_tags %||% "")
  vec <- trimws(unlist(strsplit(row_tags, ";|,")))
  vec <- vec[vec != "" & !is.na(vec)]
  
  none_selected <- js_show_none(prefix)
  
  if (length(vec)) {
    row_match <- paste(sprintf("input['%s_%s']", prefix, sanitize_id(vec)), collapse = " || ")
    sprintf("(%s) || (%s)", none_selected, row_match)
  } else {
    # Row has no tags → only pass when nothing is selected in this group
    sprintf("(%s)", none_selected)
  }
}

# Wrap a UI object with Expertise/Crop/Rotation combined visibility -----------
wrap_with_filters <- function(ui_obj, expertise_tags, crop_tags, rotation_tags) {
  cats_cond  <- group_condition(expertise_tags, "cat")
  crops_cond <- group_condition(crop_tags,      "crop")
  # rotation is future-ready; harmless if you never render rot_ checkboxes
  rot_cond   <- group_condition(rotation_tags,  "rot")
  
  js_cond <- sprintf("(%s) && (%s) && (%s)", cats_cond, crops_cond, rot_cond)
  conditionalPanel(js_cond, ui_obj)
}

# -----------------------------------------------------------------------------
# Main factory for row → UI
# -----------------------------------------------------------------------------
create_ui_element <- function(row) {
  
  # ---------- quick check for layout-only rows -------------------------------
  special <- tolower(trimws(row[["ui_type"]]))
  if (special %in% c("header4", "horizontal line", "break")) {
    
    ui_obj <- switch(
      special,
      "header4"         = h4(row[["name"]] %||% ""),
      "horizontal line" = tags$hr(),
      "break"           = tags$br()
    )
    
    # Apply the same combined filter logic to layout rows:
    expertise_tags <- row[["Expertise"]] %||% ""
    crop_tags      <- row[["Crop"]] %||% ""
    rotation_tags  <- row[["Crop_rotation"]] %||% ""
    
    return(wrap_with_filters(ui_obj, expertise_tags, crop_tags, rotation_tags))
  }
  
  # ---------- logic for real input widgets -----------------------------------
  input_id      <- row[["variable"]]
  input_alias   <- row[["name"]]
  label         <- row[["description"]]
  min_val       <- suppressWarnings(as.numeric(row[["lower"]]))
  max_val       <- suppressWarnings(as.numeric(row[["upper"]]))
  distr         <- row[["distribution"]]
  ui_type       <- row[["ui_type"]]
  ui_step       <- suppressWarnings(as.numeric(row[["ui_steps"]]))
  ui_cond       <- row[["ui_conditional"]]
  ui_cond_nam   <- row[["ui_conditional_name"]]
  ui_opt_nam    <- row[["ui_option_name"]]
  ui_opt_var    <- row[["ui_option_variable"]]
  
  expertise_tags <- row[["Expertise"]] %||% ""
  crop_tags      <- row[["Crop"]] %||% ""
  rotation_tags  <- row[["Crop_rotation"]] %||% ""
  
  # sensible defaults if numeric fields are missing
  if (is.na(min_val)) min_val <- 0
  if (is.na(max_val)) max_val <- 1
  if (is.na(ui_step)) ui_step <- (max_val - min_val) / 100
  
  default        <- min_val
  default_2side  <- c(min_val, max_val)
  
  if (identical(distr, "posnorm")) {
    min_val <- min_val - abs(min_val) * 0.5
    if (min_val <= 0) min_val <- 0.0001
    max_val <- max_val + abs(max_val) * 0.5
  } else if (identical(distr, "tnorm_0_1")) {
    # never use exact 0 or 1 — avoids qnorm/qlogis infinities
    eps <- 1e-6
    min_val <- eps
    max_val <- 1 - eps
    
    # min_val <- 0
    # max_val <- 1
  } else {
    min_val <- min_val - abs(min_val) * 0.5
    max_val <- max_val + abs(max_val) * 0.5
  }
  
  ui_cond <- if (!is.null(ui_cond)) as.logical(ui_cond) else FALSE
  
  if (!is.null(ui_type) && startsWith(ui_type, "select")) {
    ui_opt_nam  <- unlist(strsplit(ui_opt_nam %||% "",  " next_option "))
    ui_opt_var  <- unlist(strsplit(ui_opt_var %||% "",  " next_option "))
    names(ui_opt_var) <- ui_opt_nam
  }
  
  label_with_tooltip <- function(input_id, input_alias, tooltip) {
    tags$label(`for` = input_id,
               title = tooltip,
               style = "font-weight: bold; cursor: help;",
               input_alias)
  }
  
  real_ui <- switch(
    ui_type,
    "header"  = tagList(
      tags$span(style="font-size:120%;font-weight:bold;", label),
      br(), br()),
    "slider1" = tagList(
      label_with_tooltip(input_id, input_alias, label),
      sliderInput(input_id, NULL, min_val, max_val, default, step = ui_step)
    ),
    "slider2" = tagList(
      label_with_tooltip(input_id, input_alias, label),
      sliderInput(input_id, NULL, min_val, max_val, default_2side, step = ui_step)
    ),
    "numeric" = tagList(
      label_with_tooltip(input_id, input_alias, label),
      numericInput(input_id, NULL, default, step = ui_step)
    ),
    "select"  = tagList(
      label_with_tooltip(input_id, input_alias, label),
      selectInput(input_id, NULL, choices = ui_opt_var)
    ),
    "select2" = tagList(
      label_with_tooltip(input_id, input_alias, label),
      selectInput(input_id, NULL, choices = ui_opt_var, multiple = TRUE)
    ),
    "checkbox" = tagList(
      checkboxInput(
        input_id,
        label_with_tooltip(input_id, input_alias, label),
        value = as.logical(default)
      )
    ),
    # fallback
    tagList(
      label_with_tooltip(input_id, input_alias, label),
      textInput(input_id, NULL, "")
    )
  )
  
  if (ui_cond) {
    toggle_id <- paste0(input_id, "_toggle")
    real_ui <- tagList(
      checkboxInput(toggle_id, ui_cond_nam %||% "Show", FALSE),
      conditionalPanel(sprintf("input['%s']", toggle_id), real_ui)
    )
  }
  
  # Apply the unified filter wrapper to all "real" elements
  wrap_with_filters(real_ui, expertise_tags, crop_tags, rotation_tags)
}

# -----------------------------------------------------------------------------
# Helpers for filter UI (to be used in server) --------------------------------
# -----------------------------------------------------------------------------

# # all categories (Expertise) across every sheet
# categories <- function(excelData) {
#   cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
#   cats <- cats[!is.na(cats) & cats != ""]
#   trimws(unique(unlist(strsplit(cats, ";|,"))))
# }
# 
# # all crops across every sheet
# crop_categories <- function(excelData) {
#   crop_cats <- unique(unlist(lapply(excelData(), function(df) df$Crop)))
#   crop_cats <- crop_cats[!is.na(crop_cats) & crop_cats != ""]
#   trimws(unique(unlist(strsplit(crop_cats, ";|,"))))
# }
# 
# # all crop rotations across every sheet (optional / future)
# rotation_categories <- function(excelData) {
#   rot <- unique(unlist(lapply(excelData(), function(df) df$Crop_rotation)))
#   rot <- rot[!is.na(rot) & rot != "" & tolower(trimws(rot)) != "na"]
#   trimws(unique(unlist(strsplit(rot, ";|,"))))
# }

# -----------------------------------------------------------------------------
# Example snippets you can drop into server -----------------------------------
# -----------------------------------------------------------------------------
# output$category_filter_ui <- renderUI({
#   cats <- categories(excelData)
#   if (length(cats) == 0) return(NULL)
#   tagList(lapply(cats, function(cat){
#     checkboxInput(paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
#   }))
# })
#
# output$crop_filter_ui <- renderUI({
#   crops <- crop_categories(excelData)
#   if (length(crops) == 0) return(NULL)
#   tagList(lapply(crops, function(c){
#     checkboxInput(paste0("crop_", sanitize_id(c)), c, value = FALSE)
#   }))
# })
#
# # Optional, only if you want to expose rotation filters later:
# output$rotation_filter_ui <- renderUI({
#   rots <- rotation_categories(excelData)
#   if (length(rots) == 0) return(NULL)
#   tagList(lapply(rots, function(r){
#     checkboxInput(paste0("rot_", sanitize_id(r)), r, value = FALSE)
#   }))
# })
#
# # Example to render a sheet’s rows:
# output$dynamic_ui <- renderUI({
#   sheet_df <- excelData()[["SomeSheetName"]]
#   tagList(lapply(seq_len(nrow(sheet_df)), function(i) create_ui_element(sheet_df[i, ])))
# })
