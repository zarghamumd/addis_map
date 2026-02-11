
library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)

# Load data from the same folder as app.R
woreda_yearly_shp <- readRDS("woreda_yearly_shp.rds")
subcity_boundaries <- readRDS("subcity_boundaries.rds")
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
woreda_yearly_shp <- st_transform(woreda_yearly_shp, 4326)
subcity_boundaries <- st_transform(subcity_boundaries, 4326)

# ---- RENAME COLUMNS ----
rename_cols_safe <- function(df, ren) {
  stopifnot(is.character(ren), !is.null(names(ren)))
  
  old <- names(ren)
  new <- unname(ren)
  
  missing <- setdiff(old, names(df))
  if (length(missing)) stop("These columns are missing in data: ", paste(missing, collapse = ", "))
  
  final_names <- names(df)
  final_names[match(old, final_names)] <- new
  if (any(duplicated(final_names))) {
    dups <- unique(final_names[duplicated(final_names)])
    stop("Renaming creates duplicate column names: ", paste(dups, collapse = ", "))
  }
  
  names(df) <- final_names
  df
}

# old = "new"
rename_map <- c(
  "n_tins" = "Number of Firms",
  "total_payroll" = "Sum of Payroll Amount",
  "avg_payroll"    = "Avg Payroll Per Firm",
  "median_payroll" = "Median Payroll per Firm",
  "total_headcount"    = "Total Number of Workers",
  "avg_headcount"  = "Avg Workers per Firm",
  "median_headcount" = "Median Number of Workers per Frim", 
  "avg_salary" = "Avg Salary per Worker",
  "median_salary" = "Median Salary per Worker",
  "p25_salary" = "25th Percentile Salary per Worker",
  "p75_salary" = "75th Percentile Salary per Worker")

woreda_yearly_shp <- rename_cols_safe(woreda_yearly_shp, rename_map)

exclude_cols <- c("FID_1","OBJECTID","Sub_City","Woreda","Shape_Le_1","Shape_Area",
                  "Region","subcity_raw","woreda_raw","subcity_clean.x","woreda_num.x",
                  "woreda_str","woreda_id","subcity_clean.y","woreda_num.y","year","n_months",
                  "p25_headcount" , "p75_headcount" , "n_records" , "p75_payroll", "p25_payroll", 
                  "avg_amount", "median_amount", "pct_known_tins", "n_months", "total_headcount_rel",
                  "headcount_rel", "headcount_lrel", "total_amount")

num_cols <- names(woreda_yearly_shp)[sapply(st_drop_geometry(woreda_yearly_shp), is.numeric)]
outcomes <- setdiff(num_cols, exclude_cols)

years <- sort(unique(woreda_yearly_shp$year))

cap_q <- c(0.01, 0.99)
lims_by_outcome <- lapply(outcomes, function(v) {
  x <- st_drop_geometry(woreda_yearly_shp)[[v]]
  as.numeric(quantile(x, probs = cap_q, na.rm = TRUE))
})
names(lims_by_outcome) <- outcomes

ui <- fluidPage(
  titlePanel("Addis Ababa — Woreda maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", "Outcome", choices = outcomes, selected = outcomes[1]),
      selectInput("year_dd", "Year (jump to)", choices = years, selected = years[1]),
      sliderInput(
        "year_idx", "Year (play)",
        min = 1, max = length(years), value = 1, step = 1,
        animate = animationOptions(interval = 1200, loop = TRUE)
      ),
      hr(),
      checkboxInput("use_full_range", "Use full range (min/max) instead of capped scale", value = FALSE)
    ),
    mainPanel(plotOutput("map", height = "580px"))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$year_dd, {
    idx <- match(as.integer(input$year_dd), years)
    if (!is.na(idx) && idx != input$year_idx) {
      updateSliderInput(session, "year_idx", value = idx)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$year_idx, {
    sel <- years[input$year_idx]
    if (!identical(as.character(input$year_dd), as.character(sel))) {
      shiny::freezeReactiveValue(input, "year_dd")   # <- key line
      updateSelectInput(session, "year_dd", selected = sel)
    }
  }, ignoreInit = TRUE)
  
  output$map <- renderPlot({
    yr <- years[input$year_idx]
    var <- input$outcome
    
    df_year <- woreda_yearly_shp %>% filter(year == yr)
    
    lims <- if (isTRUE(input$use_full_range)) {
      x <- st_drop_geometry(woreda_yearly_shp)[[var]]
      range(x, na.rm = TRUE)
    } else {
      lims_by_outcome[[var]]
    }
    
    subcity_lwd_fixed <- 0.15
    
    ggplot(df_year) +
      geom_sf(aes(fill = .data[[var]]), color = "grey35", linewidth = 0.08) +
      geom_sf(data = subcity_boundaries, fill = NA, color = "black", linewidth = subcity_lwd_fixed) +
      scale_fill_gradient(
        low = "#f7fbff", high = "#08519c",
        limits = lims, oob = squish,
        name = var,
        labels = comma,
        breaks = pretty_breaks(n = 6)
      ) +
      coord_sf(datum = NA) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10)
      ) +
      ggtitle(paste0(var, " — ", yr))
  }, res = 120)
}

shinyApp(ui, server)