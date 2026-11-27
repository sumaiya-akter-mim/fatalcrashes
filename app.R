suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly)
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

DATA   <- readRDS("data/fars_app.rds")
POP    <- read.csv("data/census_state_pop_2023.csv")
STATES <- data.frame(state = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))

COL <- list(text = "#0b0b0b", text2 = "#52514e", grid = "#e6e5e0", surface = "#fcfcfb",
            land = "#efeee9", ref = "#8a8984",
            dark = "#2a78d6", daylight = "#eb6834", duskdawn = "#1baf7a", unknown = "#c3c2b7",
            dark_lit = "#2a78d6", dark_unlit = "#184f95", dark_unk = "#86b6ef",
            rural = "#008300", urban = "#6250d6", ped = "#d55181")
SEQ <- c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

hour_lab <- function(h) ifelse(h == 0, "12am", ifelse(h < 12, paste0(h, "am"),
                                                      ifelse(h == 12, "12pm", paste0(h - 12, "pm"))))
pct <- function(x, acc = 1) percent(x, accuracy = acc)
num <- function(x) comma(x)

base_layout <- function(p, ...) {
  p %>% layout(font = list(family = "Helvetica, Arial, sans-serif", color = COL$text, size = 12),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               hoverlabel = list(bgcolor = "white"), ...) %>%
    config(displaylogo = FALSE)
}
ax <- function(title = "", ...) list(title = title, gridcolor = COL$grid, zeroline = FALSE,
                                     linecolor = COL$grid, tickfont = list(color = COL$text2), ...)

graph_card <- function(title, output, caption) {
  card(full_screen = TRUE,
       card_header(title),
       output,
       card_footer(textOutput(caption, inline = TRUE)))
}


ui <- page_navbar(
  title = "Deadly Roads – Fatal Crashes in the USA, 2023",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = COL$dark),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "d3.min.js"),
    tags$script(src = "topojson-client.min.js"),
    tags$script(src = "spikemap.js")
  ),
  sidebar = sidebar(
    selectizeInput("states", "State", choices = sort(unique(DATA$state)), multiple = TRUE,
                   options = list(placeholder = "All states")),
    radioButtons("area", "Area", c("All", "Rural", "Urban"), inline = TRUE),
    sliderInput("months", "Months", min = 1, max = 12, value = c(1, 12), step = 1),
    actionButton("reset", "Reset filters"),
    downloadButton("dl_report", "Download report (PDF)")
  ),
  
  nav_panel("Overview",
            layout_columns(
              value_box("Fatal crashes", textOutput("kpi_crashes")),
              value_box("Deaths", textOutput("kpi_deaths")),
              value_box("Crashes in the dark", textOutput("kpi_dark")),
              value_box("Median EMS wait", textOutput("kpi_ems"))
            ),
            card(card_header("How to use"),
                 p("Main question: how do time of day, darkness and rural or urban location affect fatal crashes in the USA in 2023?"),
                 p("Use the filters on the left. Every graph follows them. Click a state on the map in 'Where' to select it."))
  ),
  
  nav_panel("When",
            graph_card("Graph 1 · Weekday × hour", plotlyOutput("g1"), "c1"),
            graph_card("Graph 2 · Crashes per month by light", plotlyOutput("g2"), "c2"),
            graph_card("Graph 3 · Crashes hour by hour (press Play)", plotlyOutput("g3", height = "520px"), "c3")
  ),
  
  nav_panel("Darkness",
            graph_card("Graph 4 · Light condition: all crashes vs pedestrian crashes", plotlyOutput("g4"), "c4"),
            graph_card("Graph 5 · When in the day: pedestrians vs all", plotlyOutput("g5"), "c5"),
            graph_card("Graph 6 · AI-generated graph: share of crashes in the dark by state",
                       plotOutput("g6", height = "800px"), "c6")
  ),
  
  nav_panel("Where",
            graph_card("Graph 7 · Deaths per 100,000 people (click a state)", plotlyOutput("g7", height = "480px"), "c7"),
            graph_card("Graph 8 · Deaths by county (custom D3.js spike map)",
                       tags$div(id = "spikemap", class = "spikemap"), "c8"),
            graph_card("Graph 9 · Deaths per crash: rural vs urban by road type", plotlyOutput("g9"), "c9"),
            graph_card("Graph 10 · The 15 counties with the most deaths", plotlyOutput("g10", height = "480px"), "c10")
  ),
  
  nav_panel("Help",
            graph_card("Graph 11 · Minutes until EMS arrives: rural vs urban", plotlyOutput("g11"), "c11"),
            graph_card("Graph 12 · Rural share vs median EMS wait, by state", plotlyOutput("g12"), "c12")
  )
)


server <- function(input, output, session) {
  
  filt <- function(use_state = TRUE, use_area = TRUE) {
    d <- DATA[DATA$month >= input$months[1] & DATA$month <= input$months[2], ]
    if (use_state && length(input$states)) d <- d[d$state %in% input$states, ]
    if (use_area && input$area != "All") d <- d[d$area == input$area, ]
    d
  }
  fd         <- reactive(filt(TRUE, TRUE))
  fd_noarea  <- reactive(filt(TRUE, FALSE))
  fd_nostate <- reactive(filt(FALSE, TRUE))
  fd_states  <- reactive(filt(FALSE, FALSE))
  need_rows  <- function(d) validate(need(nrow(d) > 0, "No crashes match these filters."))
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "states", selected = character(0))
    updateRadioButtons(session, "area", selected = "All")
    updateSliderInput(session, "months", value = c(1, 12))
  })
  
  output$dl_report <- downloadHandler(
    filename = function() "DeadlyRoads_Report.pdf",
    content  = function(file) file.copy("www/DeadlyRoads_Report.pdf", file)
  )
  
  output$kpi_crashes <- renderText(num(nrow(fd())))
  output$kpi_deaths  <- renderText(num(sum(fd()$fatals)))
  output$kpi_dark    <- renderText({
    k <- fd()$light_group[fd()$light_group != "Unknown"]
    if (length(k)) pct(mean(k == "Dark")) else "–"
  })
  output$kpi_ems <- renderText({
    m <- median(fd()$ems_response_min, na.rm = TRUE)
    if (is.na(m)) "–" else paste(m, "min")
  })
  
  
  output$g1 <- renderPlotly({
    d <- fd() %>% filter(!is.na(hour)); need_rows(d)
    hm <- d %>% count(weekday, hour) %>%
      complete(weekday, hour = 0:23, fill = list(n = 0)) %>%
      mutate(label = paste0(weekday, ", ", hour_lab(hour), "–", hour_lab((hour + 1) %% 24)))
    plot_ly(hm, x = ~hour, y = ~weekday, z = ~n, type = "heatmap", colors = SEQ,
            xgap = 2, ygap = 2, text = ~label,
            hovertemplate = "%{text}<br><b>%{z} crashes</b><extra></extra>",
            colorbar = list(title = list(text = "Crashes"), thickness = 12)) %>%
      base_layout(xaxis = ax("", tickvals = seq(0, 21, 3), ticktext = hour_lab(seq(0, 21, 3)), showgrid = FALSE),
                  yaxis = ax("", autorange = "reversed", showgrid = FALSE))
  })
  output$c1 <- renderText({
    d <- fd() %>% filter(!is.na(hour)); if (!nrow(d)) return("")
    top <- d %>% count(weekday, hour) %>% slice_max(n, n = 1, with_ties = FALSE)
    sprintf("Busiest hour of the week: %s %s (%s crashes).", top$weekday, hour_lab(top$hour), num(top$n))
  })
  
  
  output$g2 <- renderPlotly({
    d <- fd(); need_rows(d)
    lv <- c("Dark", "Dusk/Dawn", "Daylight")
    cols <- c(Dark = COL$dark, `Dusk/Dawn` = COL$duskdawn, Daylight = COL$daylight)
    m <- d %>% filter(light_group %in% lv) %>%
      count(month_name, light_group) %>%
      complete(month_name, light_group = lv, fill = list(n = 0))
    p <- plot_ly()
    for (g in lv) {
      mg <- m %>% filter(light_group == g)
      p <- p %>% add_trace(data = mg, x = ~month_name, y = ~n, type = "scatter", mode = "lines",
                           stackgroup = "one", name = g, fillcolor = paste0(cols[[g]], "B3"),
                           line = list(color = cols[[g]], width = 1))
    }
    p %>% base_layout(xaxis = ax(""), yaxis = ax("Fatal crashes"),
                      legend = list(orientation = "h"), hovermode = "x unified")
  })
  output$c2 <- renderText({
    d <- fd() %>% filter(light_group != "Unknown"); if (!nrow(d)) return("")
    s <- d %>% group_by(month_name) %>% summarise(share = mean(light_group == "Dark"))
    hi <- s %>% slice_max(share, n = 1, with_ties = FALSE)
    lo <- s %>% slice_min(share, n = 1, with_ties = FALSE)
    sprintf("Share of crashes in the dark: highest in %s (%s), lowest in %s (%s).",
            hi$month_name, pct(hi$share), lo$month_name, pct(lo$share))
  })
  
  
  output$g3 <- renderPlotly({
    a <- fd() %>% filter(!is.na(lat), !is.na(hour), light_group != "Unknown") %>%
      mutate(tip = paste0(county, "<br>", hour_lab(hour), " · ", light_group))
    need_rows(a)
    pad <- expand.grid(hour = 0:23, light_group = c("Dark", "Daylight", "Dusk/Dawn"),
                       stringsAsFactors = FALSE)
    pad$lat <- 0; pad$lon <- 0; pad$tip <- ""
    a2 <- bind_rows(a %>% select(hour, light_group, lat, lon, tip), pad)
    a2$light_group <- factor(a2$light_group, levels = c("Dark", "Daylight", "Dusk/Dawn"))
    plot_ly(a2, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~lon, frame = ~hour,
            color = ~light_group, colors = c(COL$dark, COL$daylight, COL$duskdawn),
            text = ~tip, hoverinfo = "text", marker = list(size = 4.5, opacity = 0.75)) %>%
      base_layout(geo = list(scope = "usa", projection = list(type = "albers usa"),
                             showland = TRUE, landcolor = COL$land, subunitcolor = "#ffffff"),
                  showlegend = TRUE,
                  legend = list(orientation = "h", x = 0, y = 1.05, itemsizing = "constant",
                                title = list(text = "Each dot = one fatal crash. Light at the time: "))) %>%
      animation_opts(frame = 700, transition = 0, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Hour of day: "))
  })
  output$c3 <- renderText({
    d <- fd() %>% filter(!is.na(hour)); if (!nrow(d)) return("")
    h <- d %>% count(hour) %>% slice_max(n, n = 1, with_ties = FALSE)
    sprintf("Dots turn from orange to blue as night falls. Peak hour: %s (%s crashes).",
            hour_lab(h$hour), num(h$n))
  })
  
  
  output$g4 <- renderPlotly({
    d <- fd(); need_rows(d)
    light_levels <- c("Daylight", "Dark - Lighted", "Dark - Not Lighted",
                      "Dark - Unknown Lighting", "Dusk/Dawn", "Other/Unknown")
    light_cols <- c(COL$daylight, COL$dark_lit, COL$dark_unlit, COL$dark_unk, COL$duskdawn, COL$unknown)
    s6 <- d %>% mutate(l5 = case_when(light %in% c("Dusk", "Dawn") ~ "Dusk/Dawn",
                                      light %in% light_levels ~ light, TRUE ~ "Other/Unknown"))
    s6 <- bind_rows(s6 %>% mutate(group = "All crashes"),
                    s6 %>% filter(is_pedestrian) %>% mutate(group = "Pedestrian crashes")) %>%
      count(group, l5) %>% group_by(group) %>% mutate(share = n / sum(n)) %>% ungroup()
    p <- plot_ly()
    for (i in seq_along(light_levels)) {
      si <- s6 %>% filter(l5 == light_levels[i])
      if (!nrow(si)) next
      p <- p %>% add_trace(data = si, x = ~share, y = ~group, type = "bar", orientation = "h",
                           name = light_levels[i],
                           marker = list(color = light_cols[i], line = list(color = COL$surface, width = 2)),
                           text = ~ifelse(share >= 0.08, pct(share), ""), textposition = "inside")
    }
    p %>% base_layout(barmode = "stack",
                      xaxis = ax("", tickformat = ".0%", range = c(0, 1)),
                      yaxis = ax("", autorange = "reversed"),
                      legend = list(orientation = "h"))
  })
  output$c4 <- renderText({
    d <- fd() %>% filter(light_group != "Unknown"); if (!nrow(d)) return("")
    p <- d[d$is_pedestrian, ]
    if (!nrow(p)) return(sprintf("%s of crashes happen in the dark.", pct(mean(d$light_group == "Dark"))))
    sprintf("%s of pedestrian crashes happen in the dark, against %s of all crashes.",
            pct(mean(p$light_group == "Dark")), pct(mean(d$light_group == "Dark")))
  })
  
  
  output$g5 <- renderPlotly({
    dh <- fd() %>% filter(!is.na(hour)); need_rows(dh)
    s5 <- bind_rows(dh %>% count(hour) %>% mutate(group = "All crashes"),
                    dh %>% filter(is_pedestrian) %>% count(hour) %>% mutate(group = "Pedestrian crashes")) %>%
      group_by(group) %>% complete(hour = 0:23, fill = list(n = 0)) %>%
      mutate(share = n / sum(n)) %>% ungroup()
    al <- filter(s5, group == "All crashes")
    pe <- filter(s5, group == "Pedestrian crashes")
    plot_ly() %>%
      add_trace(data = al, x = ~hour, y = ~share, type = "scatter", mode = "lines", name = "All crashes",
                line = list(color = COL$ref, width = 2, dash = "dot")) %>%
      add_trace(data = pe, x = ~hour, y = ~share, type = "scatter", mode = "lines+markers",
                name = "Pedestrian crashes", line = list(color = COL$ped, width = 2.5),
                marker = list(color = COL$ped, size = 8)) %>%
      base_layout(xaxis = ax("", tickvals = seq(0, 21, 3), ticktext = hour_lab(seq(0, 21, 3))),
                  yaxis = ax("Share of the group's crashes", tickformat = ".0%", rangemode = "tozero"),
                  legend = list(orientation = "h"))
  })
  output$c5 <- renderText({
    d <- fd() %>% filter(!is.na(hour)); p <- d[d$is_pedestrian, ]
    if (!nrow(p)) return("")
    night <- function(x) mean(x$hour >= 18 | x$hour < 6)
    sprintf("%s of pedestrian crashes happen between 6pm and 6am (all crashes: %s).",
            pct(night(p)), pct(night(d)))
  })
  
  
  output$g6 <- renderPlot({
    d <- fd(); need_rows(d)
    ai_graph(d)
  }, res = 96)
  output$c6 <- renderText("Made with ChatGPT from the prompt in app/R/ai_graph.R. Blue = the five highest; dashed line = national share.")
  
  
  choro <- reactive({
    fd_nostate() %>% group_by(state) %>%
      summarise(deaths = sum(fatals), crashes = n(), .groups = "drop") %>%
      inner_join(STATES, by = "state") %>% inner_join(POP, by = "state") %>%
      mutate(rate = deaths / pop_2023 * 1e5)
  })
  output$g7 <- renderPlotly({
    s <- choro(); validate(need(nrow(s) > 0, "No crashes match these filters."))
    plot_ly(s, type = "choropleth", locationmode = "USA-states", locations = ~abb, z = ~rate,
            key = ~state, source = "choro", colors = SEQ,
            text = ~paste0("<b>", state, "</b><br>", sprintf("%.1f", rate), " deaths per 100,000<br>",
                           num(deaths), " deaths"),
            hoverinfo = "text", marker = list(line = list(color = "#ffffff", width = 1)),
            colorbar = list(title = list(text = "Deaths per<br>100,000"), thickness = 12)) %>%
      base_layout(geo = list(scope = "usa", projection = list(type = "albers usa"), showlakes = FALSE)) %>%
      event_register("plotly_click")
  })
  observeEvent(suppressWarnings(event_data("plotly_click", source = "choro")), {
    k <- suppressWarnings(event_data("plotly_click", source = "choro"))$key
    if (length(k)) updateSelectizeInput(session, "states", selected = k[[1]])
  })
  output$c7 <- renderText({
    s <- choro(); if (!nrow(s)) return("")
    t <- s %>% arrange(desc(rate)) %>% head(3)
    sprintf("Highest rates: %s per 100,000 people. Click a state to filter all graphs.",
            paste0(t$state, " ", sprintf("%.1f", t$rate), collapse = ", "))
  })
  
  
  observe({
    agg <- fd() %>% filter(!is.na(lat)) %>%
      group_by(county_fips, county) %>%
      summarise(deaths = sum(fatals), crashes = n(),
                lon = mean(lon), lat = mean(lat), .groups = "drop")
    session$sendCustomMessage("spikeData", list(rows = agg))
  })
  output$c8 <- renderText({
    d <- fd(); if (!nrow(d)) return("No crashes match these filters.")
    s <- d %>% group_by(county) %>% summarise(deaths = sum(fatals)) %>% arrange(desc(deaths))
    sprintf("Spike height = deaths. Tallest spike: %s (%s deaths). Scroll to zoom, hover for details.",
            s$county[1], num(s$deaths[1]))
  })
  
  
  dumbbell <- reactive({
    fd_noarea() %>% filter(area %in% c("Rural", "Urban"), road_type != "Unknown") %>%
      group_by(road_type, area) %>%
      summarise(dpc = mean(fatals), n = n(), .groups = "drop") %>%
      pivot_wider(names_from = area, values_from = c(dpc, n)) %>%
      filter(!is.na(dpc_Rural), !is.na(dpc_Urban), n_Rural >= 20, n_Urban >= 20) %>%
      arrange(dpc_Rural)
  })
  output$g9 <- renderPlotly({
    s9 <- dumbbell(); validate(need(nrow(s9) > 0, "Not enough rural and urban crashes for these filters."))
    s9$road_type <- factor(s9$road_type, levels = s9$road_type)
    plot_ly(s9) %>%
      add_segments(x = ~dpc_Urban, xend = ~dpc_Rural, y = ~road_type, yend = ~road_type,
                   line = list(color = COL$grid, width = 4), showlegend = FALSE, hoverinfo = "none") %>%
      add_markers(x = ~dpc_Urban, y = ~road_type, name = "Urban",
                  marker = list(color = COL$urban, size = 11, line = list(color = COL$surface, width = 2)),
                  customdata = ~n_Urban,
                  hovertemplate = "%{y}, urban: %{x:.3f} deaths per crash (%{customdata} crashes)<extra></extra>") %>%
      add_markers(x = ~dpc_Rural, y = ~road_type, name = "Rural",
                  marker = list(color = COL$rural, size = 11, line = list(color = COL$surface, width = 2)),
                  customdata = ~n_Rural,
                  hovertemplate = "%{y}, rural: %{x:.3f} deaths per crash (%{customdata} crashes)<extra></extra>") %>%
      base_layout(xaxis = ax("Average deaths per fatal crash"), yaxis = ax(""),
                  legend = list(orientation = "h"))
  })
  output$c9 <- renderText({
    s9 <- dumbbell(); if (!nrow(s9)) return("")
    sprintf("Rural crashes kill more people per crash on %d of %d road types. (The Area filter is not used here.)",
            sum(s9$dpc_Rural > s9$dpc_Urban), nrow(s9))
  })
  
  
  output$g10 <- renderPlotly({
    d <- fd(); need_rows(d)
    s10 <- d %>% group_by(county) %>%
      summarise(deaths = sum(fatals), crashes = n(), .groups = "drop") %>%
      slice_max(deaths, n = 15, with_ties = FALSE) %>% arrange(deaths)
    s10$county <- factor(s10$county, levels = s10$county)
    plot_ly(s10, x = ~deaths, y = ~county, type = "bar", orientation = "h",
            marker = list(color = COL$text2),
            text = ~num(deaths), textposition = "outside",
            textfont = list(color = COL$text2), cliponaxis = FALSE,
            customdata = ~crashes,
            hovertemplate = "%{y}<br>%{x} deaths in %{customdata} crashes<extra></extra>") %>%
      base_layout(xaxis = ax("Deaths", rangemode = "tozero"), yaxis = ax(""),
                  margin = list(l = 190, r = 40), bargap = 0.25)
  })
  output$c10 <- renderText({
    d <- fd(); if (!nrow(d)) return("")
    s <- d %>% group_by(county) %>% summarise(deaths = sum(fatals)) %>% arrange(desc(deaths))
    sprintf("%s has the most deaths (%s). These are counts, not rates per person.",
            s$county[1], num(s$deaths[1]))
  })
  
  
  ems <- reactive(fd_noarea() %>% filter(area %in% c("Rural", "Urban"), !is.na(ems_response_min)))
  output$g11 <- renderPlotly({
    e <- ems(); validate(need(nrow(e) > 0, "No crashes with known EMS times for these filters."))
    box_stats <- function(v) {
      q <- quantile(v, c(.25, .5, .75)); iqr <- q[3] - q[1]
      list(q1 = q[[1]], med = q[[2]], q3 = q[[3]],
           lo = min(v[v >= q[1] - 1.5 * iqr]), hi = max(v[v <= q[3] + 1.5 * iqr]))
    }
    p <- plot_ly()
    for (a in c("Urban", "Rural")) {
      v <- e$ems_response_min[e$area == a]
      if (!length(v)) next
      b <- box_stats(v)
      col <- if (a == "Rural") COL$rural else COL$urban
      p <- p %>% add_trace(type = "box", orientation = "h", y = list(a), name = a,
                           q1 = list(b$q1), median = list(b$med), q3 = list(b$q3),
                           lowerfence = list(b$lo), upperfence = list(b$hi),
                           fillcolor = paste0(col, "40"), line = list(color = col, width = 2))
    }
    p %>% base_layout(xaxis = ax("Minutes from EMS notification to arrival", rangemode = "tozero"),
                      yaxis = ax(""), showlegend = FALSE)
  })
  output$c11 <- renderText({
    e <- ems(); if (!nrow(e)) return("")
    m <- e %>% group_by(area) %>% summarise(med = median(ems_response_min),
                                            p90 = quantile(ems_response_min, 0.9))
    r <- m[m$area == "Rural", ]; u <- m[m$area == "Urban", ]
    if (!nrow(r) || !nrow(u)) return("")
    sprintf("Median wait: %s min rural vs %s min urban. One in ten rural crashes waits %s+ min.",
            r$med, u$med, round(r$p90))
  })
  
  
  scatter <- reactive({
    fd_states() %>% filter(area %in% c("Rural", "Urban")) %>% group_by(state) %>%
      summarise(rural_share = mean(area == "Rural"),
                ems_n   = sum(!is.na(ems_response_min)),
                ems_med = median(ems_response_min, na.rm = TRUE),
                .groups = "drop") %>%
      filter(ems_n >= 30)
  })
  output$g12 <- renderPlotly({
    s12 <- scatter(); validate(need(nrow(s12) > 2, "Not enough states with known EMS times."))
    sel <- s12$state %in% input$states
    fit <- lm(ems_med ~ rural_share, data = s12)
    xs  <- range(s12$rural_share)
    plot_ly() %>%
      add_lines(x = xs, y = predict(fit, data.frame(rural_share = xs)),
                line = list(color = COL$ref, dash = "dash", width = 1.5),
                hoverinfo = "none", name = "Trend") %>%
      add_markers(data = s12, x = ~rural_share, y = ~ems_med, text = ~state,
                  marker = list(color = if (any(sel)) ifelse(sel, COL$text, "#c3c2b7") else COL$text2,
                                size = ifelse(sel, 14, 10), opacity = 0.9,
                                line = list(color = COL$surface, width = 1.5)),
                  hovertemplate = "<b>%{text}</b><br>Rural crashes: %{x:.0%}<br>Median EMS wait: %{y} min<extra></extra>") %>%
      base_layout(xaxis = ax("Share of the state's fatal crashes on rural roads", tickformat = ".0%"),
                  yaxis = ax("Median EMS wait (minutes)", rangemode = "tozero"),
                  showlegend = FALSE)
  })
  output$c12 <- renderText({
    s12 <- scatter(); if (nrow(s12) < 3) return("")
    sprintf("r = %.2f across %d states: the more rural a state's crashes, the longer the typical wait. Selected states are highlighted.",
            cor(s12$rural_share, s12$ems_med), nrow(s12))
  })
}

shinyApp(ui, server)