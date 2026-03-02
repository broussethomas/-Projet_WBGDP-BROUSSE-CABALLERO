library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(DT)

# ==============================================================================
# --- 1. PRÉPARATION DES DONNÉES --
# ==============================================================================

# Chargement du fichier
world_bank <- read.csv("World_Bank_GDP.csv", sep = ";")

# Nettoyage de base
world_bank <- world_bank %>%
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(gsub(",", "", Value)) 
  ) %>%
  filter(!is.na(Value) & Value > 0)

# --- Contraintes de l'utilisateur (1986-2019) ---
NEW_MIN_YEAR <- 1986
NEW_MAX_YEAR <- 2019

# Filtrer la trame de données globale
world_bank <- world_bank %>%
  filter(Year >= NEW_MIN_YEAR & Year <= NEW_MAX_YEAR)

min_year <- NEW_MIN_YEAR
max_year <- NEW_MAX_YEAR

# Codes à exclure
regional_codes_maquette <- c("ECA", "LCN", "MEA", "SAS", "SSA", "SSF", "EAS", "NAC")
aggregate_codes_to_exclude <- c("PSE", "WLD", "OED", "NAC", "EMU", "EUU", "ARB", "CEB", "CSS", 
                                "EAS", "EAP", "ECA", "ECS", "LAC", "LCN", "LDC", "MEA", "MNA", 
                                "SAS", "SSA", "SSF", "TEA", "TEC", "TLA", "TMN", "TSA", 
                                "TSS", "EAR", "LTE", "IBD", "IDX", "IBT", "HIC", "LIC", 
                                "LMC", "UMC", "HPC", "FCS", "PSS", "SST", "OSS", "GRL", 
                                "PRI", "MNP", "GUF", "MAC", "CUW", "SXM", "TCA", "VIR", 
                                "IMN", "XKX", "AIA", "ASM", "BMU", "CYM", "FRO", "GIB", 
                                "GUM", "JEY", "KOS", "MAF", "MCO", "PLW", "PYF", "SMR", 
                                "SXA", "VGB", "HKG", "CHI", "SXB", "ABW", "BER",
                                "LMY", "MIC", "IDB", "OEC", "UMC", "LMC", "SSF", "OSS", "PST", "XKX", "ZAR",
                                "IDA", "PRE", 
                                regional_codes_maquette)

clean_regional_label <- function(name) {
  name %>%
    str_replace_all("\\s?\\(IDA & IBRD countries\\)", "") %>%
    str_replace_all("\\s?\\(all income levels\\)", "") %>%
    str_trim()
}

# --- Palette de couleurs "Neon / Tech" ---
custom_colors <- c(
  "High income" = "#00d4ff",         # Cyan
  "Upper middle income" = "#4f46e5", # Indigo
  "Lower middle income" = "#a855f7", # Violet
  "Low income" = "#ec4899",          # Pink
  "GDP World" = "#00d4ff",
  "Growth Dot" = "#fbbf24",          # Amber
  "Top 10 Bar" = "#10b981",          # Emerald
  # COULEURS NEON PLUS VIVES POUR LE GRAPHIQUE FOCUS PAYS
  "Positive" = "#00ffcc",            # Néon Turquoise/Vert
  "Negative" = "#ff073a"             # Néon Rouge/Rose
)

# Données statiques
df_world_gdp <- world_bank %>% filter(Country.Code == "WLD")

df_income_group_full <- world_bank %>%
  filter(!Country.Code %in% aggregate_codes_to_exclude) %>%
  filter(!is.na(IncomeGroup) & IncomeGroup != "") %>% 
  group_by(Year, IncomeGroup) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

income_group_choices <- sort(unique(df_income_group_full$IncomeGroup))

country_choices <- world_bank %>% 
  filter(!Country.Code %in% aggregate_codes_to_exclude) %>% 
  pull(Country.Name) %>% 
  unique() %>% 
  sort()

df_gdp_growth <- world_bank %>%
  filter(Country.Code %in% regional_codes_maquette) %>%
  filter(Year %in% c(min_year, max_year)) %>%
  group_by(Region_Label = clean_regional_label(Country.Name)) %>%
  summarise(
    GDP_1986 = sum(Value[Year == min_year], na.rm = TRUE),
    GDP_2019 = sum(Value[Year == max_year], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Growth_Pct = ifelse(GDP_1986 > 0, ((GDP_2019 - GDP_1986) / GDP_1986) * 100, 0),
    Growth_Pct = ifelse(is.infinite(Growth_Pct), 0, Growth_Pct) 
  ) %>%
  filter(GDP_1986 > 0 & GDP_2019 > 0) %>% 
  arrange(desc(Growth_Pct))


# ==============================================================================
# --- 2. INTERFACE UTILISATEUR (UI) 
# ==============================================================================

kpi_card <- function(value, title, color) {
  tags$div(
    style = paste0("background-color: rgba(255,255,255,0.05); border-left: 4px solid ", color, "; padding: 15px; margin-bottom: 20px; border-radius: 8px; backdrop-filter: blur(5px); box-shadow: 0 4px 15px rgba(0,0,0,0.2);"),
    tags$h4(value, style = paste0("font-weight: 700; color: #fff; margin-top:0; text-shadow: 0 0 10px ", color, ";")),
    tags$p(title, style = "color: #94a3b8; font-size: 0.9rem; margin-bottom: 0; text-transform: uppercase; letter-spacing: 1px;")
  )
}

ui <- fluidPage(
  
  # --- POLICES & CSS ---
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap", rel="stylesheet"),
    tags$style(HTML("
      /* --- VARIABLES --- */
      :root {
        --primary: #00d4ff;
        --secondary: #4f46e5;
        --bg-dark: #0f172a;
        --glass: rgba(30, 41, 59, 0.7);
        --text: #f8fafc;
        --text-muted: #94a3b8;
      }

      body {
        background-color: var(--bg-dark);
        color: var(--text);
        font-family: 'Inter', sans-serif;
        background-image: 
            linear-gradient(rgba(255, 255, 255, 0.03) 1px, transparent 1px),
            linear-gradient(90deg, rgba(255, 255, 255, 0.03) 1px, transparent 1px);
        background-size: 40px 40px;
        min-height: 100vh;
      }

      body::before {
        content: ''; position: fixed; top: -10%; right: -10%; width: 50vw; height: 50vw;
        background: radial-gradient(circle, rgba(79, 70, 229, 0.2), transparent 70%);
        filter: blur(80px); z-index: -1;
      }

      h1, h2, h3, h4 { font-family: 'Inter', sans-serif; letter-spacing: -0.5px; }
      h1.title {
        text-align: center;
        font-size: 3.5rem;
        background: linear-gradient(90deg, #fff 0%, var(--primary) 50%, #fff 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 6px;
        margin-top: 30px;
        margin-bottom: 50px;
        filter: drop-shadow(0 0 10px rgba(0, 212, 255, 0.5));
        border-bottom: none !important;
      }
      
      h3 { 
        color: var(--primary); 
        font-size: 1.8rem;
        font-weight: 700;
        border-bottom: 1px solid rgba(255,255,255,0.1) !important; 
        padding-bottom: 15px; 
        margin-bottom: 25px;
        margin-top: 20px;
        text-shadow: 0 0 5px rgba(0, 212, 255, 0.3);
      }

      .well {
        background: var(--glass);
        backdrop-filter: blur(12px);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 16px;
        box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.37);
        color: var(--text);
      }
      
      .control-label { color: var(--text-muted); font-weight: 600; font-size: 0.85rem; text-transform: uppercase; }
      
      .selectize-input { 
        background: rgba(0,0,0,0.3) !important; 
        border: 1px solid rgba(255,255,255,0.1) !important; 
        color: white !important; 
        border-radius: 8px !important;
      }
      .selectize-dropdown { background: #1e293b; color: white; border: 1px solid #334155; }
      .selectize-input.full { background: rgba(0,0,0,0.3) !important; }
      
      .irs-bar { border-top: 1px solid var(--primary); border-bottom: 1px solid var(--primary); background: var(--primary); }
      .irs-from, .irs-to, .irs-single { background: var(--secondary); }
      .irs-grid-text { color: var(--text-muted); }

      .nav-tabs { border-bottom: 1px solid rgba(255,255,255,0.1); }
      .nav-tabs > li > a { color: var(--text-muted); border-radius: 8px 8px 0 0; transition: 0.3s; }
      .nav-tabs > li > a:hover { background: rgba(255,255,255,0.05); color: white; border-color: transparent; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background: rgba(255,255,255,0.05);
        color: var(--primary);
        border: 1px solid rgba(255,255,255,0.1);
        border-bottom-color: transparent;
        font-weight: bold;
      }
      
      .tab-content {
        background: var(--glass);
        backdrop-filter: blur(12px);
        border: 1px solid rgba(255,255,255,0.1);
        border-top: none;
        border-radius: 0 0 16px 16px;
        padding: 25px;
        box-shadow: 0 10px 40px rgba(0,0,0,0.5);
      }

      .dataTables_wrapper { color: var(--text-muted); }
      table.dataTable tbody tr { background-color: transparent !important; }
      table.dataTable tbody td { color: #e2e8f0; border-bottom: 1px solid rgba(255,255,255,0.1); }
      table.dataTable thead th { color: var(--primary); border-bottom: 1px solid rgba(255,255,255,0.2); }
      
      .dataTables_length select, .dataTables_filter input { 
        background: rgba(0,0,0,0.3) !important; color: white !important; border: 1px solid rgba(255,255,255,0.1) !important; border-radius: 4px; padding: 4px;
      }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_processing, 
      .dataTables_wrapper .dataTables_paginate {
        color: #94a3b8 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #fff !important; 
        background: transparent !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--primary) !important;
        color: #0f172a !important;
        border: 1px solid var(--primary) !important;
        font-weight: bold;
      }
      .dt-button {
        background: rgba(255,255,255,0.05) !important;
        color: #fff !important;
        border: 1px solid rgba(255,255,255,0.2) !important;
        border-radius: 4px !important; 
      }
      
      .btn { border-radius: 8px; font-weight: 600; text-transform: uppercase; font-size: 0.8rem; letter-spacing: 0.5px; }

    "))
  ),
  
  titlePanel(
    tags$h1("Etudes sur le PIB", class = "title")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3, 
      tags$h3("Paramètres de l'étude"),
      tags$hr(style="border-color: rgba(255,255,255,0.1);"),
      
      sliderInput("selected_year",
                  "Année d'observation :",
                  min = min_year,
                  max = max_year,
                  value = max_year,
                  step = 1,
                  sep = ""),
      
      tags$br(),
      
      checkboxGroupInput("selected_income_groups",
                         "Groupes de Revenu :",
                         choices = income_group_choices,
                         selected = income_group_choices),
      
      tags$br(),
      
      selectInput("selected_country",
                  "Cible Pays (Focus) :",
                  choices = country_choices,
                  selected = "France")
      
    ),
    
    mainPanel(
      width = 9, 
      tabsetPanel(
        
        # --- Onglet 1 ---
        tabPanel("Vue d'Ensemble",
                 tags$br(),
                 fluidRow(
                   column(6, htmlOutput("kpi_world_gdp")),
                   column(6, htmlOutput("kpi_country_growth"))
                 ),
                 tags$div(style="height: 20px;"), 
                 
                 fluidRow(
                   column(12,
                          tags$h3("Historique Global"), 
                          plotlyOutput("plot_gdp_evolution", height = "350px") 
                   )
                 ),
                 tags$br(),
                 fluidRow(
                   column(12,
                          tags$h3("Tendances par Groupes"),
                          plotlyOutput("plot_income_group", height = "400px") 
                   )
                 )
        ),
        
        # --- Onglet 2 ---
        tabPanel("Analyse Régionale",
                 tags$br(),
                 
                 fluidRow(
                   column(12,
                          tags$h3("Cartographie du PIB"),
                          tags$p("Répartition géographique du PIB.", style="color: #94a3b8;"),
                          plotlyOutput("plot_map", height = "600px")
                   )
                 ),
                 tags$br(), tags$hr(style="border-color: rgba(255,255,255,0.1);"), tags$br(),
                 
                 fluidRow(
                   column(12,
                          tags$h3("Dynamique de Croissance"),
                          tags$p("Comparaison de la croissance relative entre les périodes bornées.", style="color: #94a3b8;"),
                          plotlyOutput("plot_gdp_growth_dot", height = "600px")
                   )
                 )
        ),
        
        # --- Onglet 3 ---
        tabPanel("Classement",
                 tags$br(),
                 tags$h3(textOutput("top10_title")), 
                 plotlyOutput("plot_top_countries", height = "600px")
        ),
        
        # --- Onglet 4 ---
        tabPanel("Focus Pays",
                 tags$br(),
                 tags$h3(textOutput("country_analysis_title")), 
                 tags$p("Variation annuelle du PIB (taux de croissance). En Rouge : Récession.", style="color: #94a3b8; margin-bottom: 20px;"),
                 plotlyOutput("plot_country_gdp_evolution", height = "500px")
        ),
        
        # --- Onglet 5 ---
        tabPanel("Données Brutes",
                 tags$br(),
                 tags$h3("Matrice de Données"),
                 tags$p("Exportation et filtrage des données sources.", style="color: #94a3b8;"),
                 DT::dataTableOutput("gdp_data_table")
        )
      )
    )
  )
)

# ==============================================================================
# --- 3. LOGIQUE SERVEUR (Server) ---
# ==============================================================================

server <- function(input, output, session) {
  
  # --- Fonctions utilitaires ---
  
  dark_mode_layout <- function(p) {
    p %>% layout(
      paper_bgcolor = 'rgba(0,0,0,0)', 
      plot_bgcolor = 'rgba(0,0,0,0)', 
      font = list(color = '#cbd5e1'), 
      xaxis = list(
        gridcolor = 'rgba(255,255,255,0.1)', 
        zerolinecolor = 'rgba(255,255,255,0.2)'
      ),
      yaxis = list(
        gridcolor = 'rgba(255,255,255,0.1)',
        zerolinecolor = 'rgba(255,255,255,0.2)'
      )
    )
  }
  
  theme_clean_dark <- function() {
    theme_minimal() %+replace%
      theme(
        text = element_text(color = "#cbd5e1"),
        plot.title = element_blank(), 
        axis.title = element_text(size = 10, face = "bold", color = "#94a3b8"),
        axis.text = element_text(size = 9, color = "#64748b"),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.05)"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(color = "#cbd5e1"),
        legend.title = element_text(color = "#cbd5e1")
      )
  }
  
  observe({
    updateSelectInput(session, "selected_country",
                      choices = country_choices,
                      selected = "France")
  })
  
  # --- KPI 1 ---
  output$kpi_world_gdp <- renderUI({
    gdp_value <- df_world_gdp %>%
      filter(Year == input$selected_year) %>%
      pull(Value)
    
    formatted_value <- scales::dollar(gdp_value, scale = 1e-12, suffix = " T")
    kpi_card(formatted_value, paste("PIB Mondial Total en", input$selected_year), "#00d4ff")
  })
  
  # --- KPI 2 ---
  output$kpi_country_growth <- renderUI({
    selected_country_name <- input$selected_country
    df_country_growth <- world_bank %>%
      filter(Country.Name == selected_country_name) %>%
      filter(Year %in% c(min_year, max_year))
    
    gdp_start <- df_country_growth %>% filter(Year == min_year) %>% pull(Value)
    gdp_end <- df_country_growth %>% filter(Year == max_year) %>% pull(Value)
    gdp_start <- if (length(gdp_start) == 0) NA else gdp_start
    gdp_end <- if (length(gdp_end) == 0) NA else gdp_end
    
    if (is.na(gdp_start) | is.na(gdp_end) | gdp_start == 0) {
      formatted_value <- "N/A"
      color <- "#64748b"
    } else {
      growth_pct <- ((gdp_end - gdp_start) / gdp_start) * 100
      formatted_value <- paste0(round(growth_pct, 1), " %")
      color <- if(growth_pct >= 0) "#10b981" else "#ef4444"
    }
    
    kpi_card(formatted_value, paste("Croissance Totale (", min_year, "-", max_year, ") : ", selected_country_name), color)
  })
  
  # --- Titres ---
  output$top10_title <- renderText({ paste("Top 10 des Puissances Mondiales (", input$selected_year, ")") })
  output$country_analysis_title <- renderText({ paste("Volatilité & Croissance :", input$selected_country) })
  
  # --- GRAPH 7 : MAP ---
  output$plot_map <- renderPlotly({
    req(input$selected_income_groups)
    
    df_map <- world_bank %>%
      filter(!Country.Code %in% aggregate_codes_to_exclude) %>%
      filter(IncomeGroup %in% input$selected_income_groups) %>%
      filter(Year == input$selected_year) %>%
      filter(!is.na(Country.Code)) %>%
      mutate(LogValue = log10(Value))
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'natural earth'),
      bgcolor = 'rgba(0,0,0,0)',       
      showlakes = TRUE,
      lakecolor = '#0f172a',           
      showocean = TRUE,
      oceancolor = '#0f172a',          
      showland = TRUE,
      landcolor = '#1e293b'            
    )
    
    plot_geo(df_map) %>%
      add_trace(
        z = ~LogValue,    
        color = ~LogValue,
        colors = "YlOrRd", 
        text = ~paste(Country.Name, "<br>PIB:", scales::dollar(Value, scale = 1e-9, suffix = " B")),
        locations = ~Country.Code,
        marker = list(line = list(color = toRGB("gray"), width = 0.5)),
        hoverinfo = "text" 
      ) %>%
      colorbar(title = 'PIB', tickfont = list(color='rgba(0,0,0,0)')) %>% 
      layout(
        geo = g,
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#cbd5e1')
      )
  })
  
  # --- Plot 1: Evolution ---
  output$plot_gdp_evolution <- renderPlotly({
    years_to_display <- unique(c(1990, 2000, 2008, 2009, 2010, max_year))
    if (!input$selected_year %in% years_to_display) years_to_display <- c(years_to_display, input$selected_year)
    
    df_plot <- df_world_gdp %>% filter(Year %in% years_to_display)
    
    p <- ggplot(df_plot, aes(x = as.factor(Year), y = Value, 
                             fill = factor(Year == input$selected_year),
                             text = paste("Année:", Year, "<br>PIB Total:", scales::dollar(Value, scale = 1e-12, suffix = " T")))) +
      geom_col(width = 0.6) + 
      scale_fill_manual(values = c("TRUE" = "#fbbf24", "FALSE" = "#00d4ff"), guide = "none") + 
      scale_y_continuous(labels = dollar_format(scale = 1e-12, suffix = " T")) +
      labs(x = "", y = "") +
      theme_clean_dark()
    
    ggplotly(p, tooltip = "text") %>% dark_mode_layout()
  })
  
  # --- Plot 2: Income Groups ---
  output$plot_income_group <- renderPlotly({
    req(input$selected_income_groups)
    
    df_plot <- df_income_group_full %>% filter(IncomeGroup %in% input$selected_income_groups)
    selected_colors <- custom_colors[names(custom_colors) %in% input$selected_income_groups]
    
    p <- ggplot(df_plot, aes(x = Year, y = Value, color = IncomeGroup, 
                             text = paste("Année:", Year, "<br>Groupe:", IncomeGroup, "<br>PIB:", scales::dollar(Value, scale = 1e-9, suffix = " B")))) +
      geom_line(size = 1.2) + 
      geom_point(size = 1.5, alpha = 0.8) +
      geom_vline(xintercept = input$selected_year, linetype = "dashed", color = "#fbbf24", alpha = 0.8) +
      scale_color_manual(values = selected_colors) + 
      scale_y_continuous(labels = dollar_format(scale = 1e-9, suffix = " B")) +
      labs(x = "", y = "", color = "") +
      theme_clean_dark() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text") %>% dark_mode_layout()
  })
  
  # --- Plot 3: Dot Plot ---
  output$plot_gdp_growth_dot <- renderPlotly({
    p <- ggplot(df_gdp_growth, aes(x = Growth_Pct, y = reorder(Region_Label, Growth_Pct), 
                                   text = paste("Région:", Region_Label, "<br>Croissance:", round(Growth_Pct, 2), "%"))) +
      geom_segment(aes(x = 0, xend = Growth_Pct, yend = reorder(Region_Label, Growth_Pct)), 
                   color = "#00d4ff", size = 1.5, alpha = 0.6) + 
      geom_point(size = 6, color = "#fbbf24", alpha = 0.5) + 
      geom_point(size = 4, color = "#fbbf24") + 
      scale_x_continuous(labels = scales::percent_format(scale = 1)) +
      labs(x = "", y = "") +
      theme_clean_dark() +
      theme(axis.text.y = element_text(face = "bold", color = "#00d4ff"))
    
    ggplotly(p, tooltip = "text") %>% dark_mode_layout()
  })
  
  # --- Plot 4: Top 10 ---
  output$plot_top_countries <- renderPlotly({
    req(input$selected_income_groups)
    
    df_top_countries <- world_bank %>%
      filter(!Country.Code %in% aggregate_codes_to_exclude) %>% 
      filter(IncomeGroup %in% input$selected_income_groups) %>%
      filter(IncomeGroup != "") %>%
      filter(Year == input$selected_year) %>%
      arrange(desc(Value)) %>%
      head(10)
    
    p <- ggplot(df_top_countries, aes(x = reorder(Country.Name, Value), y = Value, 
                                      text = paste("Pays:", Country.Name, "<br>PIB:", scales::dollar(Value, scale = 1e-12, suffix = " T")))) +
      geom_col(fill = "#00d4ff", color = "#00d4ff", alpha = 0.4, size = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(scale = 1e-12, suffix = " T")) +
      labs(x = "", y = "") +
      theme_clean_dark() +
      theme(axis.text.y = element_text(face = "bold", color = "#00d4ff"))
    
    ggplotly(p, tooltip = "text") %>% dark_mode_layout()
  })
  
  # --- Plot 5: Country Focus  ---
  output$plot_country_gdp_evolution <- renderPlotly({
    df_country <- world_bank %>%
      filter(Country.Name == input$selected_country) %>%
      filter(!Country.Code %in% aggregate_codes_to_exclude) %>%
      arrange(Year) %>%
      mutate(
        Prev_Value = lag(Value),
        Growth_Rate = (Value - Prev_Value) / Prev_Value,
        Trend_Color = ifelse(Growth_Rate >= 0, "Positive", "Negative")
      ) %>%
      filter(!is.na(Growth_Rate)) 
    
    
    p <- ggplot(df_country, aes(x = Year, y = Growth_Rate, 
                                text = paste("Année:", Year, 
                                             "<br>Croissance:", scales::percent(Growth_Rate, accuracy = 0.1),
                                             "<br>PIB:", scales::dollar(Value, scale = 1e-9, suffix = " B")))) +
      geom_col(aes(color = Trend_Color, fill = Trend_Color), alpha = 0.4, width = 0.7, size = 0.8) +
      geom_hline(yintercept = 0, color = "white", size = 0.5, linetype="dashed", alpha=0.5) +
      geom_vline(xintercept = input$selected_year, linetype = "dashed", color = "#fbbf24", alpha = 0.8) +
      scale_fill_manual(values = custom_colors) +
      scale_color_manual(values = custom_colors) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "", y = "", fill = "Tendance", color = "Tendance") +
      theme_clean_dark() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% dark_mode_layout()
  })
  
  # --- Table DT ---
  output$gdp_data_table <- DT::renderDataTable({
    df_table <- world_bank %>%
      filter(!Country.Code %in% aggregate_codes_to_exclude) %>%
      filter(IncomeGroup %in% input$selected_income_groups) %>%
      filter(IncomeGroup != "") %>%
      filter(Year == input$selected_year) %>%
      select(Country.Name, Region, IncomeGroup, Year, Value) %>% 
      mutate(Value = scales::dollar(Value, big.mark = " ", prefix = "$"))
    
    DT::datatable(
      df_table,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel', 'pdf'),
        pageLength = 10,
        order = list(4, 'desc'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#1e293b', 'color': '#00d4ff'});",
          "}"
        )
      ),
      colnames = c('Pays', 'Région', 'Groupe de revenu', 'Année', 'PIB (USD)'),
      rownames = FALSE
    )
  })
  
}

shinyApp(ui = ui, server = server)