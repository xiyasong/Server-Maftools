# Load required libraries
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

library(shiny)
library(shinydashboard)
library(DT)
library(maftools)
library(plotly)
library(shinycssloaders)
library(shinyFiles)
library(vcfR)
library(mclust)
library(g3viz)
library(TCGAmutations)
library(NMF)
library(pheatmap)
library(grid)
library(BSgenome.Hsapiens.UCSC.hg19)
library(R.utils)
library(shinyjs)
library(ggplot2)
library(NMF)

#BiocManager::install(c(
#    "BSgenome.Hsapiens.UCSC.hg19",
#    "BSgenome.Hsapiens.UCSC.hg38",
#    "BSgenome"
#))
#rsconnect::deployApp(appName = "maftools")
# nolint
# UI ==============================
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      style = "font-weight: 600; letter-spacing: 0.5px; color: #f7f7f7;",
      "Advanced Maftools"
    ),
    tags$li(
      class = "dropdown",
      style = "margin: 8px; display: flex; align-items: center;",
      actionButton("theme_toggle", 
                   "", 
                   icon = icon("moon"),
                   class = "btn-sm",
                   style = "background: transparent; border: none; color: white; font-size: 16px; margin-right: 6px;") ,
      span("switch light/night mode", style = "color: white; font-size: 14px; font-weight: 500; margin-left: 2px;")
    )
  ),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* Import modern font */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap'); # nolint
        
        /* Make all <pre> tags wrap (for verbatimTextOutput) */
        pre {
          white-space: pre-wrap !important;
          word-break: break-all !important;
        }
        
        /* Global styles */
        * {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        
        /* Dashboard customization - Academic Theme */

        .main-header {
          background-color: #f7f7f7 !important;
        }

        .main-header .navbar {
          background: #c2c8cc !important;
          border: none !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
        }
        
        .main-sidebar {
          background-color: #f7f7f7 !important;
          border-right: 1px solid #b2b4b4 !important;
        }
        
        .main-sidebar .sidebar {
          padding-top: 10px;
        }
        
        .sidebar-menu > li > a {
          color: #c2c8cc !important;
          font-weight: 500 !important;
          padding: 15px 20px !important;
          border-radius: 0 8px 8px 0 !important;
          margin: 5px 15px 5px 0 !important;
          transition: all 0.3s ease !important;
          border-left: 3px solid transparent !important;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background-color:#ecdccc !important;
          color: #1f231b !important;
          border-left-color: #1f231b !important;
          box-shadow: 0 2px 8px rgba(31, 35, 27, 0.15) !important;
        }
        
        .sidebar-menu > li > a > .fa {
          color: #1f231b !important;
          margin-right: 10px !important;
        }
        
        /* Sub-menu styling - Light background */
        .sidebar-menu > li > .treeview-menu > li > a {
          background-color: #f7f7f7 !important;
          color: #1f231b !important;
          padding: 10px 15px 10px 35px !important;
          border-left: 3px solid transparent !important;
          font-size: 0.9em !important;
        }
        
        .sidebar-menu > li > .treeview-menu > li > a:hover {
          background-color: #ecdccc !important;
          color: #1f231b !important;
          border-left-color: #1f231b !important;
        }
        
        .sidebar-menu > li > .treeview-menu > li.active > a {
          background-color: #ecdccc !important;
          color: #1f231b !important;
          border-left-color: #1f231b !important;
        }
        
        /* Content area */
        .content-wrapper {
          background-color: #f7f7f7 !important;
          min-height: 100vh;
        }
        
        .content {
          padding: 25px !important;
        }
        
        /* Academic box styling */
        .box {
          border-radius: 8px !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08) !important;
          border: 1px solid #b2b4b4 !important;
          background-color: #ffffff !important;
          margin-bottom: 25px !important;
          overflow: hidden !important;
        }
        
        .box-header {
          background-color: #ecdccc !important;
          color: #ffffff !important;
          padding: 20px 25px !important;
          border-bottom: none !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
        }
        
        .box-header.with-border {
          border-bottom: none !important;
        }
        
        .box-header > .fa {
          margin-right: 10px !important;
        }
        
        .box-body {
          padding: 30px 25px !important;
          background-color: #ffffff !important;
        }
        
        /* Status boxes - Academic colors */
        .box.box-primary .box-header {
          background: linear-gradient(135deg, #1f231b 0%, #2a2f22 100%) !important;
        }
        
        .box.box-info .box-header {
          background: linear-gradient(135deg, #6c6c6c 0%, #1f231b 100%) !important;
        }
        
        .box.box-success .box-header {
          background: linear-gradient(135deg, #c4cbcb 0%, #b2b4b4 100%) !important;
          color: #1f231b !important;
        }
        
        .box.box-warning .box-header {
          background: linear-gradient(135deg, #ecdccc 0%, #ccc4b4 100%) !important;
          color: #1f231b !important;
        }
        
        .box.box-danger .box-header {
          background: linear-gradient(135deg, #6c6c6c 0%, #1f231b 100%) !important;
        }
        
        /* Academic buttons - Enhanced Load Data button */
        .btn-primary {
          background: linear-gradient(135deg, #1f231b 0%, #2a2f22 100%) !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 15px 30px !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 4px 8px rgba(31, 35, 27, 0.3) !important;
          color: #ffffff !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .btn-primary:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 6px 12px rgba(31, 35, 27, 0.4) !important;
          background: linear-gradient(135deg, #2a2f22 0%, #1f231b 100%) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #c4cbcb 0%, #b2b4b4 100%) !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 12px 25px !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 2px 4px rgba(196, 203, 203, 0.3) !important;
          color: #1f231b !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .btn-success:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(196, 203, 203, 0.4) !important;
        }
        
        .btn-info {
          background: linear-gradient(135deg, #1f231b 0%, #2a2f22 100%) !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 12px 25px !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 2px 4px rgba(31, 35, 27, 0.3) !important;
          color: #ffffff !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .btn-info:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(31, 35, 27, 0.4) !important;
          background: linear-gradient(135deg, #2a2f22 0%, #1f231b 100%) !important;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #ecdccc 0%, #ccc4b4 100%) !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 12px 25px !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 2px 4px rgba(236, 220, 204, 0.3) !important;
          color: #1f231b !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .btn-warning:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(236, 220, 204, 0.4) !important;
          color: #1f231b !important;
        }
        
        .btn-danger {
          background: linear-gradient(135deg, #1f231b 0%, #2a2f22 100%) !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 12px 25px !important;
          font-weight: 600 !important;
          font-size: 1.1em !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 2px 4px rgba(31, 35, 27, 0.3) !important;
          color: #ffffff !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .btn-danger:hover {
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(31, 35, 27, 0.4) !important;
          background: linear-gradient(135deg, #2a2f22 0%, #1f231b 100%) !important;
        }
        
        .btn-block {
          width: 100% !important;
          margin-top: 15px !important;
        }
        
        /* Academic form elements */
        .form-control {
          border-radius: 6px !important;
          border: 2px solid #b2b4b4 !important;
          padding: 12px 15px !important;
          transition: all 0.3s ease !important;
          font-weight: 500 !important;
          background-color: #ffffff !important;
        }
        
        .form-control:focus {
          border-color: #1f231b !important;
          box-shadow: 0 0 0 3px rgba(31, 35, 27, 0.1) !important;
        }
        
        .selectize-input {
          border-radius: 6px !important;
          border: 2px solid #b2b4b4 !important;
          min-height: 45px !important;
          background-color: #ffffff !important;
        }
        
        .selectize-input.focus {
          border-color: #1f231b !important;
          box-shadow: 0 0 0 3px rgba(31, 35, 27, 0.1) !important;
        }
        
        /* Academic upload area styling */
        .upload-area {
          border: 2px dashed #b2b4b4 !important;
          border-radius: 8px !important;
          padding: 30px !important;
          text-align: center !important;
          background: linear-gradient(135deg, #f7f7f7 0%, #dcdcdc 100%) !important;
          cursor: pointer !important;
          transition: all 0.3s ease !important;
          margin: 20px 0 !important;
        }
        
        .upload-area:hover {
          border-color: #1f231b !important;
          background: linear-gradient(135deg, #ecdccc 0%, #ccc4b4 100%) !important;
          transform: translateY(-1px) !important;
          box-shadow: 0 4px 8px rgba(31, 35, 27, 0.1) !important;
        }
        
        /* Academic information panels */
        .info-panel {
          background: linear-gradient(135deg, #f7f7f7 0%, #dcdcdc 100%) !important;
          border: 1px solid #b2b4b4 !important;
          border-radius: 8px !important;
          padding: 20px !important;
          margin: 15px 0 !important;
          border-left: 4px solid #1f231b !important;
        }
        
        .info-panel h5 {
          color: #1f231b !important;
          font-weight: 600 !important;
          margin-bottom: 10px !important;
        }
        
        .info-panel p {
          color: #2a2f22 !important;
          margin-bottom: 8px !important;
          line-height: 1.6 !important;
        }
        
        .info-panel ul {
          color: #2a2f22 !important;
          margin-bottom: 0 !important;
        }
        
        .info-panel ul li {
          margin-bottom: 5px !important;
        }
        
        /* Academic success indicators */
        .success-indicator {
          background: linear-gradient(135deg, #ecdccc 0%, #ccc4b4 100%) !important;
          border: 1px solid #b2b4b4 !important;
          border-radius: 6px !important;
          padding: 15px !important;
          color: #1f231b !important;
          font-weight: 600 !important;
          margin: 15px 0 !important;
        }
        
        .success-indicator .fa {
          margin-right: 8px !important;
        }
        
        /* Academic empty state styling */
        .empty-state {
          text-align: center !important;
          padding: 60px 20px !important;
          color: #6c6c6c !important;
        }
        
        .empty-state .fa {
          font-size: 64px !important;
          margin-bottom: 20px !important;
          color: #b2b4b4 !important;
        }
        
        .empty-state h3, .empty-state h4 {
          color: #1f231b !important;
          font-weight: 500 !important;
        }
        
        /* Academic DataTable styling */
        .dataTables_wrapper {
          margin-top: 20px !important;
        }
        
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter {
          margin-bottom: 20px !important;
        }
        
        .dataTables_wrapper .dataTables_filter input {
          border-radius: 6px !important;
          border: 2px solid #b2b4b4 !important;
          padding: 8px 12px !important;
        }
        
        table.dataTable thead th {
          background: linear-gradient(135deg, #f7f7f7 0%, #dcdcdc 100%) !important;
          border-bottom: 2px solid #b2b4b4 !important;
          font-weight: 600 !important;
          color: #1f231b !important;
        }
        
        table.dataTable tbody tr:hover {
          background-color: #f7f7f7 !important;
        }
        
        /* Academic spinner customization */
        .sk-spinner {
          color: #1f231b !important;
        }
        
        /* Academic tab panels */
        .tab-content {
          background-color: #ffffff !important;
          border-radius: 0 0 8px 8px !important;
          padding: 25px !important;
        }
        
        .nav-tabs > li > a {
          border-radius: 6px 6px 0 0 !important;
          color: #6c6c6c !important;
          font-weight: 500 !important;
        }
        
        .nav-tabs > li.active > a {
          background: linear-gradient(135deg, #f7f7f7 0%, #dcdcdc 100%) !important;
          border-color: #b2b4b4 !important;
          color: #1f231b !important;
          font-weight: 600 !important;
        }
        
        /* Academic checkbox and radio styling */
        .checkbox input[type='checkbox']:checked + span:before,
        .radio input[type='radio']:checked + span:before {
          background-color: #1f231b !important;
          border-color: #1f231b !important;
        }
        
        /* Academic number input styling */
        input[type='number'] {
          border-radius: 6px !important;
          border: 2px solid #b2b4b4 !important;
          padding: 12px 15px !important;
        }
        
        input[type='number']:focus {
          border-color: #1f231b !important;
          box-shadow: 0 0 0 3px rgba(31, 35, 27, 0.1) !important;
        }
        
        /* Academic help text styling */
        .help-block {
          color: #6c6c6c !important;
          font-style: italic !important;
          font-size: 0.9em !important;
        }
        
        /* Academic progress indicators */
        .progress {
          border-radius: 6px !important;
          height: 8px !important;
          background-color: #dcdcdc !important;
        }
        
        .progress-bar {
          background: linear-gradient(135deg, #1f231b 0%, #2a2f22 100%) !important;
          border-radius: 6px !important;
        }
        
        /* Academic responsive adjustments */
        @media (max-width: 768px) {
          .content {
            padding: 15px !important;
          }
          
          .box-body {
            padding: 20px 15px !important;
          }
          
          .btn {
            padding: 10px 20px !important;
            font-size: 0.9em !important;
          }
        }
        
        /* 深色主题变量 */
        :root {
          --bg-primary: #f7f7f7;
          --bg-secondary: #ffffff;
          --bg-accent: #ecdccc;
          --text-primary: #1f231b;
          --text-secondary: #2a2f22;
          --border-color: #b2b4b4;
        }
        [data-theme=\"dark\"] {
          --bg-primary: #1a1a1a;
          --bg-secondary: #2d2d2d;
          --bg-accent: #3d3d3d;
          --text-primary: #e0e0e0;
          --text-secondary: #b0b0b0;
          --border-color: #4a4a4a;
        }
        /* 应用主题变量 */
        .content-wrapper {
          background-color: var(--bg-primary) !important;
          transition: all 0.3s ease !important;
        }
        .box {
          background-color: var(--bg-secondary) !important;
          border: 1px solid var(--border-color) !important;
          transition: all 0.3s ease !important;
        }
        .box-body {
          background-color: var(--bg-secondary) !important;
          color: var(--text-primary) !important;
        }
        /* 深色主题下的表格 */
        [data-theme=\"dark\"] .dataTables_wrapper {
          color: var(--text-primary) !important;
        }
        [data-theme=\"dark\"] table.dataTable {
          background-color: var(--bg-secondary) !important;
          color: var(--text-primary) !important;
        }
        [data-theme=\"dark\"] table.dataTable thead th {
          background: var(--bg-accent) !important;
          color: var(--text-primary) !important;
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Available TCGA Cohorts", tabName = "cohorts", icon = icon("table")),
      menuItem("Load & Analyze", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Basic Visualizations", tabName = "basic_plots", icon = icon("chart-line"),
        menuSubItem("MAF Summary Plot", tabName = "basic_plots"),
        menuSubItem("Oncoplots", tabName = "basic_plots"),
        menuSubItem("Ti/Tv Analysis", tabName = "basic_plots")
      ),
      menuItem("Advanced Visualizations", tabName = "advanced_plots", icon = icon("dna"),
        menuSubItem("Lollipop Plots", tabName = "advanced_plots"),
        menuSubItem("Rainfall Plots", tabName = "advanced_plots"),
        menuSubItem("Co-occurrence Plots", tabName = "advanced_plots")
      ),
      menuItem("Advanced Analysis", tabName = "advanced_analysis", icon = icon("project-diagram"),
        menuSubItem("0 TMB Calculation", tabName = "advanced_analysis"),
        menuSubItem("1 Somatic Interactions", tabName = "advanced_analysis"),
        menuSubItem("2 Cancer Driver Genes", tabName = "advanced_analysis"),
        menuSubItem("3 Pfam Domains", tabName = "advanced_analysis"),
        menuSubItem("4 Survival Analysis (mut-based)", tabName = "advanced_analysis"),
        menuSubItem("6 Clinical Enrichment ", tabName = "advanced_analysis"),
        menuSubItem("7 Drug-Gene Interactions", tabName = "advanced_analysis"),
        menuSubItem("8 Oncogenic Pathways", tabName = "advanced_analysis"),
        menuSubItem("9 Tumor Heterogeneity", tabName = "advanced_analysis"),
        menuSubItem("10 Mutation Signatures", tabName = "advanced_analysis")
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      ## Available Cohorts Tab------------------
      tabItem(tabName = "cohorts",
              fluidRow(
                box(
                  title = "Available TCGA Cohorts", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  collapsible = TRUE,
                  
                  p("The following TCGA cohorts are available for analysis:", 
                    style = "color: #557a9e; font-size: 1.5em; margin-bottom: 25px;"),
                  
                  withSpinner(
                    DT::dataTableOutput("cohorts_table"),
                    type = 4,
                    color = "#1f231b"
                  ),
                  
                  br(),
                  
                  div(
                    class = "info-panel",
                    h5("Data Sources:"),
                    tags$ul(
                      tags$li(strong("MC3:"), "TCGA MC3 project somatic mutations"),
                      tags$li(strong("Firehose:"), "Broad Institute Firehose pipeline data"),
                      tags$li(strong("CCLE:"), "Cancer Cell Line Encyclopedia data (where available)")
                    )
                  )
                )
              )
      ),

      ## Load & Analysis Tab-----------------
      tabItem(tabName = "analysis",
              # Data source selection area
              fluidRow(
                box(
                  title = "Data Source Selection", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  radioButtons("data_source_type", 
                               "Choose your data source:",
                               choices = list(
                                 "Load TCGA Cohort Data" = "tcga",
                                 "Upload My Own MAF File" = "upload"
                               ),
                               selected = "tcga",
                               inline = TRUE)
                )
              ),
              
              # TCGA data loading area
              conditionalPanel(
                condition = "input.data_source_type == 'tcga'",
                fluidRow(
                  box(
                    title = "Load TCGA Cohort", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    selectInput("selected_study", 
                                "Select TCGA Study:",
                                choices = NULL,
                                selected = NULL),
                    selectInput(
                    "tcga_source",
                    "Select Data Source:",
                    choices = c("MC3" = "MC3", "Firehose" = "Firehose", "CCLE" = "CCLE"),
                    selected = "MC3"
                  ),
                    helpText("Data will be loaded from TCGA MC3,Firehose or CCLE project (default source is MC3)"),
                    
                    actionButton("load_data", 
                                 "Load Data", 
                                 class = "btn-primary btn-block",
                                 icon = icon("download")),
                    
                    br(), br(),
                    
                    conditionalPanel(
                      condition = "output.data_loaded",
                      div(
                        class = "success-indicator",
                        icon("check-circle"), 
                        " Data loaded successfully!"
                      )
                    )
                  ),
                  
                  # TCGA data information
                  box(
                    title = "Data Info", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,  # changed from 8 to 12
                    
                    conditionalPanel(
                      condition = "output.data_loaded",
                      
                      # Data basic information
                      fluidRow(
                        column(6,
                               h5("Basic Statistics:", style = "color: #17a2b8; font-weight: 600;"),
                               verbatimTextOutput("data_basic_stats")
                        ),
                        column(6,
                               h5("Column Information:", style = "color: #17a2b8; font-weight: 600;"),
                               verbatimTextOutput("data_column_info")
                        )
                      ),
                      
                      br(),
                      
                      # All column names display
                      h5("All Column Names:", style = "color: #17a2b8; font-weight: 600;"),
                      div(
                        style = "overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",  # removed max-height
                        verbatimTextOutput("all_column_names")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "!output.data_loaded",
                      div(
                        class = "empty-state",
                        icon("download"),
                        h4("Select and load a TCGA cohort to view data information")
                      )
                    )
                  )
                )
              ),
              
              ### MAF data upload handling------------------
              conditionalPanel(
                condition = "input.data_source_type == 'upload'",
                fluidRow(
                  box(
                    title = "Upload Your MAF File", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 4,
                    
                    div(class = "upload-area",
                        fileInput("upload_file", 
                                  label = NULL,
                                  accept = c(".maf", ".txt", ".tsv"),
                                  placeholder = "Choose MAF file"),
                        h4("Upload your MAF file", 
                           style = "color: #c2c8cc; font-weight: 600; margin-bottom: 15px;"),
                        p("Supported formats: .maf, .txt, .tsv", 
                          style = "color: #6c757d; font-size: 1.1em;")
                    ),
                    
                    # File options
                    checkboxInput("has_header", "File has header row", value = TRUE),
                    selectInput("file_separator", 
                                "File separator:",
                                choices = list("Tab" = "\t", "Comma" = ","),
                                selected = "\t"),
                    
                    conditionalPanel(
                      condition = "output.uploaded_data_loaded",
                      br(),
                      actionButton("use_uploaded", 
                                   "Use Uploaded Data for Analysis", 
                                   class = "btn-success btn-block",
                                   icon = icon("check"))
                    )
                  ),
                  
                  # Uploaded data information
                  box(
                    title = "Uploaded Data Info", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 8,
                    
                    conditionalPanel(
                      condition = "output.uploaded_data_loaded",
                      
                      fluidRow(
                        column(6,
                               h5("Basic Statistics:", style = "color: #17a2b8; font-weight: 800;"),
                               verbatimTextOutput("uploaded_data_basic_stats")
                        ),
                        column(6,
                               h5("Column Information:", style = "color: #17a2b8; font-weight: 800;"),
                               verbatimTextOutput("uploaded_data_column_info")
                        )
                      ),
                      
                      br(),
                      
                      h5("All Column Names:", style = "color: #17a2b8; font-weight: 800;"),
                      div(
                        style = "max-height: 120px; overflow-y: auto; background-color: #f8fffd; padding: 10px; border-radius: 5px; border: 1px solid #c3e6cb;",
                        verbatimTextOutput("uploaded_all_column_names")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "!output.uploaded_data_loaded",
                      div(
                        class = "empty-state",
                        icon("upload"),
                        h4("Upload a MAF file to view data information")
                      )
                    )
                  )
                )
              ),
              
              # Keep the original Sample Statistics and Gene Statistics
              fluidRow(
                box(
                  title = "Sample Statistics", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded || output.uploaded_data_loaded",
                    withSpinner(
                      DT::dataTableOutput("sample_summary"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                ),
                
                box(
                  title = "Gene Statistics", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded || output.uploaded_data_loaded",
                    withSpinner(
                      DT::dataTableOutput("gene_summary"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              ),
              
              # Data preview (display first 5 rows) - display for all data sources
              conditionalPanel(
                condition = "output.data_loaded || output.uploaded_data_loaded",
                fluidRow(
                  box(
                    title = "Data Preview (First 5 Rows)", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    p("Preview of your loaded data:", style = "color: #6c757d; margin-bottom: 15px;"),
                    
                    withSpinner(
                      DT::dataTableOutput("data_preview_5rows"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              )
      ),
      

      ## Basic Visualization Plots Tab------------------
      tabItem(tabName = "basic_plots",
              ###  MAF summary plots ==================
              fluidRow(
                box(
                  title = "Mutation (MAF file) Summary Plot", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    downloadButton("download_maf_plot", "Download PNG", class = "btn-primary btn-sm"),
                    br(), br(),
                    withSpinner(
                      plotOutput("maf_plot", height = "600px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "!output.data_loaded",
                    div(
                      class = "empty-state",
                      icon("chart-bar"),
                      h3("Load a dataset to view mutation plots")
                    )
                  )
                )
              ),
              ### Onco plots ==================
              fluidRow(
                box(
                  title = "Oncoplots (Top Mutated Genes)", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    div(style = "margin-bottom: 20px;",
                        numericInput("top_genes", 
                                     "Number of top genes to display:", 
                                     value = 10, 
                                     min = 5, 
                                     max = 50, 
                                     step = 5)
                    ),
                    selectInput(
                      "pathways_option",
                      "Pathways Bar",
                      choices = c("None" = "none", 
                      "5 most affected pathways (sigpw)" = "sigpw",
                      "5 most affected SMG pathways (smgbp)" = "smgbp"),
                      selected = "none"
                      ),
                    downloadButton("download_oncoplot", "Download PNG", class = "btn-primary btn-sm"),
                    br(), br(),
                    withSpinner(
                      plotOutput("oncoplot", height = "800px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              ),
              ### TiTV plots ==================
              fluidRow(
                box(
                  title = "Transitions and Transversions (TiTv) Analysis", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 8,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    div(
                      class = "info-panel",
                      h5("About Ti/Tv Analysis:"),
                      p("Transition and Transversion analysis classifies SNPs into six different conversion types:"),
                      tags$ul(
                        tags$li(strong("Transitions:"), "C>T, G>A, A>G, T>C"),
                        tags$li(strong("Transversions:"), "C>A, C>G, T>A, T>G, A>C, A>T, G>C, G>T")
                      ),
                      p("Ti/Tv ratio is an important quality metric for sequencing data.")
                    ),
                    
                    checkboxInput("use_synonymous", 
                                  "Include synonymous mutations in Ti/Tv analysis", 
                                  value = TRUE),
                    downloadButton("download_titv_plot", "Download PNG", class = "btn-primary btn-sm"),
                    br(), br(),
                    withSpinner(
                      plotOutput("titv_plot", height = "600px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                ),
                
                box(
                  title = "Ti/Tv Summary Statistics", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 4,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    withSpinner(
                      DT::dataTableOutput("titv_summary"),
                      type = 4,
                      color = "#1f231b"
                    ),
                    
                    br(),
                    
                    div(
                      class = "info-panel",
                      h5("Interpretation:"),
                      tags$ul(
                        tags$li("Normal Ti/Tv ratio: ~2.0-2.1"),
                        tags$li("Lower ratios may indicate technical artifacts"),
                        tags$li("Higher ratios may suggest hypermutation")
                      )
                    )
                  )
                )
              )
      ),
      
      ## Advanced Visualization Plots Tab------------------
      tabItem(tabName = "advanced_plots",
              fluidRow(
                box(
                  title = "Lollipop Plot Comparison", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             selectInput("lollipop_gene", 
                                         "Select Gene:",
                                         choices = NULL,
                                         selected = NULL),
                             selectInput("aa_col", 
                                         "Amino Acid Column Name:", 
                                         choices = c("HGVSp_Short" = "HGVSp_Short",
                                                     "Protein_Change" = "Protein_Change",
                                                     "i_AAChange" = "i_AAChange",
                                                     "i_HGVS_protein_change" = "i_HGVS_protein_change"),
                                         selected = "HGVSp_Short"),  
                             checkboxInput("show_mutation_rate", 
                                           "Show mutation rate", 
                                           value = TRUE),
                            checkboxInput("show_labelPos", 
                                           "Show label position", 
                                           value = TRUE),
                             selectInput("g3_theme",
                                         "G3viz Theme:",
                                         choices = c("default" = "default",
                                                     "blue" = "blue", 
                                                     "simple" = "simple",
                                                     "cbioportal" = "cbioportal",
                                                     "nature" = "nature",
                                                     "nature2" = "nature2", 
                                                     "ggplot2" = "ggplot2",
                                                     "dark" = "dark"),
                                         selected = "cbioportal")
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Lollipop Plots:"),
                               p("Visualizes amino acid changes along protein sequence. Each 'lollipop' represents mutations at specific amino acid positions."),
                               p(strong("Left:"), "Traditional maftools plot | ", strong("Right:"), "Interactive g3viz plot")
                             )
                      )
                    ),
                    
                    # display two plots side by side
                    fluidRow(
                      column(5,
                             h4("MAFtools Lollipop Plot", style = "text-align: center; margin-bottom: 15px;"),
                             withSpinner(
                               plotOutput("lollipop_plot", height = "400px"),
                               type = 4,
                               color = "#1f231b"
                             )
                      ),
                      column(7,
                             h4("G3viz Interactive Lollipop Plot", style = "text-align: center; margin-bottom: 15px;"),
                             withSpinner(
                               uiOutput("g3_lollipop_plot"),
                               type = 4,
                               color = "#1f231b"
                             )
                      )
                    )
                  )
                )
              ),
              ### Rainfall plots ----------------
              fluidRow(
                box(
                  title = "Rainfall Plot", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    selectInput("rainfall_sample", 
                                "Select Sample:",
                                choices = NULL,
                                selected = NULL),
                    
                    checkboxInput("detect_changepoints", 
                                  "Detect kataegis (change points)", 
                                  value = TRUE),
                    
                    numericInput("point_size", 
                                 "Point size:", 
                                 value = 0.6, 
                                 min = 0.1, 
                                 max = 2, 
                                 step = 0.1),
                    
                    div(
                      class = "info-panel",
                      h5("About Rainfall Plots:"),
                      p("Shows inter-mutation distances across the genome. Clusters indicate hypermutation regions (kataegis).")
                    ),
                    
                    withSpinner(
                      plotOutput("rainfall_plot", height = "400px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                ),
                ### VAF Distribution plot ----------------
                box(
                  title = "VAF Distribution", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    textInput("vaf_col", 
                              "VAF Column Name:", 
                              value = "t_alt_count",
                              placeholder = "e.g., i_TumorVAF_WU, t_alt_count"),
                    
                    numericInput("top_genes_vaf", 
                                 "Number of top genes:", 
                                 value = 10, 
                                 min = 5, 
                                 max = 50, 
                                 step = 5),
                    
                    div(
                      class = "info-panel",
                      h5("About VAF Plots:"),
                      p("Variant Allele Frequency distribution helps estimate clonal status. Clonal mutations typically show VAF around 50%.")
                    ),
                    
                    withSpinner(
                      plotOutput("vaf_plot", height = "400px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              ),
              ### TCGA Mutation Load Comparison ----------------
              fluidRow(
                box(
                  title = "TCGA Mutation Load Comparison", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             textInput("cohort_name", 
                                       "Cohort Name:", 
                                       value = "My-Cohort"),
                             
                             checkboxInput("log_scale", 
                                           "Use log scale", 
                                           value = TRUE),
                             
                             numericInput("capture_size", 
                                          "Capture size (Mb):", 
                                          value = 35, 
                                          min = 1, 
                                          max = 100, 
                                          step = 1)
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About TCGA Comparison:"),
                               p("Compares mutation burden of your dataset against 33 TCGA cohorts to provide context for mutation load.")
                             )
                      )
                    ),
                    
                    withSpinner(
                      plotOutput("tcga_compare_plot", height = "500px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              )
      ),
      
      ## Maftools Section 9 Analysis Tab------------------
      tabItem(tabName = "advanced_analysis",
              # 9.0 Tumor Mutational Burden
              fluidRow(
                box(
                  title = "9.0 Tumor Mutational Burden", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             numericInput("tmb_capture_size", 
                                          "Capture size (Mb):", 
                                          value = 50, min = 1, max = 200, step = 1),
                             
                             checkboxInput("tmb_log_scale", 
                                           "Use log scale", 
                                           value = TRUE),
                             
                             actionButton("run_tmb", 
                                          "Calculate TMB", 
                                          class = "btn-primary",
                                          icon = icon("chart-bar"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("Tumor Mutational Burden:"),
                               p("Calculates mutations per megabase for immunotherapy response prediction."),
                               p("Higher TMB is associated with better response to immune checkpoint inhibitors.")
                             )
                      )
                    ),
                    
                    withSpinner(
                      plotOutput("tmb_plot", height = "500px"),
                      type = 4,
                      color = "#1f231b"
                    )
                  )
                )
              ),
              
              ### 9.1 Somatic Interactions plot------------------
              fluidRow(
                box(
                  title = "9.1 Somatic Interactions", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             numericInput("interaction_top_genes", 
                                          "Number of top genes:", 
                                          value = 25, min = 10, max = 50, step = 5),
                             
                             numericInput("pvalue_threshold", 
                                          "P-value threshold:", 
                                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                             
                             actionButton("run_interactions", 
                                          "Run Somatic Interactions Analysis", 
                                          class = "btn-primary",
                                          icon = icon("play"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Somatic Interactions:"),
                               p("Identifies mutually exclusive or co-occurring gene pairs using Fisher's exact test."),
                               p("Useful for understanding functional relationships between genes.")
                             )
                      )
                    ),
                    
                    withSpinner(
                      plotOutput("somatic_interactions_plot", height = "600px"),
                      type = 4,
                      color = "#1f231b"
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "output.interactions_run",
                      h4("Interaction Results:", style = "color: #2a76bd; font-weight: 600;"),
                      withSpinner(
                        DT::dataTableOutput("interactions_table"),
                        type = 4,
                        color = "#1f231b"
                      )
                    )
                  )
                )
              ),
              
              ### 9.2 Cancer Driver Genes and Pfam Domains Analysis------------------
              fluidRow(
                box(
                  title = "9.2 Cancer Driver Genes Detection", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    textInput("oncodrive_aa_col", 
                              "Amino Acid Column Name:", 
                              value = "HGVSp_Short",
                              placeholder = "e.g., Protein_Change, HGVSp_Short, AAChange"),
                    
                    numericInput("oncodrive_min_mut", 
                                 "Minimum mutations per gene:", 
                                 value = 5, min = 2, max = 20, step = 1),
                    
                    selectInput("oncodrive_pval_method", 
                                "P-value calculation method:",
                                choices = c("zscore" = "zscore", 
                                            "poisson" = "poisson"),
                                selected = "zscore"),
                    
                    actionButton("run_oncodrive", 
                                 "Detect Cancer Driver Genes", 
                                 class = "btn-primary",
                                 icon = icon("dna")),
                    
                    br(), br(),
                    
                    div(
                      class = "info-panel",
                      h5("OncodriveCLUST Algorithm:"),
                      p("Identifies cancer driver genes based on positional clustering of mutations."),
                      p("Most variants in cancer genes are enriched at specific hotspots.")
                    ),
                    
                    withSpinner(
                      plotOutput("oncodrive_plot", height = "400px"),
                      type = 4,
                      color = "#1f231b"
                    ),
                    
                    conditionalPanel(
                      condition = "output.oncodrive_run",
                      br(),
                      h5("Driver Gene Results:", style = "color: #17a2b8; font-weight: 600;"),
                      withSpinner(
                        DT::dataTableOutput("oncodrive_table"),
                        type = 4,
                        color = "#1f231b"
                      )
                    )
                  )
                ),
                
                ### 9.3 Pfam Domains Analysis------------------
                box(
                  title = "9.3 Pfam Domains Analysis", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 6,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    textInput("pfam_aa_col", 
                              "Amino Acid Column Name:", 
                              value = "HGVSp_Short",
                              placeholder = "e.g., Protein_Change, HGVSp_Short, AAChange"),
                    
                    numericInput("pfam_top_genes", 
                                 "Number of top genes to analyze:", 
                                 value = 10, min = 5, max = 50, step = 5),
                    
                    actionButton("run_pfam", 
                                 "Analyze Pfam Domains", 
                                 class = "btn-primary",
                                 icon = icon("project-diagram")),
                    
                    br(), br(),
                    
                    div(
                      class = "info-panel",
                      h5("Pfam Domain Analysis:"),
                      p("Adds pfam domain information to amino acid changes and summarizes affected domains."),
                      p("Inspired by Pfam annotation module from MuSiC tool.")
                    ),
                    
                    conditionalPanel(
                      condition = "output.pfam_run",
                      tabsetPanel(
                        tabPanel("Domain Plot", 
                                 withSpinner(
                                   plotOutput("pfam_plot", height = "400px"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        ),
                        tabPanel("Protein Summary", 
                                 withSpinner(
                                   DT::dataTableOutput("pfam_protein_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        ),
                        tabPanel("Domain Summary", 
                                 withSpinner(
                                   DT::dataTableOutput("pfam_domain_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        )
                      )
                    )
                  )
                )
              ),
              
              ### 9.4 Survival Analysis------------------
              fluidRow(
                box(
                  title = "9.4 Survival Analysis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             selectInput("survival_genes", 
                                         "Select genes for survival analysis:",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             textInput("time_col", 
                                       "Time column name:", 
                                       value = "CDR_OS.time"),
                             
                             textInput("status_col", 
                                       "Status column name:", 
                                       value = "CDR_OS"),
                             
                             actionButton("run_survival", 
                                          "Run Survival Analysis", 
                                          class = "btn-primary",
                                          icon = icon("heartbeat")),
                             br(), br()
                      ),
                      
                      column(8,
                             withSpinner(
                               plotOutput("survival_plot", height = "400px"),
                               type = 4,
                               color = "#1f231b"
                             ),
                             
                             br(),
                             
                             conditionalPanel(
                               condition = "output.survival_geneset_run",
                               h4("Gene Set Results:", style = "color: #2a76bd; font-weight: 600;"),
                               withSpinner(
                                 DT::dataTableOutput("survival_geneset_table"),
                                 type = 4,
                                 color = "#1f231b"
                               )
                             )
                      )
                    )
                  )
                )
              ),
              
              ### 9.6 Clinical Enrichment Analysis------------------
              fluidRow(
                box(
                  title = "9.6 Clinical Enrichment Analysis", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             fileInput("clinical_file_enrichment", 
                                       "Upload Clinical Data (optional):",
                                       accept = c(".txt", ".tsv", ".csv")),
                             
                             textInput("clinical_feature_enrichment", 
                                       "Clinical feature column:", 
                                       value = "FAB_classification",
                                       placeholder = "e.g., FAB_classification, Stage, Grade, Subtype"),
                             
                             numericInput("enrichment_pval", 
                                          "P-value threshold:", 
                                          value = 0.05, min = 0.001, max = 0.1, step = 0.01),
                             
                             actionButton("run_clinical_enrichment", 
                                          "Run Clinical Enrichment", 
                                          class = "btn-primary",
                                          icon = icon("user-md"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("Clinical Enrichment:"),
                               p("Performs groupwise and pairwise comparisons to identify enriched mutations for every category within a clinical feature."),
                               p("Uses Fisher's exact test to test for associations.")
                             )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "output.clinical_enrichment_run",
                      tabsetPanel(
                        tabPanel("Enrichment Plot", 
                                 withSpinner(
                                   plotOutput("clinical_enrichment_plot", height = "400px"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        ),
                        tabPanel("Groupwise Results", 
                                 withSpinner(
                                   DT::dataTableOutput("clinical_enrichment_groupwise_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        ),
                        tabPanel("Pairwise Results", 
                                 withSpinner(
                                   DT::dataTableOutput("clinical_enrichment_pairwise_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        )
                      )
                    )
                  )
                )
              ),
              
              ### 9.7 Drug-Gene Interactions------------------
              fluidRow(
                box(
                  title = "9.7 Drug-Gene Interactions", 
                  status = "danger", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             selectInput("drug_genes", 
                                         "Extract specific gene's drug interaction:",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             numericInput("drug_font_size", 
                                          "Font size:", 
                                          value = 0.75, min = 0.5, max = 1.5, step = 0.05),
                             
                             checkboxInput("show_drug_details", 
                                           "Show detailed drug information", 
                                           value = FALSE),
                             
                             actionButton("run_drug_interactions", 
                                          "Analyze Drug-Gene Interactions", 
                                          class = "btn-primary",
                                          icon = icon("pills"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Drug-Gene Interactions:"),
                               p("Checks for drug–gene interactions and gene druggability information compiled from Drug Gene Interaction database."),
                               p("Shows potential druggable gene categories along with top genes involved in them."),
                               p("Provides information on known/reported drugs that interact with specific genes.")
                             )
                      )
                    ),
                    
                    withSpinner(
                      plotOutput("drug_interactions_plot", height = "600px"),
                      type = 4,
                      color = "#1f231b"
                    ),
                    
                    conditionalPanel(
                      condition = "output.drug_interactions_run",
                      br(),
                      tabsetPanel(
                        tabPanel("Drug Summary", 
                                 h4("Drug-Gene Interaction Summary:", style = "color: #dc3545; font-weight: 600;"),
                                 withSpinner(
                                   DT::dataTableOutput("drug_summary_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        ),
                        tabPanel("Detailed Interactions", 
                                 h4("Detailed Drug-Gene Interactions:", style = "color: #dc3545; font-weight: 600;"),
                                 withSpinner(
                                   DT::dataTableOutput("drug_specific_gene_table"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                        )
                      )
                    )
                  )
                )
              ),
              
              ### 9.8 Oncogenic Signaling Pathways------------------
              fluidRow(
                box(
                  title = "9.8 Oncogenic Signaling Pathways", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             selectInput("pathway_plot_type", 
                                         "Plot type:",
                                         choices = c("Treemap" = "treemap"),
                                         selected = "treemap"),
                             
                             numericInput("pathway_font_size", 
                                          "Font size:", 
                                          value = 1, min = 0/8, max = 1.8, step = 0.1),
                             
                             actionButton("run_pathways_analysis", 
                                          "Analyze Oncogenic Pathways", 
                                          class = "btn-primary",
                                          icon = icon("project-diagram"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Oncogenic Signaling Pathways:"),
                               p("Checks for enrichment of known Oncogenic Signaling Pathways from TCGA cohorts."),
                               p("Identifies which pathways are significantly altered in your dataset."),
                               p("Uses pathway information compiled from various cancer genomics studies.")
                             )
                      )
                    ),
                    
                    withSpinner(
                      plotOutput("pathways_analysis_plot", height = "600px"),
                      type = 4,
                      color = "#1f231b"
                    ),
                    
                  )
                )
              ),
              ### 9.9 Tumor Heterogeneity and MATH Scores-------------------
              fluidRow(
                box(
                  title = "9.9 Tumor Heterogeneity and MATH Scores", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    fluidRow(
                      column(4,
                             selectInput("heterogeneity_sample", 
                                         "Select Sample for Analysis:",
                                         choices = NULL,
                                         selected = NULL),
                             
                             selectizeInput("heterogeneity_vaf_col",
                                            "VAF Column Name:",
                                            choices = c("Auto-calculate" = "",
                                                        "VAF" = "VAF",
                                                        "tumor_vaf" = "tumor_vaf",
                                                        "i_ESP_MAF" = "i_ESP_MAF"),
                                            selected = "",
                                            options = list(
                                              create = TRUE,
                                              placeholder = 'Select or leave empty to auto-calculate'
                                            )),
                             
                             fileInput("seg_file", 
                                       "Upload Segmentation File (optional):",
                                       accept = c(".txt", ".tsv", ".seg"),
                                       placeholder = "Segmented copy number file"),
                             
                             checkboxInput("highlight_cn_vars", 
                                           "Highlight copy number altered variants", 
                                           value = TRUE),
                             
                             actionButton("run_heterogeneity", 
                                          "Run Heterogeneity Analysis", 
                                          class = "btn-primary",
                                          icon = icon("sitemap"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Tumor Heterogeneity:"),
                               p("Tumors consist of multiple clones with different variant allele frequencies (VAF)."),
                               p("This analysis clusters variants by VAF to infer clonality and calculate MATH scores."),
                               tags$ul(
                                 tags$li(strong("MATH Score:"), "Quantitative measure of intra-tumor heterogeneity"),
                                 tags$li(strong("Higher MATH:"), "Associated with poor outcomes"),
                                 tags$li(strong("Segmentation file:"), "Helps exclude copy number altered variants")
                               )
                             )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "output.heterogeneity_run",
                      
                      br(),
                      
                      # Analysis Summary
                      fluidRow(
                        box(
                          title = "Analysis Summary", 
                          status = "success", 
                          solidHeader = TRUE, 
                          width = 6,
                          
                          verbatimTextOutput("heterogeneity_summary")
                        ),
                        
                        box(
                          title = "Cluster Means", 
                          status = "primary", 
                          solidHeader = TRUE, 
                          width = 6,
                          
                          withSpinner(
                            DT::dataTableOutput("cluster_means_table"),
                            type = 4,
                            color = "#1f231b"
                          )
                        )
                      ),
                      
                      # Main Heterogeneity Plot
                      fluidRow(
                        box(
                          title = "Heterogeneity Clustering Plot", 
                          status = "info", 
                          solidHeader = TRUE, 
                          width = 12,
                          
                          div(
                            class = "info-panel",
                            h5("Plot Interpretation:"),
                            p("This plot shows variant allele frequency distribution and clustering results."),
                            tags$ul(
                              tags$li("Each point represents a variant"),
                              tags$li("Colors indicate different clones/clusters"), 
                              tags$li("MATH score is displayed in the plot title"),
                              tags$li("Higher VAF clusters represent major clones")
                            )
                          ),
                          
                          withSpinner(
                            plotOutput("heterogeneity_plot", height = "500px"),
                            type = 4,
                            color = "#1f231b"
                          )
                        )
                      ),
                      
                      # Copy Number Altered Variants (if segmentation file provided)
                      conditionalPanel(
                        condition = "input.seg_file",
                        fluidRow(
                          box(
                            title = "Copy Number Altered Variants", 
                            status = "danger", 
                            solidHeader = TRUE, 
                            width = 12,
                            
                            div(
                              class = "info-panel",
                              h5("Copy Number Altered Variants:"),
                              p("These variants are located in copy number altered regions and are excluded from clustering."),
                              p("Copy number alterations can cause abnormally high or low VAFs that don't reflect true clonality.")
                            ),
                            
                            withSpinner(
                              DT::dataTableOutput("cn_altered_table"),
                              type = 4,
                              color = "#1f231b"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              ### 9.10 Mutational Signatures Analysis ------------------
              fluidRow(
                box(
                  title = "9.10 Mutational Signatures Analysis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "output.data_loaded",
                    
                    # Step 1: Trinucleotide Matrix Extraction
                    h4("Step 1: Trinucleotide Matrix & APOBEC Enrichment", style = "color: #2a76bd; font-weight: 600;"),
                    
                    fluidRow(
                      column(4,
                             selectInput("ref_genome_sig", 
                                         "Reference Genome:",
                                         choices = c("BSgenome.Hsapiens.UCSC.hg19" = "BSgenome.Hsapiens.UCSC.hg19",
                                                     "BSgenome.Hsapiens.UCSC.hg38" = "BSgenome.Hsapiens.UCSC.hg38"),
                                         selected = "BSgenome.Hsapiens.UCSC.hg19"),
                             
                             textInput("chr_prefix", 
                                       "Chromosome prefix:", 
                                       value = "chr"),
                             
                             actionButton("run_trinucleotide", 
                                          "Extract Trinucleotide Matrix", 
                                          class = "btn-primary",
                                          icon = icon("dna"))
                      ),
                      
                      column(8,
                             div(
                               class = "info-panel",
                               h5("About Mutational Signatures:"),
                               p("Every cancer leaves a signature characterized by specific nucleotide substitution patterns."),
                               tags$ul(
                                 tags$li(strong("Trinucleotide Matrix:"), "96 substitution classes based on immediate bases surrounding mutations"),
                                 tags$li(strong("APOBEC Enrichment:"), "C>T transitions in TCW motifs, common in solid tumors"),
                                 tags$li(strong("NMF Decomposition:"), "Extract signatures using non-negative matrix factorization")
                               )
                             )
                      )
                    ),
                    
                    # Results from Step 1
                    conditionalPanel(
                      condition = "output.trinucleotide_run",
                      
                      br(),
                      
                      fluidRow(
                        column(6,
                               h5("Matrix Summary:", style = "color: #17a2b8; font-weight: 600;"),
                               verbatimTextOutput("tnm_summary")
                        ),
                        column(6,
                               h5("APOBEC Enrichment:", style = "color: #17a2b8; font-weight: 600;"),
                               verbatimTextOutput("apobec_summary")
                        )
                      ),
                      
                      hr(),
                      
                      # Step 2: APOBEC Difference Analysis
                      h4("Step 2: APOBEC Enriched vs Non-Enriched Analysis", style = "color: #2a76bd; font-weight: 600;"),
                      
                      fluidRow(
                        column(4,
                               numericInput("apobec_pval", 
                                            "P-value threshold:", 
                                            value = 0.2, min = 0.01, max = 1, step = 0.01),
                               
                               actionButton("run_apobec_diff", 
                                            "Analyze APOBEC Differences", 
                                            class = "btn-info",
                                            icon = icon("chart-bar"))
                        ),
                        
                        column(8,
                               conditionalPanel(
                                 condition = "output.apobec_diff_run",
                                 withSpinner(
                                   plotOutput("apobec_diff_plot", height = "300px"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                               )
                        )
                      ),
                      
                      hr(),
                      
                      # Step 3: Signature Analysis
                      h4("Step 3: Signature Estimation & Extraction", style = "color: #2a76bd; font-weight: 600;"),
                      
                      fluidRow(
                        column(4,
                               numericInput("n_signatures", 
                                            "Number of signatures to try:", 
                                            value = 6, min = 2, max = 10, step = 1),
                               
                               actionButton("run_estimate_signatures", 
                                            "Estimate Signatures", 
                                            class = "btn-success",
                                            icon = icon("search")),
                               
                               br(), br(),
                               
                               conditionalPanel(
                                 condition = "output.signatures_estimated",
                                 numericInput("final_n_signatures", 
                                              "Final number of signatures:", 
                                              value = 3, min = 1, max = 10, step = 1),
                                 
                                 actionButton("run_extract_signatures", 
                                              "Extract Signatures", 
                                              class = "btn-warning",
                                              icon = icon("extract"))
                               )
                        ),
                        
                        column(8,
                               conditionalPanel(
                                 condition = "output.signatures_estimated",
                                 h5("Cophenetic Correlation Plot:", style = "color: #28a745; font-weight: 600;"),
                                 p("Choose optimal number where correlation drops significantly:"),
                                 withSpinner(
                                   plotOutput("cophenetic_plot", height = "300px"),
                                   type = 4,
                                   color = "#1f231b"
                                 )
                               )
                        )
                      ),
                      
                      # Step 4: Results Visualization
                      conditionalPanel(
                        condition = "output.signatures_extracted",
                        
                        hr(),
                        
                        h4("Step 4: Signature Results", style = "color: #2a76bd; font-weight: 600;"),
                        
                        tabsetPanel(
                          tabPanel("Signature Plots", 
                                   selectInput("sig_db_plot", 
                                               "Signature Database:",
                                               choices = c("SBS (v3.4)" = "SBS", 
                                                           "Legacy (v2)" = "legacy"),
                                               selected = "SBS"),
                                   
                                   withSpinner(
                                     plotOutput("signatures_plot", height = "600px"),
                                     type = 4,
                                     color = "#1f231b"
                                   )
                          ),
                          
                          tabPanel("COSMIC Comparison", 
                                   fluidRow(
                                     column(6,
                                            h5("Legacy COSMIC:", style = "color: #17a2b8; font-weight: 600;"),
                                            verbatimTextOutput("cosmic_legacy_comparison")
                                     ),
                                     column(6,
                                            h5("SBS COSMIC:", style = "color: #17a2b8; font-weight: 600;"),
                                            verbatimTextOutput("cosmic_sbs_comparison")
                                     )
                                   ),
                                   
                                   br(),
                                   plotOutput("cosine_similarity_heatmap", height = "400px")
                          ),
                          
                          tabPanel("Sample Contributions", 
                                   withSpinner(
                                     DT::dataTableOutput("signature_contributions_table"),
                                     type = 4,
                                     color = "#1f231b"
                                   )
                          ),
                          
                          tabPanel("APOBEC Details", 
                                   conditionalPanel(
                                     condition = "output.apobec_diff_run",
                                     
                                     h5("Differentially Altered Genes:", style = "color: #17a2b8; font-weight: 600;"),
                                     withSpinner(
                                       DT::dataTableOutput("apobec_diff_results_table"),
                                       type = 4,
                                       color = "#1f231b"
                                     ),
                                     
                                     br(),
                                     
                                     h5("Sample Summary:", style = "color: #17a2b8; font-weight: 600;"),
                                     withSpinner(
                                       DT::dataTableOutput("apobec_sample_summary_table"),
                                       type = 4,
                                       color = "#1f231b"
                                     )
                                   )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.signatures_estimated",
                h5("Signature Estimation Console Output:"),
                verbatimTextOutput("estimation_console")
              ),
              conditionalPanel(
                condition = "output.trinucleotide_run",
                h5("Trinucleotide Matrix Extraction Console Output:"),
                verbatimTextOutput("trinucleotide_console")
              )
      )
    )
  )
)

# Server ==============================

options(shiny.maxRequestSize = 100*1024^2)  # 允许100MB文件
server <- function(input, output, session) {
  
  values <- reactiveValues(
    tcga_available = NULL,
    loaded_maf = NULL,
    uploaded_maf = NULL,
    data_loaded = FALSE,
    uploaded_data_loaded = FALSE,
    current_maf = NULL,
    interactions_results = NULL,
    interactions_run = FALSE,
    oncodrive_results = NULL,
    oncodrive_run = FALSE,
    pfam_results = NULL,
    pfam_run = FALSE,
    clinical_enrichment_results = NULL,
    clinical_enrichment_run = FALSE,
    drug_interactions_results = NULL,
    drug_interactions_run = FALSE,
    pathways_analysis_results = NULL,
    pathways_analysis_run = FALSE,
    survival_run = FALSE,
    survival_geneset_run = FALSE,
    trinucleotide_results = NULL,
    trinucleotide_run = FALSE,
    apobec_diff_results = NULL,
    apobec_diff_run = FALSE,
    signatures_estimation = NULL,
    signatures_estimated = FALSE,
    extracted_signatures = NULL,
    signatures_extracted = FALSE,
    trinucleotide_output = NULL,
    estimation_output = NULL,
    cosmic_comparisons = list()
  )
  
  # File upload status indicator
  output$uploaded_data_loaded <- reactive({
    return(values$uploaded_data_loaded)
  })
  outputOptions(output, "uploaded_data_loaded", suspendWhenHidden = FALSE)
  # MAF file upload processing-----------------
  observeEvent(input$upload_file, {
    req(input$upload_file)
    # Immediately display notification, indicating start of processing
    showNotification("Processing file...", type = "message", duration = 2)
    # Output debugging information to console
    cat("\n\n========== File upload debugging information ==========\n")
    cat("Time:", Sys.time(), "\n")
    cat("File name:", input$upload_file$name, "\n")
    cat("File size:", input$upload_file$size, "bytes (", 
        round(input$upload_file$size/1024/1024, 2), "MB)\n")
    cat("File type:", input$upload_file$type, "\n")
    cat("Temporary path:", input$upload_file$datapath, "\n")
    cat("File exists?", file.exists(input$upload_file$datapath), "\n")
    
    withProgress(message = 'Processing uploaded file...', value = 0, {
      tryCatch({
        file_path <- input$upload_file$datapath

        incProgress(0.3, detail = "Reading MAF file...")

        # Directly use read.maf to read file path, not first read as data.frame
        incProgress(0.6, detail = "Converting to MAF object...")

        # Key fix: directly pass file path to read.maf
        values$uploaded_maf <- maftools::read.maf(
          maf = file_path,  # Use file path, not data.frame
          verbose = FALSE,
          useAll = TRUE     # Include all mutations for user uploaded files
        )

        values$uploaded_data_loaded <- TRUE

        incProgress(1, detail = "Complete!")

        showNotification(
          paste("Successfully uploaded", input$upload_file$name),
          type = "message", duration = 5
        )

      }, error = function(e) {
        values$uploaded_data_loaded <- FALSE

        # Provide more detailed error information
        error_msg <- e$message
        if (grepl("missing required fields", error_msg, ignore.case = TRUE)) {
          showNotification(
            "Error: MAF file is missing required columns. Required: Hugo_Symbol, Chromosome, Start_Position, End_Position, Reference_Allele, Tumor_Seq_Allele2, Variant_Classification, Variant_Type, Tumor_Sample_Barcode",
            type = "error", duration = 10
          )
        } else if (grepl("no non-synonymous", error_msg, ignore.case = TRUE)) {
          showNotification(
            "Error: No non-synonymous mutations found. Check Variant_Classification column.",
            type = "error", duration = 10
          )
        } else {
          showNotification(
            paste("Error processing file:", error_msg),
            type = "error", duration = 10
          )
        }
      })
    })
  })

  # Use uploaded data (smart replace mechanism---------------------------
  observeEvent(input$use_uploaded, {
    if (!is.null(values$uploaded_maf)) {
      values$current_maf <- values$uploaded_maf  # Replace current data
      values$data_loaded <- TRUE
      
      # Update all analysis options
      tryCatch({
        gene_summary <- maftools::getGeneSummary(values$current_maf)
        gene_choices <- gene_summary$Hugo_Symbol[1:min(50, nrow(gene_summary))]
        updateSelectInput(session, "lollipop_gene", choices = gene_choices, selected = gene_choices[1])
        updateSelectInput(session, "drug_genes", choices = gene_choices)
        updateSelectInput(session, "survival_genes", choices = gene_choices)
        
        sample_summary <- maftools::getSampleSummary(values$current_maf)
        sample_choices <- sample_summary$Tumor_Sample_Barcode[1:min(20, nrow(sample_summary))]
        updateSelectInput(session, "rainfall_sample", choices = sample_choices, selected = sample_choices[1])
      }, error = function(e) {
        # Ignore update errors
      })
      
      showNotification("✓ Uploaded data is now active for all analyses", 
                       type = "message", duration = 5)
    }
  })
  
  # Modify Sample and Gene statistics table - use current active data
  output$sample_summary <- DT::renderDataTable({
    if (!is.null(values$current_maf)) {
      sample_summary <- maftools::getSampleSummary(values$current_maf)
      DT::datatable(
        sample_summary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
  
  output$gene_summary <- DT::renderDataTable({
    if (!is.null(values$current_maf)) {
      gene_summary <- maftools::getGeneSummary(values$current_maf)
      DT::datatable(
        gene_summary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
  
  # Fix data preview - use simple head()
  output$data_preview_5rows <- DT::renderDataTable({
    if (!is.null(values$current_maf)) {
      # Directly use head() to get first 5 rows
      preview_data <- head(values$current_maf@data, 5)
      
      DT::datatable(
        preview_data,
        options = list(
          scrollX = TRUE, 
          pageLength = 5, 
          dom = 't'  # Only display table
        ),
        rownames = FALSE
      )
    }
  })
  
  # TCGA data output function
  output$data_basic_stats <- renderText({
    if (values$data_loaded && !is.null(values$current_maf)) {
      n_samples <- length(unique(values$current_maf@data$Tumor_Sample_Barcode))
      n_variants <- nrow(values$current_maf@data)
      n_genes <- length(unique(values$current_maf@data$Hugo_Symbol))
      
      paste(
        paste("Total Samples:", n_samples),
        paste("Total Variants:", n_variants),
        paste("Total Genes:", n_genes),
        paste("Study:", input$selected_study),
        sep = "\n"
      )
    }
  })
  
  output$data_column_info <- renderText({
    if (values$data_loaded && !is.null(values$current_maf)) {
      cols <- colnames(values$current_maf@data)
      vaf_cols <- cols[grepl("vaf|VAF|freq|count|alt", cols, ignore.case = TRUE)]
      
      paste(
        paste("Total Columns:", length(cols)),
        paste("VAF(Variant Allele Frequency)-related:", length(vaf_cols)),
        if(length(vaf_cols) > 0) { paste("VAF Columns:", paste(head(vaf_cols, 8), collapse = ", ")) } else { "No VAF columns" },
        sep = "\n"
      )
    }
  })
  
  output$all_column_names <- renderText({
    if (values$data_loaded && !is.null(values$current_maf)) {
      cols <- colnames(values$current_maf@data)
      paste(cols, collapse = ", ")
    }
  })
  
  # Uploaded data output function
  output$uploaded_data_basic_stats <- renderText({
    if (values$uploaded_data_loaded && !is.null(values$uploaded_maf)) {
      n_samples <- length(unique(values$uploaded_maf@data$Tumor_Sample_Barcode))
      n_variants <- nrow(values$uploaded_maf@data)
      n_genes <- length(unique(values$uploaded_maf@data$Hugo_Symbol))
      
      paste(
        paste("Total Samples:", n_samples),
        paste("Total Variants:", n_variants),
        paste("Total Genes:", n_genes),
        paste("File:", input$upload_file$name),
        sep = "\n"
      )
    }
  })
  
  output$uploaded_data_column_info <- renderText({
    if (values$uploaded_data_loaded && !is.null(values$uploaded_maf)) {
      cols <- colnames(values$uploaded_maf@data)
      vaf_cols <- cols[grepl("vaf|VAF|freq|count", cols, ignore.case = TRUE)]
      
      paste(
        paste("Total Columns:", length(cols)),
        paste("VAF-related:", length(vaf_cols)),
        if(length(vaf_cols) > 0) { paste("VAF Columns:", paste(head(vaf_cols, 3), collapse = ", ")) } else { "No VAF columns" },
        sep = "\n"
      )
    }
  })
  
  output$uploaded_all_column_names <- renderText({
    if (values$uploaded_data_loaded && !is.null(values$uploaded_maf)) {
      cols <- colnames(values$uploaded_maf@data)
      paste(cols, collapse = ", ")
    }
  })
  
  # Available cohorts on startup ===============
  observe({
  tryCatch({
    if (!requireNamespace("maftools", quietly = TRUE)) {
      showNotification("maftools package not found. Please install it using BiocManager::install('maftools')", 
                       duration = NULL)
      return()
    }
    # Load both info sources
    mutations_info <- TCGAmutations::tcga_available()
    maftools_info <- maftools::tcgaAvailable()

    # Merge by Study_Abbreviation
    merged_info <- merge(maftools_info, mutations_info, by = "Study_Abbreviation", all.x = TRUE)

    # Optional: clean duplicate columns from merge
    if ("MC3.x" %in% colnames(merged_info)) {
      merged_info$MC3 <- merged_info$MC3.x
      merged_info$MC3.x <- NULL
      merged_info$MC3.y <- NULL
    }

    # Store everything in `values`
    values$tcga_available <- mutations_info
    values$abbre_info <- maftools_info
    values$merged_info <- merged_info

    # Use merged_info to build choices
    study_choices <- setNames(
      merged_info$Study_Abbreviation,
      paste0(merged_info$Study_Abbreviation, 
             " (", merged_info$Study_Name, ")")
    )

    updateSelectInput(session, "selected_study", 
                      choices = study_choices,
                      selected = "LAML")
  }, error = function(e) {
    showNotification(
      paste("Error loading TCGA cohorts:", e$message), 
      duration = NULL
    )
  })
})
  
  # TCGA study list display
  output$tcga_study_list <- renderText({
    if (!is.null(values$tcga_available)) {
      studies <- values$tcga_available$Study_Abbreviation
      paste("Available studies:", paste(head(studies, 10), collapse = ", "), 
            ifelse(length(studies) > 10, "...", ""))
    } else {
      "Loading available studies..."
    }
  })
  
  ## Load selected TCGA data ============
  #observeEvent(input$load_data, {
  #  req(input$selected_study, input$tcga_source)
  #  values$current_maf <- TCGAmutations::tcga_load(study = input$selected_study, source = input$tcga_source)
  #  values$data_loaded <- TRUE
  #})
  observeEvent(input$load_data, {
  req(input$selected_study, input$tcga_source)
  
  tryCatch({
    values$current_maf <- TCGAmutations::tcga_load(study = input$selected_study, source = input$tcga_source)
    values$data_loaded <- TRUE
    
    # 添加这部分 - 更新基因选择
    gene_summary <- maftools::getGeneSummary(values$current_maf)
    gene_choices <- gene_summary$Hugo_Symbol[1:min(50, nrow(gene_summary))]
    updateSelectInput(session, "lollipop_gene", choices = gene_choices, selected = gene_choices[1])
    updateSelectInput(session, "drug_genes", choices = gene_choices)
    updateSelectInput(session, "survival_genes", choices = gene_choices)
    
    sample_summary <- maftools::getSampleSummary(values$current_maf)
    sample_choices <- sample_summary$Tumor_Sample_Barcode[1:min(20, nrow(sample_summary))]
    updateSelectInput(session, "rainfall_sample", choices = sample_choices, selected = sample_choices[1])
    
    showNotification("✓ TCGA data loaded successfully", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error loading TCGA data:", e$message), type = "error", duration = 5)
    values$data_loaded <- FALSE
  })
})
  # Output status variables
  output$data_loaded <- reactive({ values$data_loaded })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$interactions_run <- reactive({ values$interactions_run })
  outputOptions(output, "interactions_run", suspendWhenHidden = FALSE)
  
  output$oncodrive_run <- reactive({ values$oncodrive_run })
  outputOptions(output, "oncodrive_run", suspendWhenHidden = FALSE)
  
  output$pfam_run <- reactive({ values$pfam_run })
  outputOptions(output, "pfam_run", suspendWhenHidden = FALSE)
  
  output$clinical_enrichment_run <- reactive({ values$clinical_enrichment_run })
  outputOptions(output, "clinical_enrichment_run", suspendWhenHidden = FALSE)
  
  output$drug_interactions_run <- reactive({ values$drug_interactions_run })
  outputOptions(output, "drug_interactions_run", suspendWhenHidden = FALSE)
  
  output$pathways_analysis_run <- reactive({ values$pathways_analysis_run })
  outputOptions(output, "pathways_analysis_run", suspendWhenHidden = FALSE)
  output$trinucleotide_run <- reactive({ values$trinucleotide_run })

  outputOptions(output, "trinucleotide_run", suspendWhenHidden = FALSE)

  output$apobec_diff_run <- reactive({ values$apobec_diff_run })
  outputOptions(output, "apobec_diff_run", suspendWhenHidden = FALSE)

  output$signatures_estimated <- reactive({ values$signatures_estimated })
  outputOptions(output, "signatures_estimated", suspendWhenHidden = FALSE)

  output$signatures_extracted <- reactive({ values$signatures_extracted })
  outputOptions(output, "signatures_extracted", suspendWhenHidden = FALSE)
  # Upload status
  output$upload_status <- reactive({ !is.null(input$upload_file) })
  outputOptions(output, "upload_status", suspendWhenHidden = FALSE)
  
  # Uploaded MAF Summary
  output$uploaded_maf_summary <- renderText({
    if (values$uploaded_data_loaded && !is.null(values$uploaded_maf)) {
      capture.output(values$uploaded_maf)
    }
  })
  
  # MAF Summary (original functionality)
  output$maf_summary <- renderText({
    if (values$data_loaded && !is.null(values$current_maf)) {
      capture.output(values$current_maf)
    }
  })
  
  # Data preview (keep original functionality)
  output$data_preview <- DT::renderDataTable({
    if (!is.null(values$current_maf)) {
      preview_data <- head(values$current_maf@data, 5)
      DT::datatable(
        preview_data,
        options = list(scrollX = TRUE, pageLength = 5, dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Display available cohorts table
  output$cohorts_table <- DT::renderDataTable({
    if (!is.null(values$tcga_available)) {
      DT::datatable(
        values$tcga_available,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    }
  })
  
  ## MAF summary plots ======================
  output$maf_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        vc_cols = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
        names(vc_cols) = c(
          'Frame_Shift_Del',
          'Missense_Mutation',
          'Nonsense_Mutation',
          'Multi_Hit',
          'Frame_Shift_Ins',
          'In_Frame_Ins',
          'Splice_Site',
          'In_Frame_Del'
        )
        maftools::plotmafSummary(maf = values$current_maf, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE,titvRaw = FALSE,color =vc_cols )
      }, error = function(e) {
        # Try alternative approach if plotmafSummary fails
        tryCatch({
          
          maftools::plotmafSummary(maf = values$current_maf, rmOutlier = TRUE, addStat = 'median')
        }, error = function(e2) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("Error generating MAF summary plot:", e2$message), cex = 1.2, col = "red")
        })
      })
    }
  })
  output$download_maf_plot <- downloadHandler(
    filename = function() { paste("mafplot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      if (values$data_loaded && !is.null(values$current_maf)) {
        vc_cols = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
        names(vc_cols) = c(
          'Frame_Shift_Del',
          'Missense_Mutation',
          'Nonsense_Mutation',
          'Multi_Hit',
          'Frame_Shift_Ins',
          'In_Frame_Ins',
          'Splice_Site',
          'In_Frame_Del'
        )
        png(file, width = 14, height = 10, units = "in", res = 300)
        maftools::plotmafSummary(maf = values$current_maf, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE,titvRaw = FALSE,color =vc_cols )
        dev.off()
      }
    }
  )
  ## OnCo plots ======================
  output$oncoplot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        observe({
            values$pathways <- switch(
              input$pathways_option,
              "none" = NULL,
              "sigpw" = "sigpw",
              "smgbp" = "smgbp"
            )
          })
        vc_cols = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
        names(vc_cols) = c(
          'Frame_Shift_Del',
          'Missense_Mutation',
          'Nonsense_Mutation',
          'Multi_Hit',
          'Frame_Shift_Ins',
          'In_Frame_Ins',
          'Splice_Site',
          'In_Frame_Del'
        )
        maftools::oncoplot(
                maf = values$current_maf,
                top = input$top_genes,
                draw_titv = TRUE,
                pathways = values$pathways,  
                colors = vc_cols
              )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error generating oncoplot:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  output$download_oncoplot <- downloadHandler(
    filename = function() { paste("oncoplot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      if (values$data_loaded && !is.null(values$current_maf)) {
        vc_cols = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
        names(vc_cols) = c(
          'Frame_Shift_Del',
          'Missense_Mutation',
          'Nonsense_Mutation',
          'Multi_Hit',
          'Frame_Shift_Ins',
          'In_Frame_Ins',
          'Splice_Site',
          'In_Frame_Del'
        )
        png(file, width = 14, height = 10, units = "in", res = 300)
        maftools::oncoplot(maf = values$current_maf, top = input$top_genes, showTumorSampleBarcodes = FALSE,draw_titv = TRUE,colors = vc_cols)
        dev.off()
      }
    }
  )
  ## Ti/Tv Analysis Plot ========================
  output$titv_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        titv_res <- maftools::titv(maf = values$current_maf, plot = FALSE, useSyn = input$use_synonymous)
        maftools::plotTiTv(res = titv_res)
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error generating Ti/Tv plot:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  output$download_titv_plot <- downloadHandler(
    filename = function() { paste("titvplot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      if (values$data_loaded && !is.null(values$current_maf)) {
        png(file, width = 14, height = 10, units = "in", res = 300)
        maftools::oncoplot(maf = values$current_maf, top = input$top_genes, showTumorSampleBarcodes = FALSE)
        dev.off()
      }
    }
  )
  ## TiTv summary table ========================
  output$titv_summary <- DT::renderDataTable({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        titv_res <- maftools::titv(maf = values$current_maf, plot = FALSE, useSyn = input$use_synonymous)
        titv_summary <- titv_res$fraction.contribution
        DT::datatable(
          titv_summary,
          options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
          rownames = FALSE,
          caption = "Fraction of each conversion type"
        ) %>%
          DT::formatRound(columns = 2:ncol(titv_summary), digits = 3)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  ## Lollipop plots ========================
  # prepare 
  prepare_g3_data <- function(maf_obj, gene_name) {
    # extract
    gene_data <- maf_obj@data[maf_obj@data$Hugo_Symbol == gene_name, ]
    
    # 转换为g3Lollipop需要的格式
    mutation_data <- data.frame(
      Hugo_Symbol = gene_data$Hugo_Symbol,
      AA_Position = as.numeric(gsub("\\D", "", gene_data$HGVSp_Short)),
      HGVSp_Short = gene_data$HGVSp_Short,
      Protein_Change = gene_data$HGVSp_Short,
      Mutation_Class = gene_data$Variant_Classification,
      stringsAsFactors = FALSE
    )
    
    # 移除无效数据
    mutation_data <- mutation_data[
      !is.na(mutation_data$AA_Position) & 
        !is.na(mutation_data$HGVSp_Short) & 
        mutation_data$HGVSp_Short != "" & 
        mutation_data$HGVSp_Short != ".", 
    ]
    
    return(mutation_data)
  }
  
  # MAFtools lollipop plot 
  output$lollipop_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf) && !is.null(input$lollipop_gene)) {
      tryCatch({
        maftools::lollipopPlot(
          maf = values$current_maf,
          gene = input$lollipop_gene,
          AACol = input$aa_col,
          showMutationRate = input$show_mutation_rate,
          labelPos = if (isTRUE(input$show_labelPos)) "all" else NULL,
          labPosAngle = "90",
          showDomainLabel = TRUE
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error: Check if AACol", input$aa_col, "exists in data"), cex = 1.2, col = "red")
      })
    }
  })
  
  # G3viz lollipop plot ========================
  output$g3_lollipop_plot <- renderUI({
    if (values$data_loaded && !is.null(values$current_maf) && !is.null(input$lollipop_gene)) {
      tryCatch({
        # prepare data
        mutation_dat <- prepare_g3_data(values$current_maf, input$lollipop_gene)
        
        # check if there is data
        if (nrow(mutation_dat) == 0) {
          return('<div style="text-align: center; padding: 50px; color: #666;">
                <h4>No mutation data found</h4>
                <p>No valid mutations found for the selected gene.</p>
                </div>')
        }
        
        # use g3Lollipop.theme function to set theme
        plot_options <- g3Lollipop.theme(
          theme.name = input$g3_theme,
          title.text = paste(input$lollipop_gene, "gene mutations"),
          y.axis.label = paste("# of", input$lollipop_gene, "Mutations")
        )
        
        # generate g3viz plot - use parameters from documentation
        widget <- g3Lollipop(
          mutation_dat,
          gene.symbol = input$lollipop_gene,
          plot.options = plot_options,
          protein.change.col = "HGVSp_Short",
          output.filename = paste0(input$lollipop_gene, "_lollipop")
        )

        # return widget object
        return(widget)
        
      }, error = function(e) {
        return(paste0('<div style="text-align: center; padding: 50px; color: red;">
                    <h4>Error generating G3viz plot</h4>
                    <p>Error message: ', e$message, '</p>
                    <p>Please check your data format and gene selection.</p>
                    </div>'))
      })
    } else {
      return('<div style="text-align: center; padding: 50px; color: #666;">
            <h4>Please load data and select a gene</h4>
            </div>')
    }
  })
  ## Rainfall plots ========================
  output$rainfall_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf) && !is.null(input$rainfall_sample)) {
      tryCatch({
        maftools::rainfallPlot(
          maf = values$current_maf,
          tsb = input$rainfall_sample,
          detectChangePoints = input$detect_changepoints,
          pointSize = input$point_size
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error generating rainfall plot:", e$message), cex = 1, col = "red")
      })
    }
  })
  ## Plot VAF ===============
  output$vaf_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        maftools::plotVaf(maf = values$current_maf, vafCol = input$vaf_col, top = input$top_genes_vaf)
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error: Check if VAF column", input$vaf_col, "exists"), cex = 1.2, col = "red")
      })
    }
  })
  ## Compare mutation load plots ========================
  output$tcga_compare_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      tryCatch({
        maftools::tcgaCompare(
          maf = values$current_maf,
          cohortName = input$cohort_name,
          logscale = input$log_scale,
          capture_size = input$capture_size
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error in TCGA comparison:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  
  
  ## Analysis Functions ====================
  
  ### 9.1 Somatic Interactions ====================
  observeEvent(input$run_interactions, {
    req(values$current_maf)
    
    withProgress(message = 'Running somatic interactions analysis...', value = 0, {
      tryCatch({
        incProgress(0.5, detail = "Calculating interactions...")
        
        # Store the results in reactive values
        values$interactions_results <- maftools::somaticInteractions(
          maf = values$current_maf,
          top = input$interaction_top_genes,
          pvalue = c(input$pvalue_threshold, 0.1)
        )
        
        values$interactions_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Somatic interactions analysis completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in somatic interactions:", e$message), duration = 10)
      })
    })
  })
  
  #### Somatic interactions plot ====================
  output$somatic_interactions_plot <- renderPlot({
    if (values$interactions_run && !is.null(values$interactions_results)) {
      tryCatch({
        # Use the correct function to create the plot
        maftools::somaticInteractions(
          maf = values$current_maf,
          top = input$interaction_top_genes,
          pvalue = c(input$pvalue_threshold, 0.1)
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting interactions:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  #### Somatic interactions table  ====================
  output$interactions_table <- DT::renderDataTable({
    if (values$interactions_run && !is.null(values$interactions_results)) {
      DT::datatable(
        values$interactions_results,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "Somatic Interactions Analysis Results"
      ) %>%
        # Use actual existing column names for formatting
        DT::formatRound(
          columns = c("pValue", "pAdj", "oddsRatio", "event_ratio"), 
          digits = 4
        )
    }
  })
  
  ### 9.2 Cancer Driver Genes Detection using OncodriveCLUST ===================
  observeEvent(input$run_oncodrive, {
    req(values$current_maf)
    
    withProgress(message = 'Detecting cancer driver genes...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Checking data columns...")
        
        # Get available columns in the MAF
        current_maf <- values$current_maf@data
        available_cols <- colnames(current_maf)
        
        # Check if the specified AA column exists
        aa_col <- input$oncodrive_aa_col
        if (!aa_col %in% available_cols) {
          # Try common alternatives
          possible_cols <- c("HGVSp_Short", "Protein_Change", "AAChange", "amino_acid_change")
          aa_col <- possible_cols[possible_cols %in% available_cols][1]
          
          if (is.na(aa_col)) {
            showNotification(paste("Amino acid column not found. Available columns:", 
                                   paste(available_cols, collapse = ", ")), 
                             duration = 10, type = "error")
            return()
          } else {
            showNotification(paste("Using column:", aa_col), duration = 5, type = "message")
          }
        }
        
        incProgress(0.5, detail = "Running OncodriveCLUST algorithm...")
        
        # Run oncodrive analysis
        values$oncodrive_results <- maftools::oncodrive(
          maf = values$current_maf,
          AACol = aa_col,
          minMut = input$oncodrive_min_mut,
          pvalMethod = input$oncodrive_pval_method
        )
        
        values$oncodrive_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Cancer driver gene detection completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in oncodrive analysis:", e$message), duration = 10)
      })
    })
  })
  #### plot onco drive ===========
  output$oncodrive_plot <- renderPlot({
    if (values$oncodrive_run && !is.null(values$oncodrive_results)) {
      tryCatch({
        # Plot the oncodrive results
        maftools::plotOncodrive(
          res = values$oncodrive_results,
          fdrCutOff = 0.1,
          useFraction = TRUE
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting oncodrive results:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  
  output$oncodrive_table <- DT::renderDataTable({
    if (values$oncodrive_run && !is.null(values$oncodrive_results)) {
      DT::datatable(
        values$oncodrive_results,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "Cancer Driver Genes detected by OncodriveCLUST"
      ) %>%
        DT::formatRound(columns = c("pval", "fdr", "fract_muts_in_clusters"), digits = 4)
    }
  })
  
  ### 9.3 Pfam Domains Analysis =====================
  observeEvent(input$run_pfam, {
    req(values$current_maf)
    
    withProgress(message = 'Analyzing Pfam domains...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Checking data columns...")
        
        # Get available columns in the MAF
        current_maf <- values$current_maf@data
        available_cols <- colnames(current_maf)
        
        # Check if the specified AA column exists
        aa_col <- input$pfam_aa_col
        if (!aa_col %in% available_cols) {
          # Try common alternatives
          possible_cols <- c("HGVSp_Short", "Protein_Change", "AAChange", "amino_acid_change")
          aa_col <- possible_cols[possible_cols %in% available_cols][1]
          
          if (is.na(aa_col)) {
            showNotification(paste("Amino acid column not found. Available columns:", 
                                   paste(available_cols, collapse = ", ")), 
                             duration = 10, type = "error")
            return()
          } else {
            showNotification(paste("Using column:", aa_col), duration = 5, type = "message")
          }
        }
        
        incProgress(0.5, detail = "Adding pfam domain information...")
        
        # Run pfam domains analysis
        values$pfam_results <- maftools::pfamDomains(
          maf = values$current_maf,
          AACol = aa_col,
          top = input$pfam_top_genes
        )
        
        values$pfam_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Pfam domains analysis completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in pfam analysis:", e$message), duration = 10)
      })
    })
  })
  #### Pfam domain plot ==================
  output$pfam_plot <- renderPlot({
    if (values$pfam_run && !is.null(values$pfam_results)) {
      tryCatch({
        # Get available columns and use the correct one
        current_maf <- values$current_maf@data
        available_cols <- colnames(current_maf)
        aa_col <- input$pfam_aa_col
        
        if (!aa_col %in% available_cols) {
          possible_cols <- c("HGVSp_Short", "Protein_Change", "AAChange", "amino_acid_change")
          aa_col <- possible_cols[possible_cols %in% available_cols][1]
        }
        
        # Re-run pfam analysis to generate the plot
        maftools::pfamDomains(
          maf = values$current_maf,
          AACol = aa_col,
          top = input$pfam_top_genes
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting pfam domains:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  #### Pfam protein table ==================
  output$pfam_protein_table <- DT::renderDataTable({
    if (values$pfam_run && !is.null(values$pfam_results)) {
      tryCatch({
        protein_summary <- values$pfam_results$proteinSummary
        DT::datatable(
          protein_summary,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Protein Summary - Mutations by Pfam Domain"
        ) %>%
          DT::formatRound(columns = "fraction", digits = 3)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  #### Pfam domain table ==================
  output$pfam_domain_table <- DT::renderDataTable({
    if (values$pfam_run && !is.null(values$pfam_results)) {
      tryCatch({
        domain_summary <- values$pfam_results$domainSummary
        DT::datatable(
          domain_summary,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Domain Summary - Most Frequently Affected Domains"
        ) 
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  ### 9.4 Survival Analysis ===================
  #### survival analysis by selecting genes --------
  # Update survival gene choices
  observe({
    if (values$data_loaded && !is.null(values$current_maf)) {
      gene_summary <- maftools::getGeneSummary(values$current_maf)
      gene_choices <- gene_summary$Hugo_Symbol[1:min(30, nrow(gene_summary))]
      updateSelectInput(session, "survival_genes", choices = gene_choices)
    }
  })
  
  # Simple: Remove the observeEvent for run_survival, put everything in renderPlot
  observeEvent(input$run_survival, {
    values$survival_run <- TRUE
  })
  
  # Render survival plot 
  output$survival_plot <- renderPlot({
    if (values$survival_run && !is.null(values$current_maf) && !is.null(input$survival_genes)) {
      
      # Just run mafSurvival directly here
      maftools::mafSurvival(
        maf = values$current_maf,
        genes = input$survival_genes,
        time = input$time_col,
        Status = input$status_col
      )
      
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Click 'Run Survival Analysis' to generate plot", cex = 1, col = "gray")
    }
  })
  
  # Status outputs
  output$survival_geneset_run <- reactive({ values$survival_geneset_run })
  outputOptions(output, "survival_geneset_run", suspendWhenHidden = FALSE)
  
  ### 9.10 Mutational Signatures-------------------------------
  observeEvent(input$run_signatures, {
    req(values$current_maf)
    
    withProgress(message = 'Extracting mutational signatures...', value = 0, {
      tryCatch({
        # Check if BSgenome package is available
        if (!requireNamespace("BSgenome", quietly = TRUE)) {
          showNotification("BSgenome package not installed. Please install it using BiocManager::install('BSgenome')", 
                           duration = 10, type = "warning")
          return()
        }
        
        # Check if specific genome package is available
        genome_pkg <- input$ref_genome_sig
        if (!requireNamespace(genome_pkg, quietly = TRUE)) {
          showNotification(paste("Genome package", genome_pkg, "not installed. Please install it using BiocManager::install('", genome_pkg, "')", sep = ""), 
                           duration = 10, type = "warning")
          return()
        }
        
        incProgress(0.3, detail = "Building trinucleotide matrix...")
        
        # Extract trinucleotide matrix
        tnm <- maftools::trinucleotideMatrix(
          maf = values$current_maf,
          prefix = 'chr',
          add = TRUE,
          ref_genome = input$ref_genome_sig
        )
        
        incProgress(0.7, detail = "Extracting signatures...")
        
        # Extract signatures
        signatures <- maftools::extractSignatures(
          mat = tnm,
          nTry = input$n_signatures,
          plotBestFitRes = FALSE
        )
        
        incProgress(1, detail = "Complete!")
        showNotification("Mutational signatures extracted!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error extracting signatures:", e$message), duration = 10)
      })
    })
  })
  
  output$signatures_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      if (input$run_signatures > 0) {
        tryCatch({
          # Check if BSgenome package is available
          if (!requireNamespace("BSgenome", quietly = TRUE)) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, "BSgenome package not installed.\nPlease install using:\nBiocManager::install('BSgenome')", 
                 cex = 1.2, col = "red")
            return()
          }
          
          # Check if specific genome package is available
          genome_pkg <- input$ref_genome_sig
          if (!requireNamespace(genome_pkg, quietly = TRUE)) {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("Genome package", genome_pkg, "not installed.\nPlease install using:\nBiocManager::install('", genome_pkg, "')", sep = ""), 
                 cex = 1.2, col = "red")
            return()
          }
          
          tnm <- maftools::trinucleotideMatrix(
            maf = values$current_maf,
            prefix = 'chr',
            add = TRUE,
            ref_genome = input$ref_genome_sig
          )
          
          signatures <- maftools::extractSignatures(
            mat = tnm,
            nTry = input$n_signatures,
            plotBestFitRes = FALSE
          )
          
          maftools::plotSignatures(signatures, title_size = 0.8)
          
        }, error = function(e) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("Error plotting signatures:", e$message), cex = 1, col = "red")
        })
      }
    }
  })
  
  ### 9.6 Clinical Enrichment Analysis============================
  observeEvent(input$run_clinical_enrichment, {
    req(values$current_maf)
    
    withProgress(message = 'Running clinical enrichment analysis...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Checking clinical data...")
        
        # Check if clinical data exists
        clinical_data <- values$current_maf@clinical.data
        
        if (is.null(clinical_data) || nrow(clinical_data) == 0) {
          showNotification("No clinical data found in MAF. Clinical enrichment requires clinical annotations.", 
                           duration = 10, type = "warning")
          return()
        }
        
        # Check if the specified clinical feature exists
        available_cols <- colnames(clinical_data)
        clinical_feature <- input$clinical_feature_enrichment
        
        if (!clinical_feature %in% available_cols) {
          showNotification(paste("Clinical feature column not found"), 
                           duration = 10, type = "error")
          return()
        }
        
        incProgress(0.7, detail = "Running enrichment analysis...")
        
        # Run clinical enrichment analysis
        values$clinical_enrichment_results <- maftools::clinicalEnrichment(
          maf = values$current_maf,
          clinicalFeature = clinical_feature,
          annotationDat = clinical_data
        )
        
        values$clinical_enrichment_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Clinical enrichment analysis completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in clinical enrichment analysis:", e$message), duration = 10)
      })
    })
  })
  ##### enrichment plot----------
  output$clinical_enrichment_plot <- renderPlot({
    if (values$clinical_enrichment_run && !is.null(values$clinical_enrichment_results)) {
      tryCatch({
        # Plot clinical enrichment results
        maftools::plotEnrichmentResults(
          enrich_res = values$clinical_enrichment_results,
          pVal = input$enrichment_pval
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting clinical enrichment:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  
  output$clinical_enrichment_groupwise_table <- DT::renderDataTable({
    if (values$clinical_enrichment_run && !is.null(values$clinical_enrichment_results)) {
      tryCatch({
        groupwise_results <- values$clinical_enrichment_results$groupwise_comparision
        DT::datatable(
          groupwise_results,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Groupwise Clinical Enrichment Results"
        ) %>%
          DT::formatRound(columns = "fdr", digits = 4)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  output$clinical_enrichment_pairwise_table <- DT::renderDataTable({
    if (values$clinical_enrichment_run && !is.null(values$clinical_enrichment_results)) {
      tryCatch({
        pairwise_results <- values$clinical_enrichment_results$pairwise_comparision
        DT::datatable(
          pairwise_results,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Pairwise Clinical Enrichment Results"
        ) %>%
          DT::formatRound(columns = "fdr", digits = 4)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  ### 9.7 Drug-Gene Interactions----------------------------
  observeEvent(input$run_drug_interactions, {
    req(values$current_maf)
    
    withProgress(message = 'Analyzing drug-gene interactions...', value = 0, {
      tryCatch({
        incProgress(0.5, detail = "Running drug interaction analysis...")
        
        # Run drug interactions analysis
        values$drug_interactions_results <- maftools::drugInteractions(
          maf = values$current_maf,
          fontSize = input$drug_font_size
        )
        
        values$drug_interactions_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Drug-gene interactions analysis completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in drug interactions analysis:", e$message), duration = 10)
      })
    })
  })
  
  output$drug_interactions_plot <- renderPlot({
    if (values$drug_interactions_run && !is.null(values$drug_interactions_results)) {
      tryCatch({
        # The plot is generated automatically by the drugInteractions function
        maftools::drugInteractions(
          maf = values$current_maf,
          fontSize = input$drug_font_size
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting drug interactions:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  
  output$drug_summary_table <- DT::renderDataTable({
    if (values$drug_interactions_run && !is.null(values$drug_interactions_results)) {
      tryCatch({
        # Extract summary information about drugs per gene
        drug_summary <- values$drug_interactions_results
        DT::datatable(
          drug_summary,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Drug-Gene Interaction Summary"
        )
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  output$drug_specific_gene_table <- DT::renderDataTable({
    if (values$drug_interactions_run && length(input$drug_genes) > 0) {
      tryCatch({
        # Get detailed drug interactions for selected genes
        if (length(input$drug_genes) > 0) {
          detailed_results <- maftools::drugInteractions(
            genes = input$drug_genes,
            drugs = TRUE
          )
          
          # Select relevant columns for display
          if (!is.null(detailed_results) && nrow(detailed_results) > 0) {
            display_cols <- c("Gene", "interaction_types", "drug_name", "drug_claim_name")
            available_cols <- colnames(detailed_results)
            display_cols <- display_cols[display_cols %in% available_cols]
            
            if (length(display_cols) > 0) {
              detailed_subset <- detailed_results[, display_cols, with = FALSE]
              DT::datatable(
                detailed_subset,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                caption = paste("Detailed Drug Interactions for:", paste(input$drug_genes, collapse = ", "))
              )
            } else {
              DT::datatable(data.frame(Message = "No detailed interaction data available"))
            }
          } else {
            DT::datatable(data.frame(Message = "No drug interactions found for selected genes"))
          }
        } else {
          DT::datatable(data.frame(Message = "Please select genes for detailed analysis"))
        }
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  ### 9.8 Oncogenic Signaling Pathways------------------
  observeEvent(input$run_pathways_analysis, {
    req(values$current_maf)
    
    withProgress(message = 'Analyzing oncogenic signaling pathways...', value = 0, {
      tryCatch({
        incProgress(0.5, detail = "Running pathway enrichment analysis...")
        
        # Run pathways analysis
        values$pathways_analysis_results <- maftools::pathways(
          maf = values$current_maf,
          plotType = input$pathway_plot_type,
          fontSize = input$pathway_font_size
        )
        
        values$pathways_analysis_run <- TRUE
        incProgress(1, detail = "Complete!")
        
        showNotification("Oncogenic signaling pathways analysis completed!", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error in pathways analysis:", e$message), duration = 10)
      })
    })
  })
  
  output$pathways_analysis_plot <- renderPlot({
    if (values$pathways_analysis_run && !is.null(values$pathways_analysis_results)) {
      tryCatch({
        # The plot is generated automatically by the pathways function
        maftools::pathways(
          maf = values$current_maf,
          plotType = input$pathway_plot_type,
          fontSize = input$pathway_font_size
        )
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error plotting pathways analysis:", e$message), cex = 1.2, col = "red")
      })
    }
  })
  
  output$pathways_analysis_table <- DT::renderDataTable({
    if (values$pathways_analysis_run && !is.null(values$pathways_analysis_results)) {
      tryCatch({
        # Extract pathway enrichment results
        pathway_results <- values$pathways_analysis_results
        DT::datatable(
          pathway_results,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "Oncogenic Signaling Pathways Enrichment Results"
        ) %>%
          DT::formatRound(columns = c("pval", "fdr"), digits = 4)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  ### 9.0 Tumor Mutational Burden (moved to top)---------------
  observeEvent(input$run_tmb, {
    req(values$current_maf)
    
    tryCatch({
      maftools::tmb(maf = values$current_maf, captureSize = input$tmb_capture_size, logScale = input$tmb_log_scale)
      showNotification("TMB analysis completed!", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error in TMB analysis:", e$message), duration = 10)
    })
  })
  
  output$tmb_plot <- renderPlot({
    if (values$data_loaded && !is.null(values$current_maf)) {
      if (input$run_tmb > 0) {
        tryCatch({
          maftools::tmb(maf = values$current_maf, captureSize = input$tmb_capture_size, logScale = input$tmb_log_scale)
        }, error = function(e) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, paste("Error in TMB analysis:", e$message), cex = 1, col = "red")
        })
      }
    }
  })

### 9.10.1 Trinucleotide Matrix Extraction ------------------
observeEvent(input$run_trinucleotide, {
  req(values$current_maf)
  
  withProgress(message = 'Extracting trinucleotide matrix...', value = 0, {
    tryCatch({
      genome_pkg <- input$ref_genome_sig
      if (!requireNamespace(genome_pkg, quietly = TRUE)) {
        showNotification(paste("Genome package", genome_pkg, "not installed. Please install it using BiocManager::install('", genome_pkg, "')", sep = ""), 
                         duration = 10, type = "error")
        return()
      }
      incProgress(0.3, detail = "Loading genome...")
      incProgress(0.6, detail = "Extracting trinucleotide matrix...")
      capture_output <- capture.output({
        values$trinucleotide_results <- maftools::trinucleotideMatrix(
          maf = values$current_maf,
          prefix = input$chr_prefix,
          add = TRUE,
          ref_genome = genome_pkg
        )
      }, type = "message")
      values$trinucleotide_output <- paste(capture_output, collapse = "\n")
      values$trinucleotide_run <- TRUE
      incProgress(1, detail = "Complete!")
      showNotification("Trinucleotide matrix extracted successfully!", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error extracting trinucleotide matrix:", e$message), duration = 10)
      values$trinucleotide_run <- FALSE
    })
  })
})

### APOBEC Summary Output ------------------
output$apobec_summary <- renderText({
  if (values$trinucleotide_run && !is.null(values$trinucleotide_results)) {
    tryCatch({
      tnm <- values$trinucleotide_results
      
      # Extract APOBEC enrichment information
      if (!is.null(tnm$APOBEC_scores)) {
        apobec_scores <- tnm$APOBEC_scores
        enriched_samples <- sum(apobec_scores$Enrichment_Score > 2, na.rm = TRUE)
        total_samples <- nrow(apobec_scores)
        
        summary_text <- paste(
          paste("Matrix Dimensions:", nrow(tnm$nmf_matrix), "samples x", ncol(tnm$nmf_matrix), "trinucleotide contexts"),
          paste("APOBEC Enriched Samples:", enriched_samples, "of", total_samples, 
                paste0("(", round(enriched_samples/total_samples*100, 2), "%)")),
          paste("Enrichment Threshold: Score > 2"),
          sep = "\n"
        )
        
        return(summary_text)
      } else {
        return("APOBEC enrichment scores not available in results")
      }
    }, error = function(e) {
      return(paste("Error processing APOBEC results:", e$message))
    })
  } else {
    return("Run trinucleotide matrix extraction first")
  }
})

### APOBEC Samples Table ------------------
output$apobec_samples_table <- DT::renderDataTable({
  if (values$trinucleotide_run && !is.null(values$trinucleotide_results)) {
    tryCatch({
      tnm <- values$trinucleotide_results
      
      if (!is.null(tnm$APOBEC_scores)) {
        apobec_data <- tnm$APOBEC_scores
        apobec_data$APOBEC_Status <- ifelse(apobec_data$Enrichment_Score > 2, "Enriched", "Non-Enriched")
        
        DT::datatable(
          apobec_data,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE,
          caption = "APOBEC Enrichment Scores by Sample"
        ) %>%
          DT::formatRound(columns = c("Enrichment_Score", "pval"), digits = 4)
      } else {
        DT::datatable(data.frame(Message = "APOBEC scores not available"))
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  }
})

### Trinucleotide Matrix Summary ------------------
output$tnm_summary <- renderText({
  if (values$trinucleotide_run && !is.null(values$trinucleotide_results)) {
    tnm <- values$trinucleotide_results
    paste(
      paste("Matrix Dimensions:", nrow(tnm$nmf_matrix), "samples x", ncol(tnm$nmf_matrix), "trinucleotide contexts"),
      paste("Total Mutations:", sum(tnm$nmf_matrix)),
      paste("Average Mutations per Sample:", round(mean(rowSums(tnm$nmf_matrix)), 2)),
      sep = "\n"
    )
  }
})

### Trinucleotide Matrix Preview ------------------
output$tnm_preview_table <- DT::renderDataTable({
  if (values$trinucleotide_run && !is.null(values$trinucleotide_results)) {
    tryCatch({
      tnm_matrix <- values$trinucleotide_results$nmf_matrix
      preview_data <- tnm_matrix[1:min(10, nrow(tnm_matrix)), 1:min(20, ncol(tnm_matrix))]
      
      DT::datatable(
        preview_data,
        options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
        rownames = TRUE,
        caption = "Trinucleotide Matrix Preview (First 10 samples, 20 contexts)"
      )
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  }
})

### 9.10.2 APOBEC Differences Analysis ------------------
observeEvent(input$run_apobec_diff, {
  req(values$trinucleotide_results)
  
  withProgress(message = 'Analyzing APOBEC differences...', value = 0, {
    tryCatch({
      incProgress(0.5, detail = "Running differential analysis...")
      
      # Run APOBEC difference analysis
      values$apobec_diff_results <- maftools::plotApobecDiff(
        tnm = values$trinucleotide_results,
        maf = values$current_maf,
        pVal = input$apobec_pval
      )
      
      values$apobec_diff_run <- TRUE
      incProgress(1, detail = "Complete!")
      
      showNotification("APOBEC difference analysis completed!", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error in APOBEC difference analysis:", e$message), duration = 10)
      values$apobec_diff_run <- FALSE
    })
  })
})

### APOBEC Difference Plot ------------------
output$apobec_diff_plot <- renderPlot({
  if (values$apobec_diff_run && !is.null(values$apobec_diff_results)) {
    tryCatch({
      # Re-run to generate plot
      maftools::plotApobecDiff(
        tnm = values$trinucleotide_results,
        maf = values$current_maf,
        pVal = input$apobec_pval
      )
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error plotting APOBEC differences:", e$message), cex = 1.2, col = "red")
    })
  }
})

### APOBEC Difference Results Table ------------------
output$apobec_diff_results_table <- DT::renderDataTable({
  if (values$apobec_diff_run && !is.null(values$apobec_diff_results)) {
    tryCatch({
      results_data <- values$apobec_diff_results$results
      DT::datatable(
        results_data,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        caption = "Differentially Altered Genes between APOBEC Enriched and Non-Enriched Samples"
      ) %>%
        DT::formatRound(columns = c("pval", "or", "ci.up", "ci.low", "adjPval"), digits = 4)
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  }
})

### APOBEC Sample Summary Table ------------------
output$apobec_sample_summary_table <- DT::renderDataTable({
  if (values$apobec_diff_run && !is.null(values$apobec_diff_results)) {
    tryCatch({
      sample_summary <- values$apobec_diff_results$SampleSummary
      DT::datatable(
        sample_summary,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        caption = "Sample Summary by APOBEC Enrichment Status"
      ) %>%
        DT::formatRound(columns = c("Mean", "Median"), digits = 2)
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  }
})

### 9.10.3 Signature Estimation ------------------
observeEvent(input$run_estimate_signatures, {
  req(values$trinucleotide_results)
  withProgress(message = 'Estimating optimal number of signatures...(it will take few seconds)', value = 0, {
    tryCatch({
      if (!requireNamespace("NMF", quietly = TRUE)) {
        showNotification("NMF package not installed. Please install it using install.packages('NMF')", 
                         duration = 10, type = "error")
        return()
      }
      incProgress(0.3, detail = "Loading NMF package...")
      incProgress(0.6, detail = "Running signature estimation...")

      # Real-time output capture for progress bar
      output_lines <- character()
      progress_con <- textConnection("output_lines", open = "w", local = TRUE)
      sink(progress_con, type = "message")
      sink(progress_con, type = "output")
      on.exit({
        sink(type = "message"); sink(type = "output"); close(progress_con)
      }, add = TRUE)

      values$signatures_estimation <- NULL
      # Run estimation and update progress bar with last 5 lines
      values$signatures_estimation <- maftools::estimateSignatures(
        mat = values$trinucleotide_results,
        nTry = input$n_signatures
      )
      flush.connection(progress_con)
      # Show last lines in progress bar
      incProgress(1, detail = paste(tail(output_lines, 5), collapse = "\n"))
      values$estimation_output <- paste(output_lines, collapse = "\n")
      values$signatures_estimated <- TRUE
      showNotification("Signature estimation completed!", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error estimating signatures:", e$message), duration = 10)
      values$signatures_estimated <- FALSE
    })
  })
})

### Cophenetic Plot ------------------
output$cophenetic_plot <- renderPlot({
  if (values$signatures_estimated && !is.null(values$signatures_estimation)) {
    tryCatch({
      maftools::plotCophenetic(res = values$signatures_estimation)
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error plotting cophenetic correlation:", e$message), cex = 1.2, col = "red")
    })
  }
})

### Signature Extraction ------------------
observeEvent(input$run_extract_signatures, {
  req(values$trinucleotide_results, values$signatures_estimated)
  
  withProgress(message = 'Extracting signatures...', value = 0, {
    tryCatch({
      incProgress(0.3, detail = "Extracting signatures using NMF.Best possible value is the one at which the correlation value on the y-axis drops significantly...")
      
      # Extract signatures
      values$extracted_signatures <- maftools::extractSignatures(
        mat = values$trinucleotide_results,
        n = input$final_n_signatures
      )
      
      incProgress(0.7, detail = "Compare detected signatures to COSMIC Legacy or SBS signature database...")
      
      # Compare against COSMIC databases
      values$cosmic_comparisons$legacy <- maftools::compareSignatures(
        nmfRes = values$extracted_signatures,
        sig_db = "legacy"
      )
      
      values$cosmic_comparisons$sbs <- maftools::compareSignatures(
        nmfRes = values$extracted_signatures,
        sig_db = "SBS"
      )
      
      values$signatures_extracted <- TRUE
      incProgress(1, detail = "Complete!")
      
      showNotification("Signatures extracted and compared successfully!", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error extracting signatures:", e$message), duration = 10)
      values$signatures_extracted <- FALSE
    })
  })
})

### Signatures Plot ------------------
output$signatures_plot <- renderPlot({
  if (values$signatures_extracted && !is.null(values$extracted_signatures)) {
    tryCatch({
      maftools::plotSignatures(
        nmfRes = values$extracted_signatures,
        title_size = 1.0,
        sig_db = input$sig_db_plot
      )
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error plotting signatures:", e$message), cex = 1.2, col = "red")
    })
  }
})

### COSMIC Comparison Outputs ------------------
output$cosmic_legacy_comparison <- renderText({
  if (values$signatures_extracted && !is.null(values$cosmic_comparisons$legacy)) {
    tryCatch({
      # Extract and format the comparison results
      legacy_comp <- values$cosmic_comparisons$legacy
      
      # Get the best matches for each signature
      best_matches <- legacy_comp$best_match
      similarity_scores <- legacy_comp$cosine_similarities
      
      result_text <- "COSMIC Legacy (30 signatures) Comparison:\n"
      result_text <- paste0(result_text, "==========================================\n\n")
      
      if (!is.null(best_matches)) {
        for (i in 1:length(best_matches)) {
          sig_name <- names(best_matches)[i]
          match_name <- best_matches[i]
          
          # Find the cosine similarity score
          if (!is.null(similarity_scores)) {
            cosine_score <- max(similarity_scores[i, ], na.rm = TRUE)
            result_text <- paste0(result_text, 
                                  sprintf("Signature_%d -> %s (cosine: %.3f)\n", 
                                          i, match_name, cosine_score))
          } else {
            result_text <- paste0(result_text, 
                                  sprintf("Signature_%d -> %s\n", i, match_name))
          }
        }
      }
      
      result_text <- paste0(result_text, "\nNote: Cosine similarity > 0.8 indicates high similarity")
      return(result_text)
      
    }, error = function(e) {
      return(paste("Legacy comparison completed. Check cosine similarity heatmap below for detailed results."))
    })
  } else {
    return("Extract signatures first to see Legacy COSMIC comparison results")
  }
})

output$cosmic_sbs_comparison <- renderText({
  if (values$signatures_extracted && !is.null(values$cosmic_comparisons$sbs)) {
    tryCatch({
      # Extract and format the SBS comparison results
      sbs_comp <- values$cosmic_comparisons$sbs
      
      # Get the best matches for each signature
      best_matches <- sbs_comp$best_match
      similarity_scores <- sbs_comp$cosine_similarities
      
      result_text <- "COSMIC SBS (v3.4, 60+ signatures) Comparison:\n"
      result_text <- paste0(result_text, "=============================================\n\n")
      
      if (!is.null(best_matches)) {
        for (i in 1:length(best_matches)) {
          sig_name <- names(best_matches)[i]
          match_name <- best_matches[i]
          
          # Find the cosine similarity score
          if (!is.null(similarity_scores)) {
            cosine_score <- max(similarity_scores[i, ], na.rm = TRUE)
            result_text <- paste0(result_text, 
                                  sprintf("Signature_%d -> %s (cosine: %.3f)\n", 
                                          i, match_name, cosine_score))
          } else {
            result_text <- paste0(result_text, 
                                  sprintf("Signature_%d -> %s\n", i, match_name))
          }
        }
      }
      
      result_text <- paste0(result_text, "\nNote: SBS signatures represent the latest COSMIC database")
      return(result_text)
      
    }, error = function(e) {
      return(paste("SBS comparison completed. Check cosine similarity heatmap below for detailed results."))
    })
  } else {
    return("Extract signatures first to see SBS COSMIC comparison results")
  }
})

### Cosine Similarity Heatmap ------------------
output$cosine_similarity_heatmap <- renderPlot({
  if (values$signatures_extracted && !is.null(values$cosmic_comparisons$legacy)) {
    tryCatch({
      cosine_sim <- values$cosmic_comparisons$legacy$cosine_similarities
      
      # Debug prints (you can disable later)
      print("cosine_sim dimensions:")
      print(dim(cosine_sim))
      print("cosine_sim head:")
      print(head(cosine_sim))
      print(paste("signatures_extracted:", values$signatures_extracted))
      print(paste("cosmic_comparisons$legacy is null:", is.null(values$cosmic_comparisons$legacy)))
      
      if (!is.null(cosine_sim) && nrow(cosine_sim) > 0 && ncol(cosine_sim) > 0) {
        # Key Part: capture the heatmap using grid.grabExpr
        p <- grid::grid.grabExpr({
          pheatmap::pheatmap(
            mat = cosine_sim,
            cluster_rows = FALSE,
            # cluster_cols = TRUE,
            main = "Cosine Similarity against COSMIC Legacy Signatures",
            fontsize = 8,
            fontsize_row = 8,
            fontsize_col = 8,
            angle_col = 90
          )
        })
        # Draw to Shiny plot device
        grid::grid.draw(p)
      } else {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No cosine similarity data available", cex = 1.2, col = "red")
      }
      
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      if (grepl("pheatmap", e$message, ignore.case = TRUE)) {
        text(1, 1.2, "pheatmap package not found", cex = 1.2, col = "red", font = 2)
        text(1, 1.0, "Install using:", cex = 1.0, col = "black")
        text(1, 0.8, "install.packages('pheatmap')", cex = 1.0, col = "blue", font = 3)
        text(1, 0.6, "Then restart your session", cex = 1.0, col = "black")
      } else {
        text(1, 1, paste("Error plotting heatmap:", e$message), 
             cex = 1, col = "red", adj = 0.5)
      }
    })
    
  } else {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Extract signatures and run COSMIC comparison first", 
         cex = 1.2, col = "gray", adj = 0.5)
  }
})

### Signature Contributions Table ------------------
output$signature_contributions_table <- DT::renderDataTable({
  if (values$signatures_extracted && !is.null(values$extracted_signatures)) {
    tryCatch({
      # Extract signature contributions
      contributions <- values$extracted_signatures$contributions
      
      DT::datatable(
        contributions,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = TRUE,
        caption = "Signature Contributions per Sample"
      ) %>%
        DT::formatRound(columns = 1:ncol(contributions), digits = 4)
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  }
})

  ### 9.9 Tumor Heterogeneity and MATH Scores Server Code---------------
  # Fix heterogeneity analysis
  observeEvent(input$run_heterogeneity, {
    if (!is.null(values$current_maf) && !is.null(input$heterogeneity_sample)) {
      tryCatch({
        withProgress(message = 'Running heterogeneity analysis...', value = 0, {
          
          if (!requireNamespace("mclust", quietly = TRUE)) {
            showNotification("Please install mclust package", type = "error", duration = 5)
            return()
          }
          
          incProgress(0.3, detail = "Checking VAF column...")
          
          # Check all available columns
          available_cols <- colnames(values$current_maf@data)
          vaf_col <- input$heterogeneity_vaf_col
          
          # Display all column names for debugging
          cat("All available columns:\n")
          print(available_cols)
          
          # If the specified VAF column does not exist
          if (!vaf_col %in% available_cols) {
            
            # Check if there are other VAF columns
            existing_vaf <- available_cols[grepl("vaf|VAF", available_cols, ignore.case = TRUE)]
            
            if (length(existing_vaf) > 0) {
              # There is an existing VAF column, use the first one
              vaf_col <- existing_vaf[1]
              showNotification(paste("Using existing VAF column:", vaf_col), type = "message", duration = 5)
              
            } else if (all(c("t_ref_count", "t_alt_count") %in% available_cols)) {
              # There is no VAF column, but there are counts, calculate VAF
              showNotification("Calculating VAF from counts...", type = "message", duration = 3)
              
              # Directly modify values$current_maf data
              values$current_maf@data$VAF <- values$current_maf@data$t_alt_count / 
                (values$current_maf@data$t_ref_count + values$current_maf@data$t_alt_count)
              
              # Handle abnormal values
              values$current_maf@data$VAF[is.nan(values$current_maf@data$VAF)] <- NA
              values$current_maf@data$VAF[is.infinite(values$current_maf@data$VAF)] <- NA
              
              vaf_col <- "VAF"
              showNotification("VAF calculated successfully!", type = "message", duration = 5)
              
            } else {
              # Neither VAF column nor counts - check if there are other possible columns
              cat("Looking for alternative VAF columns...\n")
              alt_vaf <- available_cols[grepl("freq|rate|ratio|alt.*count|tumor.*count", available_cols, ignore.case = TRUE)]
              
              if (length(alt_vaf) > 0) {
                vaf_col <- alt_vaf[1]
                showNotification(paste("Using alternative VAF column:", vaf_col), type = "warning", duration = 8)
              } else {
                showNotification("No VAF column found and cannot calculate from counts", type = "error", duration = 10)
                cat("Available columns for reference:\n")
                cat(paste(available_cols, collapse = ", "))
                return()
              }
            }
          }
          
          incProgress(0.6, detail = "Running analysis...")
          
          # Verify that the selected VAF column actually exists
          if (!vaf_col %in% colnames(values$current_maf@data)) {
            showNotification(paste("VAF column", vaf_col, "not found after processing"), type = "error", duration = 10)
            return()
          }
          
          # Now run analysis
          if (!is.null(input$seg_file)) {
            het_results <- maftools::inferHeterogeneity(
              maf = values$current_maf,
              tsb = input$heterogeneity_sample,
              segFile = input$seg_file$datapath,
              vafCol = vaf_col
            )
          } else {
            het_results <- maftools::inferHeterogeneity(
              maf = values$current_maf,
              tsb = input$heterogeneity_sample,
              vafCol = vaf_col
            )
          }
          
          values$heterogeneity_results <- het_results
          values$heterogeneity_run <- TRUE
          
          incProgress(1, detail = "Complete!")
          showNotification(paste("Analysis completed using VAF column:", vaf_col), type = "message", duration = 5)
        })
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 15)
        values$heterogeneity_run <- FALSE
        
        # Display full column names list
        if (!is.null(values$current_maf)) {
          cols <- colnames(values$current_maf@data)
          cat("Full column list for debugging:\n")
          for(i in 1:length(cols)) {
            cat(sprintf("%2d. %s\n", i, cols[i]))
          }
          
          # Also display in notification (but limit length)
          showNotification(paste("Total columns:", length(cols), "- Check R console for full list"), 
                           type = "warning", duration = 10)
        }
      })
    }
  })
  
  # Sample selection update
  observe({
    if (!is.null(values$current_maf)) {
      sample_choices <- as.character(maftools::getSampleSummary(values$current_maf)$Tumor_Sample_Barcode)
      updateSelectInput(session, "heterogeneity_sample", 
                        choices = sample_choices,
                        selected = sample_choices[1])
    }
  })
  
  # Fix MATH score comparison
  output$math_score_comparison <- renderPlot({
    if (values$heterogeneity_run && !is.null(values$current_maf)) {
      tryCatch({
        withProgress(message = 'Calculating MATH scores...', value = 0, {
          
          # Ensure VAF column exists
          available_cols <- colnames(values$current_maf@data)
          vaf_col <- input$heterogeneity_vaf_col
          
          if (!vaf_col %in% available_cols) {
            existing_vaf <- available_cols[grepl("vaf|VAF", available_cols, ignore.case = TRUE)]
            
            if (length(existing_vaf) > 0) {
              vaf_col <- existing_vaf[1]
            } else if (all(c("t_ref_count", "t_alt_count") %in% available_cols)) {
              if (!"VAF" %in% available_cols) {
                values$current_maf@data$VAF <- values$current_maf@data$t_alt_count / 
                  (values$current_maf@data$t_ref_count + values$current_maf@data$t_alt_count)
                values$current_maf@data$VAF[is.nan(values$current_maf@data$VAF)] <- NA
                values$current_maf@data$VAF[is.infinite(values$current_maf@data$VAF)] <- NA
              }
              vaf_col <- "VAF"
            } else {
              plot.new()
              text(0.5, 0.5, "No VAF column available", cex = 1.2, col = "red")
              return()
            }
          }
          
          all_samples <- as.character(maftools::getSampleSummary(values$current_maf)$Tumor_Sample_Barcode)
          selected_samples <- head(all_samples, 4)  # Reduce to 4 samples to avoid too slow
          
          math_data <- data.frame(Sample = character(), MATH_Score = numeric())
          
          for (i in seq_along(selected_samples)) {
            incProgress(1/length(selected_samples))
            tryCatch({
              het_result <- maftools::inferHeterogeneity(
                maf = values$current_maf,
                tsb = selected_samples[i],
                vafCol = vaf_col,
                ignoreOutliers = TRUE
              )
              if (!is.null(het_result$mathScore)) {
                math_data <- rbind(math_data, 
                                   data.frame(Sample = selected_samples[i], 
                                              MATH_Score = het_result$mathScore))
              }
            }, error = function(e) {})
          }
          
          if (nrow(math_data) > 0) {
            math_data$Sample <- factor(math_data$Sample, levels = math_data$Sample[order(math_data$MATH_Score)])
            math_data$Highlight <- ifelse(math_data$Sample == input$heterogeneity_sample, "Current", "Other")
            ggplot(math_data, aes(x = Sample, y = MATH_Score, fill = Highlight)) +
              geom_col() +
              scale_fill_manual(values = c("Current" = "#e74c3c", "Other" = "#3498db")) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              labs(title = "MATH Scores Comparison", y = "MATH Score")
          } else {
            plot.new()
            text(0.5, 0.5, "No MATH scores calculated", cex = 1.2, col = "red")
          }
        })
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
    }
  }, height = 400)
  
  # Other outputs remain unchanged
  output$heterogeneity_plot <- renderPlot({
    if (values$heterogeneity_run && !is.null(values$heterogeneity_results)) {
      tryCatch({
        if (input$highlight_cn_vars && !is.null(input$seg_file)) {
          maftools::plotClusters(clusters = values$heterogeneity_results, genes = 'CN_altered', showCNvars = TRUE)
        } else {
          maftools::plotClusters(clusters = values$heterogeneity_results)
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
    }
  }, height = 500)
  
  output$cluster_means_table <- DT::renderDataTable({
    if (values$heterogeneity_run && !is.null(values$heterogeneity_results)) {
      cluster_means <- values$heterogeneity_results$clusterMeans
      if (!is.null(cluster_means) && nrow(cluster_means) > 0) {
        if ("meanVaf" %in% colnames(cluster_means)) {
          cluster_means$meanVaf <- round(cluster_means$meanVaf, 4)
        }
        DT::datatable(cluster_means, options = list(pageLength = 10), rownames = FALSE)
      } else {
        DT::datatable(data.frame(Message = "No clustering results"))
      }
    } else {
      DT::datatable(data.frame(Message = "Run analysis first"))
    }
  })
  
  output$heterogeneity_summary <- renderText({
    if (values$heterogeneity_run && !is.null(values$heterogeneity_results)) {
      het_data <- values$heterogeneity_results
      summary_text <- paste("Sample:", input$heterogeneity_sample)
      
      if (!is.null(het_data$clusterMeans)) {
        for (i in 1:nrow(het_data$clusterMeans)) {
          cluster_info <- het_data$clusterMeans[i, ]
          summary_text <- paste0(summary_text, sprintf("\nCluster %s: VAF = %.4f", 
                                                       cluster_info$cluster, cluster_info$meanVaf))
        }
      }
      
      if (!is.null(het_data$mathScore)) {
        summary_text <- paste0(summary_text, sprintf("\nMATH Score: %.4f", het_data$mathScore))
      }
      
      summary_text
    } else {
      "Run analysis to see results"
    }
  })
  
  output$cn_altered_table <- DT::renderDataTable({
    if (values$heterogeneity_run && !is.null(values$heterogeneity_results) && !is.null(input$seg_file)) {
      cn_altered <- values$heterogeneity_results$cnAltered
      if (!is.null(cn_altered) && nrow(cn_altered) > 0) {
        DT::datatable(cn_altered, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
      } else {
        DT::datatable(data.frame(Message = "No CN altered variants"))
      }
    } else {
      DT::datatable(data.frame(Message = "Upload segmentation file to see CN variants"))
    }
  })
  
  output$heterogeneity_run <- reactive({
    return(values$heterogeneity_run)
  })
  outputOptions(output, "heterogeneity_run", suspendWhenHidden = FALSE)

  output$estimation_console <- renderText({
    values$estimation_output
  })
  output$trinucleotide_console <- renderText({
    values$trinucleotide_output
  })
  
  # 主题切换功能
  observeEvent(input$theme_toggle, {
    shinyjs::runjs("
      const body = document.body;
      const currentTheme = body.getAttribute('data-theme');
      const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
      body.setAttribute('data-theme', newTheme);
      // 更新切换按钮图标
      const icon = document.querySelector('#theme_toggle i');
      if (newTheme === 'dark') {
        icon.className = 'fa fa-sun';
      } else {
        icon.className = 'fa fa-moon';
      }
      // 保存主题偏好到localStorage
      localStorage.setItem('theme-preference', newTheme);
    ")
  })

  # 页面加载时恢复主题设置
  shinyjs::runjs("
    document.addEventListener('DOMContentLoaded', function() {
      const savedTheme = localStorage.getItem('theme-preference') || 'light';
      document.body.setAttribute('data-theme', savedTheme);
      const icon = document.querySelector('#theme_toggle i');
      if (savedTheme === 'dark') {
        icon.className = 'fa fa-sun';
      } else {
        icon.className = 'fa fa-moon';
      }
    });
  ")
}

# Run the application
shinyApp(ui = ui, server = server)