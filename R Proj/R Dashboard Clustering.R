#benerin rata rata
# Dashboard Analisis Clustering Polinomial - Data Ekspor Kopi
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2) 
library(cluster)
library(factoextra)
library(corrplot)
library(gridExtra)
library(dtw) 
library(dendextend) 
library(tidyr) 
library(tibble) # Ditambahkan untuk rownames_to_column
library(mclust) # Ditambahkan untuk Adjusted Rand Index jika diperlukan

# UI (Perubahan pada K-Means dan Perbandingan Hasil)
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Clustering Polinomial - Ekspor Kopi"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Model Polinomial", tabName = "polynomial", icon = icon("chart-line")),
      menuItem("K-Means Clustering", tabName = "kmeans", icon = icon("project-diagram")),
      menuItem("Hierarchical Clustering", tabName = "hierarchical", icon = icon("sitemap")),
      menuItem("Perbandingan Hasil", tabName = "comparison", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
      "))
    ),
    
    tabItems(
      # Tab Upload Data
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Upload Data Ekspor Kopi", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("file", "Pilih File CSV atau Excel:",
                            accept = c(".csv", ".xlsx", ".xls", ".txt"),
                            multiple = FALSE),
                  radioButtons("separator", "Pilih Separator (untuk CSV/TXT):",
                               choices = c("Koma" = ",",
                                           "Titik Koma" = ";"),
                               selected = ";"),
                  helpText("Format data: Kolom pertama berisi nama negara, kolom selanjutnya adalah tahun (2014-2023)"),
                  
                  conditionalPanel(
                    condition = "output.fileUploaded",
                    h4("Preview Data:"),
                    DT::dataTableOutput("dataPreview")
                  )
                )
              ),
              
              fluidRow(
                conditionalPanel(
                  condition = "output.fileUploaded",
                  box(
                    title = "Statistik Deskriptif", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("dataSummary")
                  ),
                  box(
                    title = "Informasi Dataset", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("dataInfo")
                  )
                )
              )
      ),
      
      # Tab Model Polinomial
      tabItem(tabName = "polynomial",
              fluidRow(
                box(
                  title = "Koefisien Model Kuadratik", status = "primary", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("quadraticCoeff")
                ),
                box(
                  title = "Profil Tren (Sample)", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("selectedCountry", "Pilih Negara:", choices = NULL),
                  plotlyOutput("trendProfile")
                )
              ),
              
              fluidRow(
                box(
                  title = "Visualisasi Semua Profil Tren", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("allTrendProfiles", height = "800px")
                )
              )
      ),
      
      # Tab K-Means Clustering
      tabItem(tabName = "kmeans",
              fluidRow(
                box(
                  title = "Hasil K-Means Clustering (Model Kuadratik)", status = "primary", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("kmeansResults")
                ),
                box(
                  title = "Scatter Plot Clustering", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("scatterClustering")
                )
              ),
              
              fluidRow(
                box(
                  title = "Karakteristik Cluster", status = "success", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("clusterCharacteristics")
                )
              ),
              fluidRow(
                box(
                  title = "Profil Model Tren per Cluster (K-Means)", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("kmeans_model_profile_plot", height = "600px")
                )
              )
      ),
      
      # Tab Hierarchical Clustering
      tabItem(tabName = "hierarchical",
              fluidRow(
                box(
                  title = "Parameter Clustering Hierarki", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("linkageMethod", "Metode Linkage:", 
                                          choices = c("single", "complete", "average", "ward.D2"),
                                          selected = "average")),
                    column(4, numericInput("numClusters", "Jumlah Cluster:", value = 2, min = 2, max = 8)),
                    column(4, actionButton("runHierarchical", "Jalankan Analisis", class = "btn-success"))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Dendrogram", status = "info", solidHeader = TRUE, width = 8,
                  plotOutput("dendrogram", height = "500px")
                ),
                box(
                  title = "Analisis Silhouette", status = "info", solidHeader = TRUE, width = 4,
                  plotOutput("silhouetteAnalysis", height = "300px"),
                  verbatimTextOutput("silhouetteScores")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hasil Clustering Hierarki", status = "success", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("hierarchicalResults")
                ),
                box(
                  title = "Cophenetic Correlation", status = "success", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("copheneticCorr")
                )
              ),
              
              fluidRow(
                box(
                  title = "Fluktuasi Volume Ekspor per Cluster", status = "warning", solidHeader = TRUE, width = 12,
                  plotOutput("clusterFluctuation", height = "600px")
                )
              )
      ),
      
      # Tab Perbandingan
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Perbandingan K-Means vs Hierarchical", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("comparisonResults")
                )
              ),
              
              fluidRow(
                box(
                  title = "Informasi Rata-rata Volume Ekspor per Cluster (Tabel)", status = "info", solidHeader = TRUE, width = 12,
                  tableOutput("avg_volume_table") 
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    data_raw = NULL,
    countries = NULL,
    years_data = NULL,
    volume_matrix = NULL,
    zt_results = NULL,
    quadratic_models = NULL,
    quadratic_coefficients = NULL,
    kmeans_result = NULL,
    hierarchical_result = NULL,
    data_winsorized = NULL,
    original_data_for_clusters = NULL 
  )
  
  # File upload handler
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv" || ext == "txt") {
      values$data_raw <- read.csv(input$file$datapath, sep = input$separator, check.names = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data_raw <- read_excel(input$file$datapath)
    } else {
      showNotification("Format file tidak didukung.", type = "error")
      return(NULL)
    }
    
    values$data_raw[[1]] <- as.character(values$data_raw[[1]])
    
    values$countries <- values$data_raw[[1]]
    values$years_data <- values$data_raw[, -1] 
    
    original_col_names <- colnames(values$years_data)
    
    # Perbaikan: Ekstrak angka tahun dari nama kolom
    numeric_col_names <- suppressWarnings(as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", original_col_names))) 
    
    if(any(is.na(numeric_col_names)) || length(numeric_col_names) != ncol(values$years_data)) {
      numeric_col_names <- as.numeric(gsub("[^0-9]", "", original_col_names)) 
      if (any(is.na(numeric_col_names)) || length(numeric_col_names) != ncol(values$years_data)) {
        numeric_col_names <- 2014:(2014 + ncol(values$years_data) - 1) 
        warning("Nama kolom tahun tidak valid atau mengandung NA, menggunakan 2014-2023 sebagai asumsi.")
      }
    }
    colnames(values$years_data) <- numeric_col_names
    
    values$volume_matrix <- as.matrix(values$years_data)
    rownames(values$volume_matrix) <- values$countries
    
    values$original_data_for_clusters <- values$data_raw %>%
      pivot_longer(
        cols = -names(values$data_raw)[1], 
        names_to = "Year_Raw", 
        values_to = "Volume"
      ) %>%
      rename(Country = names(values$data_raw)[1]) %>% 
      mutate(Year = suppressWarnings(as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", Year_Raw)))) %>% 
      select(-Year_Raw) 
    
    updateSelectInput(session, "selectedCountry", choices = values$countries)
    
    calculate_zt_values()
    build_quadratic_models()
    prepare_hierarchical_data()
  })
  
  calculate_zt_values <- function() {
    req(values$volume_matrix)
    
    zt_results <- list()
    y_bar_values <- numeric(length(values$countries))
    names(y_bar_values) <- values$countries
    
    for(i in 1:length(values$countries)) {
      country <- values$countries[i]
      volume_series <- values$volume_matrix[i, ]
      y_bar <- mean(volume_series, na.rm = TRUE)
      zt <- volume_series - y_bar
      zt_results[[country]] <- zt
      y_bar_values[country] <- y_bar
    }
    
    values$zt_results <- zt_results
  }
  
  build_quadratic_models <- function() {
    req(values$zt_results, length(values$zt_results) > 0)
    
    quadratic_models <- list()
    quadratic_coefficients <- data.frame(
      Country = values$countries,
      beta1_quad = numeric(length(values$countries)),
      beta2_quad = numeric(length(values$countries)),
      r_squared_quad = numeric(length(values$countries)),
      stringsAsFactors = FALSE
    )
    
    current_row_idx <- 1 
    
    for(i in 1:length(values$countries)) {
      country <- values$countries[i]
      zt_values <- values$zt_results[[country]]
      
      if (is.null(zt_values) || length(zt_values) < 3 || all(is.na(zt_values))) { 
        warning(paste("Tidak cukup data (atau semua NA) untuk model kuadratik untuk", country))
        quadratic_coefficients[current_row_idx, "Country"] <- country
        quadratic_coefficients[current_row_idx, c("beta1_quad", "beta2_quad", "r_squared_quad")] <- NA
        current_row_idx <- current_row_idx + 1
        next 
      }
      
      t <- 1:length(zt_values)
      
      tryCatch({
        model <- lm(zt_values ~ t + I(t^2) - 1)
        coeffs <- coef(model)
        
        quadratic_models[[country]] <- list(
          model = model,
          coefficients = coeffs,
          fitted_values = fitted(model),
          r_squared = summary(model)$r.squared
        )
        
        quadratic_coefficients[current_row_idx, "Country"] <- country
        quadratic_coefficients[current_row_idx, "beta1_quad"] <- ifelse("t" %in% names(coeffs), coeffs["t"], NA) 
        quadratic_coefficients[current_row_idx, "beta2_quad"] <- ifelse("I(t^2)" %in% names(coeffs), coeffs["I(t^2)"], NA)
        quadratic_coefficients[current_row_idx, "r_squared_quad"] <- summary(model)$r.squared
        current_row_idx <- current_row_idx + 1
        
      }, error = function(e) {
        warning(paste("Error building quadratic model for", country, ":", e$message))
        quadratic_coefficients[current_row_idx, "Country"] <- country
        quadratic_coefficients[current_row_idx, c("beta1_quad", "beta2_quad", "r_squared_quad")] <- NA
        current_row_idx <- current_row_idx + 1
      })
    }
    
    values$quadratic_models <- quadratic_models
    values$quadratic_coefficients <- quadratic_coefficients %>% filter(!is.na(beta1_quad) & !is.na(beta2_quad))
    
    perform_kmeans_clustering()
  }
  
  kmeans_num_clusters <- reactiveVal(2) 
  
  perform_kmeans_clustering <- function() {
    req(values$quadratic_coefficients)
    
    clustering_data <- values$quadratic_coefficients[, c("beta1_quad", "beta2_quad"), drop = FALSE]
    clustering_data_clean <- clustering_data[complete.cases(clustering_data) & is.finite(clustering_data$beta1_quad) & is.finite(clustering_data$beta2_quad), , drop = FALSE]
    
    rownames(clustering_data_clean) <- values$quadratic_coefficients$Country[
      complete.cases(clustering_data) & is.finite(clustering_data$beta1_quad) & is.finite(clustering_data$beta2_quad)
    ]
    
    if(nrow(clustering_data_clean) > 1) { 
      clustering_matrix_scaled <- scale(clustering_data_clean)
      
      wss <- numeric(5) 
      silhouette_scores <- numeric(5) 
      
      for(k in 2:6) {
        if(k <= nrow(clustering_matrix_scaled)) {
          set.seed(123) 
          kmeans_result_temp <- kmeans(clustering_matrix_scaled, centers = k, nstart = 25)
          wss[k-1] <- kmeans_result_temp$tot.withinss
          
          if(k > 1) {
            if (nrow(clustering_matrix_scaled) >= 2) { 
              sil <- silhouette(kmeans_result_temp$cluster, dist(clustering_matrix_scaled))
              silhouette_scores[k-1] <- mean(sil[, 3])
            } else {
              silhouette_scores[k-1] <- NA 
            }
          }
        }
      }
      
      k_for_kmeans <- kmeans_num_clusters()
      if (k_for_kmeans > nrow(clustering_matrix_scaled)) {
        k_for_kmeans <- max(2, floor(nrow(clustering_matrix_scaled) / 2)) 
      }
      if (k_for_kmeans < 2) {
        k_for_kmeans <- 2
      }
      
      set.seed(123) 
      final_kmeans <- kmeans(clustering_matrix_scaled, centers = k_for_kmeans, nstart = 25)
      
      clustering_results <- data.frame(
        Country = rownames(clustering_data_clean), 
        Cluster = final_kmeans$cluster,
        stringsAsFactors = FALSE
      )
      
      values$kmeans_result <- list(
        kmeans = final_kmeans,
        results = clustering_results,
        wss = wss,
        silhouette_scores = silhouette_scores,
        scaled_data = clustering_matrix_scaled
      )
    } else {
      showNotification("Tidak cukup data bersih untuk K-Means Clustering (minimal 2 negara).", type = "warning")
      values$kmeans_result <- NULL 
    }
  }
  
  prepare_hierarchical_data <- function() {
    req(values$volume_matrix)
    
    winsorize_outliers <- function(data, lower_percentile = 0.05, upper_percentile = 0.95) {
      winsorized_data <- as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
      colnames(winsorized_data) <- colnames(data)
      rownames(winsorized_data) <- rownames(data)
      
      for (i in 1:ncol(data)) {
        col_data <- as.numeric(data[, i]) 
        if (!all(is.na(col_data))) { 
          lower_val <- quantile(col_data, probs = lower_percentile, na.rm = TRUE)
          upper_val <- quantile(col_data, probs = upper_percentile, na.rm = TRUE)
          winsorized_col <- col_data
          winsorized_col[winsorized_col < lower_val] <- lower_val
          winsorized_col[winsorized_col > upper_val] <- upper_val
          winsorized_data[, i] <- winsorized_col
        } else {
          winsorized_data[, i] <- col_data 
        }
      }
      return(winsorized_data)
    }
    
    data_mentah <- as.data.frame(values$volume_matrix)
    numeric_cols_idx <- sapply(data_mentah, is.numeric)
    if (any(numeric_cols_idx)) {
      values$data_winsorized <- winsorize_outliers(data_mentah[, numeric_cols_idx, drop=FALSE])
      rownames(values$data_winsorized) <- rownames(data_mentah)
    } else {
      showNotification("Tidak ada kolom numerik yang valid untuk winsorization.", type = "warning")
      values$data_winsorized <- NULL
    }
  }
  
  observeEvent(input$runHierarchical, {
    req(values$data_winsorized)
    
    if (nrow(values$data_winsorized) < 2) {
      showNotification("Insufficient data for Hierarchical Clustering (DTW distance). Need at least 2 data points.", type = "warning")
      values$hierarchical_result <- NULL
      return(NULL)
    }
    
    data_for_dtw <- values$data_winsorized[complete.cases(values$data_winsorized), ]
    if (nrow(data_for_dtw) < 2) {
      showNotification("Data tidak lengkap untuk perhitungan jarak DTW setelah membersihkan NA/NaN.", type = "warning")
      values$hierarchical_result <- NULL
      return(NULL)
    }
    
    jarak_dtw <- dist(data_for_dtw, method = "DTW") 
    
    hc <- hclust(jarak_dtw, method = input$linkageMethod)
    
    num_clusters_hierarchical <- min(input$numClusters, nrow(data_for_dtw)) 
    if (num_clusters_hierarchical < 2) num_clusters_hierarchical <- 2
    
    clusters <- cutree(hc, k = num_clusters_hierarchical)
    
    sil_values <- numeric(6) 
    for(k_sil in 2:7) {
      if(k_sil <= nrow(data_for_dtw)) { 
        temp_clusters <- cutree(hc, k = k_sil)
        sil <- silhouette(temp_clusters, dist = jarak_dtw)
        sil_values[k_sil-1] <- mean(sil[, 3])
      } else {
        sil_values[k_sil-1] <- NA
      }
    }
    
    cophenetic_dist <- cophenetic(hc)
    cophenetic_corr <- cor(jarak_dtw, cophenetic_dist)
    
    values$hierarchical_result <- list(
      hc = hc,
      clusters = clusters,
      dtw_distance = jarak_dtw,
      silhouette_values = sil_values,
      cophenetic_corr = cophenetic_corr,
      clustered_data_rownames = rownames(data_for_dtw) 
    )
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(values$data_raw) && nrow(values$data_raw) > 0)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  output$dataPreview <- DT::renderDataTable({
    req(values$data_raw)
    DT::datatable(values$data_raw, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$dataSummary <- renderText({
    req(values$data_raw)
    paste(
      "Jumlah Negara:", nrow(values$data_raw), "\n",
      "Jumlah Tahun:", ncol(values$data_raw) - 1, "\n",
      "Total Observasi:", nrow(values$data_raw) * (ncol(values$data_raw) - 1)
    )
  })
  
  output$dataInfo <- renderText({
    req(values$data_raw)
    years_cols <- names(values$data_raw)[-1]
    if (length(years_cols) > 0) {
      numeric_years <- suppressWarnings(as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", years_cols)))
      numeric_years <- numeric_years[!is.na(numeric_years)] 
      if (length(numeric_years) > 0) {
        paste(
          "Periode Analisis:", min(numeric_years), "-", max(numeric_years), "\n",
          "Negara Tujuan Ekspor:\n",
          paste(values$countries, collapse = ", ")
        )
      } else {
        "Tidak ada kolom tahun yang terdeteksi dengan format angka."
      }
    } else {
      "Tidak ada kolom tahun yang terdeteksi."
    }
  })
  
  output$quadraticCoeff <- DT::renderDataTable({
    req(values$quadratic_coefficients)
    DT::datatable(values$quadratic_coefficients, 
                  options = list(pageLength = 15, scrollX = TRUE)) %>%
      DT::formatRound(columns = c("beta1_quad", "beta2_quad", "r_squared_quad"), digits = 4)
  })
  
  output$trendProfile <- renderPlotly({
    req(values$zt_results, input$selectedCountry)
    
    country <- input$selectedCountry
    zt_values <- values$zt_results[[country]]
    
    if (is.null(zt_values) || all(is.na(zt_values))) {
      return(plotly_empty() %>% layout(title = "Data Zt tidak tersedia atau semua NA untuk negara ini."))
    }
    
    years <- as.numeric(colnames(values$years_data))
    
    if (any(is.na(years)) || length(years) != length(zt_values)) {
      years <- 1:length(zt_values) 
    }
    
    quad_fitted <- rep(NA, length(zt_values))
    if(!is.null(values$quadratic_models[[country]]) && !is.null(values$quadratic_models[[country]]$fitted_values)) {
      quad_fitted <- values$quadratic_models[[country]]$fitted_values
      if (length(quad_fitted) != length(zt_values)) {
        quad_fitted <- rep(NA, length(zt_values)) 
      }
    }
    
    plot_data <- data.frame(
      Year = years,
      Observed = zt_values,
      Quadratic = quad_fitted
    )
    
    p <- ggplot(plot_data, aes(x = Year)) +
      geom_line(aes(y = Observed, color = "Observed"), size = 1) +
      geom_point(aes(y = Observed, color = "Observed"), size = 2) +
      geom_line(aes(y = Quadratic, color = "Quadratic"), size = 1, na.rm = TRUE) +
      geom_point(aes(y = Quadratic, color = "Quadratic"), size = 2, na.rm = TRUE) +
      ggtitle(paste("Profil Tren:", country)) +
      xlab("Tahun") +
      ylab("Zt (Volume - Mean)") +
      theme_minimal() +
      scale_color_manual(values = c("Observed" = "black", "Quadratic" = "blue")) +
      scale_x_continuous(breaks = unique(years), labels = as.character(unique(years))) 
    
    ggplotly(p)
  })
  
  output$allTrendProfiles <- renderPlot({
    req(values$zt_results, values$quadratic_models, values$volume_matrix, length(values$zt_results) > 0)
    
    plot_list <- list()
    num_countries_to_plot <- min(12, length(values$countries)) 
    
    years <- as.numeric(colnames(values$years_data)) 
    if (any(is.na(years)) || length(years) != ncol(values$years_data)) { 
      years <- 2014:(2014 + ncol(values$years_data) - 1) 
    }
    
    for(i in 1:num_countries_to_plot) {
      country <- values$countries[i]
      zt_values <- values$zt_results[[country]]
      
      if (is.null(zt_values) || all(is.na(zt_values))) {
        warning(paste("Skipping plot for", country, ": Zt values are null or all NA."))
        next
      }
      
      quad_fitted <- rep(NA, length(zt_values))
      if(!is.null(values$quadratic_models[[country]]) && !is.null(values$quadratic_models[[country]]$fitted_values)) {
        quad_fitted <- values$quadratic_models[[country]]$fitted_values
        if (length(quad_fitted) != length(zt_values)) {
          quad_fitted <- rep(NA, length(zt_values))
        }
      }
      
      plot_data <- data.frame(
        Year = years,
        Observed = zt_values,
        Quadratic = quad_fitted
      )
      
      plot_data_long <- reshape2::melt(plot_data, id.vars = "Year", 
                                       variable.name = "Type", value.name = "Value")
      
      p <- ggplot(plot_data_long, aes(x = Year, y = Value, color = Type)) +
        geom_line(size = 0.8, na.rm = TRUE) +
        geom_point(size = 1.5, na.rm = TRUE) +
        ggtitle(country) +
        theme_minimal() +
        theme(legend.position = "none", plot.title = element_text(size = 10)) +
        scale_color_manual(values = c("Observed" = "black", "Quadratic" = "blue")) +
        scale_x_continuous(breaks = unique(years), labels = as.character(unique(years))) 
      
      plot_list[[i]] <- p
    }
    
    if(length(plot_list) > 0) {
      do.call(grid.arrange, c(plot_list, ncol = 3))
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
      text(0.5, 0.5, "No trend profiles to display (not enough valid data or models).", cex=1.2)
    }
  })
  
  output$kmeansResults <- DT::renderDataTable({
    req(values$kmeans_result)
    
    results_with_coeff <- merge(values$kmeans_result$results, 
                                values$quadratic_coefficients[, c("Country", "beta1_quad", "beta2_quad")],
                                by = "Country",
                                all.x = TRUE) 
    
    DT::datatable(results_with_coeff, options = list(pageLength = 15)) %>%
      DT::formatRound(columns = c("beta1_quad", "beta2_quad"), digits = 4)
  })
  
  output$scatterClustering <- renderPlotly({
    req(values$kmeans_result, values$quadratic_coefficients)
    
    plot_data <- merge(values$quadratic_coefficients[, c("Country", "beta1_quad", "beta2_quad")], 
                       values$kmeans_result$results, by = "Country", all.x = TRUE)
    
    plot_data <- plot_data %>% filter(!is.na(Cluster))
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>% layout(title = "Tidak ada data untuk scatter plot clustering (setelah pemfilteran NA)."))
    }
    
    p <- ggplot(plot_data, aes(x = beta1_quad, y = beta2_quad, color = factor(Cluster), label = Country)) +
      geom_point(size = 3) +
      ggtitle("K-Means Clustering: Beta1 vs Beta2") +
      xlab("Beta1 (Linear Term)") +
      ylab("Beta2 (Quadratic Term)") +
      theme_minimal() +
      labs(color = "Cluster")
    
    ggplotly(p, tooltip = c("label", "x", "y"))
  })
  
  output$clusterCharacteristics <- renderText({
    req(values$kmeans_result, values$quadratic_coefficients)
    
    result_text <- ""
    clustered_coeffs <- merge(values$kmeans_result$results, 
                              values$quadratic_coefficients,
                              by = "Country", all.x = TRUE)
    
    clustered_coeffs <- clustered_coeffs %>% filter(!is.na(Cluster))
    
    if (nrow(clustered_coeffs) == 0) {
      return("Tidak ada karakteristik cluster untuk ditampilkan (data cluster kosong atau tidak valid).")
    }
    
    for(cluster_id in sort(unique(clustered_coeffs$Cluster))) {
      cluster_data <- clustered_coeffs[clustered_coeffs$Cluster == cluster_id, ]
      
      if (nrow(cluster_data) > 0) {
        result_text <- paste(result_text, 
                             sprintf("Cluster %d:\n", cluster_id),
                             sprintf("Negara: %s\n", paste(cluster_data$Country, collapse = ", ")),
                             sprintf("Rata-rata Beta1: %.4f\n", mean(cluster_data$beta1_quad, na.rm = TRUE)),
                             sprintf("Rata-rata Beta2: %.4f\n\n", mean(cluster_data$beta2_quad, na.rm = TRUE)))
      }
    }
    if (result_text == "") {
      result_text = "Tidak ada karakteristik cluster untuk ditampilkan."
    }
    result_text
  })
  
  # Perubahan: Profil Model Tren per Cluster (K-Means) dengan Legenda
  output$kmeans_model_profile_plot <- renderPlot({
    req(values$kmeans_result, values$quadratic_models)
    
    kmeans_clusters_df <- values$kmeans_result$results
    
    if (is.null(kmeans_clusters_df) || nrow(kmeans_clusters_df) == 0) {
      return(plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", 
                  main="Tidak ada data cluster K-Means untuk menampilkan profil model tren."))
    }
    
    years <- as.numeric(colnames(values$years_data))
    
    plot_data_list <- list()
    for (i in 1:nrow(kmeans_clusters_df)) {
      country <- kmeans_clusters_df$Country[i]
      cluster_id <- kmeans_clusters_df$Cluster[i]
      
      if (!is.null(values$quadratic_models[[country]])) {
        fitted_vals <- values$quadratic_models[[country]]$fitted_values
        
        if (length(fitted_vals) == length(years)) {
          plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
            Country = country,
            Year = years,
            Fitted = fitted_vals,
            Cluster = factor(cluster_id)
          )
        } else {
          warning(paste("Panjang fitted values tidak cocok untuk negara:", country))
        }
      }
    }
    
    plot_data_combined <- do.call(rbind, plot_data_list)
    
    if (is.null(plot_data_combined) || nrow(plot_data_combined) == 0) {
      return(plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", 
                  main="Tidak ada data model tren untuk ditampilkan per cluster K-Means."))
    }
    
    ggplot(plot_data_combined, aes(x = Year, y = Fitted, group = Country, color = Country)) +
      geom_line(alpha = 0.8, size = 1) +
      facet_wrap(~ Cluster, scales = "free_y") + 
      labs(title = "Profil Model Tren per Cluster (K-Means)",
           x = "Tahun", y = "Zt (Volume - Mean) Model", color = "Negara") +
      theme_minimal() +
      scale_x_continuous(breaks = unique(years), labels = as.character(unique(years))) +
      theme(legend.position = "right", # Mengubah posisi legenda menjadi di kanan
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9))
  })
  
  output$dendrogram <- renderPlot({
    req(values$hierarchical_result)
    
    dend <- as.dendrogram(values$hierarchical_result$hc)
    
    num_clusters_to_display <- min(input$numClusters, nrow(values$hierarchical_result$clustered_data_rownames)) 
    if (num_clusters_to_display < 2) num_clusters_to_display <- 2 
    
    dend <- color_branches(dend, k = num_clusters_to_display, col = rainbow(num_clusters_to_display))
    
    plot(dend, 
         main = paste("Dendrogram -", input$linkageMethod, "Linkage"),
         xlab = "Negara", ylab = "Jarak",
         horiz = FALSE, 
         leaflab = "perpendicular") 
    
    rect.dendrogram(dend, k = num_clusters_to_display, border = rainbow(num_clusters_to_display), lty = 2, lwd = 2)
  })
  
  output$silhouetteAnalysis <- renderPlot({
    req(values$hierarchical_result)
    
    sil_data <- data.frame(k = 2:7, silhouette = values$hierarchical_result$silhouette_values) %>%
      filter(!is.na(silhouette)) 
    
    if (nrow(sil_data) == 0) {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
      text(0.5, 0.5, "Tidak ada data Silhouette untuk ditampilkan.", cex=1.2)
    } else {
      ggplot(sil_data, aes(x = k, y = silhouette)) +
        geom_line() + geom_point(size = 3) +
        ggtitle("Silhouette Analysis (Hierarchical)") +
        xlab("Number of Clusters (k)") +
        ylab("Average Silhouette Width") +
        theme_minimal() +
        scale_x_continuous(breaks = sil_data$k)
    }
  })
  
  output$silhouetteScores <- renderText({
    req(values$hierarchical_result)
    
    result_text <- "Silhouette Scores:\n"
    for(k_sil in 2:7) { 
      if(k_sil-1 <= length(values$hierarchical_result$silhouette_values) && !is.na(values$hierarchical_result$silhouette_values[k_sil-1])) {
        result_text <- paste(result_text, sprintf("k=%d: %.3f\n", k_sil, values$hierarchical_result$silhouette_values[k_sil-1]))
      }
    }
    if (result_text == "Silhouette Scores:\n") {
      result_text = "Tidak ada skor Silhouette untuk ditampilkan."
    }
    result_text
  })
  
  output$hierarchicalResults <- DT::renderDataTable({
    req(values$hierarchical_result)
    
    clustered_countries <- values$hierarchical_result$clustered_data_rownames
    
    results_df <- data.frame(
      Country = clustered_countries,
      Cluster = values$hierarchical_result$clusters
    )
    
    DT::datatable(results_df, options = list(pageLength = 15))
  })
  
  output$copheneticCorr <- renderText({
    req(values$hierarchical_result)
    
    paste("Cophenetic Correlation Coefficient:", 
          round(values$hierarchical_result$cophenetic_corr, 4))
  })
  
  # Perubahan: Fluktuasi Volume Ekspor per Cluster dengan Legenda
  output$clusterFluctuation <- renderPlot({
    req(values$hierarchical_result, values$original_data_for_clusters)
    
    hierarchical_clusters_results <- data.frame(
      Country = values$hierarchical_result$clustered_data_rownames,
      Cluster = values$hierarchical_result$clusters
    )
    
    data_to_plot <- values$original_data_for_clusters %>%
      left_join(hierarchical_clusters_results, by = "Country") %>%
      filter(!is.na(Cluster)) 
    
    if (nrow(data_to_plot) == 0) {
      return(plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", 
                  main="Tidak ada data untuk Fluktuasi Volume Ekspor per Cluster (Hierarchical)."))
    }
    
    plots <- list()
    for (k_plot in sort(unique(data_to_plot$Cluster))) { 
      cluster_k_data <- data_to_plot %>%
        filter(Cluster == k_plot) 
      
      if(nrow(cluster_k_data) > 0) {
        p <- ggplot(cluster_k_data, aes(x = Year, y = Volume, group = Country, color = Country)) +
          geom_line(size = 1) +
          geom_point() +
          labs(title = paste("Fluktuasi Ekspor - Cluster", k_plot),
               x = "Tahun", y = "Volume Ekspor") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_x_continuous(breaks = unique(cluster_k_data$Year), labels = as.character(unique(cluster_k_data$Year))) + 
          theme(legend.position = "right", # Mengubah posisi legenda menjadi di kanan
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9))
        
        plots[[as.character(k_plot)]] <- p
      }
    }
    
    if(length(plots) > 0) {
      do.call(grid.arrange, c(plots, ncol = 1))
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
      text(0.5, 0.5, "No data available for plotting cluster fluctuations.", cex=1.2)
    }
  })
  
  output$comparisonResults <- DT::renderDataTable({
    req(values$kmeans_result, values$hierarchical_result)
    
    kmeans_df <- values$kmeans_result$results
    hierarchical_df <- data.frame(
      Country = values$hierarchical_result$clustered_data_rownames,
      Cluster_Hierarchical = values$hierarchical_result$clusters
    )
    
    comparison <- merge(kmeans_df, hierarchical_df, by = "Country", all = TRUE) 
    names(comparison)[2] <- "Cluster_KMeans"
    
    DT::datatable(comparison, options = list(pageLength = 15))
  })
  
  observe({
    req(values$kmeans_result, values$hierarchical_result, values$data_winsorized) # Menggunakan values$data_winsorized
    
    # --- K-Means Averages ---
    kmeans_clusters_results <- values$kmeans_result$results
    
    # Gunakan values$data_winsorized
    kmeans_data_for_avg <- values$data_winsorized %>%
      rownames_to_column("Country") %>%
      left_join(kmeans_clusters_results, by = "Country") %>%
      filter(!is.na(Cluster)) 
    
    kmeans_avg_volume <- kmeans_data_for_avg %>%
      pivot_longer(
        cols = -c(Country, Cluster), 
        names_to = "Year_Raw", 
        values_to = "Volume"
      ) %>%
      mutate(Year = suppressWarnings(as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", Year_Raw)))) %>% 
      group_by(Cluster, Year) %>% 
      summarise(Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Method = "K-Means")
    
    # --- Hierarchical Averages ---
    hierarchical_clusters_results <- data.frame(
      Country = values$hierarchical_result$clustered_data_rownames,
      Cluster = values$hierarchical_result$clusters
    )
    
    # Gunakan values$data_winsorized
    hierarchical_data_for_avg <- values$data_winsorized %>%
      rownames_to_column("Country") %>%
      left_join(hierarchical_clusters_results, by = "Country") %>%
      filter(!is.na(Cluster)) 
    
    hierarchical_avg_volume <- hierarchical_data_for_avg %>%
      pivot_longer(
        cols = -c(Country, Cluster), 
        names_to = "Year_Raw", 
        values_to = "Volume"
      ) %>%
      mutate(Year = suppressWarnings(as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", Year_Raw)))) %>% 
      group_by(Cluster, Year) %>% 
      summarise(Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Method = "Hierarchical")
    
    combined_avg_volume <- bind_rows(kmeans_avg_volume, hierarchical_avg_volume)
    
    # Output: Informasi Rata-rata Volume Ekspor per Cluster (Tabel)
    output$avg_volume_table <- renderTable({
      if (nrow(combined_avg_volume) == 0) {
        return(data.frame(Pesan = "Tidak ada data untuk ditampilkan di tabel rata-rata volume ekspor."))
      }
      combined_avg_volume %>%
        arrange(Method, Cluster, Year)
    }, bordered = TRUE, hover = TRUE, width = "100%")
  })
}

shinyApp(ui, server)