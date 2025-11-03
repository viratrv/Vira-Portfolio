library(readxl) 
library(dtw) 
library(cluster) 
library(factoextra) 
library(dendextend) 

# 1. Mempersiapkan Data 
data_ekspor <- read_excel(file.choose()) 

# Mengambil hanya kolom volume ekspor (tahun 2014-2023) dan mengubah menjadi 
data frame 
data_mentah <- as.data.frame(data_ekspor[, -1]) 
rownames(data_mentah) <- data_ekspor$Negara # Memberikan nama baris 
berdasarkan nama negara 
colnames(data_mentah) <- names(data_ekspor)[-1] # Memberikan nama kolom yang 
benar 
data_mentah 

# Pastikan semua kolom adalah numerik 
for (i in 1:ncol(data_mentah)) { 
  data_mentah[, i] <- as.numeric(as.character(data_mentah[, i])) 
} 

# 2. Pengecekan Outlier menggunakan IQR 
outlier_flags <- detect_outliers_iqr(data_mentah) 
outlier_flags 
cat("Identifikasi Outlier (TRUE = Outlier):\n") 
print(outlier_flags) 

# Jumlah outlier per negara dan per tahun 
cat("\nJumlah Outlier per Negara:\n") 
print(rowSums(outlier_flags)) 
cat("\nJumlah Outlier per Tahun:\n") 
print(colSums(outlier_flags)) 

# 3. Penanganan Outlier  
data_series_winsorized <- winsorize_outliers(data_mentah) 
data_series_winsorized 
cat("\nData setelah Winsorization (beberapa baris):\n") 
print(head(data_series_winsorized)) 

# 4. Menghitung Jarak Dynamic Time Warping (DTW)  
jarak_dtw <- dist(data_series_winsorized, method = "DTW") 
jarak_dtw
as.matrix(jarak_dtw) 

# 5. Melakukan Pengelompokkan Hierarki  
# Metode Single Linkage 
hc_single <- hclust(jarak_dtw, method = "single") 
plot(hc_single, labels = rownames(data_series_winsorized), main = "Dendrogram - 
     Single Linkage", xlab = "Negara", ylab = "Jarak") 

# Metode Complete Linkage 
hc_complete <- hclust(jarak_dtw, method = "complete") 
plot(hc_complete, labels = rownames(data_series_winsorized), main = "Dendrogram - 
     Complete Linkage", xlab = "Negara", ylab = "Jarak") 

# Metode Average Linkage 
hc_average <- hclust(jarak_dtw, method = "average") 
plot(hc_average, labels = rownames(data_series_winsorized), main = "Dendrogram - 
     Average Linkage", xlab = "Negara", ylab = "Jarak") 

# Metode Ward's Method 
hc_ward <- hclust(jarak_dtw, method = "ward.D2") # Menggunakan ward.D2 yang 
lebih disarankan 
plot(hc_ward, labels = rownames(data_series_winsorized), main = "Dendrogram - 
     Ward's Method", xlab = "Negara", ylab = "Jarak") 

# 6. Menentukan Jumlah Klaster Optimal dengan Koefisien Silhouette 
# Fungsi yang dimodifikasi untuk menghitung, memvisualisasikan, dan menampilkan 
# koefisien silhouette 
silhouette_analysis_extended <- function(hc, jarak, method_name, k_max = 10){ 
  sil_values <- numeric(k_max - 1) 
  cat(paste("Koefisien Silhouette Rata-rata -", method_name, ":\n")) 
  for(k in 2:k_max){ 
    clusters <- cutree(hc, k = k) 
    sil <- silhouette(clusters, dist = jarak) 
    avg_sil <- mean(sil[, 3]) 
    sil_values[k-1] <- avg_sil 
    cat(paste("Jumlah Klaster =", k, ":", round(avg_sil, 3), "\n")) 
  } 
  plot(2:k_max, sil_values, type = "b", xlab = "Jumlah Klaster", ylab = "Koefisien 
       Silhouette Rata-rata", 
       main = paste("Analisis Silhouette -", method_name)) 
  optimal_k <- which.max(sil_values) + 1 
  abline(v = optimal_k, lty = 2, col = "red") 
  points(optimal_k, max(sil_values), pch = 16, col = "red") 
  text(optimal_k, max(sil_values), paste("Optimal k =", optimal_k), pos = 4, col = "red") 
  cat("\nJumlah Klaster Optimal (Silhouette) -", method_name, ":", optimal_k, "\n\n") 
  return(optimal_k) 
} 

# Melakukan analisis silhouette yang diperluas untuk setiap metode 
optimal_k_single <- silhouette_analysis_extended(hc_single, jarak_dtw, "Single 
                                                 Linkage") 
optimal_k_complete <- silhouette_analysis_extended(hc_complete, jarak_dtw, 
                                                   "Complete Linkage") 
optimal_k_average <- silhouette_analysis_extended(hc_average, jarak_dtw, "Average 
                                                  Linkage") 
optimal_k_ward <- silhouette_analysis_extended(hc_ward, jarak_dtw, "Ward's 
                                               Method") 

cat("Jumlah Klaster Optimal (Silhouette):\n") 
cat("Single Linkage:", optimal_k_single, "\n") 
cat("Complete Linkage:", optimal_k_complete, "\n") 
cat("Average Linkage:", optimal_k_average, "\n") 
cat("Ward's Method:", optimal_k_ward, "\n") 

# 7. Menghitung Koefisien Cophenetic Correlation  
# Fungsi untuk menghitung koefisien cophenetic 
cophenetic_correlation <- function(hc, jarak){ 
  cophenetic_dist <- cophenetic(hc) 
  correlation <- cor(jarak, cophenetic_dist) 
  return(correlation) 
} 

# Menghitung koefisien cophenetic untuk setiap metode 
cophenetic_single <- cophenetic_correlation(hc_single, jarak_dtw) 
cophenetic_complete <- cophenetic_correlation(hc_complete, jarak_dtw) 
cophenetic_average <- cophenetic_correlation(hc_average, jarak_dtw) 
cophenetic_ward <- cophenetic_correlation(hc_ward, jarak_dtw) 

cat("\nKoefisien Cophenetic Correlation:\n") 
cat("Single Linkage:", cophenetic_single, "\n") 
cat("Complete Linkage:", cophenetic_complete, "\n") 
cat("Average Linkage:", cophenetic_average, "\n") 
cat("Ward's Method:", cophenetic_ward, "\n") 

# Menentukan Metode Klaster Terbaik berdasarkan Koefisien Cophenetic Tertinggi 
cophenetic_values <- c(Single = cophenetic_single, Complete = cophenetic_complete, 
                       Average = cophenetic_average, Ward = cophenetic_ward) 
best_method <- names(which.max(cophenetic_values)) 

cat("\nMetode Klaster Terbaik (berdasarkan koefisien cophenetic tertinggi):", 
    best_method, "\n") 

# 8. Memunculkan Dendrogram Metode Terbaik  
if (best_method == "Single") { 
  plot(hc_single, labels = rownames(data_series_winsorized), main = paste("Dendrogram -", best_method, "Linkage"), xlab = "Negara", ylab = "Jarak") 
} else if (best_method == "Complete") { 
  plot(hc_complete, labels = rownames(data_series_winsorized), main = paste("Dendrogram -", best_method, "Linkage"), xlab = "Negara", ylab = "Jarak") 
} else if (best_method == "Average") { 
  plot(hc_average, labels = rownames(data_series_winsorized), main = paste("Dendrogram -", best_method, "Linkage"), xlab = "Negara", ylab = "Jarak") 
} else if (best_method == "Ward") { 
  plot(hc_ward, labels = rownames(data_series_winsorized), main = paste("Dendrogram -", best_method, "Linkage"), xlab = "Negara", ylab = "Jarak") 
} 

# Plot dendrogram average linkage 
hc_average <- hclust(jarak_dtw, method = "average") 
plot(hc_average, 
     main = "Dendrogram - Average Linkage (Potongan 2 Klaster)", 
     xlab = "Negara", 
     ylab = "Jarak") 

# Tambahkan kotak untuk menandai 2 klaster 
rect.hclust(hc_average, k = 2, border = c("red", "blue"))  

# Melihat anggota klaster: 
clusters_average <- cutree(hc_average, k = 2) 
cat("\nAnggota Klaster (Average Linkage, k = 2):\n") 
print(clusters_average) 

library(gplots) 
library(tidyr) 
library(dplyr) 
library(ggplot2) 
library(gridExtra) 

# Munculkan pola fluktuasi 
cluster_result <- cutree(hc_average, k = 2) 
data_with_cluster <- data_series_winsorized 
data_with_cluster$Cluster <- factor(cluster_result) 

plots <- list() 

for (k in 1:2) { 
  cluster_k_data <- data_with_cluster %>% 
    filter(Cluster == k) %>% 
    select(-Cluster) 
  
  cluster_k_long <- cluster_k_data %>% 
    mutate(Negara = rownames(cluster_k_data)) %>% pivot_longer(-Negara, names_to = "Tahun", values_to = "Volume") 
  p <- ggplot(cluster_k_long, aes(x = Tahun, y = Volume, group = Negara, color = 
                                    Negara)) + 
    geom_line(size = 1) + 
    geom_point() + 
    labs(title = paste("Fluktuasi Ekspor - Klaster", k)) + 
    theme_minimal() 
  plots[[k]] <- p 
} 
# Tampilkan semua grafik klaster secara berdampingan 
grid.arrange(grobs = plots, ncol = 1)   
    