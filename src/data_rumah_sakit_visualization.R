# Analisis Data Rumah Sakit dengan R
# ----------------------------------

# 1. Instalasi dan memuat paket-paket yang diperlukan
# Uncomment baris di bawah ini jika paket belum terinstal
# install.packages(c("tidyverse", "ggplot2", "plotly", "dplyr", "lubridate", "treemap", "corrplot", "gridExtra", "DT"))

library(tidyverse)  # Untuk manipulasi data
library(ggplot2)    # Untuk visualisasi
library(plotly)     # Untuk visualisasi interaktif
library(dplyr)      # Untuk manipulasi data
library(lubridate)  # Untuk manipulasi tanggal
library(treemap)    # Untuk visualisasi treemap
library(corrplot)   # Untuk visualisasi korelasi
library(gridExtra)  # Untuk layout visualisasi
library(DT)         # Untuk tabel interaktif

# 2. Membaca data
# Mengubah path sesuai dengan lokasi file
data_rs <- read.csv("./data/DATA_RUMAH_SAKIT_400.csv", stringsAsFactors = FALSE)

# 3. Menampilkan struktur data
str(data_rs)
summary(data_rs)
head(data_rs)

# 4. Pembersihan dan persiapan data
# ----------------------------------

# Menghapus spasi ekstra di nama kolom
names(data_rs) <- trimws(names(data_rs))

# Memperbaiki nama kolom (menghapus spasi di akhir nama kolom)
names(data_rs) <- gsub("\\s+$", "", names(data_rs))

# Memastikan kolom kategori sebagai faktor
data_rs$`Jenis kelamin` <- as.factor(data_rs$`Jenis.kelamin`)
data_rs$`Diangnosa Penyakit` <- as.factor(data_rs$`Diangnosa.Penyakit`)
data_rs$`Kategori umur` <- as.factor(data_rs$`Kategori.umur`)
data_rs$`Jenis cluster` <- as.factor(data_rs$`Jenis.cluster`)

# Pembersihan data umur (menghapus "tahun" dan mengubah ke numerik)
data_rs$Umur_clean <- as.numeric(gsub("[^0-9.]", "", data_rs$Umur))

# Parsing tanggal lahir ke format date
data_rs$Tanggal_Lahir_Clean <- dmy(data_rs$`Tanggal.Lahir`)

# 5. Analisis Deskriptif
# ----------------------

# Ringkasan statistik umur
summary(data_rs$Umur_clean)

# Distribusi jenis kelamin
table(data_rs$`Jenis kelamin`)

# Distribusi diagnosis penyakit
table(data_rs$`Diangnosa Penyakit`)

# Distribusi kategori umur
table(data_rs$`Kategori umur`)

# Distribusi jenis cluster
table(data_rs$`Jenis cluster`)

# 6. Visualisasi Data
# -------------------

# Plot 1: Distribusi Jenis Kelamin
plot1 <- ggplot(data_rs, aes(x = `Jenis kelamin`, fill = `Jenis kelamin`)) +
  geom_bar() +
  labs(title = "Distribusi Pasien Berdasarkan Jenis Kelamin",
       x = "Jenis Kelamin", y = "Jumlah Pasien") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Plot 2: Distribusi Diagnosis Penyakit
plot2 <- ggplot(data_rs, aes(x = `Diangnosa Penyakit`, fill = `Diangnosa Penyakit`)) +
  geom_bar() +
  labs(title = "Distribusi Pasien Berdasarkan Diagnosis Penyakit",
       x = "Diagnosis Penyakit", y = "Jumlah Pasien") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")

# Plot 3: Distribusi Kategori Umur
plot3 <- ggplot(data_rs, aes(x = `Kategori umur`, fill = `Kategori umur`)) +
  geom_bar() +
  labs(title = "Distribusi Pasien Berdasarkan Kategori Umur",
       x = "Kategori Umur", y = "Jumlah Pasien") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Plot 4: Distribusi Jenis Cluster
plot4 <- ggplot(data_rs, aes(x = `Jenis cluster`, fill = `Jenis cluster`)) +
  geom_bar() +
  labs(title = "Distribusi Pasien Berdasarkan Jenis Cluster",
       x = "Jenis Cluster", y = "Jumlah Pasien") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Menampilkan plot secara grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Plot 5: Hubungan antara Umur dan Diagnosis Penyakit
plot5 <- ggplot(data_rs, aes(x = `Diangnosa Penyakit`, y = Umur_clean, fill = `Diangnosa Penyakit`)) +
  geom_boxplot() +
  labs(title = "Hubungan Umur Pasien dan Diagnosis Penyakit",
       x = "Diagnosis Penyakit", y = "Umur (tahun)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")

# Plot 6: Visualisasi Treemap untuk Diagnosis Penyakit dan Kategori Umur
data_treemap <- data_rs %>%
  group_by(`Diangnosa Penyakit`, `Kategori umur`) %>%
  summarise(count = n(), .groups = 'drop')

treemap(data_treemap,
        index = c("Diangnosa Penyakit", "Kategori umur"),
        vSize = "count",
        type = "index",
        title = "Treemap Diagnosis Penyakit dan Kategori Umur",
        palette = "Set3")

# Plot 7: Heatmap Hubungan Diagnosis Penyakit dan Jenis Cluster
heatmap_data <- table(data_rs$`Diangnosa Penyakit`, data_rs$`Jenis cluster`)
heatmap(heatmap_data, 
        main = "Heatmap Hubungan Diagnosis Penyakit dan Jenis Cluster",
        col = colorRampPalette(c("white", "steelblue"))(25),
        scale = "column")

# Plot 8: Visualisasi interaktif dengan Plotly
plot8 <- ggplot(data_rs, aes(x = `Diangnosa Penyakit`, fill = `Jenis cluster`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribusi Diagnosis Penyakit Berdasarkan Jenis Cluster",
       x = "Diagnosis Penyakit", y = "Jumlah Pasien") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Konversi ke plotly
plotly_plot <- ggplotly(plot8)
plotly_plot

# 7. Analisis Statistik Lanjutan
# ------------------------------

# Tabulasi silang antara diagnosis penyakit dan jenis kelamin
cross_tab <- table(data_rs$`Diangnosa Penyakit`, data_rs$`Jenis kelamin`)
print(cross_tab)

# Chi-square test untuk menguji hubungan antara diagnosis penyakit dan jenis kelamin
chi_sq_test <- chisq.test(cross_tab, simulate.p.value = TRUE)
print(chi_sq_test)

# Analisis korelasi untuk variabel numerik
# Misalnya, jika kita memiliki beberapa variabel numerik, kita bisa menghitung korelasinya
# Untuk contoh ini, kita membuat data dummy
# numeric_data <- data.frame(Umur = data_rs$Umur_clean,
#                          Biaya = rnorm(nrow(data_rs), mean = 5000000, sd = 2000000),
#                          Lama_Rawat = sample(1:14, nrow(data_rs), replace = TRUE))
# 
# correlation_matrix <- cor(numeric_data, use = "complete.obs")
# corrplot(correlation_matrix, method = "circle")

# 8. Segmentasi Pasien
# -------------------

# Analisis cluster berdasarkan faktor-faktor penting
# Catatan: Data sampel terlalu kecil untuk analisis cluster sebenarnya
# Ini hanya contoh jika data lebih besar

# Jika kita punya beberapa variabel numerik lebih banyak:
# numeric_data_scaled <- scale(numeric_data)
# k_means <- kmeans(numeric_data_scaled, centers = 3)
# data_rs$Cluster <- k_means$cluster
# 
# ggplot(data_rs, aes(x = Umur, y = Lama_Rawat, color = factor(Cluster))) +
#   geom_point(size = 3) +
#   labs(title = "Segmentasi Pasien Berdasarkan Umur dan Lama Rawat",
#        x = "Umur", y = "Lama Rawat (hari)") +
#   theme_minimal()

# 9. Analisis Tren
# ---------------

# Jika ada variabel waktu atau tanggal, kita bisa menganalisis tren
# Misalnya, tren kunjungan pasien berdasarkan tanggal lahir (meskipun ini bukan contoh terbaik)
data_rs %>%
  mutate(Bulan_Lahir = month(Tanggal_Lahir_Clean)) %>%
  group_by(Bulan_Lahir) %>%
  summarise(Jumlah = n()) %>%
  ggplot(aes(x = factor(Bulan_Lahir), y = Jumlah)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribusi Pasien Berdasarkan Bulan Lahir",
       x = "Bulan", y = "Jumlah Pasien") +
  theme_minimal() +
  scale_x_discrete(labels = month.abb)

# 10. Dashboard Interaktif (dalam produksi sebenarnya bisa menggunakan Shiny)
# -------------------

# Untuk menampilkan data dalam format tabel interaktif
datatable(data_rs, 
          options = list(pageLength = 5, scrollX = TRUE),
          caption = "Data Rumah Sakit Interaktif")

# 11. Expor hasil analisis
# -----------------------

# Simpan plot sebagai file gambar
ggsave("./assets/distribusi_jenis_kelamin.png", plot1, width = 8, height = 6)
ggsave("./assets/distribusi_diagnosis.png", plot2, width = 8, height = 6)
ggsave("./assets/distribusi_kategori_umur.png", plot3, width = 8, height = 6)
ggsave("./assets/hubungan_umur_diagnosis.png", plot5, width = 10, height = 6)

# Simpan hasil analisis deskriptif dalam file CSV
write.csv(data.frame(
  Variable = c("Jenis Kelamin", "Diagnosis Penyakit", "Kategori Umur", "Jenis Cluster"),
  Value = c(
    paste(names(table(data_rs$`Jenis kelamin`)), ":", table(data_rs$`Jenis kelamin`), collapse = ", "),
    paste(names(table(data_rs$`Diangnosa Penyakit`)), ":", table(data_rs$`Diangnosa Penyakit`), collapse = ", "),
    paste(names(table(data_rs$`Kategori umur`)), ":", table(data_rs$`Kategori umur`), collapse = ", "),
    paste(names(table(data_rs$`Jenis cluster`)), ":", table(data_rs$`Jenis cluster`), collapse = ", ")
  )
), "./data/analisis_deskriptif.csv", row.names = FALSE)

# 12. Kesimpulan dan Insight
# -------------------------

# Berdasarkan analisis data, beberapa insight yang bisa didapatkan:
# 1. [Insight tentang distribusi jenis kelamin pasien]
# 2. [Insight tentang distribusi diagnosis penyakit]
# 3. [Insight tentang hubungan umur pasien dengan jenis penyakit]
# 4. [Insight tentang clustering pasien]

# Note: Insight spesifik akan bergantung pada hasil analisis data sebenarnya

