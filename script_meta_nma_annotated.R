
# ======================
# 1. Setup Awal
# ======================
# Memuat package yang dibutuhkan
# Jalankan install.packages() hanya sekali jika belum terinstal
# install.packages(c("dplyr", "ggplot2", "metafor", "netmeta"))
library(dplyr)
library(ggplot2)
library(metafor)
library(netmeta)

# ======================
# 2. Baca Data Pasien
# ======================
data_pasien <- read.csv("dummy_data_pasien.csv")
head(data_pasien)

# ======================
# 3. Manipulasi Data
# ======================
data_pasien <- data_pasien %>%
  mutate(
    Tinggi_Badan_m = Tinggi_Badan_cm / 100,
    BMI = Berat_Badan_kg / (Tinggi_Badan_m^2)
  )

rata2_bmi_per_kelamin <- data_pasien %>%
  group_by(Jenis_Kelamin) %>%
  summarise(Rata2_BMI = mean(BMI))

print(rata2_bmi_per_kelamin)

# ======================
# 4. Visualisasi Data
# ======================
ggplot(data_pasien, aes(x = Jenis_Kelamin, y = BMI, fill = Jenis_Kelamin)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot BMI berdasarkan Jenis Kelamin", y = "BMI")

# ======================
# 5. Meta-Analisis Dasar (menggunakan metafor)
# ======================
meta_data <- data.frame(
  Study = c("Studi A", "Studi B", "Studi C"),
  n.e = c(30, 40, 35),
  mean.e = c(25.5, 24.0, 26.2),
  sd.e = c(5.1, 4.8, 5.3),
  n.c = c(30, 40, 35),
  mean.c = c(28.1, 27.5, 29.0),
  sd.c = c(5.0, 4.5, 5.4)
)

# Hitung efektivitas (effect size) dari masing-masing studi
# yi = mean difference, vi = variance
efek_meta <- escalc(
  measure = "MD",
  m1i = mean.e, sd1i = sd.e, n1i = n.e,
  m2i = mean.c, sd2i = sd.c, n2i = n.c,
  data = meta_data
)

# Jalankan meta-analisis model random effects (DL = DerSimonian-Laird)
hasil_meta <- rma(yi, vi, data = efek_meta, method = "DL")
summary(hasil_meta)
forest(hasil_meta, slab = meta_data$Study)

# ======================
# 6. Network Meta-Analysis
# ======================
nma_data <- data.frame(
  study = c("S1", "S2", "S2", "S3", "S3"),
  treat1 = c("Plasebo", "Plasebo", "Obat A", "Plasebo", "Obat B"),
  treat2 = c("Obat A", "Obat A", "Obat B", "Obat B", "Obat A"),
  mean1 = c(20, 22, 24, 19, 21),
  sd1 = c(5, 4.5, 4.8, 5.2, 4.9),
  n1 = c(30, 30, 30, 30, 30),
  mean2 = c(22, 24, 25, 21, 23),
  sd2 = c(4.8, 5.0, 4.7, 5.1, 5.0),
  n2 = c(30, 30, 30, 30, 30)
)

# Buat data pairwise perbandingan antartreatment
pairwise_data <- pairwise(
  treat = list(treat1, treat2),
  mean = list(mean1, mean2),
  sd = list(sd1, sd2),
  n = list(n1, n2),
  data = nma_data,
  studlab = study
)

# Jalankan network meta-analysis (netmeta)
hasil_nma <- netmeta(
  TE, seTE, treat1, treat2,
  studlab, data = pairwise_data,
  sm = "MD"
)

forest(hasil_nma)
netgraph(hasil_nma)
