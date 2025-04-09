Kita ingin mengetahui apakah Obat A menurunkan tekanan darah lebih baik dibandingkan 
tanpa pengobatan (kontrol). Data dari 3 studi berisi rerata (mean), simpangan baku (standard deviation / SD), dan jumlah subjek.

# Install jika belum
install.packages("metafor")

# Panggil package
library(metafor)

# Data dummy 3 studi-Menyimpan informasi jumlah subjek, rata-rata, dan SD dari masing-masing studi dalam satu data.frame.
meta_data <- data.frame(
  study = c("Studi 1", "Studi 2", "Studi 3"),
  n_treat = c(30, 40, 35),
  mean_treat = c(120, 115, 118),
  sd_treat = c(10, 12, 11),
  n_control = c(30, 40, 35),
  mean_control = c(130, 125, 123),
  sd_control = c(9, 11, 10)
)

# Hitung Mean Difference (MD) dan variansnya
#escalc() = fungsi untuk menghitung ukuran efek (yi) dan variansnya (vi)
#measure = "MD" = menghitung Mean Difference
#m1i = rata-rata group treatment, m2i = group control
#sd1i, sd2i, n1i, n2i = SD dan n masing-masing
#Hasil akan muncul dalam kolom yi dan vi dalam escalc_data

escalc_data <- escalc(measure = "MD",
                      m1i = mean_treat, sd1i = sd_treat, n1i = n_treat,
                      m2i = mean_control, sd2i = sd_control, n2i = n_control,
                      data = meta_data)

#rma() = fungsi utama meta-analisis
#yi = effect size, vi = varians dari tiap studi
#method = "REML" = metode random-effects menggunakan Restricted Maximum Likelihood

res <- rma(yi, vi, data = escalc_data, method = "REML")
summary(res)

#Interpretasi
#Estimate: Perbedaan rata-rata gabungan (pooled MD)
#CI 95%: Interval kepercayaan, menunjukkan rentang di mana nilai sebenarnya mungkin berada
#Z / p-value: Apakah pooled effect signifikan?
#τ² (tau squared): Varians antar studi (heterogenitas)
#I²: Proporsi varians karena heterogenitas (>50% = tinggi)


#Forestplot
forest(res, slab = meta_data$study,
       xlab = "Mean Difference (mmHg)",
       mlab = "Pooled Effect",
       addfit = TRUE,
       col = "darkblue")

#slab = nama-nama studi ditampilkan di sisi kiri
#xlab = label sumbu X
#mlab = label hasil gabungan (pooled effect)
#addfit = TRUE = tambahkan garis efek gabungan

# Tambahkan variabel dummy misalnya durasi terapi
escalc_data$durasi <- c(4, 8, 12)  # minggu

# Jalankan meta-regression
res_reg <- rma(yi, vi, mods = ~ durasi, data = escalc_data)
summary(res_reg)

Tujuan --> Melihat apakah durasi terapi memengaruhi hasil. 
Jika p-value dari durasi < 0.05 → durasi punya pengaruh signifikan




#Whatcanweevaluate?

Jika pooled MD negatif dan signifikan → Obat A menurunkan tekanan darah dibanding kontrol.

Jika I² tinggi → pertimbangkan analisis sensitivitas atau meta-regresi.

Forest plot membantu menilai kekonsistenan hasil antar studi.