
install.packages("tidyverse")
install.packages("skimr")
install.packages("GGally")
install.packages("readxl")   
library(readxl)
library(tidyverse)
library(skimr)
library(GGally)


#MELIHAT STRUKTUR AWAL DATA
# Melihat 6 baris pertama
head(data)

# Melihat struktur variabel
str(data)

# Melihat ringkasan statistik
summary(data)

# Melihat jumlah baris & kolom
dim(data)

#CEK MISSING VALUE
# Total missing value tiap kolom
colSums(is.na(data))


# MENANGANI MISSING VALUE
# Fungsi mencari modus (untuk kategorik)
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Menangani NA:
# - Numerik diisi median
# - Kategorik diisi modus
data_clean <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor),  ~ ifelse(is.na(.), mode_func(.), .)))

# Cek setelah pembersihan
colSums(is.na(data_clean))


# CEK OUTLIER DENGAN IQR
# Menampilkan jumlah outlier tiap variabel numerik
check_outlier <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  sum(x < lower | x > upper, na.rm = TRUE)
}

sapply(data_clean %>% select_if(is.numeric), check_outlier)

# MENANGANI OUTLIER (WINSORIZING)

handle_outlier <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

data_no_outlier <- data_clean %>%
  mutate(across(where(is.numeric), handle_outlier))

#Cek Ulang Outlier Setelah Ditangani
sapply(data_no_outlier %>% select_if(is.numeric), check_outlier)

#STATISTIK DESKRIPTIF
skim(data)

#HISTOGRAM UNTUK VARIABEL AGE
ggplot(data_no_outlier, aes(x = age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram Variabel Age",
       x = "Age",
       y = "Frekuensi")


#VISUALISASI PIE CHART UNTUK VARIABEL SEX
# Hitung frekuensi + persentase
sex_freq <- data_no_outlier %>%
  count(sex) %>%
  mutate(persen = round(n / sum(n) * 100, 1),
         label = paste0(persen, "%"))

# Pie chart 
ggplot(sex_freq, aes(x = "", y = n, fill = factor(sex))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 5) +
  labs(title = "Pie chart Variabel Sex ",
       fill = "Sex") +
  theme_void()

#VISUALISASI BAR CHART VARIABEL CHEST PAIN TYPE (CP)
cp_freq <- data_no_outlier %>%
  count(cp)

ggplot(cp_freq, aes(x = factor(cp), y = n)) +
  geom_col(fill = "pink", color = "black") +
  geom_text(aes(label = n), vjust = -0.5, size = 6) +
  labs(title = "Bar Chart Variabel CP",
       x = "CP (Chest Pain Type)",
       y = "Jumlah") +
  theme_minimal()

colnames(data_no_outlier)

#REGRESI LOGISTIK
# Membentuk variabel target
data_no_outlier$target <- ifelse(
  data_no_outlier$chol >= 240, 1, 0
)

# Ubah ke factor
data_no_outlier$target <- as.factor(data_no_outlier$target)

# Cek distribusi kelas
table(data_no_outlier$target)

#Model regresi logistik
model_logit <- glm(
  target ~ age + sex + cp + trestbps + thalch + oldpeak,
  data = data_no_outlier,
  family = binomial(link = "logit")
)

# Ringkasan model
summary(model_logit)

#ODDS RATIO
install.packages("broom")
library(broom)
OR_table <- tidy(
  model_logit,
  exponentiate = TRUE,   # jadi Odds Ratio
  conf.int = TRUE        # CI 95%
)

OR_table

#Probability P(X)
model_logit <- glm(
  target ~ age + sex + cp + trestbps + thalch + oldpeak,
  data = data_no_outlier,
  family = binomial(link = "logit"),
  na.action = na.exclude
)

# Menghitung probabilitas kejadian (P(Y=1|X))
data_no_outlier$Probability <- predict(
  model_logit,
  type = "response"
)

# Lihat beberapa hasil
head(data_no_outlier[, c("Probability", "target")])



