setwd("C:/Users/fadli/OneDrive - Institut Teknologi Sepuluh Nopember/Semester 1/Statistika Bisnis (B)/Pertemuan 2/EDA_Project_1")

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Lock5Data)
library(corrplot)

options(max.print = 1000)

housing<-read.csv("housing.csv")
# print(head(housing, 1000))

# dim(housing) # gives the number of rows and columns
# row.names(housing) # get number of rows
# str(housing) # get details such as column names, data types in each column and sample data

# Mendapatkan summary dari dataset
summary(housing)

# Create a named vector with descriptive titles in Bahasa Indonesia
titles <- c(
  CRIM = "Tingkat Kejahatan per Kapita di Kota",
  ZN = "Persentase Lahan Hunian yang Dizonasi untuk Lot Lebih dari 25.000 sq.ft.",
  INDUS = "Persentase Lahan Bisnis Non-Retail per Kota",
  CHAS = "Variabel Dummy Sungai Charles (1 jika Traktat Berbatasan dengan Sungai; 0 jika tidak)",
  NOX = "Konsentrasi Oksida Nitrogen (bagian per 10 juta)",
  RM = "Rata-rata Jumlah Kamar per Hunian",
  AGE = "Persentase Unit yang Dimiliki yang Dibangun Sebelum 1940",
  DIS = "Jarak Tertimbang ke Lima Pusat Pekerjaan di Boston",
  RAD = "Indeks Aksesibilitas ke Jalan Raya Radial",
  TAX = "Tarif Pajak Properti Penuh per $10.000",
  PTRATIO = "Rasio Murid-Guru per Kota",
  B = "1000(Bk - 0.63)^2 di mana Bk adalah Proporsi Kulit Hitam per Kota",
  LSTAT = "Persentase Populasi Berstatus Rendah",
  MEDV = "Nilai Median Rumah yang Dimiliki dalam Ribuan Dolar"
)

# Visualisasi distribusi pada masing-masing variabel numeric

# Check for missing values
missing_values <- colSums(is.na(housing))
print(missing_values)

# Melakukan visualisasi distribusi pada masing-masing variabel
numeric_vars <- housing %>% select_if(is.numeric)

for (var in names(numeric_vars)) {
  p <- ggplot(housing, aes_string(x = var)) +
    geom_histogram(binwidth = 30, fill = "orange", color = "white", alpha = 0.7) +
    labs(title = titles[var], x = var, y = "Frekuensi") +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "white"),
      axis.title.y = element_text(color = "white")
    )
  
  ggsave(paste0(var, "_histogram.png"), p)
}

# Histogram untuk masing-masing variabel numeric
# titles <- c(
#   CRIM = "Per Capita Crime Rate by Town",
#   ZN = "Proportion of Residential Land Zoned for Lots over 25,000 sq.ft.",
#   INDUS = "Proportion of Non-Retail Business Acres per Town",
#   CHAS = "Charles River Dummy Variable (1 if Tract Bounds River; 0 otherwise)",
#   NOX = "Nitric Oxides Concentration (parts per 10 million)",
#   RM = "Average Number of Rooms per Dwelling",
#   AGE = "Proportion of Owner-Occupied Units Built Prior to 1940",
#   DIS = "Weighted Distances to Five Boston Employment Centres",
#   RAD = "Index of Accessibility to Radial Highways",
#   TAX = "Full-Value Property-Tax Rate per $10,000",
#   PTRATIO = "Pupil-Teacher Ratio by Town",
#   B = "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town",
#   LSTAT = "% Lower Status of the Population",
#   MEDV = "Median Value of Owner-Occupied Homes in $1000's"
# )

titles <- c(
  CRIM = "Tingkat Kejahatan per Kapita di Kota",
  ZN = "Persentase Lahan Hunian yang Dizonasi untuk Lot Lebih dari 25.000 sq.ft.",
  INDUS = "Persentase Lahan Bisnis Non-Retail per Kota",
  CHAS = "Variabel Dummy Sungai Charles (1 jika Traktat Berbatasan dengan Sungai; 0 jika tidak)",
  NOX = "Konsentrasi Oksida Nitrogen (bagian per 10 juta)",
  RM = "Rata-rata Jumlah Kamar per Hunian",
  AGE = "Persentase Unit yang Dimiliki yang Dibangun Sebelum 1940",
  DIS = "Jarak Tertimbang ke Lima Pusat Pekerjaan di Boston",
  RAD = "Indeks Aksesibilitas ke Jalan Raya Radial",
  TAX = "Tarif Pajak Properti Penuh per $10.000",
  PTRATIO = "Rasio Murid-Guru per Kota",
  B = "1000(Bk - 0.63)^2 di mana Bk adalah Proporsi Kulit Hitam per Kota",
  LSTAT = "Persentase Populasi Berstatus Rendah",
  MEDV = "Nilai Median Rumah yang Dimiliki dalam Ribuan Dolar"
)

for (var in names(numeric_vars)) {
  p <- ggplot(housing, aes_string(x = var)) +
    geom_histogram(binwidth = 30, fill = "orange", color = "white", alpha = 0.7) +
    labs(title = paste("Distribution of ", titles[var]), x = var, y = "Frequency") +
    theme_minimal() + 
    theme(
      plot.title = element_text(color = "white"),
      axis.title.y = element_text(color = "white")
    )
  
  ggsave(paste0(var, "_histogram.png"), p)
}

# Visualize relationship between variables
# Scatter plot matrix
pairs(numeric_vars)

# Correlation matrix
cor_matrix = cor(numeric_vars)
corrplot(cor_matrix, method = "number")

ggplot(housing, aes(x = RM, y = MEDV)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between RM and MEDV", x = "Average number of rooms per dwelling (RM)", y = "Median value of owner-occupied homes (MEDV)") +
  theme_minimal()

ggplot(housing, aes(x = LSTAT, y = MEDV)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between LSTAT and MEDV", x = "% lower status of the population (LSTAT)", y = "Median value of owner-occupied homes (MEDV)") +
  theme_minimal()

ggplot(housing, aes(x = CRIM, y = MEDV)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between CRIM and MEDV", x = "Per capita crime rate by town (CRIM)", y = "Median value of owner-occupied homes (MEDV)") +
  theme_minimal()


p <- ggplot(housing, aes_string(x = "RM")) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "white", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )
print(p)

p <- ggplot(housing, aes_string(x = "NOX")) +
  geom_bar(fill = "orange", color = "white", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )
print(p)

p <- ggplot(housing, aes_string(x = "NOX")) +
  geom_histogram(binwidth = 0.01, fill = "orange", color = "white", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )
print(p)