setwd("C:/Users/fadli/OneDrive - Institut Teknologi Sepuluh Nopember/Semester 1/Statistika Bisnis (B)/Pertemuan 2/EDA_Project_1")
# Create the 'histogram' and 'plot' folder if it doesn't exist
if (!dir.exists("histogram")) {
  dir.create("histogram")
}
if (!dir.exists("plot")) {
  dir.create("plot")
}

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Lock5Data)
library(corrplot)
library(ggExtra)

options(max.print = 1000)

housing<-read.csv("housing.csv")
# print(head(housing, 1000))

# dim(housing) # gives the number of rows and columns
# row.names(housing) # get number of rows
str(housing) # get details such as column names, data types in each column and sample data

# Mendapatkan summary dari dataset
summary(housing)

# Create a named vector with descriptive titles in Bahasa Indonesia
titles <- c(
  CRIM = "Tingkat Kejahatan per Kapita di Kota",
  ZN = "Persentase Lahan Hunian yang Dizonasi untuk Lot Lebih dari 25.000 sq.ft.",
  INDUS = "Persentase Lahan Bisnis Non-Retail per Kota",
  CHAS = "Bangunan dekat sungai Charles (1), lainnya (0)",
  NOX = "Konsentrasi Oksida Nitrogen (bagian per 10 juta)",
  RM = "Rata-rata Jumlah Kamar per Hunian",
  AGE = "Persentase Unit yang Dimiliki yang Dibangun Sebelum 1940",
  DIS = "Jarak rumah menuju tempat kerja / pusat pekerjaan",
  RAD = "Indeks Aksesibilitas ke Jalan Raya",
  TAX = "Tarif Pajak Properti per $10.000",
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

# CRIM
summary(housing$CRIM)
var = "CRIM"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, color = "black", size = 3) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p) 

# ZN
summary(housing$ZN)
var = "ZN"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, color = "black", size = 3) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p) 

# INDUS
summary(housing$INDUS)
var = "INDUS"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, color = "black", size = 3) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# CHAS
summary(housing$CHAS)
var = "CHAS"
p <- ggplot(housing, aes_string(x = var)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5, color = "black") +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# NOX
summary(housing$NOX)
var = "NOX"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(binwidth = 0.01, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# RM
summary(housing$RM)
var = "RM"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# AGE
summary(housing$AGE)
var = "AGE"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# DIS
summary(housing$DIS)
var = "DIS"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# RAD
summary(housing$RAD)
var = "RAD"
p <- ggplot(housing, aes_string(x = var)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5, color = "black") +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# TAX
summary(housing$TAX)
var = "TAX"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

#PTRATIO
summary(housing$PTRATIO)
var = "PTRATIO"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

#B
summary(housing$B)
var = "B"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

#LSTAT
summary(housing$LSTAT)
var = "LSTAT"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

#MEDV
summary(housing$MEDV)
var = "MEDV"
p <- ggplot(housing, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("histogram/", var, "_histogram.png"), p)

# Relationship between MEDV and LSTAT
p <- ggplot(housing, aes(x = MEDV, y = LSTAT)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between LSTAT and MEDV", x = "Median value of owner-occupied homes (MEDV)", y = "% lower status of the population (LSTAT)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("plot/", "MEDV_LSTAT_comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("plot/", "MEDV_LSTAT_marginal.png"), p_with_marginals)

# Relationship between MEDV and DIS
p <- ggplot(housing, aes(x = MEDV, y = DIS)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between DIS and MEDV", x = "Median value of owner-occupied homes (MEDV)", y = "weighted distances to five Boston employment centres (DIS)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("plot/", "MEDV_DIS_comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("plot/", "MEDV_DIS_marginal.png"), p_with_marginals)

# Relationship between MEDV and CRIM
p <- ggplot(housing, aes(x = MEDV, y = CRIM)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between CRIM and MEDV", x = "Median value of owner-occupied homes (MEDV)", y = "per capita crime rate by town (CRIM)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  )
print(p)
ggsave(paste0("plot/", "MEDV_CRIM_comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("plot/", "MEDV_CRIM_marginal.png"), p_with_marginals)