setwd("C:/Users/fadli/OneDrive - Institut Teknologi Sepuluh Nopember/Semester 1/Statistika Bisnis (B)/Pertemuan 2/EDA_Project_1")
# Create the 'histogram' and 'plot' folder if it doesn't exist
if (!dir.exists("sales_histogram")) {
  dir.create("sales_histogram")
}
if (!dir.exists("sales_plot")) {
  dir.create("sales_plot")
}

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Lock5Data)
library(corrplot)
library(ggExtra)
library(scales)
options(max.print = 1000)

sales<-read.csv("sales.csv")
print(head(sales, 1000))

str(sales)

summary(sales)

titles <- c(
  TRANSACTION_DATE = "Tanggal Transaksi",
  BUBUR_LAKU = "Bubur",
  BUBUR_SISA = "Bubur Sisa",
  TIM_LAKU = "Nasi Tim",
  TIM_SISA = "Nasi Tim Sisa",
  PUDING_BESAR = "Puding Besar",
  PUDING_KECIL = "Puding Kecil",
  PUDING_SACHET = "Puding Sachet",
  PUDING_SISA = "Puding Sisa",
  OATMEAL_BASAH = "Oatmeal",
  OATMEAL_SACHET = "Oatmeal Sachet",
  OATMEAL_SISA = "Oatmeal Sisa",
  ABON = "Abon",
  AMOUNT_OMZET = "Omzet dalam 1 hari"
)

missing_values <- colSums(is.na(sales))
print(missing_values)

numeric_vars <- sales %>% select_if(is.numeric)
print(numeric_vars)

pairs(numeric_vars)

# BUBUR_LaKU
summary(sales$BUBUR_LAKU)
var = "BUBUR_LAKU"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p) 

# BUBUR_SISA
summary(sales$BUBUR_SISA)
var = "BUBUR_SISA"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# TIM_LAKU
summary(sales$TIM_LAKU)
var = "TIM_LAKU"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# TIM_SISA
summary(sales$TIM_SISA)
var = "TIM_SISA"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# PUDING_BESAR
summary(sales$PUDING_BESAR)
var = "PUDING_BESAR"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# PUDING_KECIL
summary(sales$PUDING_KECIL)
var = "PUDING_KECIL"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# PUDING_SACHET
summary(sales$PUDING_SACHET)
var = "PUDING_SACHET"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# PUDING_SISA
summary(sales$PUDING_SISA)
var = "PUDING_SISA"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# OATMEAL_BASAH
summary(sales$OATMEAL_BASAH)
var = "OATMEAL_BASAH"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# OATMEAL_SACHET
summary(sales$OATMEAL_SACHET)
var = "OATMEAL_SACHET"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# OATMEAL_SISA
summary(sales$OATMEAL_SISA)
var = "OATMEAL_SISA"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

# ABON
summary(sales$ABON)
var = "ABON"
p <- ggplot(sales, aes_string(x = var)) +
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
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)

label_with_period <- function(x) {
  format(x, big.mark = ".", scientific = FALSE)
}
# AMOUNT_OMZET
summary(sales$AMOUNT_OMZET)
var = "AMOUNT_OMZET"
p <- ggplot(sales, aes_string(x = var)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7) +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, color = "black", size = 3) +
  labs(title = titles[var], x = var, y = "Frekuensi") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black")
  ) +
  scale_x_continuous(labels = comma)
print(p)
ggsave(paste0("sales_histogram/", var, "_histogram.png"), p)



# 1. Bagaimana hubungan omzet dengan terjualnya bubur (AMOUNT_OMZET & BUBUR_LAKU)
p <- ggplot(sales, aes(x = AMOUNT_OMZET, y = BUBUR_LAKU)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between AMOUNT_OMZET and BUBUR_LAKU", x = "Besarnya omzet dalam 1 hari (AMOUNT_OMZET)", y = "Terjualnya bubur (BUBUR_LAKU)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  ) +
  scale_x_continuous(labels = comma)
print(p)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-BUBUR_LAKU-comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-BUBUR_LAKU-marginal.png"), p_with_marginals)


# 2. Bagaimana hubungan omzet dengan terjualnya abon (AMOUNT_OMZET & ABON)
p <- ggplot(sales, aes(x = AMOUNT_OMZET, y = ABON)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between AMOUNT_OMZET and ABON", x = "Besarnya omzet dalam 1 hari (AMOUNT_OMZET)", y = "Terjualnya abon (ABON)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  ) +
  scale_x_continuous(labels = comma)
print(p)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-ABON-comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-ABON-marginal.png"), p_with_marginals)

# 3. Bagaimana hubungan terjualnya bubur dengan terjualnya nasi tim
p <- ggplot(sales, aes(x = BUBUR_LAKU, y = TIM_LAKU)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between BUBUR_LAKU and TIM_LAKU", x = "Terjualnya bubur (BUBUR_LAKU)", y = "Terjualnya nasi tim (TIM_LAKU)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  ) +
  scale_x_continuous(labels = comma)
print(p)
ggsave(paste0("sales_plot/", "BUBUR_LAKU-TIM_LAKU-comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("sales_plot/", "BUBUR_LAKU-TIM_LAKU-marginal.png"), p_with_marginals)


# 4. Bagaimana hubungan omzet dalam 1 hari dengan terjualnya nasi tim
p <- ggplot(sales, aes(x = AMOUNT_OMZET, y = TIM_LAKU)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between AMOUNT_OMZET and TIM_LAKU", x = "Besarnya omzet dalam sehari (AMOUNT_OMZET)", y = "Terjualnya nasi tim (TIM_LAKU)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black")
  ) +
  scale_x_continuous(labels = comma)
print(p)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-TIM_LAKU-comparison.png"), p)
# Add marginal histograms to the scatter plot
p_with_marginals <- ggMarginal(p, type = "histogram", fill = "orange", color = "black", alpha = 0.7)
# Print the plot with marginal histograms
print(p_with_marginals)
ggsave(paste0("sales_plot/", "AMOUNT_OMZET-TIM_LAKU-marginal.png"), p_with_marginals)

