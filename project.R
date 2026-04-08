# =========================
# 📚 CHARGEMENT DES LIBRAIRIES
# =========================
library(ggplot2)   # Visualisation
library(dplyr)     # Manipulation
library(tidyr)     # Organisation

# =========================
# 📊 CREATION DU DATASET
# =========================
# Données simulées des étudiants
data <- data.frame(
  heures_etude = c(1,2,3,4,5,6,7,8,9,10),
  presence = c(50,55,60,65,70,75,80,85,90,95),
  note = c(30,40,45,50,55,60,70,75,85,90)
)

# =========================
# 📈 STATISTIQUES DESCRIPTIVES
# =========================

mean_note <- mean(data$note)
median_note <- median(data$note)

mode_function <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_note <- mode_function(data$note)

range_note <- range(data$note)
variance_note <- var(data$note)
sd_note <- sd(data$note)
summary_data <- summary(data)

# =========================
# 📊 CREATION D’UNE VARIABLE CONTINUE
# =========================

data <- data %>%
  mutate(score_global = (heures_etude * 4 + presence * 0.4 + note) / 3)

# =========================
# 📉 CORRELATION
# =========================

correlation <- cor(data$heures_etude, data$note)
correlation_test <- cor.test(data$heures_etude, data$note)

# =========================
# 📈 REGRESSION LINEAIRE
# =========================

model <- lm(note ~ heures_etude, data = data)
model_summary <- summary(model)
coefficients <- coef(model)

data <- data %>%
  mutate(predicted_note = predict(model, data))

# =========================
# 📊 VISUALISATION DES DONNEES
# =========================

# Histogram
ggplot(data, aes(x = note)) +
  geom_histogram(binwidth = 10, fill = "skyblue") +
  ggtitle("Histogram of Grades")

# Density Plot
ggplot(data, aes(x = note)) +
  geom_density(fill = "lightgreen") +
  ggtitle("Density Distribution of Grades")

# Line Plot
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_line(color = "blue") +
  geom_point() +
  ggtitle("Trend of Grades Based on Study Hours")

# Box Plot
ggplot(data, aes(x = "", y = note)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Boxplot of Grades")

# Scatter Plot
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_point(color = "red") +
  ggtitle("Relationship Between Study Hours and Grades")

# Plot Regression Line
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Linear Regression Line")
