# =========================
# 📚 CHARGEMENT DES LIBRAIRIES
# =========================
library(ggplot2)   # Pour les graphiques
library(dplyr)     # Pour manipuler les données
library(tidyr)     # Pour organiser les données

# =========================
# 📊 CREATION DU DATASET
# =========================
data <- data.frame(
  heures_etude = c(2,3,5,6,8,1,4,7,9,10),
  presence = c(60,70,80,90,95,50,65,85,88,92),
  note = c(40,45,55,60,75,35,50,70,80,90)
)

# =========================
# 📈 STATISTIQUES DESCRIPTIVES
# =========================

# Mean (Moyenne)
mean(data$note)

# Median (Médiane)
median(data$note)

# Mode (valeur la plus fréquente)
mode_function <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_function(data$note)

# Range (Min et Max)
range(data$note)

# Variance
var(data$note)

# Standard Deviation (écart-type)
sd(data$note)

# Summary Statistics
summary(data)

# =========================
# 📊 CREATION D’UNE VARIABLE CONTINUE
# =========================

# Création d'un score global
data$score <- (data$heures_etude * 5 + data$presence * 0.5 + data$note) / 3

# =========================
# 📉 CORRELATION
# =========================

# Corrélation entre deux variables
cor(data$heures_etude, data$note)

# Exemple de corrélation négative (explication)
# Si une variable augmente et l'autre diminue, la corrélation est négative

# Test de corrélation
cor.test(data$heures_etude, data$note)

# =========================
# 📈 REGRESSION LINEAIRE
# =========================

# Fit linear model
model <- lm(note ~ heures_etude, data = data)

# Display model summary
summary(model)

# Regression Equation (coefficients)
coef(model)

# Predict Value Using Regression
data$prediction <- predict(model, data)

# =========================
# 📊 GRAPHIQUES
# =========================

# Histogram
ggplot(data, aes(x = note)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogramme des notes")

# Density Plot
ggplot(data, aes(x = note)) +
  geom_density() +
  ggtitle("Densité des notes")

# Line Plot
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_line() +
  geom_point() +
  ggtitle("Evolution des notes")

# Box Plot
ggplot(data, aes(x = "", y = note)) +
  geom_boxplot() +
  ggtitle("Boxplot des notes")

# Scatter Plot
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_point() +
  ggtitle("Relation heures et note")

# Plot Regression line
ggplot(data, aes(x = heures_etude, y = note)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Regression lineaire")
