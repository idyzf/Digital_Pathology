library(openxlsx)
library(dplyr)

rm(list = ls())

patologista <- read.xlsx("/Users/idelizanesco/Library/CloudStorage/GoogleDrive-ideli.zanesco@gmail.com/Meu Drive/Colab Notebooks/Mestrado/Projeto/Patologia/CK7_resultados_IHQ_08-12.xlsx")
ml <- read.csv("ratio_med.csv", sep = ';')

names_ml <- ml$REDCAP_ID
patologista <- subset(patologista, patologista$ID_REDCAP %in% names_ml)
names(patologista)

pat <- patologista[,c(1, 49)]
ml$ID_REDCAP <- ml$REDCAP_ID
data1 <- left_join(pat, ml)
data1<- data1[,-3:-4]
names(data1)

library(ordinal)

data1$Score.ImunoHistoquimica <- as.factor(data1$Score.ImunoHistoquimica)
# Assuming Score.ImunoHistoquimica is ordered
model <- clm(Score.ImunoHistoquimica ~ med_pixels, data = data1, link = "logit")

# Analyze and interpret the model coefficients
summary(model)

data1$Score.ImunoHistoquimica <- as.numeric(data1$Score.ImunoHistoquimica)
c <-cor(data1$med_pixels, data1$Score.ImunoHistoquimica)

plot(data1$med_pixels, data1$Score.ImunoHistoquimica, main="Regression for Pixels and mRNA counts", 
     xlab= "pixels", ylab="counts")
abline(model, col="red")

# Assuming the model summary and data manipulation steps are already performed

# Calculate the correlation coefficient (if not already done)
c <- cor(data1$med_pixels, data1$Score.ImunoHistoquimica)

# Linear regression for the correlation line
lm <- lm(Score.ImunoHistoquimica ~ med_pixels, data = data1)

# Extract slope and intercept from the model
slope <- lm$coefficients[[2]]
intercept <- lm$coefficients[[1]]

# Generate x-axis values for the line
x_values <- seq(min(data1$med_pixels), max(data1$med_pixels), length = 100)

# Calculate corresponding y-values for the line using the formula
y_values <- slope * x_values + intercept

# Plot the data points and the correlation line
plot(data1$med_pixels, data1$Score.ImunoHistoquimica, main = "Regression for Pixels and mRNA counts",
     xlab = "pixels", ylab = "counts")
abline(lm, col = "red", lwd = 2)  # Add the correlation line

# Add correlation coefficient to the plot (optional)
text(max(data1$med_pixels) * 0.8, max(data1$Score.ImunoHistoquimica) * 0.8, paste("R:", round(c, 2)), col = "red")

data1 %>%
  ggplot(aes(x = Score.ImunoHistoquimica, y = med_pixels, colour =Score.ImunoHistoquimica)) +
  geom_point(alpha = 0.4) +
  ylab("Average of DAB/HE ratio") +
  xlab("Patologist Score")

library(psych)
cohen.kappa(x=cbind(data1$Score.ImunoHistoquimica, data1$med_pixels))
