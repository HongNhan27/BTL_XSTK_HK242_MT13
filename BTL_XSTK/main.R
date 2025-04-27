path <- "Intel_CPUs.csv"

data <- read.csv(file = path, header = TRUE, sep = ",")

df <- data[, c("Vertical_Segment", "Lithography", "nb_of_Cores", "nb_of_Threads",
                "Processor_Base_Frequency", "TDP", "Max_Memory_Size",
                "Max_Memory_Bandwidth", "Max_nb_of_PCI_Express_Lanes")]

head(df)

df$Vertical_Segment <- as.factor(df$Vertical_Segment)

convert_frequency <- function(frequency) {
   if (grepl("GHz", frequency)) {
     return(as.numeric(gsub(" GHz", "", frequency)) * 1000)
   } else if (grepl("MHz", frequency)) {
     return(as.numeric(gsub(" MHz", "", frequency)))
   } else {
     return(as.numeric(frequency))
   }
 }

df$Processor_Base_Frequency <- sapply(df$Processor_Base_Frequency, convert_frequency)

df$TDP <- gsub(" W", "", df$TDP)     
df$TDP <- gsub("[^0-9.]", "", df$TDP)    
df$TDP <- as.numeric(df$TDP)
df$Max_Memory_Size <- gsub(" GB", "", df$Max_Memory_Size)
df$Max_Memory_Size <- as.numeric(df$Max_Memory_Size)


df$Max_Memory_Bandwidth <- gsub(" GB/s", "", df$Max_Memory_Bandwidth)
df$Max_Memory_Bandwidth <- as.numeric(df$Max_Memory_Bandwidth)
df$Lithography <- gsub(" nm", "", df$Lithography)
df$Lithography <- as.numeric(df$Lithography)
df$nb_of_Cores <- as.numeric(df$nb_of_Cores)
df$nb_of_Threads <- as.numeric(df$nb_of_Threads)
df$Max_nb_of_PCI_Express_Lanes <- as.numeric(df$Max_nb_of_PCI_Express_Lanes)
head(df)

apply(is.na(df), 2, sum)

apply(is.na(df), 2, mean)

replace_na_with_median <- function(x) {
   x[is.na(x)] <- median(x, na.rm = TRUE)
   return(x)
 }


numeric_columns <- sapply(df, is.numeric)

df[numeric_columns] <- sapply(df[numeric_columns], replace_na_with_median)

df <- as.data.frame(df)

apply(is.na(df), 2, sum)


#Thong ke mo ta
data <- read.csv("output.csv")

library(ggplot2)
head(data)
df <- data[, c("Vertical_Segment", "Lithography", "nb_of_Cores", "nb_of_Threads",
               "Processor_Base_Frequency", "TDP", "Max_Memory_Size", "Max_Memory_Bandwidth",
               "Max_nb_of_PCI_Express_Lanes")]
head(df)
table(data$Vertical_Segment)
# Tính khoảng tứ phân vị (IQR) cho mỗi cột số.
IQR_values <- sapply(df[sapply(df, is.numeric)], IQR) 
# Tính ngưỡng trên và ngưỡng dưới
lower_bounds <- sapply(df[sapply(df, is.numeric)], quantile, probs = 0.25)- 1.5 * IQR_values
upper_bounds <- sapply(df[sapply(df, is.numeric)], quantile, probs = 0.75) + 1.5 * IQR_values
# Tìm giá trị ngoại lai
outliers <- lapply(names(df), function(i) { 
  if (is.numeric(df[[i]])) { 
    df[[i]] < lower_bounds[i] | df[[i]] > upper_bounds[i] 
  } else { 
    rep(FALSE, length(df[[i]]))
  } 
})
# Vẽ biểu đồ bloxplot
boxplot(Processor_Base_Frequency ~ Vertical_Segment, 
        data = df, 
        xlab = "Vertical_Segment", 
        ylab = "Processor Base Frequency (MHz)", 
        col = "blue", 
        pch = 19, 
        outcol = "black")
# Vẽ đồ thị histogram
hist(df$Processor_Base_Frequency, 
     main = "Histogram of Processor Base Frequency", 
     xlab = "Processor Base Frequency (MHz)", 
     col = "blue", 
     border = "black")
# Vẽ biểu đồ phân tán thể hiện phân phối của Processor_Base_Frequency theo các biến 
numeric_cols <- sapply(df, is.numeric) 
numeric_columns <- df[, numeric_cols]
plot_number <- 1 
for (col_name in names(numeric_columns)) { 
  p <- ggplot(df, aes(x = !!sym(col_name), 
                      y = Processor_Base_Frequency)) + 
    geom_point(color = "blue", size = 2) + 
    labs(	x = col_name, 
          y = "Processor Base Frequency (MHz)", 
          title = paste("Processor Base Frequency vs", col_name)
    ) + theme_minimal()
  print(p) 
  plot_number <- plot_number + 1 
}
# Tính các giá trị thống kê theo từng nhóm dữ liệu
summary(df)
by(df, df$Vertical_Segment, summary)

#Bai toan 1 mau

n<-length(data$Processor_Base_Frequency)
xtb<-mean(data$Processor_Base_Frequency)
sd<-sd(data$Processor_Base_Frequency)
data.frame(n, xtb, sd)

qqnorm(data$Processor_Base_Frequency)
qqline(data$Processor_Base_Frequency)

shapiro.test(data$Processor_Base_Frequency)

z <- qnorm(0.975)
print(z)
Epison <- (z * sd)/ sqrt(n)
print(Epison)
data.frame(CT=xtb-Epison, CP=xtb+Epison)

data_desktop<-subset(data, Vertical_Segment == "Desktop")
n1<-length(data_desktop$Processor_Base_Frequency)
xtb1<-mean(data_desktop$Processor_Base_Frequency)
sd1<-sd(data_desktop$Processor_Base_Frequency)

data_server<-subset(data, Vertical_Segment == "Server")
n2<-length(data_server$Processor_Base_Frequency)
xtb2<-mean(data_server$Processor_Base_Frequency)
sd2<-sd(data_server$Processor_Base_Frequency)
data.frame(n1, xtb1, sd1, n2, xtb2, sd2)

qqnorm(data_desktop$Processor_Base_Frequency)
qqline(data_desktop$Processor_Base_Frequency)

shapiro.test(data_desktop$Processor_Base_Frequency)

qqnorm(data_server$Processor_Base_Frequency)
qqline(data_server$Processor_Base_Frequency)

shapiro.test(data_server$Processor_Base_Frequency)

z_0 <- (xtb1 - xtb2) / sqrt(sd1^2 / n1 + sd2^2 / n2)
print(z_0)

#Anova 

library(nortest)  
library(car)      
library(ggplot2)  
library(dplyr)    

data <- read.csv("output.csv")

data_1 <- subset(data, Vertical_Segment == "Mobile")
data_2 <- subset(data, Vertical_Segment == "Embedded")
data_3 <- subset(data, Vertical_Segment == "Server")
data_4 <- subset(data, Vertical_Segment == "Desktop")

par(mfrow = c(2, 2)) 
qqnorm(data_1$TDP, main = "QQ-Plot for Mobile"); qqline(data_1$TDP)
qqnorm(data_2$TDP, main = "QQ-Plot for Embedded"); qqline(data_2$TDP)
qqnorm(data_3$TDP, main = "QQ-Plot for Server"); qqline(data_3$TDP)
qqnorm(data_4$TDP, main = "QQ-Plot for Desktop"); qqline(data_4$TDP)


print(ad.test(data_1$TDP))
print(ad.test(data_2$TDP))
print(ad.test(data_3$TDP))
print(ad.test(data_4$TDP))

data$Vertical_Segment <- as.factor(data$Vertical_Segment)

leveneTest(TDP ~ Vertical_Segment, data = data)
cat("p-value=", leveneTest(TDP ~ Vertical_Segment, data = data)$"Pr(>F)"[1], "\n")


anova_model <- aov(TDP ~ Vertical_Segment, data = data)
anova_summary <- summary(anova_model)
print(anova_summary)

p_value <- anova_summary[[1]]$"Pr(>F)"[1] 
cat("p-value=", p_value, "\n")

anova_table <- summary(anova_model)[[1]]

SSB <- anova_table["Vertical_Segment", "Sum Sq"]
SSW <- anova_table["Residuals", "Sum Sq"]
SST <- (SSB+SSW)
R_squared <- SSB / SST
cat("R^2 =", round(R_squared, 4), "\n")


TukeyHSD(anova_model)
par(mar = c(5, 10, 4, 2)) 
plot(TukeyHSD(anova_model), las = 1)

#HQTT

data <- read.csv("output.csv")
df <- read.csv("output.csv")

# Build a linear regression model.
model <- lm(Processor_Base_Frequency ~ ., data = df)

# Print the results.
options(scipen = 999)
summary(model)

# Rebuild the linear regression model.
model <- lm(Processor_Base_Frequency ~ Vertical_Segment + Lithography + nb_of_Cores + TDP + Max_Memory_Size, data = df)

# Print the results.
options(scipen = 999)
summary(model)

# Draw a graph to check assumptions.
plot (model)

#Prediction
plot(df$Processor_Base_Frequency,
     predict(model, df),
     xlab = "Processor_Base_Frequency",
     ylab = "Predicted Processor_Base_Frequency",
     main = "Processor_Base_Frequency and Predicted Processor_Base_Frequency")

compair <- lm(predict(model, df) ~ Processor_Base_Frequency, data = df)
abline(compair, col = "red")

