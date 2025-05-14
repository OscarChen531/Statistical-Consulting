library(readr)
library(finalfit)
library(dplyr)

data <- read_csv("Project 2/journal.pone.0279274.s007.csv")
#data2 <- read_csv("Project 2/journal.pone.0279274.s008.csv")

data$RBC_Quartile <- ifelse(data$`Red blood cell count(106/?L)` <= 4.26, "Q1",
                            ifelse(data$`Red blood cell count(106/?L)` >= 4.27 & data$`Red blood cell count(106/?L)` <= 4.52, "Q2",
                                   ifelse(data$`Red blood cell count(106/?L)` >= 4.53 & data$`Red blood cell count(106/?L)` <= 4.78, "Q3",
                                          "Q4")))

# Convert to factor with correct order
data$RBC_Quartile <- factor(data$RBC_Quartile, levels = c("Q1", "Q2", "Q3", "Q4"))
df <- subset(data, data$Gender==2)
table(df$RBC_Quartile, factor(df$`Smoker classification`))

dm <- subset(data, data$Gender==1)
table(dm$RBC_Quartile, dm$Race)


selected_data <- data[, c(2:5, 7:11, 17:22, 33, 35:36, 38, 47)]

#names(selected_data)
#str(selected_data)

# categorical
selected_data$MAFLD <- as.factor(selected_data$`MAFLD risk`)
selected_data$Gender.factor <- as.factor(selected_data$Gender)
selected_data$Race.factor <- as.factor(selected_data$Race)
selected_data$Hypertension <- as.factor(selected_data$`Self-report hypertension`)
selected_data$Diabetes <- as.factor(selected_data$`Diabetes mellitus`)

table(selected_data$Gender.factor)

# continuous
colnames(selected_data)[1] <- "CAP"
colnames(selected_data)[5:8] <- c("Weight", "Height", "BMI", "Waist")
colnames(selected_data)[10:18] <- 
  c("ALT", "AST", "GGT", "TC", "TG", "SUA", "SBP", "RBC", "Hemoglobin")


# Table 1 - Patient demographics by variable of interest ----
explanatory = c("Age ", "Gender.factor", "Race.factor", "Weight", "Height", 
                "BMI", "Waist", "CAP", "ALT", "AST", "GGT", "TC", "TG", 
                "SUA", "SBP", "RBC", "Hemoglobin", "Hypertension", "Diabetes")
dependent = "MAFLD" # Bowel perforation
selected_data %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r"))



CAP_model <- lm(CAP ~ Age + BMI + Waist + ALT + AST + GGT + TC + TG + 
                  SUA + SBP + RBC + Hemoglobin, data=selected_data)
summary(CAP_model)
vif(CAP_model)

