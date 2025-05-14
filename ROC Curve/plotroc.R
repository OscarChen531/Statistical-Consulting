# Load packages
library(gclus)
library(pROC)

# ----------------------------
# 1. Data preparation and GLM model
# ----------------------------
# Load the data 
data(body, package="gclus")
# names(body)
# str(body)
# head(body)

# Subset to only the three variables and convert Gender to a factor.
body <- within(body[, c("Weight", "Height",  "Gender")], {
  Gender <- as.factor(Gender)
  }) 

# Fit the GLM (logistic regression) using Height and Weight as predictors.
GLM_model <- glm(Gender ~ Weight +Height, family=binomial(logit), data=body)
summary(GLM_model)

# Extract fitted probabilities.
body<- within(body, {
  fitted.GLM_model <- fitted(GLM_model) 
})

# ----------------------------
# 2. Generate Predictions for a Sequence of Cutoffs
# ----------------------------
# Create a sequence of cutoff values.
thresholds <- seq(0, 1, 0.001)

# Using sapply to compute predictions for each threshold.
# Each column in pred_matrix corresponds to 1*(fitted.GLM_model > cutoff) for one cutoff.
pred_matrix <- sapply(thresholds, function(cutoff) 1 * (body$fitted.GLM_model > cutoff))

# ----------------------------
# 3. Compute Sensitivity and Specificity for each cutoff
# ----------------------------
# Convert actual Gender values to binary indicator.
# Assuming the second level of Gender is the "positive" class.
actual <- ifelse(body$Gender == levels(body$Gender)[2], 1, 0)

# Sensitivity (TP / (TP + FN)) and Specificity (TN / (TN + FP))
sensitivity <- sapply(1:ncol(pred_matrix), function(i) {
  preds <- pred_matrix[, i]
  TP <- sum(preds == 1 & actual == 1)
  FN <- sum(preds == 0 & actual == 1)
  if ((TP + FN) == 0) NA else TP / (TP + FN)
})

specificity <- sapply(1:ncol(pred_matrix), function(i) {
  preds <- pred_matrix[, i]
  TN <- sum(preds == 0 & actual == 0)
  FP <- sum(preds == 1 & actual == 0)
  if ((TN + FP) == 0) NA else TN / (TN + FP)
})

# Combine into a data frame.
roc_data <- data.frame(Threshold = thresholds, Specificity = specificity, Sensitivity = sensitivity)

# ----------------------------
# 4. Plot the ROC Curve (x: Specificity, y: Sensitivity)
# ----------------------------
plot(1 - roc_data$Specificity, roc_data$Sensitivity,
     xlab = "1 - Specificity (FPR)", 
     ylab = "Sensitivity (TPR)",
     main = "ROC Curve (x: Specificity, y: Sensitivity)",
     pch = 20,          # Small filled circles
     col = "blue")      # Points in blue

# Connect the points with a line.
lines(1 - roc_data$Specificity, roc_data$Sensitivity, col = "red")

# Optionally, add a reference line (a random classifier would be near the diagonal of specificity vs. sensitivity)
abline(0, 1, lty = 2)
