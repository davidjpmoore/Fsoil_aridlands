# 21-07-2024
# Dave Moore, Anastasia Makhnykina
# Model estimations

# Load necessary library
library(dplyr)

# Constants
SMoptL <- 0.125

# Parameter means and standard errors

FrefL_mean <- 1.183562
FrefL_SE <- 0.04
c4L_mean <- -7.875260
c4L_SE <- 1.4
b4L_mean <- 0.034806
b4L_SE <- 0.003
nL_mean <- 0.079416
nL_SE <- 0.014

# Assume these are known and constant

All_meanGPP <- years_sum1$meanGPP  # Replace with actual value
All_GPPmax <- max(years_sum1$meanGPP, na.rm = TRUE)   # Replace with actual value
All_meanSWC5 <- years_sum1$meanSWC5/100  # Replace with actual value
All_meanST5 <- years_sum1$meanST5  # Replace with actual value

# Function to run the model

run_model <- function(FrefL, c4L, b4L, nL, SMoptL, All_meanGPP, All_GPPmax, All_meanSWC5, All_meanST5) {
  FrefL * ((All_meanGPP / All_GPPmax + nL) / (1 + nL)) *
        (1 - c4L * (SMoptL - All_meanSWC5)^2) * exp(b4L * All_meanST5)
  }

# Run the model 1000 times

results <- replicate(1000, {
  FrefL <- rnorm(1, FrefL_mean, FrefL_SE)
  c4L <- rnorm(1, c4L_mean, c4L_SE)
  b4L <- rnorm(1, b4L_mean, b4L_SE)
  nL <- rnorm(1, nL_mean, nL_SE)
  run_model(FrefL, c4L, b4L, nL, SMoptL, All_meanGPP, All_GPPmax, All_meanSWC5, All_meanST5)
  
}) 

# Convert results to a data frame
results_df <- data.frame(Model_Output = results)

# Display the first few rows of the results
print(head(results_df)) 

# Print summary statistics
summary(results_df)

results_df$MeanR <- rowMeans(results_df, na.rm=TRUE)


