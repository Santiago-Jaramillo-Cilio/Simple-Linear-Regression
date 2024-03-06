# Load and view the initial part of the women dataset
data(women)
head(women)


# Initial scatter plot of women's height vs. weight
ggplot(data = women, aes(x = height, y = weight)) +
  geom_point(size = 3, alpha = .5) +
  ggtitle("Women's Height vs Weight") +
  xlab("Height (inches)") +
  ylab("Weight (pounds)")

# linear model predicting weight based on height
model1 <- lm(weight ~ height, data = women)
# View the summary of the linear model
summary(model1)

# Define new data for which predictions are to be made
new_data <- data.frame(height = c(68,70,72,71,63,65,67,57))

#Predict weights for the new heights using the fitted model
predicted_weights <- predict(model1, new_data)

# Create a new data frame including both the new heights and their predicted weights
new_data_with_predictions <- data.frame(
  Height = new_data$height,
  PredictedWeight = predicted_weights)

# Print the rounded predicted weights
print(round(predicted_weights))

# Plot the original data and overlay predictions
ggplot() +
  geom_point(data = women, aes(x = height, y = weight), 
             color = "blue", size = 3, alpha = .5, show.legend = TRUE, name = "Original Data") +
  geom_point(data = new_data_with_predictions, aes(x = Height, y = PredictedWeight), 
             color = "red", size = 3, alpha = .5, show.legend = TRUE, name = "Predictions") +
  geom_smooth(data = women, aes(x = height, y = weight), method = "lm", se = FALSE, color = "darkblue") +
  ggtitle("Women's Height vs Weight with Predictions") +
  xlab("Height (inches)") +
  ylab("Weight (pounds)") +
  theme_minimal() +
  scale_color_manual("", labels = c("Original Data", "Predictions"), 
                     values = c("blue", "red"))

