# Create a simple dummy dataset

amount = 100

set.seed(123) # Setting seed for reproducibility
student_data <- data.frame(
  student_id = 1:amount,
  math_score = rnorm(amount, mean = 75, sd = 10),
  science_score = rnorm(amount, mean = 70, sd = 12),
  history_score = rnorm(amount, mean = 80, sd = 8)
)

# Display the first few rows of the dataset
head(student_data)

# Basic Statistics
summary(student_data)

# Install required packages if you haven't (remove # if needed)
# install.packages("ggplot2")
# install.packages("tidyr")

# Load libraries
library(ggplot2)
library(tidyr)

# Example 1: Basic scatter plot
ggplot(student_data, aes(x = math_score, y = science_score)) +
  geom_point(aes(color = history_score), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship Between Math and Science Scores",
       x = "Math Score",
       y = "Science Score",
       color = "History Score") +
  theme_minimal()

# Example 2: Histogram of math scores
ggplot(student_data, aes(x = math_score)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Math Scores",
       x = "Math Score",
       y = "Frequency") +
  theme_light()

# Example 3: Box plots for all subjects
student_data_long <- pivot_longer(
  student_data,
  cols = c(math_score, science_score, history_score),
  names_to = "subject",
  values_to = "score"
)

ggplot(student_data_long, aes(x = subject, y = score, fill = subject)) +
  geom_boxplot() +
  labs(title = "Score Distribution by Subject",
       x = "Subject",
       y = "Score") +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel1")

# Example 4: Density plot comparing all subjects
ggplot(student_data_long, aes(x = score, fill = subject)) +
  geom_density(alpha = 0.5) +
  labs(title = "Score Density by Subject",
       x = "Score",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Example 5: Correlation heatmap
cor_matrix <- cor(student_data[,2:4])
cor_data <- as.data.frame(as.table(cor_matrix))
names(cor_data) <- c("Var1", "Var2", "Correlation")

ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap of Student Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))