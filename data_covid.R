# Install and load COVID19 package
install.packages("COVID19")
library(COVID19)

# Download data
covid_data <- covid19()

# Check the data
head(covid_data)

# Plot for selected countries
library(ggplot2)
library(dplyr)


