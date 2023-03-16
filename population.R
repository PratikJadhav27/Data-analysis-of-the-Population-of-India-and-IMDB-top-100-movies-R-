library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


library(readr)
indian_population_new <- read_csv("indian population new.csv")


# Replace % symbol with "percentage" in column headings
names(indian_population_new) <- gsub("%", "percentage", names(indian_population_new))

#Change Uppercase to lowercase and add "_" in spaces between words
names(indian_population_new) <- tolower(names(indian_population_new))
names(indian_population_new) <- gsub(" ", "_", names(indian_population_new))

# Replace % symbol in multiple columns
indian_population_new$percentage_increase_in_population <- gsub("%", "", indian_population_new$percentage_increase_in_population)
indian_population_new$percentage_increase_in_population_density <- gsub("%", "", indian_population_new$percentage_increase_in_population_density)
indian_population_new$percentage_increase_in_urban_population <- gsub("%", "", indian_population_new$percentage_increase_in_urban_population)
indian_population_new$percentage_increase_in_rural_population <- gsub("%", "", indian_population_new$percentage_increase_in_rural_population)
indian_population_new$percentage_increase_in_life_expectancy <- gsub("%", "", indian_population_new$percentage_increase_in_life_expectancy)
indian_population_new$percentage_change_in_birth_rate <- gsub("%", "", indian_population_new$percentage_change_in_birth_rate)
indian_population_new$percentage_change_in_death_rate <- gsub("%", "", indian_population_new$percentage_change_in_death_rate)

indian_population_new$percentage_change_in_infant_mortality_rate <- gsub("%", "", indian_population_new$percentage_change_in_infant_mortality_rate)
indian_population_new$percentage_change_in_fertility_rate <- gsub("%", "", indian_population_new$percentage_change_in_fertility_rate)
indian_population_new$percentage_change_in_net_migration_rate <- gsub("%", "", indian_population_new$percentage_change_in_net_migration_rate)


view(indian_population_new)
names(indian_population_new)

#Numeric Summary
summary(indian_population_new)
table(indian_population_new$population_density)    

table(indian_population_new$population_density)
?dim

head(indian_population_new)
tail(indian_population_new)
str(indian_population_new)

median(indian_population_new$population_density)
mean(indian_population_new$population_density)
min(indian_population_new$population_density)
range(indian_population_new$population_density)
var(indian_population_new$population_density)

quantile(indian_population_new$population_density,0.5)
quantile(indian_population_new$population_density,0.25)
quantile(indian_population_new$population_density,0.75)

quantile(indian_population_new$population_density,0.5)
quantile(indian_population_new$population_density,0.25)
quantile(indian_population_new$population_density,0.75)

IQR(indian_population_new$population_density)

1:20
seq(1,20,2)

quantile(indian_population_new$population_density,0.25)
seq(0,1,0.25)

quantile(indian_population_new$population_density, seq(0,1,0.25))
quantile(indian_population_new$population_density, seq(0,1,0.05))
quantile(indian_population_new$population_density, seq(0,1,0.25),1)
round(quantile(indian_population_new$population_density, seq(0,1,0.25)),1)
round(quantile(indian_population_new$population_density, seq(0,1,0.05)),1)


##DATA  VISUALIZATION FOR ONE VARIABLE
##Plots

#Histogram of population density

hist(indian_population_new$population_density,
     breaks = 20,
     main = "Histogram of Population Density",
     xlab = "Population Density",
     col = "steelblue")

hist(indian_population_new$life_expectancy,
     breaks = 20,
     main = "Histogram of Life Expectancy",
     xlab = "Life Expectancy",
     col = "darkorchid")


# DATA VISUALIZATION FOR TWO VARIABLES



#2 variables 
ggplot(indian_population_new, aes(x = population, y = life_expectancy)) +
  geom_point() +
  labs(x = "Population", y = "Life Expectancy") +
  ggtitle("Population vs. Life Expectancy Scatter Plot")


#Two variables
# create the line plot
ggplot(indian_population_new, aes(x = year, y = life_expectancy)) +
  geom_line() +
  xlab("Year") +
  ylab("Life Expectancy") +
  ggtitle("Trend of Life Expectancy Over Time")

#2 variables 
# Create a bar plot of the population density by region
ggplot(indian_population_new, aes(x = factor(year), y = population_density)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Population Density by Year in India", x = "Year", y = "Population Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#2 variables 
# Create a new data frame with only the data 
indian_population <- subset(indian_population_new)

# Create a density plot of birth rate and death rate
ggplot(indian_population, aes(x = birth_rate, color = "Birth Rate")) +
  geom_density() +
  geom_density(aes(x = death_rate, color = "Death Rate")) +
  labs(title = "Distribution of Birth Rate and Death Rate in India",
       x = "Rate per 1000 people",
       y = "Density") +
  scale_color_manual(name = "Rate Type",
                     values = c("Birth Rate" = "blue", "Death Rate" = "red"))


# DATA VISUALIZATION FOR THREE VARIABLE
#3 variables 

# Create a heatmap of net migration rates vs percentage increase in population density
ggplot(data = indian_population_new, aes(x = net_migration_rate, y = percentage_increase_in_population_density)) + 
  geom_tile(aes(fill = population/10^5)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  labs(title = "Net Migration Rates vs Percentage Increase in Population Density", 
       x = "Net Migration Rate", y = "Percentage Increase in Population Density")

# 3 variables 
# Create example data frame
country <- c("India", "USA", "China")
urban_pop <- c(50000000, 300000000, 800000000)
rural_pop <- c(800000000, 60000000, 700000000)
population_data <- data.frame(country, urban_pop, rural_pop)

# Create stacked bar plot
barplot(height = t(as.matrix(population_data[,2:3])), 
        main = "Population by Urban and Rural areas", 
        xlab = "Country", 
        ylab = "Population",
        names.arg = population_data$country, 
        col = c("blue", "green"), 
        legend = c("Urban", "Rural"), 
        args.legend = list(x = "topright", bty = "n"))

# 4 VARIABLES
ggplot(indian_population, aes(x = year , y = population/10^5)) +
  geom_point(aes(colour = fertility_rate)) + facet_wrap(~infant_mortality_rate)+
  xlab("year") +
  ylab("population") +
  ggtitle("Distribution of population and fertility rate over years")

