# set the working directory
setwd('~./Project-Assignment')

# load the packages and the data
library(tidyverse)# for data manipulation and visualization
library(readxl) # for reading excel files
# set the column names
column_names <- c('Location', 'Price', 'Crime_Rating')
house_data <- readxl::read_xlsx('./File for Midterm.xlsx',
                                skip = 1,
                                col_names = column_names)

# clean the data
# get the outliers using boxplot.stat()
outliers <- boxplot.stats(house_data$Price)$out

# create a clean data set
clean_house_data <- house_data %>%
  mutate(# factor Location and Crime_Rating
    Location = factor(Location),
    Crime_Rating = factor(Crime_Rating)) %>%
  # remove missing values
  remove_missing() %>%
  # select data not in outliers
  filter(!Price %in% outliers)

# a barplot for the locations
clean_house_data %>%
  ggplot(aes(x = Location, y = Price, fill = Location)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = 'A bar plot showing different Locations and \nwhether its cheaper or expensive to live in those area',
       x = 'Different locations', y = 'House Prices')

# a box plot for different crimes rate
clean_house_data %>%
  ggplot(aes(x = Location, fill = Crime_Rating)) +
  geom_boxplot() +
  labs(title = 'A Box plot showing how crime rates are distributed in the 2 locations',
       x = 'Locations', y = 'House Price')

# a density plot for relationship between crime rate and house price
clean_house_data %>%
  ggplot(aes(x = Price, fill = Crime_Rating)) +
  geom_density(alpha = .5) +
  labs(title = 'A density plot showing the relationship between house price and crime rate',
       x = 'House Price')

# lowest amount she will have if she move in FL
low_amt <- 175000
# highest amount she will have when she moves to NY
high_amt <- 220000
# create a factor variable called 'able to pay' with "yes" and "no" levels
clean_house_data$Able_To_Pay <-
  ifelse(test = clean_house_data$Price <= high_amt,
         yes = 'Yes',
         no = 'No')
# create a pie chart to show which are has high chance of being able to pay
clean_house_data %>%
  aggregate(Price ~ Able_To_Pay + Location, sum) %>%
  # make the piechart
  ggplot(aes(
    x = '',
    y = Price,
    fill = interaction(Able_To_Pay, Location),
    label = interaction(Able_To_Pay, Location)
  )) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  scale_fill_manual(values = c('red', 'blue', 'green', 'yellow')) +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(fill = 'Able to Pay off house in different locations', x = NULL, y = NULL)
