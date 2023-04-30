
# load the packages and data
# package
library(tidyverse)
library(readxl)
library(ggmosaic)
library(treemapify)
# data
house_crime_data <- read_excel(
  './File for Midterm.xlsx',
  col_names = c('Location', "House_Price", 'Crime_Rate'),
  skip = 2
)

# cleaning of the data
# extract outliers
# Calculate the interquartile range (IQR)
Q1 <- quantile(house_crime_data$House_Price, 0.25)
Q3 <- quantile(house_crime_data$House_Price, 0.75)
IQR <- Q3 - Q1

# Define the outlier cutoff values
cutoff_low <- Q1 - 1.5 * IQR
cutoff_high <- Q3 + 1.5 * IQR

# Extract the outliers
outliers <-
  house_crime_data[house_crime_data$House_Price < cutoff_low |
                     house_crime_data$House_Price > cutoff_high, ]

# create a clean data set
clean_house_crime_data <- house_crime_data %>%
  mutate(# factor out Location and Crime_Rate
    Location = as.factor(Location),
    Crime_Rate = as.factor(Crime_Rate)) %>%
  filter(!House_Price %in% outliers$House_Price)

# a bar chart showing different locations and which is expensive
options(scipen = 1e6)
bplot <- clean_house_crime_data %>%
  ggplot(aes(x = Location, y = House_Price, fill = Location)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_fill_manual(values = c("red", 'green')) +
  labs(title = 'A Bar plot showing the which area is more expensive than the other',
       x = "Different Locations",
       y = 'House Prices')
plotly::ggplotly(bplot)

# A mosaic plot for crime rates on different location
clean_house_crime_data %>%
  ggplot() +
  geom_mosaic(aes(x = product(Location, Crime_Rate), fill = Crime_Rate)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = 'A mosaic plot showing the different crime rate on different locations',
       x = 'Crime Rates', y = 'Locations')

# a scatter plot for the crime rate and house prices
clean_house_crime_data %>% 
  ggplot(aes(x=House_Price, y = Crime_Rate, color = Location))+
  geom_point(size = 5)+
  labs(title = 'a scatterplot for the crime rates and house prices')

# maximum amount she will have at the end
max_amt <- 220000
# create new column
clean_house_crime_data <- clean_house_crime_data %>%
  mutate(
    # will she be able to pay off house
    Pay_Off_House = ifelse(
      test = House_Price <= max_amt,
      yes = "Able to Pay Off House",
      no = 'Not Able to Pay Off House'
    )
  )

# a treemap showing where she will be able to pay off house
treemap <- clean_house_crime_data %>%
  ggplot(aes(
    area = House_Price,
    fill = Pay_Off_House,
    label = Location,
    subgroup = Location
  )) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "bold",
    color = "white",
    place = "centre",
    reflow = TRUE
  ) +
  labs(title = 'A tree map used to display proportion of locations she can be able to \npay of house') +
  theme_void() +
  theme(legend.position = "bottom")

# Display the treemap
treemap
