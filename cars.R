library(tidyr)
library(dplyr)
library(readr)
library(aod)
library(ggplot2)
library(cowplot)
library(reshape2)
true_car_listings <- read_csv("true_car_listings.csv") #load into working directory
test <- true_car_listings%>%
  separate(Model, c("Model", "Transmission"), sep = "[0-9]-")%>%
  mutate(Transmission = replace(Transmission, Transmission == "Speed", "Manual"))%>%
  mutate(Transmission = replace(Transmission, is.na(Transmission), "Automatic")) %>%
  mutate(Model = strsplit(Model, "Automatic"))
cars_in_GA <- test[test$State == "GA", ]
makes <- levels(factor(cars_in_GA$Make))
make_count <- numeric(0)
avg_price <- numeric(0)
for (i in 1:length(makes)){
  make_count[i] <- sum(cars_in_GA$Make == makes[i])
  avg_price[i] <- mean(cars_in_GA$Price[cars_in_GA$Make == makes[i]])
}

## cars in GA with make, count, avg price
make_count_GA <- tibble(makes, make_count, avg_price)
names(make_count_GA) <- c("Make", "Number Sold", "Average Price")
scatter.smooth(x = make_count_GA$`Number Sold`, y = make_count_GA$`Average Price`)


#normality
library(e1071)
plot(density(make_count_GA$`Average Price`), main="Density Plot: Average Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")

manual_trans <- test %>%
  select(Price, Year, Mileage, City, State, Make, Model, Transmission) %>%
  filter(Transmission == "Manual")

auto_trans <- test %>%
  select(Price, Year, Mileage, City, State, Make, Model, Transmission)%>%
  filter(Transmission == "Automatic")



income <- read_csv("kaggle_income.csv")
income <- income %>%
  select(State_Name, Mean)

states <- levels(factor(income$State_Name))


states <- levels(factor(income$State_Name))
mean_income_state <- numeric(0)
for (i in 1:length(states)) {
  mean_income_state[i] <- mean(income$Mean[income$State_Name == states[i]])
}

state_mean_income <- tibble(states, mean_income_state)
names(state_mean_income) <- c("States", "Mean Income")
state_mean_income <- state_mean_income[state_mean_income$States != "Puerto Rico", ]


df <- data.frame(state_mean_income)
ggplot(df, aes(state_mean_income$States, state_mean_income$`Mean Income`)) + theme(axis.text.x = element_text(angle = 90)) + geom_jitter() + xlab("State Name") + ylab("Mean Income")

#select price and state
price_and_state <- test %>%
  select(Price, State) %>%
  mutate(State = toupper(State))%>%
  group_by(State) %>%
  mutate(Price = mean(Price)) %>%
  unique() 

#sort by state name
price_and_state <- price_and_state[order(price_and_state$State), ]




