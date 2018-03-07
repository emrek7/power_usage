

library(caTools)
library(ggplot2)
library(ggthemes)
library(tidyr)

# In the csv files there are 5 column of data: 
# 1 - Date that the counter data is taken, precise time is almost always at 09:00 am
# 2 - counter data, 
# 3/4 - high and low temperatures on the interval that starts from the time of getting the data previous day and 
# ends the moment the acquisition of the data on that day
# 5 - other transient major consumptions or non-compsumtions. Initially this data is entered because of the water 
# heater which spends 7kW energy per hour and is used approximately 1 hour while taking long baths

power <- data.frame(date = character(),counter=integer(),temp_high = integer(), temp_low = integer(),other = double())
for (month in 0:2){
	power <- rbind(power,read.csv(paste0('counter0',month,'.csv')))
}
num_days <- length(power$counter)

#The counter differences is calculated and first day data is excluded
difference <- power$counter[2:num_days] - power$counter[1:num_days-1]
power <- power[2:num_days,]
power$difference <- difference

# we subtract the "other" column from the difference since they make major changes in the counter difference. Positive
# values means we used too much power for something such as water heater which spends 7Kw per hour
# They have negative values when we are not home, therefore depending on the weather, we take a guess about how many 
# hours we would use the heater. -1 means about 2.3 hours a day, -2 is 4.6 etc. 
power$difference <- power$difference-power$other*7

# Now we have two features which consist of highest and lowest temperatures. They, as we can anticipate, affect the usage
# of the  heater. However, we know that there are daily power usages as well, e.g refrigerators, laptops, ovens etc.
# Although they are not constants we have a good feeling about the monthly average. According to our calculations we spend
# 4-5 kW of power. This, in principle, should be taken care in the bias of linear regression model. However the bias 
# should not necessarily correspond to the reality as much as we expect.


# Let's build our linear model. Though it is a good idea to split the data into train and cross-validate sets first. 
# Let's use 70-30 ratios. Create and the prediction column first.

power$predict <- power$difference

set.seed(51)
sample <- sample.split(power$difference,SplitRatio = 0.7)
power.train <- subset(power,sample == TRUE)
power.cross <- subset(power,sample == FALSE)

# First create the model from the trainin set
model.temp.hl <- lm(difference ~ temp_high + temp_low, data = power.train)

# In theory, as temperature decreases, the power consumption should increase, that's the initial intuition. 
# That should cause a positive bias and a negative coefficient.  

summary(model.temp.hl)
# R-squared is not ideal but i suppose it's not all bad. Intercept point is positive and coefficients are negative as expected. 
# Significant codes show that the most important features are, as anticipated, interceptions and highest temperature 
# but unusually lowest temperature. That may be related to how we use the heater. Generally highest temperatures are in the 
# day light, slightly lowers on the evenings but the lowest points are on the night in which we dont use the heater. 

# Plot the data. Highest/Lowest temperature vs counter difference
power.train$predict <- predict(model.temp.hl,power.train) 
plots <- list()
plots[[1]] <- ggplot(power.train, aes(x=temp_high)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict))
plots[[2]] <- ggplot(power.train, aes(x=temp_low)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict))

# As expected it's a decreasing plot and temp_low is less relevant than temp_high. Not understandable point is the increase 
# at higher highest-temps. Let's predict from the cross validation set 

power.cross$predict <- predict(model.temp.hl,power.cross)
plots[[3]] <- ggplot(power.cross,aes(x=temp_high)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict))
plots[[4]] <- ggplot(power.cross,aes(x=temp_low)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict))

# Now let's see how polynomial regression models behaves in these cases. Since the model looks not bad at all we do not need to 
# go further than 2nd order polynomial

polymodel.temp.hl <- lm(difference ~ poly(temp_high + temp_low,2), data = power.train)

power.train$predict <- predict(polymodel.temp.hl,power.train) 
plots[[5]] <- ggplot(power.train, aes(x=temp_high)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict)) + theme_bw()
plots[[6]] <- ggplot(power.train, aes(x=temp_low)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict)) + theme_bw()

power.cross$predict <- predict(polymodel.temp.hl,power.cross)
plots[[7]] <- ggplot(power.cross,aes(x=temp_high)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict)) + theme_bw()
plots[[8]] <- ggplot(power.cross,aes(x=temp_low)) + geom_point(aes(y=difference),color = 'red') + geom_line(aes(y=predict)) + theme_bw()


pdf("power_linear.pdf")
plots[1:4]
dev.off()
pdf("power_linear_poly.pdf")
plots[5:8]
dev.off()


# Now let's get the next month's weather estimations and have a guess about the electric bill this month

temp.next <- read.csv('temp.next3.2018.csv')

predict.next <- predict(model.temp.hl,temp.next)

predict.next.poly <- predict(polymodel.temp.hl,temp.next)

bill.next <- sum(predict.next)*0.4482 #In Turkish Lira 
bill.next.poly <- sum(predict.next.poly)*0.4482 #In Turkish Lira 
#untilnow <- (power$counter[num_days-1]-power$counter[power$date == '27-Feb-2018'])*0.4482

print(paste("Next month's bill is :",as.integer(bill.next), " according to one variable linear model"))
print(paste(as.integer(bill.next.poly),"according to two variable linear model"))
#print(paste('Bill until now is : ',as.integer(untilnow)))
