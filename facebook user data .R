#The dataset is called u.d (user data) It consists of mock facebook data about users
#what country they're from, gender, age as well as last log in and other variables 
#we will use this data set to predict the probability that an individual logs into facebook within a week
#It contains the following variables:

#users: Unique user ID
#country: Country code where user is located
#first_login: Date of user sign-up
#last_login: Date when user last visited Facebook
#days_since_login: Number of days since last login
#l7: 1 if user logged in during last week, 0 if not
#l28: 1 if user logged in during last 28 days, 0 if not
#age: Age bucket user falls into
#gender: User specified gender 

# we will estimate a linear probability model relating l7:1 to country, age and gender

u.d<- read.csv("user_data.csv")

mod1 <- lm(l7 ~ age + gender + country, data = u.d)
summary(mod1)

#the regression indicates 3 significant coeffs 

#The coefficient on genderOther suggests that females 
#are 0.11 more likely to visit facebook within a week 
#The coefficient Pakistan suggests that people from this country have a probability 0f
#0.18 likelihood to visit facebook within a week compared to people from Turkey who are 0.18 
#least likely to visit facebook within a week 

#all other variables had a p value of greater than 0.05 

#we will use the model to predict the probability of someone logging into facebook
#for a hypothetically least likely person
#person: a women aged 55+ and from Pakistan 

pred.prob = mod1$coef[1] + mod1$coef[6]*55 + mod1$coef[10] + mod1$coef[8]
pred.prob

#The model predicts that a women from Pakistan over the age of 55 has a probability of 0.98
#likelihood of logging into facebook within a week 

#we will produce a plot of the predicted probabilities (on the y axis) against age (on the x axis).

plot(u.d$age,fitted.values(mod1),
     ylim=c(-0.2,1.2),
     ylab="Predicted Probability",
     xlab="Age",
     las=1)
abline(h=0,lty=3)
abline(h=1,lty=3)

#this LPM predicts some probabilities that are substantially above 1
#for women of all ages. There are a few in number of such predictions, 
#suggesting that the model does an average job at predicting the 
#probability of logging into facebook within a week 

#we will estimate a logistic regression to predict log in within a week, 
#using the same set of independent variables

mod2 <- glm(l7 ~ age + gender + country,
            family=binomial(link="logit"), data = u.d)
summary(mod2)

summary(predict(mod2,type="link"))
summary(predict(mod2,type="response"))

#The max and min values in terms of logit units are (0.18, 18.67 and in terms of 
#probabilities they are (0.54, 1.00).  
#the model no longer predicts probabilities below 0 or above 1

#Use your model from before to find the predicted value - in terms of logit units - 
#for the same least likely person 

pred.logits <- mod2$coef[1] + mod2$coef[6]*55 + mod2$coef[10] +
  mod2$coef[8]
pred.logits

#The predicted value is 17.7 not very meaningful we need to transform it back to a probability

library(boot)
inv.logit(pred.logits)

#The predicted probability for this mock fb data is 1. This us very high and not very common.
#However, what we are looking at is login within 7 days which is very common nowadays even in developed regions
#also since it is mock fb data, the data set will reflect what facebook wants to portray 

