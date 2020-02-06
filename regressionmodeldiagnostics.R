library(MASS)
library(car)
library(leaps)
library(dummies)
library(plot3D)

data <- read.csv("500movies.csv")
data <- data.frame(data)
attach(data)

length = data$duration
a1pop = data$actor_1_facebook_likes
castpop = data$cast_total_facebook_likes
rating = data$imdb_score
budget = data$budget
gross = data$gross
votes = data$num_voted_users

#create MPAA dummy variables
MPAA.dummies=dummy(content_rating)
MPAA.dummies
MPAAG=MPAA.dummies[,"content_ratingG"]
MPAAPG=MPAA.dummies[,"content_ratingPG"]
MPAAPG13=MPAA.dummies[,"content_ratingPG-13"]
MPAAR=MPAA.dummies[,"content_ratingR"]

cutoff <- 0.5
plot(imdbfinal, which=4, cook.levels=cutoff)
rating.new = rating[-454]
gross.new = gross[-454]
castpop.new = castpop[-454]
budget.new = budget[-454]
votes.new = votes[-454]
MPAAG.new = MPAAG[-454]
MPAAPG.new = MPAAPG[-454]
MPAAPG13.new = MPAAPG13[-454]
MPAAR.new = MPAAR[-454]

imdbfinal = lm(gross.new^(1/3)~budget.new+votes.new+MPAAG.new+MPAAPG13.new+MPAAPG.new)
imdbfinal1 = lm(gross.new^(1/3)~budget.new+votes.new+MPAAG.new+MPAAPG13.new+MPAAPG.new)
anova(imdbfinal1,imdbfinal)
summary(imdbfinal)
anova(imdbfinal)

plot(gross~votes, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross~castpop, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross~budget, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)


resid=imdbfinal.new$residuals
y.hat = predict(imdbfinal.new)


plot(resid~y.hat, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(resid~budget, main="Plot of Residuals vs. budget", xlab="X1", ylab="Residuals")
abline(h=0)
plot(resid~castpop, main="Plot of Residuals vs. noms", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~votes, main="Plot of Residuals vs. nrUsers", xlab="X2", ylab="Residuals")
abline(h=0)


hist(resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid, main="Normal Probability Plot of Residuals")
qqline(resid)
shapiro.test(resid)

e.star = studres(imdbfinal.new)
plot(e.star~y.hat, ylim=c(-5,5), ylab="Studentized Residuals", 
     xlab="Fitted Values", main="Plot of Studentized Residuals vs. Fitted Values")

abline(h=2, col="blue", lty=2)
abline(h=-2, col="blue", lty=2)
abline(h=0)
plot(e.star, ylim=c(-3,3), ylab="Studentized Residuals", 
     xlab="Time Order", main="Plot of Studentized Residuals vs. Time Order")
abline(h=2, col="blue", lty=2)
abline(h=-2, col="blue", lty=2)
abline(h=0)

h.model2 = influence.measures(imdbfinal.new)$infmat[,"hat"]
plot(h.model2, main="Plot of h_i", ylab="Leverage", xlab="Observation Number", xaxt = "n")
axis(1, at=1:499)

cutoff <- 0.5
plot(imdbfinal, which=4, cook.levels=cutoff)
gross.new = gross[-454]
castpop.new = castpop[-454]
budget.new = budget[-454]
votes.new = votes[-454]
MPAAPG.new = MPAAPG[-454]
MPAAPG13.new = MPAAPG13[-454]
MPAAR.new = MPAAR[-454]

imdbfinal.new = lm(gross.new~sqrt(votes.new)+MPAAPG.new+MPAAPG13.new+sqrt(budget.new)+MPAAG.new)
model1 = lm(gross.new^(1/3)~votes.new+MPAAG.new+MPAAPG13.new+MPAAPG.new+budget.new+castpop.new)
model2 = lm(gross.new^(1/3)~votes.new+MPAAG.new+MPAAPG13.new+MPAAPG.new+budget.new)

anova(model2,model1)
fstat = qf(0.05, 1, 493)
fstat

summary(imdbfinal.new)
anova(imdbfinal.new)

newdata = data.frame(votes.new = 320181, MPAAG.new = 0, MPAAPG13.new = 1, MPAAPG.new = 0, budget.new = 200000000)
pred.conf = predict(imdbfinal, newdata, interval="confidence", level=0.95)
pred.conf

