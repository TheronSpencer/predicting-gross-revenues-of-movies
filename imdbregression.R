library(MASS)
library(car)
library(leaps)
library(dummies)
library(plot3D)

data <- read.csv("500movies.csv")
data <- data.frame(data)
attach(data)
data

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
MPAAPG=MPAA.dummies[,"content_ratingPG"]
MPAAPG13=MPAA.dummies[,"content_ratingPG-13"]
MPAAR=MPAA.dummies[,"content_ratingR"]

imdb = lm(gross~length+castpop+rating+budget+votes+MPAAPG+MPAAPG13+MPAAR)
summary(imdb)
imdbdf <- data.frame(length,a1pop,castpop,rating,budget,votes,MPAAPG,MPAAPG13,MPAAR)
vif()

imdb = lm(gross^(1/3)~length+castpop+rating+budget+votes+MPAAPG+MPAAPG13+MPAAR+budget*MPAAPG+budget*MPAAPG13+budget*MPAAR)
summary(imdb)

pairs(gross^(1/3)~length+a1pop+castpop+rating+budget+votes)

cor(imdbdf)

plot(gross^(1/3)~rating, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross^(1/3)~sqrt(votes), main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")

abline(h=0)
plot(gross^(1/3)~castpop, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross^(1/3)~I(budget^(1/2)), main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross^(1/3)~length, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross^(1/3)~MPAAPG, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)

resid=imdb$residuals
y.hat = predict(imdb)

resid=imdb.new$residuals
y.hat = predict(imdb.new)

plot(resid~y.hat, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(resid~budget, main="Plot of Residuals vs. budget", xlab="X1", ylab="Residuals")
abline(h=0)
plot(resid~a1pop, main="Plot of Residuals vs. gross", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~length, main="Plot of Residuals vs. length", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~castpop, main="Plot of Residuals vs. noms", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~rating, main="Plot of Residuals vs. wins", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~votes, main="Plot of Residuals vs. nrUsers", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~MPAAPG, main="Plot of Residuals vs. count", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~MPAAPG13, main="Plot of Residuals vs. count", xlab="X2", ylab="Residuals")
abline(h=0)
plot(resid~MPAAR, main="Plot of Residuals vs. count", xlab="X2", ylab="Residuals")
abline(h=0)

hist(resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid, main="Normal Probability Plot of Residuals")
qqline(resid)
shapiro.test(resid)

e.star = studres(imdb.new)
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

influence.measures(imdb)
cook.d.imdb = influence.measures(imdb)$infmat[,"cook.d"]
plot(cook.d.imdb, main="Plot of Cook's Distance", ylab="Cook's Distance", xlab="Observation Number", xaxt = "n")
axis(1, at=1:500)

cutoff <- 0.5
plot(imdb, which=4, cook.levels=cutoff)
gross.new = gross[-454]
length.new = length[-454]
a1pop.new = a1pop[-454]
castpop.new = castpop[-454]
rating.new = rating[-454]
budget.new = budget[-454]
votes.new = votes[-454]
MPAAPG.new = MPAAPG[-454]
MPAAPG13.new = MPAAPG13[-454]
MPAAR.new = MPAAR[-454]

imdb.new = lm(gross.new~length.new+castpop.new+rating.new+budget.new+votes.new+MPAAPG.new+MPAAPG13.new+MPAAR.new)
summary(imdb.new)
imdb.new

influence.measures(imdb.new)
cook.d.imdb.new = influence.measures(imdb.new)$infmat[,"cook.d"]
plot(cook.d.imdb.new, main="Plot of Cook's Distance", ylab="Cook's Distance", xlab="Observation Number", xaxt = "n")
axis(1, at=1:500)

scatterplot.matrix(imdb.new)

plot(gross.new~rating.new, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross.new^(1/3)~I(votes.new^(1/3)), main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross.new~castpop.new, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross.new~budget.new,MPAAR.new, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)
plot(gross.new~length.new, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)

gross.new1 = gross[-319]
length.new1 = length[-319]
a1pop.new1 = a1pop[-319]
castpop.new1 = castpop[-319]
rating.new1 = rating[-319]
budget.new1 = budget[-319]
votes.new1 = votes[-319]
MPAAPG.new1 = MPAAPG[-319]
MPAAPG13.new1 = MPAAPG13[-319]
MPAAR.new1 = MPAAR[-319]

imdb.new1 = lm(gross.new1~length.new1+a1pop.new1+castpop.new1+rating.new1+budget.new1+votes.new1+MPAAPG.new1+MPAAPG13.new1+MPAAR.new1)
summary(imdb.new1)

model0=lm(gross~1, data=data) ### constant mean model
model3=lm(gross~length+castpop+rating+budget+votes+MPAAPG+MPAAPG13+MPAAR, data=data) ### full model
MSEk = anova(model3)["Residuals", "Mean Sq"]
step(model0, scope=list(lower=~1, upper=model3), direction="forward", scale=MSEk)
### Since y~x1+x2 is selected, define this model as a new "full" model
model21=lm(formula = gross^(1/3) ~ votes + MPAAR + castpop + MPAAPG13 + 
             a1pop, data = data)
step(model21, scope=list(lower=~1, upper=model21), direction="backward", scale=MSEk)

best.subset.cp=leaps(x=cbind(length,castpop,rating,budget,votes,MPAAPG,MPAAPG13,MPAAR), y=gross, method="Cp")
min.cp.value=min(best.subset.cp$Cp)

min.cp.location=which.min(best.subset.cp$Cp)

best.subset.cp$which[min.cp.location,]

imdbfinal = lm(gross.new^(1/3)~length.new+budget.new+votes.new+MPAAPG.new+MPAAPG13.new+MPAAR.new)
summary(imdbfinal)

best.subset.cp=leaps(x=cbind(length.new,castpop.new,rating.new,budget.new,votes.new,MPAAPG.new,MPAAPG13.new,MPAAR.new,budget.new*MPAAPG.new,budget.new*MPAAPG13.new,budget.new*MPAAR.new), y=gross.new^(1/3), method="Cp")

plot(gross.new^(1/3)~budget.new*MPAAPG.new, main="Plot of Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0)