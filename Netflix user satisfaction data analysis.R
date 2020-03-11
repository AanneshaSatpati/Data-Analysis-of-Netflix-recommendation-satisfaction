# Outstanding Group
# Project Analysis
netflix = read.csv(file.choose())
install.packages("summarytools")
library(summarytools)

# Descriptive statistics and plots after each variable
history = netflix$How.often.are.your.Netflix.recommendations.related.to.your.viewing.history.
levels(history)
numhistory = as.numeric(history)
cor.test(numyears,numhistory)

years = netflix$For.how.many.years.have.you.been.using.your.current.Netflix.profile.
levels(years)
numyears = as.numeric(years)

plot(years, col = "red", main = "How Many Years Respondents Have \n Used Their Current Netflix Profile",
     ylab = "Frequency", xlab = "Number of Years",
     cex.main=1.25, cex.lab=1.5)
descr(numyears)

summary(lm(numyears ~ numhistory))

summary(netflix$How.often.are.your.Netflix.recommendations.related.to.your.viewing.history.)
freq(history)
descr(numhistory)
levels(history)
plot(history, col = "violetred", main = "How Often Respondents Found Recommendations \n Related to Viewing History")

history = factor(history,levels(history)[c(4,2,5,7,6,1,3)])
levels(history)
median(numhistory)

profile = netflix$Do.you.have.a.Netflix.account.or.profile.
levels(profile)
freq(profile)
plot(profile, col = "violetred", main = "Plot for Responses about Owning \n Netflix Account or Profile")
descr(as.numeric(profile))

descr(netflix$Age)
freq(netflix$Gender)

levels(netflix$Which.degree.are.you.pursuing.right.now.)
levels(degree)
degree = netflix$Which.degree.are.you.pursuing.right.now.
degree = factor(degree,levels(degree)[c(3,2,1)])
freq(netflix$Which.degree.are.you.pursuing.right.now.)
plot(degree, col = "violetred", main = "Which Degree Programs Respondents Are In")
descr(as.numeric(degree))

levels(netflix$How.many.hours.do.you.spend.on.Netflix.everyday....in.hours.)
plot(netflix$How.many.hours.do.you.spend.on.Netflix.everyday....in.hours., col = "red",
     main = "How Many Hours Respondents Spend \n on Netflix Per Day",
     ylab = "Frequency", xlab = "Number of Hours",
     cex.main=1.25, cex.lab=1.5)
descr(as.numeric(netflix$How.many.hours.do.you.spend.on.Netflix.everyday....in.hours.))

levels(netflix$How.many.hours.have.you.spent.on.Netflix.in.the.past.week...in.hours.)
hoursweek = netflix$How.many.hours.have.you.spent.on.Netflix.in.the.past.week...in.hours.
hoursweek = factor(hoursweek,levels(hoursweek)[c(1:2,6,3:5)])
levels(hoursweek)
plot(hoursweek, col = "red", 
     main = "How Many Hours Respondents Spent \n on Netflix in a Week",
     ylab = "Frequency", xlab = "Number of Hours",
     cex.main=1.25, cex.lab=1.5)
descr(as.numeric(hoursweek))

levels(netflix$Do.your.Netflix.recommendations.reflect.the.diversity.of.your.viewing.interests.)
reflect = netflix$Do.your.Netflix.recommendations.reflect.the.diversity.of.your.viewing.interests.
reflect = factor(reflect,levels(reflect)[c(4,2,5,7,6,1,3)])
levels(reflect)
plot(reflect, col = "red", main = "Whether Netflix Recommendations Reflect the \n Viewing Interests of Respondents", 
     xlab = "Respondents' Answers", ylab = "Frequency", cex.main=1.25, cex.lab=1.5)
descr(as.numeric(reflect))

levels(netflix$How.often.are.you.interested.in.your.Netflix.recommendations.)
interest = netflix$How.often.are.you.interested.in.your.Netflix.recommendations.
interest = factor(interest,levels(interest)[c(4,2,5,7,6,1,3)])
levels(interest)
plot(interest, col = "violetred", main = "How Often Respondents are Interested \n in Their Netflix Recommendations")
descr(as.numeric(interest))

# linear regression for years on profile
cor.test(numyears, as.numeric(reflect))
summary(lm(as.numeric(reflect) ~ numyears))

# Effect size for previous model
(0.04377)/(1-0.04377)

# Power for previous model
library(pwr)
pwr.f2.test(u = 1, v = 100, f2 = 0.04, sig.level = 0.05, power = NULL)

# linear regression for hours per day
hoursday = as.numeric(netflix$How.many.hours.do.you.spend.on.Netflix.everyday....in.hours.)

cor.test(as.numeric(netflix$How.many.hours.do.you.spend.on.Netflix.everyday....in.hours.), as.numeric(reflect))
summary(lm(as.numeric(reflect) ~ hoursday))

# linear regression for hours per week
hoursweek = as.numeric(netflix$How.many.hours.have.you.spent.on.Netflix.in.the.past.week...in.hours.)
summary(lm(as.numeric(reflect) ~ hoursweek))        

# Multiple linear regression
model1 = lm(as.numeric(reflect) ~ hoursweek + hoursday + numyears)
summary(model1)

# Effect size for model1
(0.04377)/(1-0.04377)

# Power for model1
pwr.f2.test(u = 3, v = 100, f2 = 0.04, sig.level = 0.05, power = NULL)