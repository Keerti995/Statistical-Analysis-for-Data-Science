# Mini Project 2----Question 2
acc <- read.csv("motorcycle.csv") #reads the motorcycle.csv file where each county and its fatal accidents of Year 2009 are listed
par(mfrow=c(1,1)) # one plot in 1 row
boxplot(acc$Fatal.Motorcycle.Accidents,xlab = "Number of Fatal Accidents",ylab = "Boxplot of Motorycle Accidents")
summary(acc$Fatal.Motorcycle.Accidents)

outlier_counties <- acc[which(acc$Fatal.Motorcycle.Accidents  > 45),names(acc)%in%c("County")]
outlier_counties

?barplot
race <- read.csv("roadrace.csv")
maine_frequency <- table(race$Maine)
barplot(maine_frequency,main="Location of the runners",ylim = c(0,6000),xlim=c(0,16),ylab = "Number of Runners",space=c(2,1.5))
maine_runners <- race[which(race$Maine=="Maine"), names(race) %in% c("Time..minutes.")]
away_runners <- race[which(race$Maine=="Away"), names(race) %in% c("Time..minutes.")]
par(mfrow=c(1,2))
hist(maine_runners, xlim = c(0,200), ylim = c(0,2000), ylab = " No. of Maine Runners", xlab = "Time in minutes", breaks = 10)
hist(away_runners, xlim = c(0,200), ylim = c(0,2000), ylab = " No. of Away Runners", xlab = "Time in minutes", breaks = 10)
mean(maine_runners)
median(maine_runners)
sd(maine_runners)
range(maine_runners)
IQR(maine_runners)
mean(away_runners)
median(away_runners)
sd(away_runners)
range(away_runners)
IQR(away_runners)
boxplot(race$Time..minutes. ~ race$Maine,xlab="Runner Type", ylab="Time Taken")
summary(maine_runners)
summary(away_runners)
boxplot(as.numeric(race$Age) ~ race$Sex, xlab = "Age",ylab = "Sex")
Male <- race[which(race$Sex=="M"),names(race)%in%c("Age")]
FeMale <- race[which(race$Sex=="F"),names(race)%in%c("Age")]
boxplot(as.numeric(Male), xlab = 'Male', ylab = 'Age', ylim = c (0,80));
boxplot(as.numeric(FeMale), xlab = 'FeMale', ylab = 'Age', ylim = c (0,80));
mean(as.numeric(Male))
median(as.numeric(Male))
sd(as.numeric(Male))
IQR(as.numeric(Male))
range(as.numeric(Male))
mean(as.numeric(FeMale))
median(as.numeric(FeMale))
sd(as.numeric(FeMale))
IQR(as.numeric(FeMale))
range(as.numeric(FeMale))
