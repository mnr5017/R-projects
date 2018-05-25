info.speeddating <- read.csv("/Users/markryan/Desktop/BU/CS544/CS544_FinalProject_Ryan/Speed Dating Data.csv")
names(info.speeddating)

set.seed(150)

#### Demographics - Categorical and Numerical Data ####
#######################################################

speeddating.age <- na.omit(info.speeddating$age)
mean(speeddating.age)
median(speeddating.age)
range(speeddating.age)
diff(range(speeddating.age))
fivenum(speeddating.age)
summary(speeddating.age)

table(info.speeddating$age)
info.age.table <- (table(info.speeddating$age))
barplot(info.age.table, main = "Ages of Speed-Daters", 
        col = "dodgerblue", ylim = c(0, 1200))

boxplot(speeddating.age, horizontal = TRUE, 
        main = "Speed Dating Demographics by Age", col = "dodgerblue")

info.speeddating.gender <- table(info.speeddating$gender)
row.names(info.speeddating.gender) <- c("female", "male")
info.speeddating.gender 

info.speeddating.race <- table(info.speeddating$race)
row.names(info.speeddating.race) <- c("African", "European",
              "Latino","Asian","Other")
info.speeddating.race
barplot(info.speeddating.race, main = "Speed Dating Demographics by Race",
        col = rainbow(6), ylim = c(0, 5000))

info.speeddating.raceandgender <- table(info.speeddating$gender, info.speeddating$race)
info.speeddating.raceandgender
row.names(info.speeddating.raceandgender) <- c("Female", "Male")
colnames(info.speeddating.raceandgender) <- c("African", "European", "Latino", "Asian", "Other")
info.speeddating.raceandgender
mosaicplot(info.speeddating.raceandgender, 
           main = "Speed Dating by Race and Gender", 
           color = rainbow(5))

#### Random Sampling and Central Limit Theorem ####
###################################################

hist(speeddating.age, ylim = c(0, 2000), main = "Speed Dating Histogram by Age",
     col = "dodgerblue")

spd.samples <- 1000
spd.sample.size <- 5
spd.xbar <- numeric(spd.samples)
for(i in 1:spd.samples) {
  spd.xbar[i] <- mean(rnorm(spd.sample.size, mean = mean(speeddating.age), 
                            sd = sd(speeddating.age)))
}
hist(spd.xbar, prob = TRUE, col = "dodgerblue", 
     main = "Histogram - 5 Samples", ylim = c(0, 0.50))

spd.sample.size <- 20
spd.xbar.20 <- numeric(spd.samples)
for(i in 1:spd.samples) {
  spd.xbar.20[i] <- mean(rnorm(spd.sample.size, mean = mean(speeddating.age), 
                            sd = sd(speeddating.age)))
}
hist(spd.xbar.20, prob = TRUE, col = "dodgerblue", 
     main = "Histogram - 20 Samples", ylim = c(0, 0.50))

#### Simple Random Sampling ####
library(sampling)

nrow(info.speeddating)
table(info.speeddating$age)

spd.srs <- srswr(170, nrow(info.speeddating))
spd.srs[spd.srs != 0]

spd.rows <- (1:nrow(info.speeddating))[spd.srs != 0]
spd.rows <- rep(spd.rows, spd.srs[spd.srs != 0])
spd.rows

spd.sample1 <- info.speeddating[spd.rows, ]
head(spd.sample1[c(1, 34)])

table(spd.sample1$age)

#### Systematic Sampling ####
spd.N <- nrow(info.speeddating)
spd.n <- 170

spd.k <- ceiling(round(spd.N/spd.n))
spd.k

spd.r <- sample(spd.k, 1)
spd.r

spd.s <- seq(spd.r, by = spd.k, length = spd.n)

spd.sample3 <- info.speeddating[spd.s, ]
head(spd.sample1[c(1, 34)])

table(spd.sample3$age)

#### Confidence Intervals ####
##############################

#### 80% & 90% Confidence Intervals ####

spd.conf <- c(80,90)
spd.conf

spd.alpha <- 1 - spd.conf/100
spd.alpha

qnorm(spd.alpha/2)

qnorm(1 - spd.alpha/2)

for (i in spd.alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
                 100*(1-i), i, 
                 qnorm(i/2),
                 qnorm(1-i/2))
  cat(str,"\n")
}

spd.pop <- mean(speeddating.age)
spd.pop
spd.sd <- sd(speeddating.age)
spd.sd

spd.x1 <- rnorm(10000, mean = spd.pop, sd = spd.sd)
spd.x1 <- as.integer(spd.x1)

spd.ci.size <- 30 

spd.sd.means <- spd.pop/sqrt(spd.ci.size)
spd.sd.means

spd.sample.data <- sample(spd.x1, size = spd.ci.size)
spd.sample.data

spd.ci.xbar <- mean(spd.sample.data)
spd.ci.xbar

spd.samples2 <- 20
spd.ci.xbar2 <- numeric(spd.samples2)

for (i in spd.alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 spd.ci.xbar - qnorm(1-i/2) * spd.sd.means,
                 spd.ci.xbar + qnorm(1-i/2) * spd.sd.means)
  cat(str,"\n")
}









