# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#Now that our packages are loaded, let's read in and take a peek at the data.
train <- read.csv('D:/Data Science/titanic/train.csv', stringsAsFactors = F)
head(train)
test  <- read.csv('D:/Data Science/titanic/test.csv', stringsAsFactors = F)
head(test)

full  <- bind_rows(train, test) # bind training & test data
head(full)
str(full)

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))



# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()


# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(rf_model)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()




# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'D:/Data Science/titanic/rf_mod_Solution.csv', row.names = F)


library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

str(temp_carbon)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(xintercept = c(1850, 1970, 2014))

str(greenhouse_gases)
data("historic_co2")
str(historic_co2)

co2_time <- historic_co2 %>%
  ggplot(aes(year,co2, color=source)) +
  geom_line() +
  #scale_x_continuous(name="Speed of cars", limits=c(-800000, -775000))
 co2_time + xlim(-3000, 2018)

str(temp_carbon)

temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")+
  geom_line(aes(y = land_anomaly), color = "red", linetype = "dashed") +
  geom_line(aes(y = ocean_anomaly), color = "green") +
  geom_vline(xintercept = c(2018))

number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid",3))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
kings
mean(deck %in% kings)


install.packages("gtools")

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(4,)    # order does not matter

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)


# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)

install.packages('gtools')
#load library
library(gtools)
#urn with 3 balls
x <- c('red', 'blue', 'black')
#pick 2 balls from the urn with replacement
#get all permutations
permutations(n=3,r=2,v=x,repeats.allowed=T)

perm = function(n, x) {
  factorial(n) / factorial(n-x)
}

perm(8,3)
nrow(permutations(8,3))
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}


nrow(permutations(3,3))/nrow(permutations(8,3))

(3/8) * (2/7) * (1/6)

B <- 10000
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

results <- replicate (B, {
winner <- sample(runners, 3 )
winner
any(winner[1] == "Jamaica"& winner[2] == "Jamaica" & winner[3] == "Jamaica")
}
)
mean(results)

combinations(3,2)

set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

help(sample)

meal <- expand.grid(entree = c(1,2,3,4,5,6), sides1 =c(1,2,3,4,5,6),sides2 =c(1,2,3,4,5,6), drink = c(1,2))

nrow(meal)

6* nrow(combinations(6,3))*3

entree_n <- function(n) {
  comb <- 6 * nrow(combinations(n,2)) * 3
  comb
}
 

x <- 2:12

sapply(x, entree_n)
  
data.frame( side = 2:12, combo = sapply(x, entree_n)) %>% 
  filter(combo > 365) %>% min(.$side)


data(esoph)
library(tidyverse)
head(esoph)
levels(esoph$agegp)
levels(esoph$alcgp)
str(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
all_controls
str(esoph)
max(levels(esoph$alcgp))

d <- nrow(esoph %>% filter(alcgp == '120+'))
n <- nrow(esoph %>% filter(alcgp == '120+') %>% filter(ncases>0))

caseesg <- esoph %>% filter(alcgp == '120+')
a1 <- sum(caseesg$ncases)
a2 <- sum(caseesg$ncontrols)

a1 /(a1 + a2)

esoph %>%
  filter(alcgp =='120+' ) %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  mutate(p_control = ncontrols / (ncases + ncontrols))
  pull(p_case)

  levels(esoph$tobgp)
  
  esoph %>%
    filter((tobgp %in% c("30+") | alcgp =='120+') ) %>%
    summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
    mutate(p_case = ncases / (ncases + ncontrols)) %>%
    mutate(p_control = ncontrols / (ncases + ncontrols))
  
  esoph %>%
    
    summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
    mutate(p_case = ncases / (ncases + ncontrols)) %>%
    mutate(p_control = ncontrols / (ncases + ncontrols))
  
  sum(esoph$ncases)
  122/200
  
  13/975
  
  66/200
  
  67/975

  66/(200 *(136/975))
  
  p_case_high_alc/p_control_high_alc
  
  high_alc_cases <- esoph %>%
    filter(alcgp == "120+") %>%
    pull(ncases) %>%
    sum()
  
  p_case_high_alc <- high_alc_cases/all_cases
  p_case_high_alc
  
  high_alc_controls <- esoph %>%
    filter(alcgp == "120+") %>%
    pull(ncontrols) %>%
    sum()
  
  p_control_high_alc <- high_alc_controls/all_controls
  p_control_high_alc
  
  p_case_high_alc/p_control_high_alc
  
  p <- seq(0.01, 0.99, 0.01)
  
  qnorm(.95, 20.9, 5.7)
  
  p
  
  cdf <- sapply(1:36, function (x){
    mean(act_scores <= x)
  })
  min(which(cdf >= .95))
  p[1]
  
  library(tidyverse)
  
  set.seed(16, sample.kind = "Rounding")
  act_scores <- rnorm(10000, 20.9, 5.7)
  mean(act_scores)
  
  p <- seq(0.01, 0.99, 0.01)
  sample_quantiles <- quantile(act_scores, p)
  names(sample_quantiles[max(which(sample_quantiles < 26))])
  qnorm(p,20.9,5.7)
  
qqplot(qnorm(p,20.9,5.7),sample_quantiles)
  x <- seq(1:99)
d <- data.frame(quant = x, value=x)
p <- seq(0.01, 0.99, 0.01)
  for (val in x)
  {
    val
    p[val]
  chk <- qnorm(p[val],20.9, 5.7)
d[val,2] <- chk
      
  }
d
     
e <- 1/5
e

x <- sample(c(-0.25,1), 1, replace = TRUE, prob = c(1/5, 4/5))
mean(x)

av <- 44*((1/4*1) +(4/5*-0.25))

mean(av>8)




sec <- sqrt(44) * abs(1) * sqrt(1/4*3/4)

1 - pnorm(8, av, sec)

n <- 44
B <- 10000
set.seed(21)
sam <- replicate(B,{
  x <- sample(c(1,-0.25), n, replace = TRUE, prob = c(1/5, 4/5)) 
  sum(x)
})

mean(sam>8)

# 44*(1/4)
# 2.872281
# 11
1- pnorm(30, 11, 2.87)

#3 - continuous probablity
p <- 5/38
a <- 6
b<- -1
5/38*6 +(-1* 33/38)
n<- 500
avg <- n *((a*p) + (b*(1-p)))
avg_ind <-avg/n
avg_ind1<-  ((a*p) + (b*(1-p)))
avg_ind
avg_ind1
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se
pnorm(0,avg,se)

B <- 10000
chk <- replicate(B, {
  simulated_data<-  sample(c(6,-1), n, replace = TRUE, prob = c(p, 1-p))    # generate 800 normally distributed random heights
  mean(simulated_data)    # determine the tallest height 
})

mean(chk)
sd(chk)*sqrt(n)

library(dslabs)
data(death_prob)
library(dplyr)

str(death_prob)
nlevels(death_prob$age)
prob <- death_prob %>% filter(sex == 'Female' & age == 50) %>%
pull(prob)
prob
death <- -150000
nodeath <- 1150
n <-1000
avs <- n* ((death * prob) + (nodeath * (1-prob)))

svs <- abs(death - nodeath) * sqrt(n *prob * (1-prob))

1-pnorm(0,avs,svs)

prob <- death_prob %>% filter(sex == 'Male' & age == 50) %>%
  pull(prob)
prob

Es <- 700000
Es <- n * ((death * prob) +(t * (1-prob)))
Pr_male <- (Es/n - (death * prob))/ (1-prob)

sd_male <- abs(death - Pr_male) *sqrt(n * prob * (1-prob))
sd_male
pnorm(0,Es,sd_male)

#p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

# exp_val <- n*(a*p + b*(1-p))
# exp_val
# se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
# se
# pnorm(0, exp_val, se)
p <- seq(.01, .03, .0025)
exp_val <- sapply(p, function(x){
  mu <- n * (a*x + (b*(1-x)))
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  pnorm(-1*10^6, mu, sigma)
})

exp_val
min(p[which(exp_val > 0.9)])

set.seed(25)
B <- 10000
n <- 1000
p_loss <- 0.015
loss <- -150000 
profit<-1150

x <- sample(c(loss,profit), n, replace = TRUE, prob = c(p_loss, 1-p_loss))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

set.seed(27)
chk <- replicate(B, {
  simulated_data<-  sample(c(loss,profit), n, replace = TRUE, prob = c(p_loss, 1-p_loss))    
  sum(simulated_data)
})

mean(chk < -1*10^6)


#------------------Page 3---------------------------

n <- 1000
p <- 0.015
l <- -150000
premium <- 1150
z <- qnorm(0.05)
  #Pr <- (Es/n - (l * p))/ (1-p)

  n * (( l * p) +(premium * (1-p)))
  
  n * pr*0.5 <- Es
  x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
  x
  n * (( l * p) +(x * (1-p)))
  
  set.seed(28)
  B <- 10000
  profit <- replicate(B, {
    draws <- sample( c(x, l), n, 
                     prob=c(1-p, p), replace = TRUE) 
    sum(draws)
  })
  mean(profit)    # expected value of the profit over n loans
  mean(profit<0)
  
  
  set.seed(28)
  B <- 10000
  n <- 1000
  p <- 0.015
  l <- -150000
  
  profit <- replicate(B, {
    new_p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
    draws <- sample( c(x, l), n, prob=c(1-new_p, new_p), replace = TRUE) 
    sum(draws)
  }) 
  mean(profit)    # expected profit
  mean(profit < 0)    # probability of losing money
  mean(profit < -1000000)    # probability of losing over $10 million
   
 #-----------Bayes-------------
  pr_B <- 0.6
  1 - pr_B
  pr_missed <- 0.1
  pr_B * pr_missed
  pr_notfinB <- 1 - pr_B + pr_B*pr_missed
  
  pr_c <- 0.15
  pr_notfinB_pr_B <- 0.1
  pr_B_pr_notfinB <- pr_notfinB_pr_B * pr_B / pr_notfinB
  pr_B_pr_notfinB
  
  pr_C_NFB <- pr_NFB_C * pr_c /pr_NFB
  1 *pr_c/pr_notfinB
  
  pr_B <- 0.6
  pr_missed <- 0.1    # prob plane in B but not found
  pr_not_found <-  1 - pr_B + pr_B*pr_missed
  pr_missed * pr_B / pr_not_found
  
  pr_C <- 0.15
  pr_missed_not_present <- 1
  pr_C * pr_missed_not_present / pr_not_found
  
  pr_A <- 0.2
  pr_D <- 0.05
  
  pr_A_post <- pr_A * pr_missed_not_present / pr_not_found
  pr_A_post
  pr_B_post <-  pr_missed * pr_B / pr_not_found
  pr_C_post <- pr_C * pr_missed_not_present / pr_not_found
  pr_D_post <- pr_D * pr_missed_not_present / pr_not_found
  
  results <- c(pr_A_post, pr_B_post, pr_C_post, pr_D_post)
  names(results) <- c("A", "B", "C", "D")
  names(which.max(results))
  pr_A_post*0.9
  
  
  
  #-----------------Brexit------------------
  library(dslabs)
  library(tidyverse)
  options(digits = 3)
  data(brexit_polls)
  head(brexit_polls)
  p=0.481
  d=(2*p)???1
  d
  N=1500
  X_hat <- N*p
  X_hat
  sd(X_hat)
  
  se_hat <- sqrt(p*(1-p)/N)
  se_hat
  
  brexit_polls_1 <- brexit_polls %>%
    mutate(x_hat =(spread + 1)/2 ) %>% summarize(mean(x_hat),sd(x_hat))
  head(brexit_polls)
  
  2*p-1
  2*sqrt(p*(1-p)/N)
  
  X_hat <- (brexit_polls[1,]$spread +1)/2
  
  se_hat <- sqrt(X_hat*(1-X_hat)/brexit_polls[1,]$samplesize)
  
  
  ci<- c(X_hat - qnorm(0.975)*se_hat,X_hat + qnorm(0.975)*se_hat)
  between(0, X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
  ci
  
  brexit_polls <- brexit_polls %>%
    mutate(x_hat =(spread + 1)/2 )
  head(brexit_polls)
  
  june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
  head(june_polls)
  
  actual_spread <- ???0.038
  june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),se_hat= 2*se_x_hat, lower = spread - qnorm(0.975)*se_hat, upper = spread + qnorm(0.975)*se_hat) %>% mutate(hit = lower <= actual_spread & upper >= actual_spread)
  head(june_polls)
  june_polls %>% filter(pollster %in% c('BMG Research','Populus','ORB/Telegraph'))
  june_polls %>% summarize(proportion_hits = mean(hit))
  
  june_polls  %>% group_by(pollster) %>% summarize(proportion_hits = mean(hit),n = n()) %>%arrange(desc(proportion_hits))
  june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot() + geom_point()
  
  combined_by_type <- june_polls %>%
    group_by(poll_type) %>%
    summarize(N = sum(samplesize),
              spread = sum(spread*samplesize)/N,
              p_hat = (spread + 1)/2) 
  
  se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)
  
  combined_by_type %>%mutate(s_p_hat = sqrt(p_hat*(1-p_hat)/N),se_p_hat = 2*s_p_hat,lower = spread - qnorm(0.975)*se_p_hat, upper = spread + qnorm(0.975)*se_p_hat)
  
  
  brexit_hit <- brexit_polls %>%
    mutate(p_hat = (spread + 1)/2,
           se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
           spread_lower = spread - qnorm(.975)*se_spread,
           spread_upper = spread + qnorm(.975)*se_spread,
           hit = spread_lower < -0.038 & spread_upper > -0.038) %>% 
    group_by(poll_type,hit) %>% summarize(num = n()) %>% spread(poll_type,num)
   
  chisq_test <- brexit_hit %>%
    select(-hit) %>%
    chisq.test()
  chisq_test$p.value
  
  odds_Tele <- (brexit_hit$Telephone[2] / sum(brexit_hit$Telephone)) /
    (brexit_hit$Telephone[1] / sum(brexit_hit$Telephone))
  odds_Tele
  
  odds_Online <- (brexit_hit$Online[2] / sum(brexit_hit$Online)) /
    (brexit_hit$Online[1] / sum(brexit_hit$Online))
  
  odds_Online
  
  odds_Online/odds_Tele
  
  
  brexit_polls %>% ggplot(aes(enddate, spread, fill=poll_type)) + 
    geom_point() +geom_smooth(method = "loess",span = 0.4) + 
    geom_hline(aes(yintercept = -.038))
  
  
  brexit_long <- brexit_polls %>%
    gather(vote, proportion, "remain":"undecided") %>%
    mutate(vote = factor(vote))
  
  levels(brexit_long$vote)
  
  brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) + 
    geom_point() +geom_smooth(method = "loess",span = 0.3)