#Assignment 1 2DF20 
#Part 2
#Exercice 3 : X is a continuous uniform random variable on [0,1], X ~ Unif(0,1)

#a) Show that Var(Xi) = 1/12 
n <- 100000 # number of sample
x <- runif(n,0,1) # draw n random number following the uniform distribution Unif(0,1)
hist(x, freq=FALSE, breaks=100) #Assignment ask us to draw histogram for each distribution
var <- var(x) #compute the var of x

############### RESULT #####################
barplot(c(1/12, var), main="1/12 vs. var(X)", names.arg=c("1/12","var(x)"))




#b) Let n = 1200. Compute P(580 ≤ SUM(Xi) i in 1 to n ≤ 620).
runs <- 10000 #Number of runs for the simulation

#Simulation function
simulationB <- function() {
  result <- vector() #empty vector to stock result
  for(i in 1:runs) {
    n <- 1200 #number of sample
    x <- runif(n,0,1) # draw n random number following distribution Unif(1,0)
    sum <- sum(x) #Sum all the random number Xi
    result[i] <- sum #Stock the sum in our vector result
  }
  return(result) #return the sum of Xi for each run in a vector
}

simB <- simulationB() #We simulate the experience
simB <- (simB <= 620 & simB >= 580) #We keep only sum that are between 580 and 620
proba = sum(simB)/runs #we divide the number of element between 580 and 600 by the total number of elements (runs can be replace by length(sim)) to obtain the simulation probability

############### RESULT #########################
proba




#c)Let N~Bin(3600, 1/3). Find Sn= [SUM(Xi) i in 1 to N] 
#As discussed with Mr. Boon, instead of using the property 1.26 of the lecture we will just repeat the question b but this time we generate n using a binomial distribution
runs <- 100000
simulationC <- function() {
  result <- vector()
  for(i in 1:runs){
    n <- rbinom(1,3600,1/3) #generate a N following the binomial distribution with n = 3600 and p = 1/3
    x <- runif(n,0,1) # draw n random number following distribution Unif(1,0)
    sum <- sum(x) #Sum all the random number Xi
    result[i] <- sum #Stock the sum in our vector result
  }
  return(result)
}

Sn <- simulationC()

####################### RESULT ########################
hist(Sn, freq=FALSE,breaks=100) #The histogram of the distribution Sn









#d)Now let's compute P(580 <= Sn <= 620) and compare the result with the one obtain in b)
#To obtain a representative result we will simulate a lot of Sn distribution (using code of question b), and then compute the probability for each one and take the average
# WARNING: Running time ~30sec, you can reduce it by modifying "nbrDist" wich is the number of distribution we will simulate to take the average probability
nbrDist <- 10
result <- vector()
for(i in 1:nbrDist) {
  sn <- simulationC()
  sn <- (sn <= 620 & sn >= 580)
  proba = sum(sn)/length(sn)
  result[i] <- proba
}
averageProba <- mean(result)
#################### RESULT #########################
barplot(c(proba, averageProba), main="P(580 <= SUM(Xi)[i in 1 to n] <= 620) VS. P(580 <= Sn <= 620", names.arg=c("Proba of b)","P(580 <= Sn <= 620)"))
proba
averageProba

