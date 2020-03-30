# Part 1
# Exercice 1 Assignment 1

##### First we will compute Sm(p) for differents values of M and P ######

p <- c(-2,-1,-0.5,0.5,0.99,1.01,2) #All values of p that we are going to test
m <- c(0, 10, 100, 500, 1000) #All values of m that we are going to test

# As advised during instruction session we create a function computing all the value p^k for a given p and a range from 0 to mValue for k and return a vector with all this value
getVectorOfAllvalues <- function(pValue, mValue) {
  seqValue <- vector() #Empty vector that we will fill in with value of p^k
  for (i in 0:mValue){
    seqValue[i+1] <- pValue^i #we compute p^k and put the result in our vector
  }
  return(seqValue) #return the vector that contains all the value of p^k
}

#Create 3 variables to stock results
seqToSum <- vector() #will contains the sequence of p^k to sum
sum <- 0 #will contains the sum of all composants of the vector seqToSum
vectorValue <- vector() #will contains all the value of Sm(p) for the differents values p and m that we are going to test

#Double iteration, for all value of m and p we compute the function Sm(p) and store the result in the vector vectorValue
for(i in 1:length(m)){ #for all value of m that we want to test
  for (j in 1 :length(p)){ #for all value of p that we want to test
    seqToSum <- getVectorOfAllvalues(p[j], m[i]) #we first get all the value of p^k
    sumSeq <- sum(seqToSum) #then we sum all this value to obtain Sm(p)
    vectorValue[(i-1)*length(p)+j] <- sumSeq #finally we stock the result in vectorValue
  }
}

#We create a matrix/table to obsevrve result
SMtable<-matrix(vectorValue, ncol=length(p),byrow=TRUE)
rownames(SMtable) <- c(paste("m = ", m))
colnames(SMtable) <- paste("p =",p)
SMtable <- as.table(SMtable)

########################################### VALUE OF Sm(p) FOR DIFFERENTS VALUES OF P AND M #################################################
SMtable







#### Now, let's compare our results with the "theoretical" one #### 
#Compute 1 / (1-p) for our value of P (WARNING : when |p| >=1 result is false by definition)
theoreticalValue <- 1/(1-p)

#Supposing that the most accurable value of S(m) is when m is large, we will compare the theoretical value with the one compute with m = 1000
#We compute the start and end index of where we have stocked our value when m is equal to 1000 in the previous simulation
start <- length(vectorValue) - length(p) + 1
end <- length(vectorValue)
valueM1000 <- vectorValue[start:end] #We extract thus value to stock them in a new vector

#Now we compare our simulation value with the theoretical one
difference <- abs(theoreticalValue-valueM1000)
diffTable <-matrix(difference, ncol=length(p), byrow=TRUE)
rownames(diffTable) <- "Abs(theory-simulation)"
colnames(diffTable) <- paste("p =", p)
diffTable <- as.table(diffTable)

########################################## COMPARISON OF THEORTICAL VALUES OF Sm(p) AND THUS OBTAIN BY SIMULATION ###########################
# We obviosuly see that when |p| < 1 we obtain similar resuts, but when |p|>=1 then result diverge because the result 1/(1-p) is no longer true
diffTable

# CONLUSION : we conclude that in fact Sm(p) tends to be equal to 1/(1-p) when |p| < 1. (The case p = -1 is special because the sum is
# 1 + (-1) + 1 ... = 0 or 1 depending of the parity of m). But we see that if |p| >= 1 then Sm(p) differ from 1/(1-p).









##### We finaly plot Sm(p) versus m and add the line 1/(1-p) to get an impression of the speed of convergence #####
#Funtion that plot Sm(p) versus m and the line 1/(1-p) for a given value of p and m
plotComparison <- function(pValue, mValue) {
  theory <- 1/(1-pValue) # The theoretical value of Sm(p)
  m <- seq(1:mValue)
  theoryseq <- rep(theory, length(m))
  Sm <- vector()#vector where we will stock value of Sm(p)
  #We use a for loop to compute the different value of Sm(p) for all m between 1 and mValue
  for(i in 1:length(m)) {
    Sm[i] <- sum(getVectorOfAllvalues(pValue,i))
  }
  #finally we plot our result and add a line in red that represents the value 1/(1-p)
  plot(m,Sm,type="l",main=paste("Sm(p) versus M for p = ", pValue), xlab = "M",ylab = "Sm(p)")
  lines(m, theoryseq, col="red")
}

################################ CALL FUNCTION plotComparison TO OBTAIN A VISUALIZATION OF THE SPEED OF CONVERGENCE ##################################
#Modify arguments to get the plot of your choice

plotComparison(0.99, 3000) #converge slowly

plotComparison(0.5,50) #converge quickly

plotComparison(2,100) #differ

# WARNING: We can see that if |p| is close to 1 then we need m to be big (plot p=0.99 and m = 1000) to obtain Sm(p) = 1/(1-p), but if p is smaller
# then we do not need m to be big to observe convergence.
# CONCLUSION : Sm(p) converge faster through 1/(1-p) if p is closed to zero than to 1, and differ if |p| >= 1












#### To visualize the speed of convergence we plot the smaller value of m such that Sm(p) = 1/(1-p) for differents value of p ####
p <- seq(-0.99,0.99,0.01) # values of p we are going to test
m <- seq(1,10000,1) #value of m we are going to test
y<-vector() #Empty vector to put our result

#For all value of p we want to test
for (i in 0:length(p)) {
  theory <- 1/(1-p[i])
  l <- length(m)
  y[i] <- -1
  j <- 1
  #While Sm(p) is different thant 1/(1-p) and that we do not have test all value of m yet, compute Sm(p) and test equality
  while(j <= l && identical(y[i],-1)) {
    smp <-sum(getVectorOfAllvalues(p[i], m[j]))
    if(smp >= theory-0.000000000000001 && smp <= theory+0.000000000000001) {
      y[i] <- m[j]
    }
    j <- j+1
  }
}

######################################## PLOT OF THE SPEED OF CONVERGENCE DEPEDNING OF P AND M ###################################################
plot(p,y,type="h",main="Speed of convergence of Sm(p) [for wich m Sm(p) = 1/(1-p)]",xlab="p", ylab="m")

#WARNING: Running time ~6sec

















#Exercice 2

# Following the same idead that in the exercice 1, we will compute different value P(X=k) with X following a poison distribution pois(lamda), for differents value of k and lamda, and then compare result we obtain by simulation and the theoretical one
n <- 100000 #number of sample (higher n, better results)
k <- seq(1,100,2)
lambdas <- seq(25,75)

#Function that give us the value of P(X=k), the mean and the variance using simulation method
simulationPois <- function(kValue, lambda) {
  x <- rpois(n,lambda) #sample of random number following the poison distribution
  mean <- mean(x) #compute the mean
  var <- var(x) #compute the var
  # Estimate P(X = kValue)
  v <- (x == kValue)
  proba <- sum(v)/n
  return(c(proba, mean, var))
}

# Function that compute the theoretical result P(X=k), mean and var of a Poison distribution
formulaPois <- function(kValue, lambda) {
  mean <- lambda #by definition
  var <- lambda #by definition
  proba <- (exp(-lambda)*(lambda^kValue))/factorial(kValue) #the formula
  return(c(proba,mean,var))
}

#We create list to stock result of our computation (simulation and theoretical one)
simulationResult <- list()
theoreticalResult <- list()

#We iterate over our differents value of lambdas we want to test
for(i in 1:length(lambdas)){
  #for each of is lamdba we will test different values of k to compute P(X=k), the mean and the var
  for(j in 1:length(k)){
    #We stock the result in the appropriate list
    simulationResult[[(i-1)*length(k)+j]] <- simulationPois(k[j], lambdas[i])
    theoreticalResult[[(i-1)*length(k)+j]] <- formulaPois(k[j], lambdas[i])
  }
}

#We create a list and for each simulatin result and theoretical one we compute the difference
difference <- list()
for(i in 1:(length(k)*length(lambdas))){
  difference[[i]] <- simulationResult[[i]]-theoreticalResult[[i]]
}

#Here there are 3 functions that compute the average difference for the proba (P(X=k)), the mean and the var between the simulation and the theoretical results
averageProbaDifference <- function(differenceList) {
  vectorOfProbaDifference <- vector()
  for (i in 1:length(differenceList)){
    v <- differenceList[[i]]
    vectorOfProbaDifference[i] <- v[1]
  }
  abs(sum(vectorOfProbaDifference))/length(vectorOfProbaDifference)
}

averageMeanDifference <- function(differenceList) {
  vectorOfMeanDifference <- vector()
  for (i in 1:length(differenceList)){
    v <- differenceList[[i]]
    vectorOfMeanDifference[i] <- v[2]
  }
  abs(sum(vectorOfMeanDifference))/length(vectorOfMeanDifference)
}

averageVarDifference <- function(differenceList) {
  vectorOfVarDifference <- vector()
  for (i in 1:length(differenceList)){
    v <- differenceList[[i]]
    vectorOfVarDifference[i] <- v[3]
  }
  abs(sum(vectorOfVarDifference))/length(vectorOfVarDifference)
}

#We stock the result of this three fonction into different argument
probaDiffAve <- averageProbaDifference(difference)
meanDiffAve <- averageMeanDifference(difference)
varDiffAve <- averageVarDifference(difference)
#We put all the difference average in 1 vector so we can plot them easily
x<-c(probaDiffAve, meanDiffAve, varDiffAve)

##################################################### AVERAGE DIFFERENCE BETWEEN SIMULATIONS AND THEORETICAL RESULTS #################################
barplot(x, main="Average difference between simultation and theoretical results", names.arg=c("Proba difference","Mean difference","Var difference"))


# CONCLUSION: We can observe that the difference between the simulation and the theoritical result is really small, sometimes equal to zero. Also if we increase n, and our sample of test for lambda and k we will obtain better results.
# So we conclude that E[X] = VAR[X] = Lamdba for a X following Pois(lambda). Also P(X=k) = (exp(-lambda)*(lambda^k))/factorial(k.)

#WARNING : We test our programm with 50 lambdas, 50 differents K for each lambda and a sample of n = 100000 number each time. (running time ~10sec)





























