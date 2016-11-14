#Create function with parameter p that creates matrix of weights. 
n.input1 <- 10
n.input2 <- 10
n.hidden1 <- 25
n.hidden2 <- 25
n.output <- 10
<<<<<<< HEAD
p <- 0.5

input1.to.hidden1.weights <- matrix(runif((n.input1*n.hidden1), 0, 1), nrow = n.input1, ncol = n.hidden1)
input2.to.hidden2.weights <- matrix(runif((n.input2*n.hidden2), 0, 1), nrow = n.input2, ncol = n.hidden2)

hidden1.to.output.weights <- matrix(runif((n.hidden1*n.output), 0, 1), nrow = n.hidden1, ncol = n.output) 
hidden2.to.output.weights <- matrix(runif((n.hidden2*n.output), 0, 1), nrow = n.hidden2, ncol = n.output)


input1.to.hidden2.weights <- matrix(0, nrow = n.input1, ncol = n.hidden2)
input2.to.hidden1.weights <- matrix(0, nrow = n.input2, ncol = n.hidden2)

level.of.integration <- function(p){ #p must be between 0 and 1

  
  for(i in 1:length(input1.to.hidden2.weights)){
    if(runif(1) <= p){
      input1.to.hidden2.weights[i] <<- runif(1)
    } else{
        input1.to.hidden2.weights[i] <<- 0
  }}
  
  for(o in 1:length(input2.to.hidden1.weights)){
    if(runif(1) <= p){
      input2.to.hidden1.weights[o] <<- runif(1)
    } else{
        input2.to.hidden1.weights[o] <<- 0
  }}
  
=======
p <-   ##integration


input1.to.hidden1.weights <- matrix(runif((n.input1*n.hidden1), 0, 1), nrow = n.input1, ncol = n.hidden1)
input2.to.hidden2.weights <- matrix(runif((n.input2*n.hidden12), 0, 1), nrow = n.input2, ncol = n.hidden2)
hidden1.to.output.weights <- matrix(runif((n.hidden1*n.output), 0, 1), nrow = n.hidden1, ncol = n.output) 
hidden2.to.output.weights <- matrix(runif((n.hidden2*n.output), 0, 1), nrow = n.hidden2, ncol = n.output)

level.of.integration <- function(p){ #p must be between 0 and 1

  hidden1.to.hidden2.weights <- matrix(0, nrow = n.hidden1, ncol = n.hidden2)
  hidden2.to.hidden1.weights <- matrix(0, nrow = n.hidden2, ncol = n.hidden2)
  
  for(i in 1:length(hidden1.to.hidden2.weights)){
    hidden1.to.hidden2.weights[i] <<- ##probability of weight being 0 or drawn from a runif(1, 0, 1) based on p
  }
  
  for(o in 1:length(hidden2.to.hidden1.weights)){
    hidden2.to.hidden1.weights[o] <<- ##probability of weight being 0 or drawn from a runif(1, 0, 1) based on p
  }

>>>>>>> origin/master
}

level.of.integration(p)



forward.pass <- function(input1, input2){

  
  
}












