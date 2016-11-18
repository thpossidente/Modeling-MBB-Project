n.hidden1 <- 25
n.hidden2 <- 25
n.outputs <- 8
learning.rate <- 0.02
trace.parameter <- 0.2
n.training <- 5000
grid.width.and.height <- 8
n.inputs1 <- grid.width.and.height^2 * 4
n.inputs2 <- grid.width.and.height^2 * 4
trace <- rep(0, times = n.outputs)
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 0.5  #p must be between 0 and 1


input1.to.hidden1.weights <- matrix(runif((n.inputs1*n.hidden1), 0, 1), nrow = n.inputs1, ncol = n.hidden1)
input2.to.hidden2.weights <- matrix(runif((n.inputs2*n.hidden2), 0, 1), nrow = n.inputs2, ncol = n.hidden2)

hidden1.to.output.weights <- matrix(runif((n.hidden1*n.output), 0, 1), nrow = n.hidden1, ncol = n.output) 
hidden2.to.output.weights <- matrix(runif((n.hidden2*n.output), 0, 1), nrow = n.hidden2, ncol = n.output)

input1.to.hidden2.weights <- matrix(0, nrow = n.inputs1, ncol = n.hidden2)
input2.to.hidden1.weights <- matrix(0, nrow = n.inputs2, ncol = n.hidden2)


integration <- function(p){ 

  
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
}

integration(p)



forward.pass <- function(input1, input2){

  hidden1 <- numeric(n.hidden1)
  for(i in 1:n.hidden1){
    hidden1[i] <<- (sigmoid.activation(sum(input1*input1.to.hidden1.weights)))
                    + (sigmoid.activation(sum(input2*input2.to.hidden1.weights)))
  }
  
  hidden2 <- numeric(n.hidden2)
  for(o in 1:n.hidden2){
    hidden2[o] <<- (sigmoid.activation(sum(input1*input1.to.hidden2.weights))) 
                    + (sigmoid.activation(sum(input2*input2.to.hidden2.weights)))
  }
  
  output <- numeric(n.outputs)
  for(j in 1:outputs){
    output[j] <<- sigmoid.activation(sum(hidden1*hidden1.to.output.weights)) 
                  + sigmoid.activation(sum(hidden2*hidden2.to.output.weights))
  }

  return(C(hidden1, hidden2, output))
}


trace.update <- function(input1, input2){
  pass <- forward.pass(input1, input2)
  output <- pass[3]
  hidden1 <- pass[1]
  hidden2 <- pass[2]
  
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    hidden1.to.output.weights[,i] <<- learning.rate * trace[i] * hidden1 - learning.rate * trace[i] * hidden1.to.output.weights[,i]
    hidden2.to.output.weights[,i] <<- learning.rate * trace[i] * hidden2 - learning.rate * trace[i] * hidden2.to.output.weights[,i]
  }
  
  for(o in 1:n.hidden1){
    trace[o] <<- (1 - trace.param) * trace[o] + trace.param * hidden1[o]
    input1.to.hidden1.weights[,i] <<- learning.rate * trace[o] * input1 - learning.rate * trace[o] * input1.to.hidden1.weights[,i]
    input2.to.hidden1.weights[,i] <<- learning.rate * trace[o] * input2 - learning.rate * trace[o] * input2.to.hidden1.weights[,i]
  }
  
  for(j in 1:n.hidden2){
    trace[j] <<- (1 - trace.param) * trace[j] + trace.param * hidden2[j]
    input1.to.hidden2.weights <<- learning.rate * trace[j] * input1 - learning.rate * trace[j] * input1.to.hidden2.weights[,i]
    input2.to.hidden2.weights <<- learning.rate * trace[j] * input2 - learning.rate * trace[j] * input2.to.hidden2.weights[,i]
  }
}


batch <- 










