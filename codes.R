n.hidden1 <- 25
n.hidden2 <- 25
n.output <- 8
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


input1.to.hidden1.weights <- matrix(runif((n.input1*n.hidden1), 0, 1), nrow = n.input1, ncol = n.hidden1)
input2.to.hidden2.weights <- matrix(runif((n.input2*n.hidden2), 0, 1), nrow = n.input2, ncol = n.hidden2)

hidden1.to.output.weights <- matrix(runif((n.hidden1*n.output), 0, 1), nrow = n.hidden1, ncol = n.output) 
hidden2.to.output.weights <- matrix(runif((n.hidden2*n.output), 0, 1), nrow = n.hidden2, ncol = n.output)

input1.to.hidden2.weights <- matrix(0, nrow = n.input1, ncol = n.hidden2)
input2.to.hidden1.weights <- matrix(0, nrow = n.input2, ncol = n.hidden2)


level.of.integration <- function(p){ 

  
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

level.of.integration(p)



forward.pass <- function(input1, input2){

  hidden1 <- numeric(n.hidden1)
  for(i in 1:n.hidden1){
    hidden1[i] <<- (sigmoid.activation(sum(input1*input1.to.hidden1.weights))) + (sigmoid.activation(sum(input2*input2.to.hidden1.weights)))
  }
  
  hidden2 <- numeric(n.hidden2)
  for(o in 1:n.hidden2){
    hidden2[o] <<- (sigmoid.activation(sum(input1*input1.to.hidden2.weights))) + (sigmoid.activation(sum(input2*input2.to.hidden2.weights)))
  }
  
  output1 <- numeric(n.output/2)
  for(j in 1:output1){
    output[j] <<- sigmoid.activation(sum(hidden1*hidden1.to.output.weights))
  }
  
  output2 <- numeric(n.output/2)
    for(g in 1:output2){
      output[g] <<- sigmoid.activation(sum(hidden2*hidden2.to.output.weights))
    }
  

  
  output1[which.max(output)] <- 1
  output1[output1!=max(output1)] <- 0
  output2[which.max(output2)] <- 1
  output2[output2!=max(output2)] <- 0
  
  output <- rbind(output1, output2)
  
  return(output)
}


trace.update <- function(input1, input2){
  output <- forward.pass(input1, input2)
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    hidden1.to.output.weights[,i] <<- hidden1.to.output.weights[,i] + learning.rate * trace[i] * (input#? - hidden1.to.output.weights[,i])
  }
}










