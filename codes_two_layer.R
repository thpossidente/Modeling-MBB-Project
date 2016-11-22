## 2 layer implementation ##

n.inputs <- 100
n.outputs <- 10
correlation.of.inputs <- 0 #must be between 0 and 1 
learning.rate <- 0.02
trace.parameter <- 0.2
n.training <- 5000
trace <- rep(0, times = n.outputs)
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 0  # p must be between 0 and 1
# A value of 0 represents a totally segregated model (visual inputs processed totally separately from auditory inptus) 
# A value of 1 represents a totally integrated model (visual inputs and auditory inputs are integrated and processed together)


weight.map.input1.output1 <- weights[1:(n.inputs/2), 1:(n.outputs/2)]# 1:50, 1:5  #input 1 to output 1
weight.map.input2.output2 <- weights[((n.inputs/2) + 1):n.inputs, ((n.outputs/2) + 1):n.outputs] # 51:100, 6:10 #input 2 to output 2
weight.map.input1.output2 <- weights[1:(n.inputs/2), ((n.outputs/2) + 1):n.outputs]    # 1:50, 6:10  # input 1 to output 2
weight.map.input2.output1 <- weights[((n.inputs/2) + 1):n.inputs, 1:(n.outputs/2)]   # 51:100, 1:5  #input 2 to output 1
weights <- cbind((rbind(weight.map.input1.output1, weight.map.input2.output1)), (rbind(weight.map.input1.output2, weight.map.input2.output2)))


integration <- function(p){ 
  
  for(i in 1:length(weight.map.input1.output2)){
    if(runif(1) <= p){
      weight.map.input1.output2[i] <<- runif(1)
    } else{
      weight.map.input1.output2[i] <<- 0
    }}
  
  
  for(i in 1:length(weight.map.input2.output1)){
    if(runif(1) <= p){
      weight.map.input2.output1[i] <<- runif(1)
    } else{
      weight.map.input2.output1[i] <<- 0
    }}
  
}

integration(p)



forward.pass <- function(input){
  
  output <- numeric(n.outputs)
  for(j in 1:output){
    output[j] <<- sigmoid.activation(sum(input*weights)) 
  }
  
  output[which.max(output[1:5])] <- 1
  output[output[1:5]!=max(output[1:5])] <- 0
  output[which.max(output[6:10])] <- 1
  output[output[6:10]!=max(output[6:10])] <- 0
  
  return(output)
}


trace.update <- function(input){
  output <- forward.pass(input)
  
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    weights[,i] <<- learning.rate * trace[i] * input - learning.rate * trace[i] * weights[,i]
  }
  
}



batch <- function(num.training){ 
  for(i in 1:numtraining){
    g <- input.correlation(correlation.of.inputs)
    for(o in 1:length(g)){
      trace.update(g[[o]])
    }}
}

batch(n.training)




# measure success of network using entropy function
# generate input sequences to input during batch function








## helper functions:


input.map.1 <- inputs[1:(n.inputs/2)]
input.map.2 <- inputs[(n.inputs/2 + 1):n.inputs]
inputs <- rbind(input.map.1, input.map.2)

input.correlation <- function(correlation.of.inputs){
  counter <- 0
  while(counter < (0.05*n.inputs)){
    sample(input.map.1, 1, replace = F) <<- 1
    if(runif<=correlation.of.inputs){
      #how to make input.map.2 value 1 in the same value that input.map.1 is?
    }
    counter <- counter + 1
  }
  
}