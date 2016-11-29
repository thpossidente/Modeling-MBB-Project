## 2 layer implementation ##

n.inputs <- 100
n.outputs <- 20
correlation.of.inputs <- 1 #must be between 0 and 1 
learning.rate <- 0.02
trace.parameter <- 0.2
n.training <- 5000
n.input.possibilities <- 100
trace <- rep(0, times = n.outputs) 
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 0.5  # p must be between 0 and 1
# A value of 0 represents a totally segregated model (visual inputs processed totally separately from auditory inptus) 
# A value of 1 represents a totally integrated model (visual inputs and auditory inputs are integrated and processed together)

weights <- matrix(runif(n.inputs*n.outputs, min=0, max=1), nrow=n.inputs, ncol=n.outputs)
weight.map.input1.output1 <- weights[1:(n.inputs/2), 1:(n.outputs/2)]# 1:50, 1:5  #input 1 to output 1
weight.map.input2.output2 <- weights[((n.inputs/2) + 1):n.inputs, ((n.outputs/2) + 1):n.outputs] # 51:100, 6:10 #input 2 to output 2
weight.map.input1.output2 <- weights[1:(n.inputs/2), ((n.outputs/2) + 1):n.outputs]    # 1:50, 6:10  # input 1 to output 2
weight.map.input2.output1 <- weights[((n.inputs/2) + 1):n.inputs, 1:(n.outputs/2)]   # 51:100, 1:5  #input 2 to output 1


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
  weights <<- cbind((rbind(weight.map.input1.output1, weight.map.input2.output1)), (rbind(weight.map.input1.output2, weight.map.input2.output2)))
}

integration(p)



forward.pass <- function(input){
  
  output <- numeric(n.outputs)
  for(j in 1:output){
    output[j] <- sigmoid.activation(sum(input*weights)) 
  }
  
  output[which.max(output[1:5])] <- 1
  output[output[1:5]!=max(output[1:5])] <- 0
  output[which.max(output[6:10])] <- 1
  output[output[6:10]!=max(output[6:10])] <- 0 
  
  return(output)
}


trace.update <- function(input){
  output <<- forward.pass(input)
  
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    weights[,i] <<- learning.rate * trace[i] * input - learning.rate * trace[i] * weights[,i]
  }
  
}





batch <- function(n.training){ ## every x number of trials, run stability functions
  counter <- 0
  while(counter <= n.training){ 
    if(counter %% 100 == 0){
      stability.across.groups()
      stability.within.groups()
    }
    counter <- counter + 1
    
    for(i in 1:n.training){
      g <- input.correlation() 
      for(o in 1:nrow(g)){   ## how to run trace on every row in g? (g is 10 inputs long)
        for(h in 1:ncol(g)){
        trace.update(g[[o,h]])
        }
      }
    }
  }
}

batch(n.training)







### generating inputs ###

possible.inputs <- matrix(0, nrow=n.input.possibilities, ncol=(n.inputs/2))

many.possible.input.vectors <- function(){
  for(i in  1:n.input.possibilities){
    possible.inputs[i,sample((n.inputs/2), ((n.inputs/2)*0.1), replace=F)] <<- 1
  }
  
}
many.possible.input.vectors()


input.correlation <- function(){
  groups <<- list(possible.inputs[1:10,],
                  possible.inputs[11:20,],  
                  possible.inputs[21:30,],
                  possible.inputs[31:40,],
                  possible.inputs[41:50,],
                  possible.inputs[51:60,],
                  possible.inputs[61:70,],
                  possible.inputs[71:80,],
                  possible.inputs[81:90,],
                  possible.inputs[91:100,])
  
  if(runif(1) <= correlation.of.inputs){
    both.inputs.drawn.from <- sample(groups,1,replace=T)
    both.inputs.drawn.from <- do.call(rbind, both.inputs.drawn.from)
    input <<- cbind(both.inputs.drawn.from, both.inputs.drawn.from)
  } else{
    else.function()
    while(all(input1.drawn.from == input2.drawn.from)){
      else.function()}
    input <<- cbind(input1.drawn.from, input2.drawn.from)
  }
  return(input)
}

else.function <- function(){
  input1.drawn.from <<- sample(groups,1,replace=T)
  input1.drawn.from <<- do.call(rbind, input1.drawn.from)
  input2.drawn.from <<- sample(groups,1,replace=T)
  input2.drawn.from <<- do.call(rbind, input2.drawn.from)
}




### Measuring Stability ###

stability.accross.inputs <- 
stability.within.inputs <- 
  
  
  
stability.accross.groups <- function(){
    for(i in 1:??){
      forward.pass(stability.accross.inputs)
    }
}



stability.within.groups <- function(){
  for(i in 1:??){
    forward.pass(stability.within.inputs)
  }
}


#two kinds of stability. Both measure # of times each node is active in output
# run stability functions after x number of training trials
# for stability function run batch of inputs through forward pass and look at activation nodes of outputs
