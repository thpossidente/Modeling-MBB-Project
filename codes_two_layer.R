## 2 layer implementation ##

n.inputs <- 100
n.outputs <- 20
correlation.of.inputs <- 1 #must be between 0 and 1 
learning.rate <- 0.02
trace.param <- 0.2
n.training <- 5000
n.input.possibilities <- 100
trace <- rep(0, times = n.outputs) 
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 0.5  # p must be between 0 and 1
# A value of 0 represents a totally segregated model (visual inputs processed totally separately from auditory inptus) 
# A value of 1 represents a totally integrated model (visual inputs and auditory inputs are integrated and processed together)


integration <- function(p){ 
  
  weights <- matrix(runif(n.inputs*n.outputs, min=0, max=1), nrow=n.inputs, ncol=n.outputs)
  weight.map.input1.output1 <- weights[1:(n.inputs/2), 1:(n.outputs/2)]# 1:50, 1:5  #input 1 to output 1
  weight.map.input2.output2 <- weights[((n.inputs/2) + 1):n.inputs, ((n.outputs/2) + 1):n.outputs] # 51:100, 6:10 #input 2 to output 2
  weight.map.input1.output2 <- weights[1:(n.inputs/2), ((n.outputs/2) + 1):n.outputs]    # 1:50, 6:10  # input 1 to output 2
  weight.map.input2.output1 <- weights[((n.inputs/2) + 1):n.inputs, 1:(n.outputs/2)]   # 51:100, 1:5  #input 2 to output 1
  
  for(i in 1:length(weight.map.input1.output2)){
    if(runif(1) > p){
      weight.map.input1.output2[i] <- NA
    }}
  
  
  for(i in 1:length(weight.map.input2.output1)){
    if(runif(1) > p){
      weight.map.input2.output1[i] <- NA
    }}
  weights <- cbind((rbind(weight.map.input1.output1, weight.map.input2.output1)), (rbind(weight.map.input1.output2, weight.map.input2.output2)))
  return(weights)
}

weights <- integration(p)



forward.pass <- function(input){
  
  output <- numeric(n.outputs)
  for(j in 1:n.outputs){
    output[j] <- sigmoid.activation(sum(input*weights[,j], na.rm = T)) 
  }
  
  output[which.max(output[1:(n.outputs/2)])] <- 1
  output[output[1:(n.outputs/2)]!=max(output[1:(n.outputs/2)])] <- 0
  output[which.max(output[(n.outputs/2 + 1):n.outputs]) + (n.outputs/2)] <- 1
  output[output[(n.outputs/2 + 1):n.outputs] != max(output[(n.outputs/2 + 1):n.outputs])] <- 0 
  
  return(output)
}



trace.update <- function(input){
  output <- forward.pass(input)
  
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    for(j in 1:n.inputs){
      if(!is.na(weights[j,i])){
        weights[j,i] <<- weights[j,i] + learning.rate * trace[i] * input[j] - learning.rate * trace[i] * weights[j,i]  
      }}
  }
}





batch <- function(n.training){ 
  #counter <- 0
  #stability.accross.vector <- numeric(0)
  #stability.within.vector <- numeric(0)
  #while(counter <= n.training){ 
  #  if(counter %% 100 == 0){
  #    stability.across.vector <- c(stability.across.vector, stability.across.groups()) 
  #    stability.within.vector <- c(stability.within.vector, stability.within.groups())
  #  }
  #  counter <- counter + 1
  
  for(i in 1:n.training){
    g <- input.correlation() 
    for(o in 1:nrow(g)){   
      trace.update(g[o,])
    }
  }
}
#}


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
    input <- cbind(both.inputs.drawn.from, both.inputs.drawn.from)
  } else{
    else.function()
    while(all(input1.drawn.from == input2.drawn.from)){
      else.function()}
    input <- cbind(input1.drawn.from, input2.drawn.from)
  }
  return(input)
}

else.function <- function(){
  input1.drawn.from <- sample(groups,1,replace=T)
  input1.drawn.from <- do.call(rbind, input1.drawn.from)
  input2.drawn.from <- sample(groups,1,replace=T)
  input2.drawn.from <- do.call(rbind, input2.drawn.from)
}




### Measuring Stability ###


stability.accross.groups <- funtion(){
  for(i in 1:10){
    system1[i] <- do.call(rbind, groups[i]) 
    system2[i] <- do.call(rbind, groups[i]) 
  }
  
}



stability.within.groups <- function(){
  storing.outputs <- matrix(0, nrow = 1000, ncol = 20)
  stability <- matrix(0, nrow = 10, ncol = 20)
  for(a in 1:10){
    inputA <- groups[[a]]  
    for(b in 1:10){
      inputB <- groups[[b]]
      for(i in 1:10){
        storing.outputs[a,] <- forward.pass(c(inputA[i,], inputB[i,])) #not working
      }
    }
  }
  for(h in 10){
    stability[h,] <- colSums(storing.outputs[((i-1)*100)+1 : i*10,])
  }
}

stability.within.groups()









