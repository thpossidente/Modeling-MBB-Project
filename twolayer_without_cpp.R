n.inputs <- 100
n.outputs <- 20
correlation.of.inputs <- 0 #must be between 0 and 1 
learning.rate <- 0.01
trace.param <- 0.2 #higher -> learning less spread out over time. Lower -> learning more spread out over time
n.training <- 5000
n.input.possibilities <- 100
trace <- rep(0, times = n.outputs) 
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 1 # p must be between 0 and 1
# A value of 0 represents a totally segregated model (visual inputs processed totally separately from auditory inptus) 
# A value of 1 represents a totally integrated model (visual inputs and auditory inputs are integrated and processed together)


integration <- function(p){ 
  
  weights <- matrix(runif(n.inputs*n.outputs, min=0, max=1), nrow=n.inputs, ncol=n.outputs)
  
  
  for(input in 1:(n.inputs/2)){
    for(output in (n.outputs/2 + 1):n.outputs){
      if(runif(1) > p){
        weights[input,output] <- NA
      }
    }
  }
  
  for(input in (n.inputs/2 + 1):n.inputs){
    for(output in 1:(n.outputs/2)){
      if(runif(1) > p){
        weights[input,output] <- NA
      }
    }
  }
  
  return(weights)
}

weights <- integration(p)


forward.pass <- function(input){
  
  output <- numeric(n.outputs)
  for(j in 1:n.outputs){
    output[j] <- sigmoid.activation(sum(input*weights[,j], na.rm = T)) 
  }
  
  o.1 <- output[1:(n.outputs/2)]
  o.2 <- output[(n.outputs/2 + 1):n.outputs]
  
  o.1[which.max(o.1)] <- 1
  o.1[o.1 != 1] <- 0
  
  o.2[which.max(o.2)] <- 1
  o.2[o.2 != 1] <- 0
  
  output <- c(o.1, o.2)
  
  return(output)
}



trace.update <- function(input){
  output <- forward.pass(input)
  
  for(i in 1:n.outputs){
    trace[i] <<- (1 - trace.param) * trace[i] + trace.param * output[i]
    for(j in 1:n.inputs){
      if(!is.na(weights[j,i])){
        weights[j,i] <<- weights[j,i] + learning.rate * trace[i] * input[j] - learning.rate * trace[i] * weights[j,i]  
      }
    }
  }
}





batch <- function(n.training){ 

  pb <- txtProgressBar(min=1, max=n.training, style=3)
  for(i in 1:n.training){
    g <- input.correlation() 
    for(o in 1:nrow(g)){   
      trace.update(g[o,])
    }
    setTxtProgressBar(pb, i)
  }
}






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
  input1.drawn.from <<- sample(groups,1,replace=T)
  input1.drawn.from <<- do.call(rbind, input1.drawn.from)
  input2.drawn.from <<- sample(groups,1,replace=T)
  input2.drawn.from <<- do.call(rbind, input2.drawn.from)
}




### Measuring Stability ###



stability.within.groups.seg <- function(){
  storing.outputs <- matrix(NA, nrow = 1000, ncol= 20)
  for(a in 1:10){
    inputA <- groups[[a]]  
    for(b in 1:10){
      inputB <- groups[[b]]
      for(i in 1:10){
        one.output <- forward.pass(c(inputA[i,], inputB[i,])) 
        storing.outputs[100 * (a-1) + 10 * (b-1) + i, ] <- one.output
      }
    }
  }
  
  stability1 <- colSums(storing.outputs[1:100,])
  stability2 <- colSums(storing.outputs[101:200,])
  stability3 <- colSums(storing.outputs[201:300,])
  stability4 <- colSums(storing.outputs[301:400,])
  stability5 <- colSums(storing.outputs[401:500,])
  stability6 <- colSums(storing.outputs[501:600,])
  stability7 <- colSums(storing.outputs[601:700,])
  stability8 <- colSums(storing.outputs[701:800,])
  stability9 <- colSums(storing.outputs[801:900,])
  stability10 <- colSums(storing.outputs[901:1000,])
  stability <- rbind(stability1, stability2, stability3, stability4, stability5, 
                     stability6, stability7, stability8, stability9, stability10)
  
  entropy1 <- c(entropy.calc(stability1[1:10]), entropy.calc(stability1[11:20]))
  entropy2 <- c(entropy.calc(stability2[1:10]), entropy.calc(stability2[11:20]))
  entropy3 <- c(entropy.calc(stability3[1:10]), entropy.calc(stability3[11:20]))
  entropy4 <- c(entropy.calc(stability4[1:10]), entropy.calc(stability4[11:20]))
  entropy5 <- c(entropy.calc(stability5[1:10]), entropy.calc(stability5[11:20]))
  entropy6 <- c(entropy.calc(stability6[1:10]), entropy.calc(stability6[11:20])) 
  entropy7 <- c(entropy.calc(stability7[1:10]), entropy.calc(stability7[11:20]))
  entropy8 <- c(entropy.calc(stability8[1:10]), entropy.calc(stability8[11:20]))
  entropy9 <- c(entropy.calc(stability9[1:10]), entropy.calc(stability9[11:20]))
  entropy10 <- c(entropy.calc(stability10[1:10]), entropy.calc(stability10[11:20]))
  entropy <- rbind(entropy1, entropy2, entropy3, entropy4, entropy5,
                   entropy6, entropy7, entropy8, entropy9, entropy10)
  entropies <- c(entropy1[1], entropy2[1], entropy3[1], entropy4[1], entropy5[1],
                 entropy6[1], entropy7[1], entropy8[1], entropy9[1], entropy10[1])
  
  entropy.mean <- mean(entropies)
  
  return(entropy.mean)
  
}




stability.within.groups.int <- function(){
  storing.outputs <- matrix(NA, nrow = 1000, ncol= 20)
  for(a in 1:10){
    inputA <- groups[[a]]  
    for(b in 1:10){
      inputB <- groups[[b]]
      for(i in 1:10){
        one.output <- forward.pass(c(inputA[i,], inputB[i,])) 
        storing.outputs[100 * (a-1) + 10 * (b-1) + i, ] <- one.output
      }
    }
  }
  
  stability1 <- colSums(storing.outputs[1:100,])
  stability2 <- colSums(storing.outputs[101:200,])
  stability3 <- colSums(storing.outputs[201:300,])
  stability4 <- colSums(storing.outputs[301:400,])
  stability5 <- colSums(storing.outputs[401:500,])
  stability6 <- colSums(storing.outputs[501:600,])
  stability7 <- colSums(storing.outputs[601:700,])
  stability8 <- colSums(storing.outputs[701:800,])
  stability9 <- colSums(storing.outputs[801:900,])
  stability10 <- colSums(storing.outputs[901:1000,])
  stability <- rbind(stability1, stability2, stability3, stability4, stability5, 
                     stability6, stability7, stability8, stability9, stability10)
  
  entropy1 <- c(entropy.calc(stability1[1:10]), entropy.calc(stability1[11:20]))
  entropy2 <- c(entropy.calc(stability2[1:10]), entropy.calc(stability2[11:20]))
  entropy3 <- c(entropy.calc(stability3[1:10]), entropy.calc(stability3[11:20]))
  entropy4 <- c(entropy.calc(stability4[1:10]), entropy.calc(stability4[11:20]))
  entropy5 <- c(entropy.calc(stability5[1:10]), entropy.calc(stability5[11:20]))
  entropy6 <- c(entropy.calc(stability6[1:10]), entropy.calc(stability6[11:20])) 
  entropy7 <- c(entropy.calc(stability7[1:10]), entropy.calc(stability7[11:20]))
  entropy8 <- c(entropy.calc(stability8[1:10]), entropy.calc(stability8[11:20]))
  entropy9 <- c(entropy.calc(stability9[1:10]), entropy.calc(stability9[11:20]))
  entropy10 <- c(entropy.calc(stability10[1:10]), entropy.calc(stability10[11:20]))
  entropy <- rbind(entropy1, entropy2, entropy3, entropy4, entropy5,
                   entropy6, entropy7, entropy8, entropy9, entropy10)
  
  return(mean(entropy))
  
}


entropy.calc <- function(v){
  v <- v / sum(v)
  e.sum <- 0
  for(i in 1:length(v)){
    if(v[i] != 0){
      e.sum <- e.sum + -v[i] * log2(v[i])
    }
  }
  return(e.sum)
}


#### wacky visualization stuff ####

output.matrix.visualization <- function() {
  output.matrix <- matrix(0, nrow=100, ncol=n.outputs)
  
  for(a in 1:10){
    inputA <- groups[[a]]
    for(i in 1:10){
      output <- numeric(n.outputs)
      for(b in 1:10){
        inputB <- groups[[b]]
        for(j in 1:10){
          output <- output + forward.pass(c(inputA[i,], inputB[j,]))
        }
      }
      output.matrix[10 * (a-1) + i, ] <- output
    }
  }
  
  image(t(output.matrix[100:1,]))
}

### Running Trials and Testing ###

batch(n.training)
for(i in 1:n.inputs){
  for(j in 1:n.outputs){
    if(weights[i, j] == -100){
      weights[i, j] <- NA
    }
  }
}

output.matrix.visualization()
stability.within.groups.seg()
stability.within.groups.int()

entropies <- c(entropy1[2], entropy2[2], entropy3[2], entropy4[2], entropy5[2],
               entropy6[2], entropy7[2], entropy8[2], entropy9[2], entropy10[2])

mean(entropies)

# for noise- in each input vector change a zero to a one and a one to a zero
# write noise function to pass input into each time and change the numbers around a little
#graphs of entropy with batches, integration, and correlation

