source('helper-functions.R')

n.hidden1 <- 25
n.hidden2 <- 25
n.outputs <- 8
learning.rate <- 0.02
trace.parameter <- 0.2
n.training <- 5000
grid.width.and.height <- 8
n.inputs1 <- grid.width.and.height^2 * 4 ## visual inputs
n.inputs2 <- 10  ## olfactory inputs ## will these be processed using unsupervised like the visual inputs?
trace <- rep(0, times = n.outputs)
sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}
p <- 0.5  # p must be between 0 and 1
          # A value of 0 represents a totally segregated model (visual inputs processed totally separately from olfactory inptus) 
          # A value of 1 represents a totally integrated model (visual inputs and olfactory inputs are integrated and processed together)


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



batch <- function(n){ #how to change for 2 inputs?
    for(i in 1:n){
      p <- select.random.pass()
      for(o in 1:length(p)){
        trace.update(p[[o]])
      }}
  }

batch(n.training)


## helper functions:
  
  convert.four.feature.matrices.to.vector <- function(horizontal, vertical, left.diagonal, right.diagonal){
    h.vec <- as.vector(t(horizontal))
    v.vec <- as.vector(t(vertical))
    ld.vec <- as.vector(t(left.diagonal))
    lr.vec <- as.vector(t(right.diagonal))
    return(c(h.vec, v.vec, ld.vec, lr.vec))
  }

convert.vector.to.four.feature.matrices <- function(v){
  n <- grid.width.and.height^2
  h.mat <- matrix(v[1:n], nrow=grid.width.and.height, ncol=grid.width.and.height, byrow=T)
  v.mat <- matrix(v[(n+1):(n*2)], nrow=grid.width.and.height, byrow=T)
  ld.mat <- matrix(v[(n*2+1):(n*3)], nrow=grid.width.and.height, byrow=T)
  lr.mat <- matrix(v[(n*3+1):(n*4)], nrow=grid.width.and.height, byrow=T)
  return(list(
    horizontal = h.mat,
    vertical = v.mat,
    left.diagonal = ld.mat,
    right.diagonal = lr.mat
  ))
}

input.seq.cache <- list()
input.pass.horizontal <- function(){
  if(!is.null(input.seq.cache$horizontal)){
    return(input.seq.cache$horiztonal)
  }
  sequence <- list()
  row <- 1
  for(row in 1:grid.width.and.height){
    b <- matrix(0, nrow=grid.width.and.height, ncol=grid.width.and.height)
    m <- b
    m[row,] <- rep(1, grid.width.and.height)
    v <- convert.four.feature.matrices.to.vector(m, b, b, b)
    sequence[[row]] <- v
  }
  input.seq.cache$horizontal <- sequence
  return(sequence)
}

input.pass.vertical <- function(){
  if(!is.null(input.seq.cache$vertical)){
    return(input.seq.cache$vertical)
  }
  sequence <- list()
  column <- 1
  for(column in 1:grid.width.and.height){
    b <- matrix(0, nrow=grid.width.and.height, ncol=grid.width.and.height)
    m <- b
    m[,column] <- rep(1, grid.width.and.height)
    v <- convert.four.feature.matrices.to.vector(b, m, b, b)
    sequence[[column]] <- v
  }
  input.seq.cache$vertical <- sequence
  return(sequence)
}

input.pass.left.diagonal <- function(){
  if(!is.null(input.seq.cache$left.diagonal)){
    return(input.seq.cache$left.diagonal)
  }
  sequence <- list()
  for(sum in 2:(grid.width.and.height*2)){
    b <- matrix(0, nrow=grid.width.and.height, ncol=grid.width.and.height)
    m <- b
    for(r in 1:grid.width.and.height){
      for(c in 1:grid.width.and.height){
        if(r+c==sum){
          m[r,c] <- 1
        }
      }
    }
    
    v <- convert.four.feature.matrices.to.vector(b, b, m, b)
    sequence[[sum-1]] <- v
  }
  input.seq.cache$left.diagonal <- sequence
  return(sequence)
}

input.pass.right.diagonal <- function(){
  if(!is.null(input.seq.cache$right.diagonal)){
    return(input.seq.cache$right.diagonal)
  }
  sequence <- list()
  i <- 1
  for(difference in (1-grid.width.and.height):(grid.width.and.height-1)){
    b <- matrix(0, nrow=grid.width.and.height, ncol=grid.width.and.height)
    m <- b
    for(r in 1:grid.width.and.height){
      for(c in 1:grid.width.and.height){
        if(r-c==difference){
          m[r,c] <- 1
        }
      }
    }
    
    v <- convert.four.feature.matrices.to.vector(b, b, b, m)
    sequence[[i]] <- v
    i <- i+1
  }
  input.seq.cache$right.diagonal <- sequence
  return(sequence)
}


visualize.weights <- function(v){
  m.list <- convert.vector.to.four.feature.matrices(v)
  plot.data <- data.frame(feature=factor(rep('horizonal',grid.width.and.height^2*4),levels=c("horizontal", "vertical", "left.diagonal", "right.diagonal")), x=numeric(grid.width.and.height^2*4), y=numeric(grid.width.and.height^2*4), weight=numeric(grid.width.and.height^2*4))
  z <- 1
  for(x in 1:grid.width.and.height){
    for(y in 1:grid.width.and.height){
      plot.data[z,]   = c('horizontal',     x, y, m.list$horizontal[x,y])
      plot.data[z+1,] = c('vertical',       x, y, m.list$vertical[x,y])
      plot.data[z+2,] = c('left.diagonal',  x, y, m.list$left.diagonal[x,y])
      plot.data[z+3,] = c('right.diagonal', x, y, m.list$right.diagonal[x,y])
      z <- z + 4
    }
  }
  plot.data$x <- as.numeric(plot.data$x)
  plot.data$y <- as.numeric(plot.data$y)
  plot.data$weight <- as.numeric(plot.data$weight)
  
  ggplot(plot.data, aes(x=x,y=y,shape=feature,alpha=weight))+
    geom_point(size=15)+
    scale_alpha(guide=F)+
    scale_y_continuous(trans="reverse")+
    scale_shape_manual(values=c('-','|','/','\\'), guide=F)
}

select.random.pass <- function(){
  dir <- sample(4, 1)
  if(dir==1){
    seq <- input.pass.horizontal()
  } else if(dir==2) {
    seq <- input.pass.vertical()
  } else if(dir==3) {
    seq <- input.pass.left.diagonal()
  } else if(dir==4) {
    seq <- input.pass.right.diagonal()
  }
  if(sample(2,1)==1){
    seq <- rev(seq)
  }
  return(seq)
}





