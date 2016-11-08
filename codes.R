#Create function with parameter p that creates matrix of weights. 
n.input1 <- 10
n.input2 <- 10
n.hidden <- 50
n.output <- 10

s <- matrix(0, nrow = 5, ncol = 5)


level.of.integration <- function(p){ #p must be between 0 and 1
  input1.and.input2.to.hidden.weights <- matrix(0, nrow = (n.input1+n.input2), ncol = n.hidden)
  hidden.to.output <- matrix(0, nrow = n.hidden, ncol = n.output)
  for(i in 1:n.input1.and.input2.to.hidden.weights){
    
  }
}


forward.pass <- function(input1, input2){
  
}
