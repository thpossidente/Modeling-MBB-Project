#Create function with parameter p that creates matrix of weights. 
n.input1 <- 10
n.input2 <- 10
n.hidden1 <- 25
n.hidden2 <- 25
n.output <- 10

rows1 <- 5
rows2 <-5
columns1 <- 6
columns2 <-6
s <- matrix(0, nrow = rows1+rows2, ncol = columns1+columns2)

for(i in (columns1+1):(columns1+columns2)){
  s[,i] <- s[,i] + 1
}

input1.and.input2.to.hidden.weights <- matrix(0, nrow = (n.input1 + n.input2), ncol = (n.hidden1 + n.hidden2))
hidden.to.output <- matrix(0, nrow = (n.hidden1 + n.hidden2), ncol = n.output)

level.of.integration <- function(p){ #p must be between 0 and 1
  
  for(i in 1:n.hidden1){
    input1.and.input2.to.hidden.weights[i,] <- 
      
  }
  
  for(i in (n.hidden1+1):(n.hidden1+n.hidden2)){
    input1.and.input2.to.hidden.weights[i,] <-
  }
  
  for(i in 1:n.input1){
    input1.and.input2.to.hidden.weights[,i] <-
  }
  
  for(i in (n.input1+1):(n.input1+n.input2)){
    input1.and.input2.to.hidden.weights[,i] <-
  }
}


forward.pass <- function(input1, input2){
  
}
