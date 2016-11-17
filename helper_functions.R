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