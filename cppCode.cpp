


#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

float sigmoidActivation(float x){
  return (1 / (1+exp(-x)));
}

// [[Rcpp::export]]
NumericVector forwardPass(NumericVector input, int nOutputs, NumericMatrix weights){
  
  NumericVector output(nOutputs);
  for(int j=0; j<nOutputs; j++){
    output[j] = 0;
    for(int k=0; k<input.size(); k++){
      double w = weights(k,j);
      if(w >= 0){
        output[j] += input[k] * w;
      }
    }
    output[j] = sigmoidActivation(output[j]); 
  }
  
  int halfway = nOutputs/2;
  IntegerVector firstHalf(nOutputs/2);
  IntegerVector secondHalf(nOutputs/2);
  for(int i=0; i<halfway; i++){
    firstHalf[i] = i;
    secondHalf[i] = halfway + i;
  }
  
  NumericVector o1 = output[firstHalf];
  NumericVector o2 = output[secondHalf];
  
  o1[which_max(o1)] = 1;
  o1[o1 != 1] = 0;
  
  o2[which_max(o2)] = 1;
  o2[o2 != 1] = 0;
  
  for(int i=0; i<halfway; i++){
    output[i] = o1[i];
    output[i+halfway] = o2[i];
  }
  
  return output;
}

// [[Rcpp::export]]
List traceUpdate(NumericVector input, NumericVector trace, NumericMatrix weights, double traceParam, double learningRate){
  NumericVector output = forwardPass(input, trace.size(), weights);
  
  for(int i = 0; i<trace.size(); i++){
    trace[i] = (1 - traceParam) * trace[i] + traceParam * output[i];
    for(int j=0; j<input.size(); j++){
      double w = weights(j,i);
      if(w >= 0){
        weights(j,i) = w + learningRate * trace[i] * input[j] - learningRate * trace[i] * w;  
      }
    }
  }
  
  //return weights
  return List::create(Rcpp::Named("weights") = weights,
                      Rcpp::Named("trace") = trace);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
n.input <- 20
n.output <- 6
learning.rate <- 0.1
trace.param <- 0.5
trace <- rep(0, n.output)
  weights <- matrix(runif(n.input*n.output), nrow=n.input, ncol=n.output)
  weights[4,2] <- -100
forwardPass(rbinom(n.input, 1, 0.2), n.output, weights)
  
  traceUpdate(rbinom(n.input,1,0.2), trace, weights, trace.param, learning.rate)
  */