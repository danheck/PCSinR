// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;



// activation function (vector input => vector output)
// [[Rcpp::export]]
arma::vec activation_function(arma::vec activation,
                              arma::vec input,
                              double decay=0.1,
                              double floor=-1.,
                              double ceiling = 1.) {
  arma::vec output =input;
  for(int i=0; i< input.n_elem; ++i){
    if(input(i) < 0){
      output(i) = activation(i)*(1-decay) + input(i)*(activation(i) - floor);    // input<0
    }else{
      output(i) = activation(i)*(1-decay) + input(i)*(ceiling - activation(i));  // input>0
    }
  }
  // vectorized, slightly slower:
  // arma::vec bound = ceiling/2*(sign(input)+1) - floor/2*(sign(-input)+1);
  // arma::vec output = (1-decay)*activation + input % (bound + activation % sign(-input));
  return output;
}


// update activation of nodes
// [[Rcpp::export]]
arma::vec update_activation(arma::vec activation,
                            arma::mat weights,
                            double decay=0.1,
                            double floor=-1.,
                            double ceiling = 1.){

  // sum of incoming activation
  arma::vec input = weights.t() * activation;

  // update activations of nodes
  arma::vec activation_updated = activation_function(activation, input, decay, floor, ceiling);

  return activation_updated;
}

// energy: check convergence
// [[Rcpp::export]]
bool check_convergence(arma::vec energy,
                       int iter,
                       double stability=10^-6,
                       String convergence= "floor"){
  if (iter < 10)
    return false;

  // Floor (=> Marc Jekel)
  if(convergence == "floor"){// [[Rcpp::export]]
    int count = sum(floor(1/stability * energy( arma::span(iter-10,iter-1)))   -
                    floor(1/stability * energy(iter))  == 0);
    return count == 10;
  }

  // Sum of consecutive differences
  if(convergence == "sum"){
    arma::vec changes = diff(energy(arma::span(iter-10,iter)));
    return max(arma::abs(changes)) < stability;
  }

  if(convergence == "max"){
    arma::vec diffs = arma::abs(energy(arma::span(iter-10,iter-1)) - energy(iter));
    return arma::all(diffs < stability) ;
  }
  return true;
}

// [[Rcpp::export]]
bool convergence_floor(arma::vec energy,
                       int iter,
                       double stability=10^-6){
  if (iter < 10)
    return false;

  return sum(floor(1/stability * energy( arma::span(iter-10,iter-1)))   -
             floor(1/stability * energy(iter))  == 0) == 10;
}


// generic PCS iteration algorithm
// [[Rcpp::export]]
Rcpp::List pcs_matrix_cpp(arma::mat weights,
                          arma::vec start,
                          arma::vec reset,
                          double decay=.1,
                          int maxiter = 1000,
                          double stability=10^-6,
                          String convergence= "floor",
                          bool full=false){

  // initialize matrices:
  int dim = weights.n_cols;
  bool converged;
  arma::vec energy = arma::zeros(maxiter);
  arma::vec input,activ = start;
  arma::mat process = arma::zeros(maxiter,dim);
  process.row(0) = start.t();
  arma::uvec reset_idx = find(reset != 0);
  activ(reset_idx) = reset(reset_idx);

  int iter = 0;
  while(iter < maxiter){

    // activation for each node for t + 1
    // SLIGHTLY SLOWER: activ = update_activation(activ, weights);
    // FASTER:
    input = weights.t() * activ;
    activ = activation_function(activ, input, decay);
    // set activation of source node to 1
    activ(reset_idx) = reset(reset_idx);

    if(full) process.row(iter+1) = activ.t();

    // energy at t + 1
    energy(iter) = arma::as_scalar(- activ.t() * weights * activ);
    // Rcpp::Rcout << "Energy:  " << energy(iter) << " in Iteration " << iter << "\n";

    // convergence?
    if(check_convergence(energy, iter, stability, convergence)){
      iter +=1;
      break;
    }
    iter +=1;
  }
  return Rcpp::List::create(Rcpp::Named("iterations") = iter,
                            Rcpp::Named("energy") = energy(iter-1),
                            Rcpp::Named("activation") = activ,
                            Rcpp::Named("process") = process.rows(0,iter),
                            Rcpp::Named("weights") = weights,
                            Rcpp::Named("convergence") = convergence);
}

