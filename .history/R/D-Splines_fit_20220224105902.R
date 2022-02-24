#' D-Splines fitting function for mortality schedules (by Carl Schmertmann)
#'
#' Fits parameters to single-year or age-group (D,N) 
#'
#'  + added fitted log mort, basis, etc to detailed output
#'  + added ability to fit age-grouped as well as single-yr data
#'  + changed fitting algorithm from Newton-Raphson 
#'      to (penalized) IRLS
#'  age_group_bounds is a (G+1) vector of ages v that define
#'  G closed age groups [v1,v2), [v2,v3)... [vG,vG+1)
#' For ex. if age_group_bounds = c(0,1,5,10,15,...,90)
#'  then the age groups are [0,1), [1,5), [5,10), ..., [85,90)
#
#'  A more complete explanation is in 
#'  https://github.com/schmert/TOPALS/blob/master/TOPALS_fitting_with_grouped_data.pdf
#'
#' @export

require('splines')
 
Dspline_fit = function(N, D, 
                       Amatrix, cvector, SIGMA.INV,
                       knots          = seq(from=3,to=96,by=3),   
                       max_iter       = 20,
                       use_MS_basis   = FALSE,
                       theta_tol      = .00005,
                       details        = FALSE) {
  
  require(splines)
  
  # cubic spline basis
  B    = bs(0:99, knots=knots, degree=3, intercept=TRUE)

  # override and use the Mortality1DSmooth basis function?
  if (use_MS_basis) {
    B = MortalitySmooth::MortSmooth_bbase(x   =  0:99, 
                                          xl  = -0.99, 
                                          xr  =  99.99, 
                                          ndx =  33, 
                                          deg =  3)  
  }

  # number of spline parameters
  K = ncol(B)  
  
  ## penalized log lik function
  pen_log_lik = function(theta) {
    
    lambda.hat = as.numeric( B %*% theta)
    eps        = Amatrix %*% lambda.hat - cvector 
    penalty    = 1/2 * t(eps) %*% SIGMA.INV %*% eps
    
    return( sum(D * lambda.hat - N * exp(lambda.hat)) - penalty)
  }
  
  ## expected deaths function
  Dhat = function(theta) {
    lambda.hat = as.numeric(B %*% theta)
    return(  as.numeric( N * exp(lambda.hat) ))
  }      
  
  ## gradient function (1st deriv of pen_log_lik wrt theta) 
  gradient = function(theta) {
    lambda.hat = as.numeric( B %*% theta)
    eps        = Amatrix %*% lambda.hat - cvector 
    return( t(B) %*% (D-Dhat(theta)) - 
              t(B) %*% t(Amatrix) %*% SIGMA.INV %*% eps )
  }
  
  ## Hessian function (2nd deriv of pen_log_lik wrt theta) 
  hessian = function(theta) {
    lambda.hat = as.numeric( B %*% theta)
    return(   -t(B) %*% diag(Dhat(theta)) %*% B - 
              t(B) %*% t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B )
  }
  
  #------------------------------------------------
  # iteration function: 
  # next theta vector as a function of current theta
  #------------------------------------------------
#  orig_next_theta = function(theta) {
#    return( theta - solve(hessian(theta) , gradient(theta) ))
#  }
  
  next_theta = function(theta) {
    H = hessian(theta)
    return( as.vector( solve( H, H %*% theta - gradient(theta) )))
  }
  
  
  ## main iteration:     
  th = rep( log(sum(D)/sum(N)), K)  #initialize at overall avg log rate
  niter = 0
  
  repeat {
    
    niter      = niter + 1
    last_param = th
    th         = next_theta( th )  # update
    change     = th - last_param
    
    converge = all( abs(change) < theta_tol)
    overrun  = (niter == max_iter)
    
    if (converge | overrun) { break }
    
  } # repeat
  
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    
    dhat = Dhat(th)
    H    = hessian(th)
    g    = gradient(th)
    
    BWB   = t(B) %*% diag(dhat) %*% B
    BAVAB = t(B) %*% t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B
    df    = sum( diag( solve(BWB+BAVAB) %*% BWB)) # trace of d[Dhat]/d[D'] matrix
    
    lambda.hat = B %*% th
    
    dev = 2 * sum( (D>0) * D * log(D/dhat), na.rm=TRUE)
    
    return( list( B             = B,
                  theta         = as.vector(th), 
                  lambda.hat    = as.vector(lambda.hat),
                  gradient      = as.vector(g),
                  dev           = dev,
                  df            = df,
                  bic           = dev + df * log(length(D)),
                  aic           = dev + 2*df,
                  fitted.values = as.vector(dhat),
                  obs.values    = D,
                  obs.expos     = N,
                  hessian       = H,
                  covar         = solve(-H), 
                  pen_log_lik   = pen_log_lik(th),
                  niter         = niter,
                  converge      = converge, 
                  maxiter       = overrun))
  } else return( th ) 
  
} # Dspline_fit

#' functions from MCMCpack to collapse and
#' re-expand symmetric matrices
#' @export
vech = function (x) 
{
  x <- as.matrix(x)
  if (dim(x)[1] != dim(x)[2]) {
    stop("Non-square matrix passed to vech().\n")
  }
  output <- x[lower.tri(x, diag = TRUE)]
  dim(output) <- NULL
  return(output)
}

#' functions from MCMCpack to collapse and
#' re-expand symmetric matrices
#' @export
xpnd = function (x, nrow = NULL) 
{
  dim(x) <- NULL
  if (is.null(nrow)) 
    nrow <- (-1 + sqrt(1 + 8 * length(x)))/2
  output <- matrix(0, nrow, nrow)
  output[lower.tri(output, diag = TRUE)] <- x
  hold <- output
  hold[upper.tri(hold, diag = TRUE)] <- 0
  output <- output + t(hold)
  return(output)
}