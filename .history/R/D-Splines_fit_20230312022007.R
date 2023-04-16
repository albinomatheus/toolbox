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

Dspline_fit = function(N, D,
                       age_group_lower_bounds = 0:99,
                       age_group_upper_bounds = 1:100,
                       Amatrix, cvector, SIGMA.INV,
                       knots          = seq(from=3,to=96,by=3),   
                       max_iter       = 20,
                       theta_tol      = .00005,
                       details        = FALSE) {
  
  require(splines)
  
  # cubic spline basis
  B    = bs(0:99, knots=knots, degree=3, intercept=TRUE)
  # number of spline parameters
  K = ncol(B)  
  
  ## number and width of age groups
  age_group_labels = paste0('[',age_group_lower_bounds,',',age_group_upper_bounds,')')
  
  G     = length(age_group_lower_bounds)   
  nages = age_group_upper_bounds - age_group_lower_bounds
  
  ## weighting matrix for mortality rates (assumes uniform
  ## distribution of single-year ages within groups)
  W = outer(seq(G), 0:99, function(g,x){ 1*(x >= age_group_lower_bounds[g])*
      (x <  age_group_upper_bounds[g])}) %>% 
    prop.table(margin=1)
  
  dimnames(W) = list(age_group_labels , 0:99)
  ## penalized log lik function
  pen_log_lik = function(theta) {
    
    lambda.hat = as.numeric( B %*% theta)
    eps        = Amatrix %*% lambda.hat - cvector 
    penalty    = 1/2 * t(eps) %*% SIGMA.INV %*% eps
    
    M    = W %*% exp(B %*% theta)   # mortality rates by group
    logL = sum(D * log(M) - N * M)
    return(logL  - penalty)
  }
  
  ## expected deaths function
  Dhat = function(theta) {
    M    = W %*% exp(B %*% theta)   # mortality rates by group
    return(  as.numeric( N * M ))
  }      
  
  ## gradient function (1st deriv of pen_log_lik wrt theta) 
  gradient = function(theta) {
    lambda.hat = as.numeric( B %*% theta)
    eps        = Amatrix %*% lambda.hat - cvector
    
    mx    = exp(lambda.hat)
    Mg    = as.numeric(W %*% mx)
    X     = W %*% diag(mx) %*% B
    return( t(X) %*% diag(1/Mg) %*% (D-Dhat(theta)) - 
              t(B) %*% t(Amatrix) %*% SIGMA.INV %*% eps )
  }
  
  hessian = function(theta) {
    
    mu     = as.vector( exp(B %*% theta)) 
    M      = as.vector( W %*% mu) 
    
    Dhat = N * M
    
    construct_zvec = function(k) {
      part1 = diag(B[,k]) %*% diag(mu) %*% t(W) %*% diag(1/M) %*% (D - Dhat)
      part2 = diag(mu) %*% t(W) %*% diag(as.vector(W %*% diag(mu) %*% B[,k])) %*% diag(1/(M^2)) %*% D
      part3 = t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B[,k]
      
      return(part1 - part2 - part3)
    }
    
    Z = sapply(1:K, construct_zvec)
    
    H = t(B) %*% Z
    
    # slight clean-up to guarantee total symmetry
    return( (H + t(H))/2 )
  } # hessian
    
  #------------------------------------------------
  # iteration function: 
  # next theta vector as a function of current theta
  #------------------------------------------------
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
    
    BWB   = t(B) %*% t(W) %*% diag(dhat) %*% W %*% B
    BAVAB = t(B) %*% t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B
    df    = sum( diag( solve(BWB+BAVAB) %*% BWB)) # trace of d[Dhat]/d[D'] matrix
    
    lambda.hat = B %*% th
    
    dev = 2 * sum( (D>0) * D * log(D/dhat), na.rm=TRUE)
      
    return( list( N                = N,
                  D                = D,
                  
                  age_group_lower_bounds = age_group_lower_bounds,
                  age_group_upper_bounds = age_group_upper_bounds,
                  
                  B                = B,
                  theta            = as.vector(th), 
                  lambda.hat       = as.vector(lambda.hat),
                  gradient         = as.vector(g),
                  dev              = dev,
                  df               = df,
                  bic              = dev + df * log(length(D)),
                  aic              = dev + 2*df,
                  fitted.values    = as.vector(dhat),
                  obs.values       = D,
                  obs.expos        = N,
                  hessian          = H,
                  covar            = solve(-H), 
                  pen_log_lik      = pen_log_lik(th),
                  niter            = niter,
                  converge         = converge, 
                  maxiter          = overrun))
  } else return( th ) 
  
} # Dspline_fit
