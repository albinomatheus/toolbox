#' Extract migration schedules (generalization of Tim Riffe's mig_un_fam)
#'
#' Smooth population counts using PCLM method
#'
#' @export
#' 

get_mig_structure <- function(NM, family, Single = TRUE, OAnew = 100, data = DemoTools::mig_un_families,
                    params = DemoTools::mig_un_params){
  
  sex             <- NULL
  age             <- NULL
  .               <- NULL
  
  mig_un_families <- data
  mig_un_params   <- params
  
  mig_sign        <- ifelse(NM < 0, "Emigration", "Inmigration")
  
  ind         <- mig_un_params$family == family &
    mig_un_params$mig_sign == mig_sign
  this_params <- mig_un_params[ind,   c("family","sex","param","median")]
  
  ind         <- mig_un_families$family == family &
    mig_un_families$mig_sign == mig_sign
  this_family <- mig_un_families[ind,  c("family","sex","age","prop")]
  
  this_family$prop <- this_family$prop + this_family$prop/sum(this_family$prop) * (1-sum(this_family$prop))
  
  this_family$nm   <- this_family$prop * NM
  this_family$prop <- NULL

  this_family <- as.data.table(this_family)
  this_family <- this_family[, .(nm = groupOAG(nm, age, OAnew = OAnew), age = 0:OAnew), by = list(family, sex)]

  if(!Single){
    nm              <- NULL
    this_family$age <- trunc(this_family$age/5)*5
    this_family     <- setDT(this_family)[order(sex,age), .(nm=sum(nm)),
                                          by=.(family, age, sex)] %>% as.data.frame()
  }
  
  list(net_migr = this_family)
}
