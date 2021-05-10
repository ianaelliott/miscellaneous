k_func <- function(a, d, k) {
  v <- a/(1+(k*d))
  v_out <- round(v,0)
  return(v_out)
}

k_func(55, 117, 0.00016)
# END #
