na = 172
nb = 43
nab = 11
no = 279
n = sum(na, nb, nab, no)

#pa = (1-pb-po)
#theta[1] : pb
#theta[2] : po
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
Qa = function(theta) {
  pb = theta[1]
  po = theta[2]
  pa = 1 - pb - po
  param_A = (pa^2 + 2*pa*po)
  param_B = (pb^2 + 2*pb*po)
  param_AB = (2*pa*pb)
  param_O = (po^2)
  
  if( !(pa > 0 & pa < 1)
      | param_A <= 0
      | param_B <= 0
      | param_AB <= 0
      | param_O <= 0 ) {
    -Inf
  }
  else {
    -1 * (na*log(param_A) + nb*log(param_B) + nab*log(param_AB) + no*log(param_O))  
  }
}
za = optim(c(0.3, 0.3), Qa, method = "BFGS")
#za$convergence
#za$par
# pa = 1-pb-po
result_pa = 1 - za$par[1] - za$par[2]

#pb = (1-pa-po)
#theta[1] : pa
#theta[2] : po
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
Qb = function(theta) {
  pa = theta[1]
  po = theta[2]
  pb = 1 - pa - po
  param_A = (pa^2 + 2*pa*po)
  param_B = (pb^2 + 2*pb*po)
  param_AB = (2*pa*pb)
  param_O = (po^2)
  
  if( !(pb > 0 & pb < 1)
      | param_A <= 0
      | param_B <= 0
      | param_AB <= 0
      | param_O <= 0 ) {
    -Inf
  }
  else {
    -1 * (na*log(param_A) + nb*log(param_B) + nab*log(param_AB) + no*log(param_O))
  }
}
zb = optim(c(0.3, 0.3), Qb, method = "BFGS")
#zb$convergence
#zb$par
# pb = 1-pa-po
result_pb = 1 - zb$par[1] - zb$par[2]

#po = (1-pa-pb)
#theta[1] : pa
#theta[2] : pb
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
Qo = function(theta) {
  pa = theta[1]
  pb = theta[2]
  po = 1 - pa - pb
  param_A = (pa^2 + 2*pa*po)
  param_B = (pb^2 + 2*pb*po)
  param_AB = (2*pa*pb)
  param_O = (po^2)
  
  if( !(po > 0 & po < 1)
      | param_A <= 0
      | param_B <= 0
      | param_AB <= 0
      | param_O <= 0 ) {
    -Inf
  }
  else {
    -1 * (na*log(param_A) + nb*log(param_B) + nab*log(param_AB) + no*log(param_O))
  }
}
zo = optim(c(0.3, 0.3), Qo, method = "BFGS")
#zo$convergence
#zo$par
# po = 1-pa-pb
result_po = 1 - zo$par[1] - zo$par[2]

result = as.data.frame(matrix(c(zo$par[1], zo$par[2], result_po, sum(zo$par[1], zo$par[2], result_po)), nrow = 4))
row.names(result) = c("P(A)", "P(B)", "P(O)", "Total"); colnames(result) = "BFGS Result" 
result