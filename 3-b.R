na = 172
nb = 43
nab = 11
no = 279

#pa = (1-pb-po)
#theta[1] : pb
#theta[2] : po
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
# -1 * (172*log((1-x-y)^2 + 2*(1-x-y)*y) + 43*log(x^2 + 2*x*y) + 11*log(2*(1-x-y)*x) + 279*log(y^2))
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

Deriv_Qa = function(theta) {
  x = theta[1]
  y = theta[2]
  gr1 = (-452*x^3 + x^2*(549-807*y)+x*(119*y^2+926*y-97)+108*y*(y^2-1)) / (x*(x-y-1)*(x+y-1)*(x+2*y))
  gr2 = -86*x/(x^2+2*x*y) + 11/(-x-y+1) + 334*y/((-x-y+1)^2+2*y*(-x-y+1))-558/y
  
  c(gr1, gr2)
}

za = optim(c(0.3, 0.3), Qa, Deriv_Qa, method = "BFGS")
#za$convergence
#za$par
# pa = 1-pb-po
result_pa = 1 - za$par[1] - za$par[2]

#pb = (1-pa-po)
#theta[1] : pa
#theta[2] : po
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
# -1 * (172*log(x^2 + 2*x*y) + 43*log((1-x-y)^2 + 2*(1-x-y)*y) + 11*log(2*x*(1-x-y)) + 279*log(y^2))
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

Deriv_Qb = function(theta) {
  x = theta[1]
  y = theta[2]
  gr1 = (-452*x^3+x^2*(807-549*y)+x*(377*y^2+926*y-355)+366*y*(y^2-1)) / (x*(x-y-1)*(x+y-1)*(x+2*y))
  gr2 = -344*x/(x^2+2*x*y) + 11/(-x-y+1) + 86*y/((-x-y+1)^2+2*y*(-x-y+1)) -558/y
  
  c(gr1, gr2)
}

zb = optim(c(0.3, 0.3), Qb, Deriv_Qb, method = "BFGS")
#zb$convergence
#zb$par
# pb = 1-pa-po
result_pb = 1 - zb$par[1] - zb$par[2]

#po = (1-pa-pb)
#theta[1] : pa
#theta[2] : pb
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
# -1 * (172*log(x^2 + 2*x*(1-x-y)) + 43*log(y^2 + 2*y*(1-x-y)) + 11*log(2*x*y) + 279*log((1-x-y)^2))
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

Deriv_Qo = function(theta) {
  x = theta[1]
  y = theta[2]
  gr1 = -344*(-x-y+1)/(x^2+2*x*(-x-y+1)) + 86*y/(2*y*(-x-y+1)+y^2) + 558/(-x-y+1) - 11/x
  gr2 = 344*x/(x^2+2*x*(-x-y+1)) -86*(-x-y+1)/(2*y*(-x-y+1)+y^2) + 558/(-x-y+1) - 11/y
  
  c(gr1, gr2)
}

Deriv_Qo2 = function(theta) {
  pa = theta[1]
  pb = theta[2]
  
  f = function(theta) {
    pa = theta[1]
    pb = theta[2]
    
    -1 * (172*log(pa^2 + 2*pa*(1 - pa - pb)) + 43*log(pb^2 + 2*pb*(1 - pa - pb)) + 11*log(2*pa*pb) + 279*log((1 - pa - pb)^2))
  }
  
  df = genD(f, c(pa, pb))
  
  c(df$D[1], df$D[2])
}

zo = optim(c(0.3, 0.3), Qo, Deriv_Qo, method = "BFGS")
#zo$convergence
#zo$par
# po = 1-pa-pb
result_po = 1 - zo$par[1] - zo$par[2]

result = as.data.frame(matrix(c(zo$par[1], zo$par[2], result_po, sum(zo$par[1], zo$par[2], result_po)), nrow = 4))
row.names(result) = c("P(A)", "P(B)", "P(O)", "Total"); colnames(result) = "BFGS+Deriv Result"
result