na = 172
nb = 43
nab = 11
no = 279

#po = (1-pa-pb)
#theta[1] : pa
#theta[2] : pb
# Q = -1 * (na*log(pa^2 + 2*pa*po) + nb*log(pb^2 + 2*pb*po) + nab*log(2*pa*pb) + no*log(po^2))
Qo2 = function(theta1, theta2) {
  pa = theta1
  pb = theta2
  po = 1 - theta1 - theta2
  
  # recycle argument
  if(length(pa) < length(pb))
    pa = rep(pa, length = length(pb))
  if(length(pb) < length(pa))
    pb = rep(pb, length = length(pa))
  
  getParameter = function (index, param){
    switch (param,
            param_A = pa[index]^2 + 2*pa[index]*(po[index]),
            param_B = pb[index]^2 + 2*pb[index]*(po[index]),
            param_AB = 2*pa[index]*pb[index],
            param_O = po[index]^2
    )
  }
  
  # Vector calculation
  ans = numeric(length(theta1))
  for(i in 1:length(ans)) {
    if( !(po[i] >= 0 & po[i] <= 1)
        | getParameter(i, "param_A") <= 0
        | getParameter(i, "param_B") <= 0
        | getParameter(i, "param_AB") <= 0
        | getParameter(i, "param_O") <= 0 ) {
      ans[i] = -Inf
    }
    else {
      ans[i] = -1 * (na*log(getParameter(i, "param_A")) + nb*log(getParameter(i, "param_B")) + nab*log(getParameter(i, "param_AB")) + no*log(getParameter(i, "param_O")))
    }
  }
  
  ans
}

#zo = optim(c(0.3, 0.3), Qo, Deriv_Qo, method = "BFGS")
#po = (1-pa-pb)
theta1 = seq(0, 0.5, length = 50)
theta2 = seq(0, 0.4, length = 60)
z = outer(theta1, theta2, Qo2)
contour(theta1, theta2, z)
