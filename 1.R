#recursive
gcd <- function(x,y) {
  if(length(x) < length(y))
    x = rep(x, length = length(y))
  if(length(y) < length(x))
    y = rep(y, length = length(x))
  
  remainder = x%%y
  return(ifelse(remainder, gcd(y, remainder), y))
}

gcd(6, 15)
gcd(6:10, 15:25)



gcd = function(a, b) {
  if(a > b) { larger = a; smaller = b }
  else { larger = b; smaller = a }
  
  remainder = larger %% smaller
  
  if(remainder != 0) {
    gcd(remainder, smaller)
  }
  else {
    smaller
  }
  #c(larger, smaller)
}
gcd(6, 15)