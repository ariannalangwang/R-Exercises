### Ex 4:
fun <- function(num){
  if(num >= 0){
    mod <- num %% 5
    quo <- num %/% 5
    min <- mod + quo
    return(min)
  }
  else{
    return(FALSE)
  }
}
# try out some numbers:
print(fun(11))
print(fun(-2))
print(fun(6))




### Ex 5:
fun <- function(a,b,c){
  v <- c(a,b,c)
  for (i in 1:length(v)){
    if(v[i] %% 3 == 0 ){
      v[i] <- 0
    }else{
      v[i] <- v[i]
    }
  }
  s <- sum(v)
  return(s)
}

# try out some numbers
print(fun(9,9,9))
print(fun(7,6,5))
print(fun(0,0,0))

 






