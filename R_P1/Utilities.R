InfToNA <- function(x){
  x[is.infinite(x)] <- NA
  x
}