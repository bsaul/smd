char_generator <- function(n){
  paste0(sample(letters, n, replace = TRUE), sample(999, n))
}
