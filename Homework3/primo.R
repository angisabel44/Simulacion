primo <- function(n) {
  if (n < 4) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, n -1), 2)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

primo2 <- function(n) {
  if (n < 4) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}