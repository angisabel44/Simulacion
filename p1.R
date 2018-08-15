pos <- 0
dur <- 10
for (t in 1:dur) {
  if (runif(1) < 0.5) {
    pos <- pos + 1
  } else {
    pos <- pos - 1
  }
  print(pos)
}