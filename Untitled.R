#options(error = recover)

bad <- function() {
  fails <- function() stop()
  fails()
  for (i in 1:10)
    fails()
}

bad()