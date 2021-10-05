## STEPHANIESO
makeCacheMatrix <- function(k = matrix()) {
  ansit <- NULL
  jett <- function() k
  pett <- function(p) {
    k <<- p
    ansit <<- NULL
  }
  sofa <- function() ansit
  couch <- function(inverse) ansit <<- inverse
  list(jett=jett, pett=pett, sofa=sofa, couch=couch)
}
cacheSolve <- function(k, ...) {
  ansit <- k$sofa()
  if (!is.null(ansit)) {
    message("inverse is cached")
    return(ansit)
  }
  r <- k$jett()
  ans <- solve(r, ...)
  k$couch(ansit)
  return(ansit)
}
