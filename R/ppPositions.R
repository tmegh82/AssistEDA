
#' @importFrom stats ppoints
#' @export ppPositions
#' @keywords misc

ppPositions <- function(n, method= c("Gringorton", "Cunane",
                                   "Filliben", "Blom", "Weibull", "ppoints"))
{
    method <- match.arg(method)

    if (method == "Gringorton"){
        m <- numeric(n)
        for (i in 1:n){
            m[i] <- (i - 0.44) / (n + 0.12)
        }
    } else if (method == "Cunane") {
        m <- numeric(n)
        for (i in 1:n){
            m[i] <- (i - 0.4) / (n + 0.2)
        }
    } else if (method == "Filliben"){
        m <- numeric(n)
        m[1] <- 1 - 0.5^(1/n)
        for (i in 2:(n-1)){
            m[i] <- (i - 0.3175) / (n + 0.365)
        }
        m[n] <- 0.5^(1/n)

    } else if (method == "Blom") {
        m <- numeric(n)
        for (i in 1:n){
            m[i] <- (i - 0.375) / (n + 0.25)
        }

    } else if (method == "Weibull"){
        m <- numeric(n)
        for (i in 1:n){
            m[i] <- i / (n + 1)
        }

    #} # else if (method == "Nguyen") {
      #  n <- length(x)
      #  m <- numeric(n)
      #  gamma <- skewness(x)
      #  stopifnot(gamma >= -3 & gamma <= 3 & n >= 5 & n <= 100)
      #  for (i in 1:n){
      #      m[i] <- (i - 0.42) / (n + 0.3 * gamma + 0.05)
      #  }
   # }
    } else if (method == "ppoints"){
        m <- ppoints(n)
    }

    return(m)
}
