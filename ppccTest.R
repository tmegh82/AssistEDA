#' @keywords htest
#' @keywords npar
#'
#' @useDynLib 'ppcc', .registration = TRUE
#'
#' @export ppccTest
#'
#' @importFrom stats qnorm qweibull qgamma qexp qunif qlnorm qlogis
#' @importFrom stats qcauchy


ppccTest <- function(x, qfn =  c("qnorm", "qlnorm", "qunif", "qexp",
                                 "qcauchy", "qlogis", "qgumbel",
                                 "qweibull", "qpearson3", "qgev",
                                 "qkappa2", "qrayleigh", "qglogis"),
                     shape = NULL, ppos = NULL, mc = 10000)
{

    qfn <- match.arg(qfn)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1L)
        stop("not enough 'x' data")
    if(!is.character(qfn))
        stop("'qfn' must be a string naming a valid function")

    if (qfn == "qnorm"){
        ##
        if (is.null(ppos)) ppos <- "Blom" #Default, user may change to Filliben
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Normal distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qnorm(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)
        STATISTIC <- .C("pmcor", x = qe, y = q, n = n, res = res)$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        PVALUE <- .C("ppcctest_norm", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval)$pval

        ##   if (ppos == "Blom"){
        ## From Eq. 5a of Heo et al. (2008, p.4)
        ##       f <- 1 / exp(1.29 + 0.283 * 0.05 +
        ##           (0.887 - 0.751 * 0.05 + 3.21 * 0.05 * 0.05) * log(n))
        ##       ESTIMATE <- 1 - f
        ##       names(ESTIMATE) <- "r(alpha < 0.05)"
        ##   } else {
        ESTIMATE <- NULL
        ##   }
    } else  if (qfn == "qlnorm"){
        if (is.null(ppos)) ppos <- "Blom"  ## Blom's plotting points
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a log-Normal distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qlnorm(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_lnorm", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL
    } else if (qfn == "qunif"){
        ## Weibull plotting positions
        ## R. M. Vogel, C. N. Kroll (1989), Low-flow frequency analysis
        ## using probability-plot correlation coefficients,
        ## Journal of Water Resources Planning and Management 115, 338--357.
        ## see p. 342
        if (is.null(ppos)) ppos <- "Weibull"  ## Weibulls plotting points
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Uniform distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qunif(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_unif", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    } else if (qfn == "qrayleigh"){
        ## Gringorton's plotting positions
        if (is.null(ppos)) ppos <- "Gringorton"  ##
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Rayleigh distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- sqrt(-2 * log(1 - pe))
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_rayleigh", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    }   else if (qfn == "qexp"){
        ## Exponential distribution
        if (is.null(ppos)) ppos <- "Gringorton"  ## Weibulls plotting points
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from an Exponential distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qexp(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_exp", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    } else if (qfn == "qlogis"){
        ## Logistic distribution
        if (is.null(ppos)) ppos <- "Blom" # similar to Normal
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Logistic distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qlogis(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_logis", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    } else if (qfn == "qcauchy"){
        ## Cauchy distribution
        if (is.null(ppos)) ppos <- "Gringorton"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Cauchy distribution")

        ## Get ppcc first
        ## theoretical quantiles
        q <- qcauchy(pe)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_cauchy", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    } else if (qfn == "qgumbel"){
        ## Gumbel distribution
        if (is.null(ppos)) ppos <- "Gringorton"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME, " differs from a Gumbel distribution")

        ## Get ppcc first
        ## theoretical quantiles for Gumbel
        q <- -log(-log(pe))
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)

        PVALUE <- .C("ppcctest_gumbel", p = pe, ppcc = r, n = n, nmc = mc,
                     pval = pval )$pval
        ESTIMATE <- NULL

    }  else if (qfn == "qpearson3"){
        ## pearson3 distribution

        if(is.null(shape)){
            stop("If 'qpearson3' is selected, the parameter
                  'shape' must be specified.")
        }

        if (is.null(ppos)) ppos <- "Blom"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME,
                              " differs from a Pearson-3 distribution\nwith shape ",
                              shape)

        ## Get ppcc first
        ## theoretical quantiles for the Pearson-3  distribution
        #q <- y(pe, ...)
        if (shape == 0){
            q <- qnorm(pe, 0, 1)
        } else if (shape > 0){
            alpha <- 4 / shape^2
            beta <- 1 / 2 * abs(shape)
            q <- -alpha * beta + qgamma(pe, alpha, scale = beta)
        } else {
            alpha <- 4 / shape^2
            beta <- 1 / 2 * abs(shape)
            q <- alpha * beta - qgamma(1 - pe, alpha, scale = beta)
        }
        q[pe == 0 & shape > 0] <- -2 / shape
        q[pe == 1 & shape < 0] <- -2 / shape
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        shape <- as.double(shape)
        if (shape == 0){
            PVALUE <- .C("ppcctest_norm", p = pe, ppcc = r, n = n, nmc = mc,
                         pval = pval )$pval
        } else {
            PVALUE <- .C("ppcctest_pearson3", p = pe, ppcc = r, shape = shape,
                         sn = n, nmc = mc, pval = pval )$pval
        }
        ESTIMATE <- NULL

    }  else if (qfn == "qweibull"){
        ## Weibull distribution

        if(is.null(shape)){
            stop("If 'qweibull' is selected, the parameter
                  'shape' must be specified.")
        }

        if (is.null(ppos)) ppos <- "Gringorton"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME,
                              " differs from a Weibull distribution\nwith shape ",
                              shape)

        ## Get ppcc first
        ## theoretical quantiles for the Weibull distribution
        #q <- y(pe, ...)
        q <- qweibull(pe, shape=shape, scale=1)
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        shape <- as.double(shape)
        PVALUE <- .C("ppcctest_weibull", p = pe, ppcc = r, shape = shape,
                     sn = n, nmc = mc, pval = pval )$pval
        ESTIMATE <- NULL

    } else if (qfn == "qgev"){
        ## GEV distribution

        if(is.null(shape)){
            stop("If 'qgev' is selected, the parameter
                  'shape' must be specified.")
        }

        if (is.null(ppos)) ppos <- "Cunane"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME,
                              " differs from a Generalized Extreme Value distribution\nwith shape ",
                              shape)

        ## Get ppcc first
        ## theoretical quantiles for the GEV distribution
        #q <- y(pe, ...)
        if (shape == 0){
            ## theoretical quantiles for Gumbel
            q <- -log(-log(pe))
        } else {
            q <- 1 / shape * (1 - ( -log(pe))^shape)
        }
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        shape <- as.double(shape)
        if (shape == 0){
            PVALUE <- .C("ppcctest_gumbel", p = pe, ppcc = r,
                         sn = n, nmc = mc, pval = pval )$pval
        } else {
            PVALUE <- .C("ppcctest_gev", p = pe, ppcc = r, shape = shape,
                         sn = n, nmc = mc, pval = pval )$pval
        }
        ESTIMATE <- NULL


    } else if (qfn == "qglogis"){
        ## GEV distribution

        if(is.null(shape)){
            stop("If 'qglogis' is selected, the parameter
                  'shape' must be specified.")
        }

        if (is.null(ppos)) ppos <- "Gringorton"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME,
                              " differs from a Generalized Logistic distribution\nwith shape ",
                              shape)

        ## Get ppcc first
        ## theoretical quantiles for the Generalized Logistic distribution
        #q <- y(pe, ...)
        if (shape == 0){
            ## theoretical quantiles
            q <- qlogis(pe)
        } else {
            q <- (1 - exp(-shape * log(pe / (1 - pe)))) / shape
        }
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        shape <- as.double(shape)
        if (shape == 0){
            PVALUE <- .C("ppcctest_logis", p = pe, ppcc = r,
                         sn = n, nmc = mc, pval = pval )$pval
        } else {
            PVALUE <- .C("ppcctest_glogis", p = pe, ppcc = r, shape = shape,
                         sn = n, nmc = mc, pval = pval )$pval
        }
        ESTIMATE <- NULL

    } else if (qfn == "qkappa2"){
        ## Mielke's Kappa distribution

        if(is.null(shape)){
            stop("If 'qkappa2' is selected, the parameter
                  'shape' must be specified.")
        }

        if (is.null(ppos)) ppos <- "Gringorton"
        pe <- ppPositions(n, ppos)
        ALTERNATIVE <- paste0(DNAME,
                              " differs from Mielke's Kappa distribution\nwith shape ",
                              shape)

        ## Get ppcc first
        ## theoretical quantiles for the Kappa distribution
        #q <- y(pe, ...)
        if (shape == 0){
            stop("Kappa distribution for 'shape = 0' is not defined")
        } else {
            q <- (shape * pe^shape / (1 - pe^shape))^( 1 / shape)
        }
        ## empirical quantiles
        qe <- sort(x)

        qe <- as.double(qe)
        q <- as.double(q)
        n <- as.integer(n)
        res <- as.double(1.0)

        STATISTIC <- .C("pmcor", x = qe, y = q, n = n,
                        res = res )$res
        r <- as.double(STATISTIC)
        mc <- as.integer(mc)
        pval <- as.double(1.0)
        pe <- as.double(pe)
        shape <- as.double(shape)

        PVALUE <- .C("ppcctest_kappa2", p = pe, ppcc = r, shape = shape,
                     sn = n, nmc = mc, pval = pval )$pval
        ESTIMATE <- NULL
    }


    names(STATISTIC) <- "ppcc"
    PARAMETER <- n
    names(PARAMETER) <- "n"
    METHOD <- "Probability Plot Correlation Coefficient Test"
    ans <- list(data.name = DNAME, method = METHOD, p.value = PVALUE,
                statistic = STATISTIC, alternative = ALTERNATIVE,
                estimate = ESTIMATE, parameter = PARAMETER)
    class(ans) <- "htest"
    #print(paste0("qfn is ", qfn))
    #print(paste0("ppcc value is ", STATISTIC))
    #print(paste0("p value is ", PVALUE))
    #print(paste0("estimate is ", ESTIMATE))
    #print(paste0("parameter is ", PARAMETER))
    #print(paste0("alternative is ", ALTERNATIVE))
    #print(paste0("method is ", METHOD))
    dis_result <- c(STATISTIC, PVALUE)
    return(dis_result)
}
