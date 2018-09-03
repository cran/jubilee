#' Standard fault line data sets
#'
#' This method defines a collection of standard fault line data sets that have been analyzed
#' and optimized in the research. It is intended for end users to produce
#' standard regressions, forecasts, and charts quickly.
#'
#' @param name character, the name of the collection. 
#'             If "list" is supplied, the list of names will be returned.
#'             If a numeric array is supplied, it will be converted to a matrix format.
#'             
#' @return numeric, pairs of fault lines, each is c(year, delta)
#'
#' @keywords model
#'
#' @author Stephen H. Lihn
#'
#' @export 
#'
#' @examples
#'   jubilee.std_fault_line("r_nom_f10_5ftr_4fl")
#'   jubilee.std_fault_line("r_nom_f20_5ftr_2fl")
#'   jubilee.std_fault_line("r_nom_f20_5ftr_2fl_ramp5y")
#'
### <======================================================================>
jubilee.std_fault_line <- function(name) {
    if (class(name) == "numeric") return(make_matrix(name))
    if (name == "list") {
        return(c("r_nom_f10_5ftr_1fl",
                 "r_nom_f10_5ftr_2fl",
                 "r_nom_f10_5ftr_3fl",
                 "r_nom_f10_5ftr_4fl", "r_nom_f10_5ftr_4fl_ramp5y",
                 "r_nom_f10_5ftr_6fl",
                 "r_nom_f10_5ftr_8fl",
                 "r_nom_f20_5ftr_1fl",
                 "r_nom_f20_5ftr_2fl", "r_nom_f20_5ftr_2fl_ramp5y", "r_nom_f20_5ftr_2fl_ramp10y",
                 "r_nom_f20_5ftr_3fl",
                 "r_nom_f20_5ftr_4fl",
                 "r_nom_f20_5ftr_6fl",
                 "r_nom_f20_5ftr_7fl",
                 "r_real_f10_1ftr_1fl",
                 "r_real_f10_1ftr_2fl",
                 "r_real_f10_1ftr_3fl",
                 "r_real_f10_1ftr_4fl",
                 "r_real_f10_1ftr_5fl",
                 "r_real_f10_1ftr_6fl",
                 "r_real_f10_1ftr_8fl",
                 "r_real_f10_5ftr_5fl",
                 "cape10_dec_4ftr_4fl",
                 "cape10_rdec_4ftr_4fl",
                 "r_nom_f10_4ftr_5fl",
                 "r_nom_f20_4ftr_1fl",
                 "r_nom_f20_4ftr_2fl"))
    }
    if (!(name %in% jubilee.std_fault_line("list"))) {
        stop(paste("Unregistered fault line collection:", name))
    }
    make_matrix <- function(v) {
        m = matrix(v, nrow=length(v)/2, ncol=2, byrow=TRUE)
        colnames(m) <- c("fraction", "shift")
        m <- data.frame(m)
        as.matrix(m[order(m$fraction),])
    }
    # -----------------------------------
    # 10y nominal return
    if (name == "r_nom_f10_5ftr_1fl") {
        v <- c(1940.33, -1.500) # the most important fault line, R2=0.68
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_2fl") { # R2 = 0.75 from combn
        v <- c(1904.47,  1.250,
               1935.31, -1.499)
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_3fl") { # R2 = 0.79 from combn
        v <- c(1904.99, 1.500,
               1935.50, -1.391,
               1944.40, -1.450)
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_4fl") { # R2 = 0.81 from combn
        v <- c(1907.06,  1.464,
               1935.61, -1.499,
               1944.48, -1.245,
               1985.85, -0.511)
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_4fl_ramp5y") { # R2 = 0.84 from combn
        v <- c(1887.30, -0.729,
               1902.66, 1.101,
               1933.76, -1.096,
               1983.48, -0.403)
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_6fl") { # R2 = 0.86
        v <- c(1906.91,  1.161,
               1921.30,  1.247,
               1925.73, -1.252,
               1935.58, -1.178,
               1944.64, -0.894,
               1986.04, -0.540)
        return(make_matrix(v))
    }
    if (name == "r_nom_f10_5ftr_8fl") {
        v <- c(1920.81, 0.300,
               1935.32,-0.827,
               1944.50,-0.628,
               1888.98,-0.625,
               1906.97, 0.608,
               1973.00,-0.294,
               1986.06,-0.407,
               1998.04, 0.309)
        return(make_matrix(v))
    }
    # -----------------------------------
    # 20y nominal return
    if (name == "r_nom_f20_5ftr_1fl") { # R2 = 0.82
        v <- c(1930.63, -1.500)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_2fl") { # R2 = 0.85
        v <- c(1904.96, -0.984,
               1930.73, -1.499)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_2fl_ramp5y") { # R2 = 0.86
        v <- c(1902.42, -1.077,
               1929.20, -1.499)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_2fl_ramp10y") { # R2 = 0.85
        v <- c(1898.00, -0.793,
               1925.79, -1.499)
        return(make_matrix(v))
    }

    if (name == "r_nom_f20_5ftr_3fl") { # R2 = 0.886
        v <- c(1905.29, -1.488,
               1930.69, -1.493,
               1976.72, -0.939)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_4fl") { # R2 = 0.89 as the second best
        v <- c(1905.28, -1.434,
               1930.86, -1.499,
               1954.52,  0.707,
               1976.81, -0.848)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_6fl") { # R2 = 0.886
        v <- c(1930.69, -1.493,
               1905.29, -1.488,
               1976.72, -0.939)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_5ftr_7fl") {
        v <- c(1930.74, -1.500,
               1934.40, -0.863,
               1905.36, -1.468,
               1976.82, -0.980,
               1954.35,  1.091,
               1917.85,  0.610,
               1988.55,  0.646)
        return(make_matrix(v))
    }
    # -----------------------------------
    # CAPE 1-factor model for r10 real prediction, R2=0.34
    if (name == "r_real_f10_1ftr_1fl") { # R2=0.46, better
        v <- c(1985.85, -0.533) 
        return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_2fl") { # R2 = 0.52, marginally better
        v <- c(1903.46,  0.318,
               1985.95, -0.602)
        return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_3fl") { # R2 = 0.64, a decent upgrade from 2fl
        v <- c(1907.06,  0.611,
               1925.60, -0.423, # instead of 1940, R2 = 0.63
               1986.81, -0.516)
        return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_4fl") { # R2 = 0.75
        v <- c(1904.46,  0.454,
               1944.32, -0.590,
               1963.89,  0.582,
               1985.83, -0.737)
        return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_5fl") { # R2 = 0.82
        v <- c(1907.21,  0.788,
               1915.67, -0.451,
               1944.50, -0.503,
               1963.92, 0.583,
               1985.85, -0.735)
    return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_6fl") { # R2 = 0.84
        v <- c(1907.19,  0.853,
               1915.68, -0.526,
               1944.44, -0.523,
               1963.96,  0.768,
               1976.15, -0.315,
               1985.99, -0.577)
        return(make_matrix(v))
    }
    if (name == "r_real_f10_1ftr_8fl") { # R2 = 0.86
        v <- c(1903.44,  0.303,
               1907.61,  0.616,
               1915.64, -0.555,
               1944.63, -0.642,
               1956.59,  0.293,
               1964.30,  0.629,
               1976.11, -0.361,
               1986.01, -0.566)
        return(make_matrix(v))
    }
    # -----------------------------------
    if (name == "r_real_f10_5ftr_5fl") { # R2 = 0.86
        v <- c(1903.14,  0.685,
               1907.07,  0.613,
               1926.50, -0.833,
               1964.46,  0.687,
               1986.86, -0.853)
        return(make_matrix(v))
    }
    # -----------------------------------
    # CAPE decomposition in 4-factor model
    # log.cape10 ~ Y(t) + ... This model is not stable
    if (name == "cape10_dec_4ftr_4fl") { 
        v <- c(1986.42, -0.378,
               1894.38, -0.508,
               2009.26, -0.122,
               1937.78,  0.223)
        return(make_matrix(v))
    }
    # Y(t) ~ log.cape10 + ... This model is more stable since Y(t) constrains fault lines
    if (name == "cape10_rdec_4ftr_4fl") { 
        v <- c(1895.10, -0.486,
               1937.98,  0.236,
               1986.56, -0.380,
               2008.82, -0.147)
        return(make_matrix(v))
    }
    # r_f10^nom ~ Y.adj + ...
    if (name == "r_nom_f10_4ftr_5fl") { 
        v <- c(1903.66,  0.524,
               1935.37, -0.527,
               1944.57, -0.467,
               1986.79, -0.392,
               1992.23,  0.436)
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_4ftr_1fl") { 
        v <- c(1930.21, -1.023) # R^2 = 0.84
        return(make_matrix(v))
    }
    if (name == "r_nom_f20_4ftr_2fl") { 
        v <- c(1905.61, -0.325, # R^2 = 0.85
               1930.35, -0.914)
        return(make_matrix(v))
    }
    
    stop(paste("Unknown fault line collection:", name))
}
### <---------------------------------------------------------------------->
