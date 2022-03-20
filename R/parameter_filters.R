
fixed_parameters <- function(params) {
    params$names[params$isFixed]
}

variable_parameters <- function(params) {
    params$names[!params$isFixed]
}

discrete_parameters <- function(params) {
    params$names[params$types %in% c("i", "c", "o")]
}

non_numeric_parameters <- function(params) {
    params$names[params$types %in% c("c", "o")]
}

continuous_parameters <- function(params) {
    params$names[params$types %in% c("r")]
}

parameters_not <- function(params_filter) {
    function (params) {
        params$names[!(params$names %in% params_filter(params))]
    }
}

parameters_all <- function(...) {
    function (params) {
        f_params <- lapply(list(...), function(f) f(params))
        Reduce(function(x, y) intersect(x, y), f_params)
    }
}

parameters_any <- function(...) {
    function (params) {
        f_params <- lapply(list(...), function(f) f(params))
        Reduce(function(x, y) union(x, y), f_params)
    }
}