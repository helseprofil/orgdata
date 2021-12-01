#' Environment to store log info
#' export log
log <- new.env()


## Helper --------------
is_log <- function(value = NULL, x = NULL){
  # value - Object to put in log
  # x - name the object
  assign(x = x, value = value, envir = log)
}
