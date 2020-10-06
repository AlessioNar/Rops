#' @title Generate access token
#'
#' @description Generation of access token for OPS authentication. The access token expires every 20 minutes.
#'
#' @param consumer_key Alphanumeric consumer key provided by OPS
#' @param consumer_secret_key Alphanumeric secret provided by OPS
#'
#' @return Access token in a string format
#'
#' @examples \dontrun{create_access_token(consumer_key, consumer_secret_key)}
#'
#'
#' @import httr
#' @import base64enc
#'
#' @export create_access_token

create_access_token<-function(consumer_key, consumer_secret_key){

  #convert to base64 encode
  auth_encoded <- base64enc::base64encode(charToRaw(paste0(consumer_key, ':', consumer_secret_key)))
  #create html head
  heads <- c(auth_encoded, 'application/x-www-form-urlencoded')
  names(heads) <- c('Authorization', 'content-type')
  #authenticate
  auth <- httr::POST(url = 'https://ops.epo.org/3.2/auth/accesstoken',  #READ BELOW
               httr::add_headers(heads), body = 'grant_type=client_credentials')
  #extract access token
  access_token<-as.list(content(auth))[[9]]

  return(access_token)
}
