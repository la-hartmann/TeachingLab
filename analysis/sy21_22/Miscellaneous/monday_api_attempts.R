library(httr)
library(jsonlite)
library(lubridate)

monday_api <- function(path) {
  
  url <- modify_url("https://api.monday.com/v2", path = path)
  
  resp <- GET(url, 
              authenticate(user = Sys.getenv("MONDAY_CLIENT_ID"),
                                password = Sys.getenv("MONDAY_CLIENT_SECRET")))
  print(resp)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "monday_api"
  )
  
}

monday_api("boards/2208860812")


create_oauth_token <- function(){
  
  endpoint <- oauth_endpoint(request = "https://auth.monday.com/oauth2/authorize?",
                             authorize = "https://auth.monday.com/oauth2/authorize",
                             access = "https://auth.monday.com/oauth2/token")
  
  
  client_id <- getOption("MondayR.client_id", "")
  client_secret <- getOption("MondayR.client_secret", "")
  cache <- getOption("MondayR.oauth_cache", "")
  
  if(client_id == ""){
    stop('options("MondayR.client_id") has not been set.', call. = FALSE)
  }
  if(client_secret == ""){
    stop('options("MondayR.client_secret") has not been set.', call. = FALSE)
  }
  if(cache == ""){
    stop('options("MondayR.oauth_cache") has not been set.', call. = FALSE)
  }
  
  app <- oauth_app(
    appname = "Monday",
    key = client_id,
    secret = client_secret
  )
  
  redirect_uri <- "http://localhost:7325/oauth/callback"
  
  tryCatch({
    token <- oauth2.0_token(
      endpoint = endpoint,
      app = app,
      use_oob = T,
      oob_value = redirect_uri,
      cache = cache
    )
    
    stopifnot(is_valid_token(token))
    
    MondayAuth$set("public", "token", token, overwrite=TRUE)
    MondayAuth$set("public", "method", "new_token", overwrite=TRUE)
    
    return(invisible(token))
  }, error = function(e){
    cat(crayon::red("Authentication error. Check the provided credentials and try again!\n"))
  })
}

