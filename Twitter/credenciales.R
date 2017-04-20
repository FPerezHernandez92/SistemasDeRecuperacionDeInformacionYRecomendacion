#Cargamos las librerías

library("ROAuth")
library("base64enc");
library("twitteR");
library("streamR");

#Cargar parámetros de configuración
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache=T)

#Cargar las credenciales obtenidas del paso anterior
consumer_key <- "qsfEl1rZ5vnUyrjPJ2gjXi1Ti"
consumer_secret <-"CXNmHS3MFFeWpcWxtvNxIqr5jDq4JYDycKQJ4BQcVuPTihDajG"
access_token <-"251749544-cWJBRGiFAP21MlghpoQN8FEOQlaH2itCfSzSCzCE"
access_secret <-"0vmLaihQ2fCrOulWEnyFJuKnHzYLhUBhWn2jY4oHOxc9c"


#Ejecutar la autenticación de TwitteR
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#streamR authentication
credentials_file <- "my_oauth.Rdata"
if (file.exists(credentials_file)){
  load(credentials_file)
} else {
  cred <- OAuthFactory$new(consumerKey = consumer_key, consumerSecret =
                             consumer_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)
  cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(cred, file = credentials_file)
}

api_key <- "qsfEl1rZ5vnUyrjPJ2gjXi1Ti"
api_secret <-"CXNmHS3MFFeWpcWxtvNxIqr5jDq4JYDycKQJ4BQcVuPTihDajG"
access_token <-"251749544-cWJBRGiFAP21MlghpoQN8FEOQlaH2itCfSzSCzCE"
access_token_secret <-"0vmLaihQ2fCrOulWEnyFJuKnHzYLhUBhWn2jY4oHOxc9c"