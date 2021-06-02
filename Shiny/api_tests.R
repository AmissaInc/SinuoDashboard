library(httr)
library(dplyr)
library(leaflet)

username = "testuser1"
password = "password1"

app_token <- "Am!ssa1"

broker_api_base_url <- "https://sinuobrokeraf.azurewebsites.net/"
# broker_api_base_url <- "http://localhost:8080/"
broker_af_code = "OlSH9OYxaTamYDu3cArtapVGTLd11aLNK3XHtpaY/kaTL3Pspb4IZQ=="

broker_api_login_path = "api/Login"
broker_api_gdr_path = "api/GetDeviceReads"

broker_api_login_url <- paste0(broker_api_base_url, "Login", "?code=", broker_af_code)

broker_api_login_url == "https://sinuobrokeraf.azurewebsites.net/api/Login?code=OlSH9OYxaTamYDu3cArtapVGTLd11aLNK3XHtpaY/kaTL3Pspb4IZQ%3D%3D"


broker_login_body = list(
  username = username,
  password = password,
  app_token = app_token
  )


broker_login_response <- POST(url = broker_api_base_url,
                             path = broker_api_login_path,
                             query = list(code = broker_af_code),
                             body = broker_login_body,
                             httr::content_type_json(),
                             encode = "json")


user_token <- jsonlite::fromJSON(rawToChar(broker_login_response$content))$user_token
device_id <- jsonlite::fromJSON(rawToChar(broker_login_response$content))$device_id


## GetDeviceReads
broker_gdr_body = list(
  device_id = "6F17A220-9377-4742-B310-67DAC21E060F", #device_id,
  username = username,
  user_token = user_token
)

broker_gdr_response <- POST(url = broker_api_base_url,
                            path = broker_api_gdr_path,
                            query = list(code = broker_af_code),
                            body = broker_gdr_body,
                            httr::content_type_json(),
                            encode = "json")

data <- as.data.frame(jsonlite::fromJSON(rawToChar(broker_gdr_response$content)))

device_reads_pretty <- data %>% 
  select(timestamp,
         latitude,
         longitude,
         altitude,
         gravity,
         rotation,
         userAccel,
         step,
         heartRate,
         motionActivity) %>% 
  mutate(timestamp = as.character(timestamp))


map_fig <- leaflet(device_reads_pretty) %>% 
  addTiles() %>% addMarkers(
    ~longitude, ~latitude,
    label = ~htmlEscape(timestamp)
    clusterOptions = markerClusterOptions()
  )
