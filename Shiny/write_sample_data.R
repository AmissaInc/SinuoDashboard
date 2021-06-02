library(mongolite)
library(readxl)

data <- read_excel("example_data.xlsx", sheet = "device_reads")

username = "shiny_user"
password = "Sh1ny"
cluster = "testcluster1"
database = "SinuoDB"
collection = "device_reads"

url = paste0("mongodb+srv://",
             username,
             ":",
             password,
             "@",
             cluster,
             ".ort2e.mongodb.net/",
             database,
             "?retryWrites=true&w=majority")

db <- mongo(collection = collection,
            url = url)

db$insert(data)


results <- db$find()


query <- db$find(
  query = '{"device_id" : 123}', 
  fields = '{"device_id" : true, "sensor1" : true}',
  limit = 5
)
