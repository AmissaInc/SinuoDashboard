library(mongolite)

username = "shiny_user"
password = "Sh1ny"
cluster = "testcluster1"
database = "SinuoDB"
# database = "sample_analytics"
collection = "device_reads"
# collection = "customers"

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
data <- db$find()

loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collection,
              url = url)
  # Read all the entries
  data <- db$find()
  data
}
