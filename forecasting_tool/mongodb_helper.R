# Read data from mongodb ####
mongo_read <- function(table, db, url){
  # Load the mongolite package
  library(mongolite)
  
  # Connect to MongoDB
  mongo_connection <- mongo(collection = table, db = db, url = url)
  
  # Retrieve all documents
  data <- mongo_connection$find()
  
  # Disconnect from MongoDB
  mongo_connection$disconnect()
  
  return(data)
}

# write to mongodb for appending ####
mongo_append <- function(df, table, db, url){
  # Load the mongolite package
  library(mongolite)
  
  # Connect to MongoDB
  mongo_connection <- mongo(collection = table, db = db, url = url)
  
  # Insert the data into MongoDB
  mongo_connection$insert(df)
  
  # Verify the data was inserted
  retrieved_data <- mongo_connection$find()
  print(head(retrieved_data))
  
  # Disconnect from MongoDB
  mongo_connection$disconnect()
}

# write to mongodb for creating new table ####
mongo_create <- function(df, table, db, url){
  # Load the mongolite package
  library(mongolite)
  
  # Connect to MongoDB
  mongo_connection <- mongo(collection = table, db = db, url = url)
  
  # Find if exists
  retrieved_data <- mongo_connection$find()
  if(nrow(retrieved_data) > 0){
    mongo_connection$drop()
  }
  
  # Insert the data into MongoDB
  mongo_connection$insert(df)
  
  # Verify the data was inserted
  retrieved_data <- mongo_connection$find()
  print(head(retrieved_data))
  
  # Disconnect from MongoDB
  mongo_connection$disconnect()
}

# list of tables ####
mongo_list <- function(db, url){
  # Load the mongolite package
  library(mongolite)
  # Connect to MongoDB
  conn <- mongo(db = db, url = url)
  collections <- conn$run('{"listCollections": 1}')
  collection_names <- collections$cursor$firstBatch$name
  return(collection_names)
}