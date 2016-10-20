library(RJDBC)
library(readr)

# download Amazon Redshift JDBC driver
if (!file.exists('RedshiftJDBC41-1.1.9.1009.jar')) {
  download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','RedshiftJDBC41-1.1.9.1009.jar')
}

# object to connect to Amazon Redshift
psql_conn_object <- function(server, schema, user, password) {
  driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "RedshiftJDBC41-1.1.9.1009.jar", identifier.quote="`")
  # url <- "<JDBCURL>:<PORT>/<DBNAME>?user=<USER>&password=<PW>
  url <- sprintf("jdbc:redshift://%s:5439/%s?ssl=true&sslFactory=org.postgresql.ssl.NonValidatingFactory&user=%s&password=%s",
    server, schema, user, password)
  dbConnect(driver, url)
}

# Gets a psql connection string from user and password
psql_conn_string <- function(server, schema, user, password) {
  sprintf("psql 'host=%s dbname=%s user=%s port=5439 password=%s'", server, schema, user, password)
}

# Copy a file from S3 into Redshift
# psql is the psql connection string obtained from psql_conn_string
s3_to_redshift <- function(psql, s3_file, table, aws_secret_id, aws_secret_key) {
  cmd <- sprintf("%s -c \"COPY %s FROM '%s' CREDENTIALS 'aws_access_key_id=%s;aws_secret_access_key=%s' DELIMITER ',' IGNOREHEADER 1 ACCEPTINVCHARS '?' TRUNCATECOLUMNS STATUPDATE ON;\""
    , psql, table, s3_file, aws_secret_id, aws_secret_key)
  system(cmd)
}

# Execute a sql script from a file
# Parameters:
#   - psql : obtained from `psql_conn_string`
#   - script_file : the script file to execute
#   - args : optional arguments in the format of -v key1=value1 -v key2=value2
psql_execute_file <- function(psql, script_file, args="") {
  cmd <- sprintf('%s %s -f %s', psql, args, script_file)
  system(cmd)
}

# Execute a sql command
psql_execute_command <- function(psql, command) {
  system(sprintf("%s -c \"%s;\"", psql, command))
}

# Download a table from redshift to local, using s3_file as temporary location
#   - psql : obtained from psql_conn_string
#   - conn : obtained from get_connection
#   - local_data_path : where to store data
#   - local_data_header : where to store header
redshift_to_local <- function(psql, conn, table, s3_file, local_data_path, local_data_header, aws_secret_id, aws_secret_key) {
  print('copying from from redshift to s3')
  cmd <- sprintf("%s -c \"unload ('select * from %s') to '%s' credentials 'aws_access_key_id=%s;aws_secret_access_key=%s' parallel off allowoverwrite gzip;\""
    , psql, table, s3_file, aws_secret_id, aws_secret_key)
  system(cmd)
  print('downloading from s3 to local')
  system(sprintf("aws s3 cp %s --region ap-southeast-2 %s", paste0(s3_file, '000.gz'), local_data_path))
  print('getting header from redshift')
  headers <- dbGetQuery(conn, sprintf('select * from %s limit 1', table))
  write_csv(headers, local_data_header)
}

# Read data from a pair of data (without header) and its associated header files
read_data_header <- function(data_file, header_file) {
  if (grepl('.gz$', data_file)) {
    d <- fread(sprintf('gunzip -c %s', data_file))
  } else {
    d <- fread(data_file)
  }
  headers <- fread(header_file)
  colnames(d) <- colnames(headers)
  d
}

# Copy a local file to S3
s3_copy_from_local <- function(local_file, s3_file) {
  system(sprintf("aws s3 cp --region ap-southeast-2 %s %s --sse", local_file, s3_file))
}

# Download a file from S3 to local
s3_download_to_local <- function(s3_file, local_file) {
  system(sprintf("aws s3 cp --region ap-southeast-2 %s %s --sse", s3_file, local_file))
}

