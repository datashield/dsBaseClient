#
# The file "connection_to_datasets/local_settings.csv" (within the directory "tests/testthat") contains, if present,
# values which can be used to affect the behavious of the continuous integration.
#
# The server URL is in a CVS file, being the value of the first column, first row.
#

init.server.url <- function()
{
   file.name <- init.local.settings()
   if (file.exists(file.name))
   {
      content    <- read.csv(file.name, header = FALSE)
      server.url <- as.character(content[[1]][1])
   }
   else
   {
      # server.url <- "http://127.0.0.1:8080/"
      server.url <- "http://localhost:8080/"
   }
   return (server.url)
}



init.local.settings <- function()
{
  path            <- getwd()
  sub.folder.name <- "/connection_to_datasets/"
  file.name       <- "local_settings.csv"

  return(paste(path, sub.folder.name, file.name, sep=""))
}
