#create a temporary file with the Ohio State Files

temp <- tempfile()
download.file("ftp://sosftp.sos.state.oh.us/free/Voter/WYANDOT.zip", temp, mode = "wb")
unzip(temp)


#create a file with a path, make sure to use wb for zip files because binary, use w for else
#had troubles with automation here, permission to save files on computer 
download.file("ftp://sosftp.sos.state.oh.us/free/Voter/WYANDOT.zip, path, mode="wb") 
unzip (path, exdir = "./")

#use either of these for reference
?download.file
?read.txt

#use the package rvest if the data you want is not in a file (probably not useful here)
#the package downloader could be useful in the future, especially if the files are not zipped