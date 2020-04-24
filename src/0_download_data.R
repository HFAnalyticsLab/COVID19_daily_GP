link <- 'https://files.digital.nhs.uk/D8/889650/Appointments_GP_Daily_Feb20.zip'
destfile <- here::here('data', 'original data' ,'GP.zip')
download.file(link, destfile  )

unzip(destfile , exdir = here::here('data', 'original data' ))
