link <- 'https://files.digital.nhs.uk/C1/DB7954/Appointments_GP_Daily_Mar20.zip'
destfile <- here::here('data', 'original data' ,'GP.zip')
download.file(link, destfile  )

unzip(destfile , exdir = here::here('data', 'original data' ))
