link <- 'https://files.digital.nhs.uk/EA/AC7352/Appointments_GP_Daily_Apr20.zip'
destfile <- here::here('data', 'original data' ,'GP.zip')
download.file(link, destfile  )

unzip(destfile , exdir = here::here('data', 'original data' ))
