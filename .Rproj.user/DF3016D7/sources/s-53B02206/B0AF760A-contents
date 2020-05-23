pollutantmean = function(directory, pollutant, id = 1:332){
    entire_data = numeric()
    for (x in id) {
        monitor_reading = read.csv(paste0(directory,'/',substring(1000+x,2),'.csv'))
        entire_data = c(entire_data,monitor_reading[, pollutant])
    }
    mean(entire_data, na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

complete = function(directory, id = 1:332){
    nobs = numeric()
    for (x in id) {
        monitor_reading = read.csv(paste0(directory,'/',substring(1000+x,2),'.csv'))
        nobs = c(nobs,sum(complete.cases(monitor_reading)))
    }
    data.frame(id = id, nobs = nobs)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

corr = function(directory, threshold = 0){
    df = complete("specdata")
    ids = df[df[,'nobs']>threshold,'id']
    
    cr = numeric()
    for(x in ids) {
        monitor_reading = read.csv(paste0(directory,'/',substring(1000+x,2),'.csv'))
        cr = c(cr,cor(monitor_reading[complete.cases(monitor_reading),'sulfate'],
                      monitor_reading[complete.cases(monitor_reading),'nitrate']))
    }
    cr
}

cr <- corr("specdata", 150)
head(cr)

summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)