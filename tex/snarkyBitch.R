library(lubridate)

pltMonth = function(solName){
    colIndex = which( colnames(datSol)==solName )
    return(data.frame(timestamp, datSol[, datSol$timestamp]))
}

trapezoidArea = function(x, y, t = 0, s = 0){ 
    for (i in 1:(length(x)-1)){
        yvid = (y[i] + y[i+1])/2
        s[i] =  yvid * as.numeric(difftime(x[i+1], + x[i], unit="secs"))
        t = t+s[i]
    }
    return(t)
}

findIndex = function(x, lower, delta_t, upper = lower + delta_t){
    strtIndex = which(date(x) == date(lower) & hour(x) == hour(lower) & minute(x) == minute(lower))[1]
    endIndex = which(date(x) == date(upper) & hour(x) == hour(upper) & minute(x) == minute(upper))[1]
    if (is.na(strtIndex)){
        strtIndex = which(date(x) == date(lower + minutes(1)) &
            hour(x) == hour(lower + minutes(1)) & minute(x) == minute(lower + minutes(1)))[1]
    }
    if (is.na(endIndex)){
        endIndex = which(date(x) == date(upper + minutes(1)) &
            hour(x) == hour(upper + minutes(1)) & minute(x) == minute(upper + minutes(1)))[1]
    }
    return(c(strtIndex, endIndex))
}

integrateInterval = function(x, y, delta_t, solname, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE)){
    lowerLimit = from
    upperLimit = lowerLimit + delta_t    
    itrTimes = floor(as.numeric(to - from) / as.numeric(upperLimit - lowerLimit))
    xres <- as_datetime(itrTimes)
    yres <- numeric(itrTimes)
    count = 0
    while (interval(lowerLimit, upperLimit) %within% interval(from, to)) {
        indices = findIndex(x, lowerLimit, delta_t)
        datInt = datTemp[indices[1]:indices[2],]
        count = count + trapezoidArea(datInt$timestamp, datInt$solVar)/3600
        xres[i] <- floor_date(x[indices[1]], unit = "5 mins")
        yres[i] <- round(wh, digits=2)

        lowerLimit =  upperLimit
        upperLimit = upperLimit + delta_t
        i = i + 1      
    }
    sumInt = data.frame("timestamp" = xres, "solVar" = yres)
    colnames(sumInt)[2] <- toString(paste0(solname["dir"], solname["degree"], solname["type"]))
    return(sumInt)
}

solNames = c("solD40", "solD13","solA13", "solR13", "solD90")
types = c('JA','LG')
devices = '_PV_'
units = c('V', 'A', 'W')

for (solName in solNames){
    for (type in types){
        for (unit in units){
            solname = paste0(solName, type, device, unit)
            solName = interpretSolPanel(solname)
            if (solName["unit"] == "W"){
                datTemp = pltMonth(solname)
                sumMin = integrateInterval(datTemp$timestamp, datTemp$solVar, dminutes(5), "min", solname)
            }
        }
    }
}
