
#stormData$EVTYPE <- as.factor(sub(pattern = "COASTALFLOOD.*", replacement = "COASTALFLOOD",stormData$EVTYPE , ignore.case = TRUE))

#stormData$EVTYPE <- as.factor(sub(pattern = " ", replacement = "",stormData$EVTYPE , ignore.case = TRUE))

#stormData$EVTYPE <- as.factor(sub(pattern = "DRYMICROBURST .*", replacement = "DRYMICROBURST",stormData$EVTYPE , ignore.case = TRUE))

#stormData$EVTYPE <- as.factor(sub(pattern = "HIGH WIND.*", replacement = "HIGH WIND",stormData$EVTYPE , ignore.case = TRUE))

#stormData$EVTYPE <- as.factor(sub(pattern = "HURRICANE .*", replacement = "HURRICANE",stormData$EVTYPE , ignore.case = TRUE))

#stormData$EVTYPE <- as.factor(sub(pattern = "WATERSPOUT.*", replacement = "WATERSPOUT",stormData$EVTYPE , ignore.case = TRUE))
