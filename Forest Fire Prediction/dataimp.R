fdata <- read.csv("C:\\Users\\nmbal\\OneDrive\\Masaüstü\\L\\Engineering Project 2\\Project\\forestfires.csv", header = TRUE, sep = ",")
fdata
gdata <- read.csv("C:\\Users\\nmbal\\OneDrive\\Masaüstü\\L\\Engineering Project 2\\Project\\forestfires(2).csv", header = TRUE, sep = ",")
gdata
gdata$type <- as.factor(gdata$type)
hdata <- read.csv("C:\\Users\\nmbal\\OneDrive\\Masaüstü\\L\\Engineering Project 2\\Project\\responses.csv", header = TRUE, sep = ",")
table(fdata$type)
str(fdata)


fdata$type <- as.factor(fdata$type)
fdata$FFMC <- as.double(fdata$FFMC)
fdata$DMC <- as.integer(fdata$rain)
fdata$DC <- as.double(fdata$rain)
fdata$ISI <- as.integer(fdata$rain)
fdata$temp <- as.integer(fdata$rain)
fdata$wind <- as.integer(fdata$rain)
fdata$rain <- as.integer(fdata$rain)

