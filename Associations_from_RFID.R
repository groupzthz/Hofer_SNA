
library(data.table)
library(lubridate)
library(zoo)

#rm(list = ls())

transponder = fread("IndividualsFulllong.csv", na.strings = "")
setup = fread("Setup.csv")

#load RFID data
fileNames1 = list.files(path = paste0(getwd(),'/RFIDraw/firstNetwork'), pattern = '*.csv' )
filePaths1 = paste0(getwd(),'/RFIDraw/firstNetwork/', fileNames1)
fileNames2 = list.files(path = paste0(getwd(),'/RFIDraw/secondNetwork'), pattern = '*.csv' )
filePaths2 = paste0(getwd(),'/RFIDraw/secondNetwork/', fileNames2)

#save all data in list
# one list for first time point one for second
listRawData1 = lapply(filePaths1, fread, sep=" ", fill = TRUE, col.names = c("Date", "Time", "Transponder", "Antenna"))
listRawData2 = lapply(filePaths2, fread, sep=" ", col.names = c("Date", "Time", "Transponder", "Antenna"))

#check first list entry to see it went well
head(listRawData1[[1]])
head(listRawData2[[1]])

#save all list entries in one data frame
RFIDdata1 = do.call("rbind", listRawData1)
RFIDdata2 = do.call("rbind", listRawData2)

#remove unnecessary large variables
rm(listRawData1, fileNames1, filePaths1, listRawData2, fileNames2, filePaths2)

#time transformations 
RFIDdata1[, TimeStamp := dmy_hms(paste(Date, Time))]
RFIDdata1[, Date := dmy(paste(Date))]
RFIDdata2[, TimeStamp := dmy_hms(paste(Date, Time))]
RFIDdata2[, Date := dmy(paste(Date))]

# round your decimal timestamps to seconds
#  use ceiling() or floor() if you prefer
RFIDdata1[, TimeStamp := as.POSIXct(round(TimeStamp))]
RFIDdata2[, TimeStamp := as.POSIXct(round(TimeStamp))]

transponder[, StartDate := dmy(StartDate)]
transponder[, EndDate := dmy(EndDate)]
setup[, StartDate := dmy(StartDate)]
setup[, EndDate := dmy(EndDate)]


#create unique identifier
transponder[, Hen := paste0(Pen, ID)]


############### DATA CHECKS #################################

#any registered transponders without animals?
unknownTransponder1 = unique(RFIDdata1$Transponder[!(RFIDdata1$Transponder %in% transponder$Transponder)])
unknownTransponder2 = unique(RFIDdata2$Transponder[!(RFIDdata2$Transponder %in% transponder$Transponder)])

# unknown transponders in both sets
intersect(unknownTransponder1, unknownTransponder2)

#how often unique transponders appear
RFIDdata1[Transponder %in% unknownTransponder1, .N, by = Transponder]
RFIDdata2[Transponder %in% unknownTransponder2, .N, by = Transponder]

#any animals without entries
transponder[Transponder %in% unique(transponder$Transponder[!(transponder$Transponder %in% RFIDdata1$Transponder)]) &
              EndDate > as.Date("2021-09-01"),]
transponder[Transponder %in% unique(transponder$Transponder[!(transponder$Transponder %in% RFIDdata2$Transponder)]) &
              EndDate > as.Date("2021-09-01"),]
#any antenna that shouldn't exist?
unique(RFIDdata1$Antenna[!(RFIDdata1$Antenna %in% setup$Antenna)])

############## DATA MAPPING #####################################

#extract Units of Antenna and Hen ID for RFID data
RFIDfull1 = setup[RFIDdata1, on = .(Antenna, StartDate <= Date, EndDate >= Date), 
                  .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Antenna = i.Antenna, Units = x.Units, Pen = x.Pen)]

RFIDfull2 = setup[RFIDdata2, on = .(Antenna, StartDate <= Date, EndDate >= Date), 
                  .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Antenna = i.Antenna, Units = x.Units, Pen = x.Pen)]

#extract transponder and Hen ID for RFID data
RFIDfull1 = transponder[RFIDfull1, on = .(Transponder, StartDate <= Date, EndDate >= Date), 
                        .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Hen = x.Hen, Pen = i.Pen, PenOrig = x.Pen, Antenna = i.Antenna, Units = i.Units)]

RFIDfull2 = transponder[RFIDfull2, on = .(Transponder, StartDate <= Date, EndDate >= Date), 
                        .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Hen = x.Hen, Pen = i.Pen, PenOrig = x.Pen, Antenna = i.Antenna, Units = i.Units)]

#extract only Pen D, E & F
RFIDsmall1 = RFIDfull1[PenOrig == "D"|PenOrig == "E"|PenOrig == "F",]

RFIDsmall2 = RFIDfull2[PenOrig == "D"|PenOrig == "E"|PenOrig == "F",]

#check if there are entries for all small group hens
length(unique(RFIDsmall1$Hen)) == 45
length(unique(RFIDsmall2$Hen)) == 45

#insert duration
RFIDsmall1 = RFIDsmall1[order(Transponder, TimeStamp),]
RFIDsmall2 = RFIDsmall2[order(Transponder, TimeStamp),]
#create running grouping variable 
RFIDsmall1[, dummy:= rleid(Units,Transponder, Date)]
RFIDsmall2[, dummy:= rleid(Units,Transponder, Date)]
#calculate duration between each registration to filter out re-entries
RFIDsmall1[, Duration := (TimeStamp - shift(TimeStamp, type="lag")), by = dummy]
RFIDsmall2[, Duration := (TimeStamp - shift(TimeStamp, type="lag")), by = dummy]

RFIDsmall1[,NewEntry := ifelse(Duration > 12, TRUE, FALSE)]
RFIDsmall1[is.na(NewEntry), NewEntry := FALSE]
RFIDsmall2[,NewEntry := ifelse(Duration > 12, TRUE, FALSE)]
RFIDsmall2[is.na(NewEntry), NewEntry := FALSE]
#create  index of first registration 
RFIDsmall1[, duplFirst:= !duplicated(dummy)]
RFIDsmall2[, duplFirst:= !duplicated(dummy)]
#adjust grouping dummy to new entries
RFIDsmall1[, dummy:= cumsum(duplFirst | NewEntry) ]
RFIDsmall2[, dummy:= cumsum(duplFirst | NewEntry) ]

#create index of last registration
RFIDsmall1[, duplLast:= !duplicated(dummy, fromLast = TRUE)]
RFIDsmall2[, duplLast:= !duplicated(dummy, fromLast = TRUE)]
#update  index of first registration 
RFIDsmall1[, duplFirst:= !duplicated(dummy)]
RFIDsmall2[, duplFirst:= !duplicated(dummy)]


# make RFID dataset sparse
#extract only first and last registration
RFIDsparse1 = RFIDsmall1[duplFirst == TRUE | duplLast == TRUE,]
RFIDsparse1[, Timing := ifelse(duplicated(dummy), "End", "Start")]

RFIDsparse2 = RFIDsmall2[duplFirst == TRUE | duplLast == TRUE,]
RFIDsparse2[, Timing := ifelse(duplicated(dummy), "End", "Start")]

fwrite(RFIDsparse1, "firstNetworkSparse.csv", sep = ";")
fwrite(RFIDsparse2, "secondNetworkSparse.csv", sep = ";")

#rm(RFIDdata, RFIDfull)

########### Association ########################

# create all seconds bird is definitely on (fill up between first and last)


# Create a data.table with min-max sequences
RFIDcomplete1 <- RFIDsparse1[, .(TimeStamp = seq(min(TimeStamp), max(TimeStamp), by = 1)), by = .(dummy)][]
RFIDcomplete2 <- RFIDsparse2[, .(TimeStamp = seq(min(TimeStamp), max(TimeStamp), by = 1)), by = .(dummy)][]

# Perform update join
RFIDcomplete1[RFIDsparse1, `:=`(Hen = i.Hen, Units = i.Units, Timing = i.Timing, 
                                PenOrig = i.PenOrig, Date = i.Date),
              on = .(dummy, TimeStamp)]
RFIDcomplete2[RFIDsparse2, `:=`(Hen = i.Hen, Units = i.Units, Timing = i.Timing, 
                                PenOrig = i.PenOrig, Date = i.Date),
              on = .(dummy, TimeStamp)]

# Fill down NA's Hen and Units columns
#  data.table's setnafill doens not (yet?) support character columns
#  so we use zoo:na.locf()
RFIDcomplete1[, c("Hen", "Units", "PenOrig", "Date") := lapply(.SD, zoo::na.locf), 
              .SDcols = c("Hen", "Units", "PenOrig", "Date")]
RFIDcomplete2[, c("Hen", "Units", "PenOrig", "Date") := lapply(.SD, zoo::na.locf), 
              .SDcols = c("Hen", "Units", "PenOrig", "Date")]

#READY FOR TRANSFORMATION
#match all hen series onto each other per pen per day
# create list that holds for each pen a list of days with day sequence from
#choose pen
#set relevant parameters
dataCol = data.table(Pens = c("D", "E", "F"),
                     Day1 = dmy(c("28.08.2021", "30.08.2021", "01.09.2021")),
                     Day2 = dmy(c("29.08.2021", "31.08.2021", "02.09.2021")),
                     Day3 = dmy(c("30.08.2021", "01.09.2021", "03.09.2021")),
                     Day4 = dmy(c("31.08.2021", "02.09.2021", "04.09.2021")),
                     Day5 = dmy(c("01.09.2021", "03.09.2021", "05.09.2021")))


#loop to save files
for (pen in dataCol$Pens){
  for (relDate in dataCol[Pens == pen,2:ncol(dataCol)]){
    cat("Pen:", pen, "\n")
    cat("Date:", as.character(relDate), "\n")
    #extract relevant RFID data
    relData = RFIDcomplete1[Date == relDate & PenOrig == pen,]
    #change format of data to show all hens along one time sequence
    relDatawide = dcast(relData, formula = TimeStamp~Hen, value.var = "Units", 
                        fun.aggregate = function(x){x[1]})
    name = paste0(pen, "_", relDate, ".csv")
    fwrite(relDatawide, name, sep = ";")
    
  }
}

dataCol2 = data.table(Pens = c("D", "E", "F"),
                      Day1 = dmy(c("21.09.2021", "23.09.2021", "25.09.2021")),
                      Day2 = dmy(c("22.09.2021", "24.09.2021", "26.09.2021")),
                      Day3 = dmy(c("23.09.2021", "25.09.2021", "27.09.2021")),
                      Day4 = dmy(c("24.09.2021", "26.09.2021", "28.09.2021")),
                      Day5 = dmy(c("25.09.2021", "27.09.2021", "29.09.2021")))


for (pen in dataCol2$Pens){
  for (relDate in dataCol2[Pens == pen,2:ncol(dataCol2)]){
    cat("Pen:", pen, "\n")
    cat("Date:", as.character(relDate), "\n")
    #extract relevant RFID data
    relData = RFIDcomplete2[Date == relDate & PenOrig == pen,]
    #change format of data to show all hens along one time sequence
    relDatawide = dcast(relData, formula = TimeStamp~Hen, value.var = "Units", 
                        fun.aggregate = function(x){x[1]})
    name = paste0(pen, "_", relDate, ".csv")
    fwrite(relDatawide, name, sep = ";")
    
  }
}



