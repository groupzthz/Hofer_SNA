
library(data.table)
library(lubridate)

#rm(list = ls())

#individuals = fread("IndividualsFull.csv", na.strings = "")
transponder = fread("IndividualsFulllong.csv", na.strings = "")
validationData = fread("DataValidation.csv")
setup = fread("Setup.csv")

#load RFID data
fileNames = list.files(path = paste0(getwd(),'/RFIDdata'), pattern = '*.csv' )
filePaths = paste0(getwd(),'/RFIDdata/', fileNames)
#save all data in list
listRawData = lapply(filePaths, fread, sep=" ", col.names = c("Date", "Time", "Transponder", "Antenna"))
#check first list entry to see it went well
head(listRawData[[1]])

#save all list entries in one data frame
RFIDdata = do.call("rbind", listRawData)
rm(listRawData)

#time trasnformations 
RFIDdata[, TimeStamp := dmy_hms(paste(Date, Time))]
RFIDdata[, Date := dmy(paste(Date))]
transponder[, StartDate := dmy(StartDate)]
transponder[, EndDate := dmy(EndDate)]
setup[, StartDate := dmy(StartDate)]
setup[, EndDate := dmy(EndDate)]
validationData[, Start := dmy_hms(paste(Date, Time_start))]
validationData[, End := dmy_hms(paste(Date, Time_end))]
validationData[, Date := dmy(Date)]

#set relevant parameters
observations = data.table(Obs = c(1,2), 
                          Start = dmy_hms(c("15.08.2021 14:45:00", "20.08.2021 02:15:00")),
                          End = dmy_hms(c("15.08.2021 15:45:00", "20.08.2021 03:15:00")))


#create unique identifier
validationData[, Hen := paste0(Pen, ID)]
transponder[, Hen := paste0(Pen, ID)]


############### DATA CHECKS #################################

#make sure all individuals were written okay
validationData= validationData[(Hen %in% transponder$Hen),]

#any registered transponders without animals?
unique(RFIDdata$Transponder[!(RFIDdata$Transponder %in% transponder$Transponder)])

#any antenna that shouldn't exist?
unique(RFIDdata$Antenna[!(RFIDdata$Antenna %in% setup$Antenna)])

############## DATA MAPPING #####################################

#extract Transponder number for comparison to RFID 
validationfull = transponder[validationData, on = .(Hen, StartDate <= Date, EndDate >= Date), 
                             .(Hen = i.Hen, Transponder = x.Transponder, Start = i.Start, End = i.End, Indicator = i.Indicator, Units = i.Antenna)]
validationfull[, LagStart1 := Start-60]
validationfull[, LagStart2 := Start+60]
validationfull[, Date := as.Date(Start)]
#extract Units of Antenna and Hen ID for RFID data
RFIDfull = setup[RFIDdata, on = .(Antenna, StartDate <= Date, EndDate >= Date), 
                 .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Antenna = i.Antenna, Units = x.Units, Pen = x.Pen)]
RFIDfull = transponder[RFIDfull, on = .(Transponder, StartDate <= Date, EndDate >= Date), 
                       .(Date = i.Date, TimeStamp = i.TimeStamp, Transponder = i.Transponder, Hen = x.Hen, Pen = i.Pen, Antenna = i.Antenna, Units = i.Units)]

#insert duration
RFIDfull = RFIDfull[order(Transponder, TimeStamp),]
#create running grouping variable 
RFIDfull[, dummy:= rleid(Units,Transponder, Date)]
#calculate duration between each registration to filter out re-entries
RFIDfull[, Duration := (TimeStamp - shift(TimeStamp, type="lag")), by = dummy]
RFIDfull[,NewEntry := ifelse(Duration > 12, TRUE, FALSE)]
RFIDfull[is.na(NewEntry), NewEntry := FALSE]
#create  index of first registration 
RFIDfull[, duplFirst:= !duplicated(dummy)]
#adjust grouping dummy to new entries
RFIDfull[, dummy:= cumsum(duplFirst | NewEntry) ]

#create index of last registration
RFIDfull[, duplLast:= !duplicated(dummy, fromLast = TRUE)]
#update  index of first registration 
RFIDfull[, duplFirst:= !duplicated(dummy)]


# make RFID dataset sparse
#extract only first and last registration
RFIDsparse = RFIDfull[duplFirst == TRUE | duplLast == TRUE,]

RFIDsparse[, Timing := ifelse(duplicated(dummy), "End", "Start")]


#rm(RFIDdata, RFIDfull)

########### Validation - Quick and Dirty ####################
Temp = RFIDsparse[rep(which(duplFirst == TRUE & duplLast == TRUE), each = 2),]

Temp[seq(2,nrow(Temp), 2),c('TimeStamp', 'duplFirst', 'duplLast'):= NA]
Temp[seq(2,nrow(Temp), 2), "Timing" := "End"]

RFIDsparse = rbind(Temp, RFIDsparse[!(duplFirst == TRUE & duplLast == TRUE),])[order(dummy),]

#make into wide set to fit onto 
RFIDsparse_wide = dcast(RFIDsparse, formula =  Date + Transponder + Hen + Units +dummy ~ Timing, value.var = "TimeStamp") 


minTrack = RFIDfull[Date == dmy("20.08.2021"), min(TimeStamp)]

#exclude where Start is not clear
#exclude time before min tracking
validationTrue = validationfull[(Date == as.Date("2021-08-15") | (Date == as.Date("2021-08-20") & Start > minTrack)) & Indicator != 1,]

Compar = RFIDsparse_wide[validationTrue, on = .(Transponder, Units,  Start >= LagStart1, Start <= LagStart2), 
                         .(Hen = i.Hen, StartObs = i.Start, StartMeas = x.Start, EndObs = i.End, EndMeas = x.End,
                           UnitsObserved = i.Units, Units = x.Units)]

#propCorrect = sum(Compar$UnitsObserved == Compar$Units, na.rm= TRUE)/length(Compar$Units)
#propNotRegis = sum(is.na(Compar$Units))/length(Compar$Units)

#delete flickering instances
Compar[, dupl := duplicated(rleid(Hen, StartObs, Units))]
Compar_Clean = Compar[dupl == FALSE,]

Compar_Clean[, duration := as.numeric(difftime(EndObs,StartObs, units = "secs"))]

propCorrect = sum(Compar_Clean$UnitsObserved == Compar_Clean$Units, na.rm= TRUE)/length(Compar_Clean$Units)
propNotRegis = sum(is.na(Compar_Clean$Units))/length(Compar_Clean$Units)

Lag = as.numeric(difftime(Compar_Clean[!is.na(StartMeas), StartObs], Compar_Clean[!is.na(StartMeas), StartMeas], units = "secs"))
maxLag = max(abs(Lag))
mean(Lag)
median(Lag)
sd(Lag)
hist(Lag)

#animals without entries
no_entry = unique(Compar[is.na(StartMeas), Hen])[(!(unique(Compar[is.na(StartMeas), Hen]) %in% unique(RFIDfull$Hen)))]

Compar_test = Compar_Clean[!(Hen %in% no_entry), ]
propCorrecttest = sum(Compar_test$UnitsObserved == Compar_test$Units, na.rm= TRUE)/length(Compar_test$Units)
propNotRegistest = sum(is.na(Compar_test$Units))/length(Compar_test$Units)

#Units without match (ratio to all instances of Unit)
unittest = Compar_Clean[, sum(is.na(StartMeas))/.N, by = UnitsObserved][order(V1)]

# Duration in relation to match
hist(Compar_test[is.na(StartMeas) & duration < 20, duration])
Compar_test[is.na(StartMeas), median(duration)]
hist(Compar_Clean[!is.na(StartMeas) & duration < 20, duration])
Compar_test[!is.na(StartMeas), median(duration)]

# quantify absolute duration correct?

########### Validation ########################

# FOR PROPER COMPARISON:
RFIDsparse = RFIDfull[duplFirst == TRUE | duplLast == TRUE,]

RFIDsparse[, Timing := ifelse(duplicated(dummy), "End", "Start")]

# create all seconds bird is definitely on (fill up between first and last)
library(zoo)
# you need to round your decimal timestamps to seconds
#  use ceiling() or floor() if you prefer
RFIDsparse[, TimeStamp := as.POSIXct(round(TimeStamp))]
# Create a data.table with min-max sequences
RFIDcomplete <- RFIDsparse[, .(TimeStamp = seq(min(TimeStamp), max(TimeStamp), by = 1)), by = .(dummy)][]
# Perform update join
RFIDcomplete[RFIDsparse, `:=`(Hen = i.Hen, Units = i.Units, Timing = i.Timing),
             on = .(dummy, TimeStamp)]
# Fill down NA's Hen and Units columns
#  data.table's setnafill doens not (yet?) support character columns
#  so we use zoo:na.locf()
RFIDcomplete[, c("Hen", "Units") := lapply(.SD, zoo::na.locf), 
             .SDcols = c("Hen", "Units")]

# create all seconds bird was seen on (fill up observations between start and end -> transform to long)
minTrack = RFIDfull[Date == dmy("20.08.2021"), min(TimeStamp)]
validationTrue = validationfull[(Date == as.Date("2021-08-15") | (Date == as.Date("2021-08-20") & Start > minTrack)),]
validationTrue[, dummy := 1:.N]
validationLong = melt(validationTrue, id.vars = c("Hen", "Transponder", "Indicator", "Units", "dummy"), 
                      measure.vars = c("Start", "End"), variable.name = "Timing", 
                      value.name = "TimeStamp")[order(dummy),]

validationComplete = validationLong[, .(TimeStamp = seq(min(TimeStamp), max(TimeStamp), by = 1)), by = .(dummy)][]
validationComplete[validationLong, `:=`(Hen = i.Hen, UnitsObserved = i.Units, TimingObserved = i.Timing, Indicator = i.Indicator),
                   on = .(dummy, TimeStamp)]
validationComplete[, c("Hen", "UnitsObserved", "Indicator") := lapply(.SD, zoo::na.locf), 
                   .SDcols = c("Hen", "UnitsObserved", "Indicator")]

# merge two sets
ComparFull = RFIDcomplete[validationComplete, on = .(Hen, TimeStamp), 
                          .(Hen = i.Hen, TimeStamp = i.TimeStamp, UnitsObserved, UnitsMeasured = Units, 
                            Indicator = i.Indicator, TimingObserved, TimingMeasured = Timing, dummy = i.dummy)]

# ratio of observations with no entries at all
nullObservationsFull = sum(ComparFull[,any(!is.na(UnitsMeasured)), by = "dummy"][, V1] ==  FALSE)/max(ComparFull$dummy)

#animals with no entries 
# in validation
no_Matches_hens = ComparFull[,all(is.na(UnitsMeasured)),by = Hen][V1 == TRUE,Hen] 
# generally in RFID data
no_regist_hens = no_Matches_hens[!(no_Matches_hens %in% unique(RFIDfull$Hen))]

# simple match - mismatch
match_entries_full = nrow(ComparFull[UnitsObserved == UnitsMeasured,])
no_match_entries_full = nrow(ComparFull[UnitsObserved != UnitsMeasured,])
no_regis_entries_full = nrow(ComparFull[is.na(UnitsMeasured),])

#units mismatch 
units_Overview = ComparFull[, .N, by = .(UnitsObserved, UnitsMeasured)][order(UnitsObserved),][!is.na(UnitsMeasured),][!(UnitsObserved == UnitsMeasured),]

# PROBLEM 1: under 10 seconds RFID has only one entry
# PROBLEM 2: time lag

#observations with mismatches
mismatchObservations = ComparFull[,all(UnitsMeasured == UnitsObserved, na.rm = TRUE), by = dummy][V1 == FALSE,dummy]
nomatchObservations = ComparFull[,all(is.na(UnitsMeasured)), by = dummy][V1 == TRUE,dummy]

ComparMismatch = ComparFull[dummy %in% mismatchObservations,]
ComparNoMatch = ComparFull[dummy %in% nomatchObservations & !(Hen %in% no_regist_hens),]

mismatch_hens = sort(unique(ComparMismatch$Hen))

mismatchExtreme = ComparMismatch[,all(UnitsMeasured != UnitsObserved, na.rm = TRUE), by = dummy][V1 == TRUE,dummy]

ComparExtremeMismatch = ComparMismatch[dummy %in% mismatchExtreme,]

MarinaCheck = ComparExtremeMismatch[!duplicated(dummy),]
MarinaCheck[, UnitsMeasured := ComparExtremeMismatch[, .(paste0(unique(UnitsMeasured),collapse=",")), by = dummy][,V1]]

MarinaCheck = rbind(MarinaCheck, ComparNoMatch[!duplicated(dummy),])[order(Hen, TimeStamp),] 

fwrite(MarinaCheck, "DoubleCheck.csv", sep = ";")
