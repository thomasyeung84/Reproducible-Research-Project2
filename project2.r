#download the file to temporary file
#read the csv file to full dataset in R
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
temp <- tempfile()
download.file (fileurl, temp) # save tge file into temp file
fulldataset <- read.csv(temp, na.strings = "NA")
unlink (temp) #unlink the temp file with the zip
rm (temp) # remove temp from the object list

#load package
library (ggplot2)
library (dplyr)


#what variables are needed??
variablelist <- names (fulldataset)
selectedvariable <- variablelist[c(8, 23, 24, 25, 26, 27, 28)]

#subset the csv to usable dataset
dataset <- fulldataset[ ,c(selectedvariable)]

# check na
sapply (dataset, function(x) sum(is.na(x)))

#calacuate the exact number of property damange
dataset <- dataset %>% mutate (ACTPROPDMG = ifelse (
        PROPDMGEXP == "K", PROPDMG*1000, ifelse (
                PROPDMGEXP == "m" | PROPDMGEXP == "M", PROPDMG*1000000, ifelse(
                        PROPDMGEXP == "B", PROPDMG*1000000000, ifelse (
                                PROPDMGEXP =="h" | PROPDMGEXP =="H", PROPDMG*100, PROPDMG)
                )
        )
))

#calculate the exact number of crop damage
dataset <- dataset %>% mutate (ACTCROPDMG = ifelse (
        CROPDMGEXP == "K" | CROPDMGEXP == "k", CROPDMG*1000, ifelse (
                CROPDMGEXP == "M" | CROPDMGEXP == "m", CROPDMG*1000000, ifelse(
                        CROPDMGEXP == "B", CROPDMG*1000000000, CROPDMG
                )
        )
)  )

#calculate the total damage
dataset <- dataset %>% mutate (TOTALDMG = ACTPROPDMG + ACTCROPDMG)


#Across the United States, 
#which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
fatalities <- dataset %>% group_by (EVTYPE) %>% summarize (FATALITIES = sum(FATALITIES))
injuries <- dataset %>% group_by (EVTYPE) %>% summarize (INJURIES = sum (INJURIES))

fatalities <- fatalities %>% arrange (desc(FATALITIES)) 
injuries <- injuries %>% arrange (desc (INJURIES)) 

ggplot (data = head (fatalities, 10),
        aes(x=reorder(EVTYPE, -FATALITIES), y=FATALITIES))+
        geom_bar(stat = "identity")+
        xlab("Weather Event")+ylab("Fatalities")+
        theme(axis.text.x = element_text(size = 7))+
        ggtitle("Top 10 fatalities of severe weather event")

ggplot (data = head (injuries, 10),
        aes(x=reorder(EVTYPE, -INJURIES), y=INJURIES))+
        geom_bar(stat = "identity")+
        xlab("Weather Event")+ylab("Injuries")+
        theme(axis.text.x = element_text(size = 7))+
        ggtitle("Top 10 injuries of severe weather event")


#which types of events have the greatest economic consequences?
totaldmg <- dataset %>% group_by (EVTYPE) %>% summarize (TOTALDMG = sum(TOTALDMG))
totaldmg <- totaldmg %>% arrange (desc (TOTALDMG))

ggplot (data = head (totaldmg, 10),
        aes (x=reorder(EVTYPE, -TOTALDMG), y=TOTALDMG/1000000000))+
        geom_bar(stat = "identity")+
        xlab("Weather Event") + ylab ("Total Damage in billion $US")+
        theme (axis.text.x = element_text(size = 7))+
        ggtitle ("Top 10 damage of severe weather event")





#Has either a (1) valid RPubs URL pointing to a data analysis document for this assignment been submitted; or (2) a complete PDF file presenting the data analysis been uploaded?
#Is the document written in English?
#Does the analysis include description and justification for any data transformations?
#Does the document have a title that briefly summarizes the data analysis?
#Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?
#Is there a section titled "Data Processing" that describes how the data were loaded into R and processed for analysis?
#Is there a section titled "Results" where the main results are presented?
#Is there at least one figure in the document that contains a plot?
#Are there at most 3 figures in this document? 3 figures >> fetality, injuries, total economic damage
#Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
#Does the analysis address the question of which types of events are most harmful to population health?
#Does the analysis address the question of which types of events have the greatest economic consequences?
#Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?
#Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?
#As far as you can determine, does it appear that the work submitted for this project is the work of the student who submitted it?