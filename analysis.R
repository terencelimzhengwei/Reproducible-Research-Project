#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename = "repdata-data-StormData.csv"
data.filename = "repdata-data-StormData.csv.bz2"

if(sum(data.filename %in% list.files())==0)
    download.file(url,filename)

storm_data <- tbl_df(read.csv(bzfile("./repdata-data-StormData.csv.bz2")))
str(storm_data)
storm_data <- storm_data[c("EVTYPE","FATALITIES","INJURIES","PROPDMG",
                           "PROPDMGEXP","CROPDMG","CROPDMGEXP")]
storm_data$EVTYPE <- tolower(as.character(storm_data$EVTYPE))
storm_data$PROPDMGEXP <- tolower(as.character(storm_data$PROPDMGEXP))
storm_data$CROPDMGEXP <- tolower(as.character(storm_data$CROPDMGEXP))

unique(storm_data$PROPDMGEXP)
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="k"]="1000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="m"]="1000000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="b"]="1000000000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP==""]="0"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="-"]="0"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="?"]="0"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP=="+"]="1"
storm_data$PROPDMGEXP[c(0,1,2,3,4,5,6,7,8,9) %in% storm_data$PROPDMGEXP]="10"

unique(storm_data$CROPDMGEXP)
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="k"]="1000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="m"]="1000000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="b"]="1000000000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP==""]="0"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="-"]="0"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="?"]="0"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP=="+"]="1"
storm_data$CROPDMGEXP[c(0,1,2,3,4,5,6,7,8,9) %in% storm_data$CROPDMGEXP]="10"

storm_data$PROPDMGEXP <- as.numeric(storm_data$PROPDMGEXP)
storm_data$CROPDMGEXP <- as.numeric(storm_data$CROPDMGEXP)
storm_data$PROP_DMG <- storm_data$PROPDMG*storm_data$PROPDMGEXP
storm_data$CROP_DMG <- storm_data$CROPDMG*storm_data$CROPDMGEXP

storm_data <- storm_data[c("EVTYPE","FATALITIES","INJURIES","PROP_DMG","CROP_DMG")]

filtered_data <- storm_data %>%
    filter(FATALITIES!=0|INJURIES!=0|PROP_DMG!=0|CROP_DMG!=0)%>%
    group_by(EVTYPE) %>%
    summarize_each(funs(sum))%>%
    arrange(desc(FATALITIES),desc(INJURIES),desc(PROP_DMG),desc(CROP_DMG))

head(filtered_data$EVTYPE,5)
head(arrange(filtered_data,desc(INJURIES))$EVTYPE,5)
head(arrange(filtered_data,desc(PROP_DMG))$EVTYPE,5)
head(arrange(filtered_data,desc(CROP_DMG))$EVTYPE,5)

filtered_data$EVTYPE[grepl("torn",filtered_data$EVTYPE)]="tornado"
filtered_data$EVTYPE[grepl("ex.+ heat",filtered_data$EVTYPE)]="excessive heat"
filtered_data$EVTYPE[grepl("flash",filtered_data$EVTYPE)]="flash flood"
filtered_data$EVTYPE[grepl("^heat",filtered_data$EVTYPE)]="heat"
filtered_data$EVTYPE[grepl("lightning",filtered_data$EVTYPE)]="lightning"
filtered_data$EVTYPE[grepl("tstm",filtered_data$EVTYPE)]="thunderstorm wind"
filtered_data$EVTYPE[grepl("thunderstorm",filtered_data$EVTYPE)]="thunderstorm wind"
filtered_data$EVTYPE[grepl("^flood",filtered_data$EVTYPE)]="flood"
filtered_data$EVTYPE[grepl("river flood",filtered_data$EVTYPE)]="flood"
filtered_data$EVTYPE[grepl("rip",filtered_data$EVTYPE)]="rip current"
filtered_data$EVTYPE[grepl("high wind",filtered_data$EVTYPE)]="high wind"
filtered_data$EVTYPE[grepl("avala",filtered_data$EVTYPE)]="avalanche"
filtered_data$EVTYPE[grepl("hurricane",filtered_data$EVTYPE)]="hurricane"
filtered_data$EVTYPE[grepl("hail",filtered_data$EVTYPE)]="hail"
filtered_data$EVTYPE[grepl("ice storm",filtered_data$EVTYPE)]="ice storm"
filtered_data$EVTYPE[grepl("drought",filtered_data$EVTYPE)]="drought"

filtered_data <- filtered_data %>%
    group_by(EVTYPE) %>%
    summarize_each(funs(sum))%>%
    mutate(TOTAL_DMG=PROP_DMG+CROP_DMG) %>%
    arrange(desc(FATALITIES),desc(INJURIES))

health_data <- filtered_data[1:10,c("EVTYPE","INJURIES","FATALITIES")] %>%
    gather("fatality_type","value",2:3)

ggplot(health_data,aes(x=reorder(EVTYPE,-value),y=value,fill=fatality_type))+
    geom_bar(position="dodge",stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Event Type")+
    ylab("Number of Fatalities/Injuries")+
    ggtitle("Top 10 weather causes of fatalities/injuries")+
    scale_fill_discrete(name="Type")

economic_data <- arrange(filtered_data,desc(TOTAL_DMG))[1:10,c("EVTYPE","PROP_DMG","CROP_DMG")] %>%
    gather("economic_type","value",2:3) %>%
    mutate(value=value/1000000000)

ggplot(economic_data,aes(x=reorder(EVTYPE,-value),y=value,fill=economic_type))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Event Type")+
    ylab("Damage in USD (billions)")+
    ggtitle("Top 10 weather causes of fatalities/injuries")+
    scale_fill_discrete(name="Type",
                        labels = c("PROPERTY","CROP"))

