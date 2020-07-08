TTC=read.csv("C:\\Users\\Tina_\\Desktop\\CKME 136_Capstone\\Documents\\Data\\TTCsubway.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

head(TTC)
str(TTC)
summary(TTC)

names(TTC)[1]="Date"

TTC$Date=as.Date(TTC$Date)

TTC$Day=as.factor(TTC$Day)

TTC$Station=as.factor(TTC$Station)

TTC$Code=as.factor(TTC$Code)

# The Delay COde attribute will not be used in this project as the it has about 200 levels and reducing the levels requires technical knowledge.

library(dplyr)

TTC=select(TTC, -c(Code))

### The Bound attrbute has to have 4 levels (N,S,E,W). In order to remover level "B", attribute was not changed in the first step.
###first, level "B" will be droped, then NA valuses will be omitted. After these two steps, Bound attribuet type will be chaged to factor. 

TTC= TTC[grepl(("^N$|^S$|^E$|^W$"),TTC$Bound),]
TTC$Bound=as.factor(TTC$Bound)
summary(TTC$Bound)


### removing records with NAs and missing values. Min Delay and Min Gap havem many rows with 0. As I reviews the data I found out zoro indicates there was no delays and rows can be removed.

TTC[TTC == 0] <- NA
TTC.clean= TTC[complete.cases(TTC),]


str(TTC.clean)

### will create min.delay categories of Short Delay<10, Medium Delay= 10-20, Significant Delay= 20-45, Crippling Delay>45

TTC.clean <- TTC.clean[which(TTC.clean$Min.Delay < 60 & TTC.clean$Min.Delay > 0 ),]


TTC.clean$Min.Delay=as.factor(TTC.clean$Min.Delay)

### will create min.Gap categories of Short Gap<10, Medium Gap= 10-20, Significant Gap= 20-45, Crippling Gap>45

TTC.clean <- TTC.clean[which(TTC.clean$Min.Gap < 60 & TTC.clean$Min.Gap > 0 ),]



TTC.clean$Min.Gap=as.factor(TTC.clean$Min.Gap)

###Time attribute will also be discretized, and data will be binned into 3-time intervals.
#Morning:From 06:00 AM to 12:00 AM,
#Afternoon: From 12:00 Am to 06:00 PM,
#Night: From 06:00 PM to 12:00 AM


install.packages("chron")
library(chron)

TTC.clean$Time=times(paste0(TTC.clean$Time, ":00"))

TTC.clean$TTC.clean_time.labeled=cut(TTC.clean$Time, breaks = times(c('00:00:00', '05:00:00', '11:00:00', '17:00:00', '23:59:00')),
                                    labels = c('Night', 'Morning', 'Afternoon', 'Evening'),  
                                    right=FALSE)


### There sould be 4 lines in our data (YU, BD, SRT, SHP). As there are many types, I'm only going to filter based the 4 codes for lines:

TTC.clean = TTC.clean[grepl(("^SHP$|^YU$|^BD$|^SRT$"),TTC.clean$Line),]

TTC.clean$Line=as.factor(TTC.clean$Line)
str(TTC.clean$Line)

# In this project I will only focus of passenger subway station. 


TTC.clean <- filter(TTC.clean, Station %in% c("BATHURST STATION",
                                               "BAY STATION",
                                               "BAYVIEW STATION",
                                               "BESSARION STATION",
                                               "BLOOR STATION",
                                               "BROADVIEW STATION",
                                               "CASTLE FRANK STATION",
                                               "CHESTER STATION",
                                               "CHRISTIE STATION",
                                               "COLLEGE STATION",
                                               "COXWELL STATION",
                                               "DAVISVILLE STATION",
                                               "DON MILLS STATION",
                                               "DONLANDS STATION",
                                               "DOWNSVIEW PARK STATION",
                                               "DUFFERIN STATION",
                                               "^DUNDAS STATION$",
                                               "DUPONT STATION",
                                               "^EGLINTON STATION$",
                                               "ELLESMERE STATION",
                                               "^FINCH STATION$",
                                               "GLENCAIRN STATION",
                                               "GREENWOOD STATION",
                                               "HIGH PARK STATION",
                                               "HIGHWAY 407 STATION",
                                               "ISLINGTON STATION",
                                               "JANE STATION",
                                               "KEELE STATION",
                                               "KENNEDY BD STATION",
                                               "KENNEDY SRT STATION",
                                               "KING STATION",
                                               "KIPLING STATION",
                                               "LANSDOWNE STATION",
                                               "LAWRENCE STATION",
                                               "LAWRENCE WEST STATION",
                                               "LAWRENCE EAST STATION",
                                               "LESLIE STATION",
                                               "MAIN STREET STATION",
                                               "MCCOWAN STATION",
                                               "MIDLAND STATION",
                                               "MUSEUM STATION",
                                               "NORTH YORK CTR STATION",
                                               "OLD MILL STATION",
                                               "OSGOODE STATION",
                                               "OSSINGTON STATION",
                                               "PAPE STATION",
                                               "PIONEER VILLAGE STATIO",
                                               "QUEEN STATION",
                                               "QUEEN'S PARK STATION",
                                               "ROSEDALE STATION",
                                               "ROYAL YORK STATION",
                                               "RUNNYMEDE STATION",
                                               "SCARB CTR STATION",
                                               "SCARBOROUGH CTR STATIO",
                                               "SHEPPARD WEST STATION",
                                               "SHEPPARD STATION",
                                               "SHERBOURNE STATION",
                                               "SPADINA BD STATION",
                                               "ST ANDREW STATION",
                                               "^ST CLAIR STATION$",
                                               "ST GEORGE BD STATION",
                                               "ST GEORGE YUS STATION",
                                               "ST PATRICK STATION",
                                               "SUMMERHILL STATION",
                                               "UNION STATION",
                                               "VAUGHAN MC STATION",
                                               "VICTORIA PARK STATION",
                                               "WARDEN STATION",
                                               "WELLESLEY STATION",
                                               "WILSON STATION",
                                               "WOODBINE STATION",
                                               "YORK MILLS STATION",
                                               "YORK UNIVERSITY STATIO",
                                               "YORKDALE STATION"))

TTC.clean$Month<- strftime(TTC.clean$Date, "%m")
TTC.clean$Month= gsub("01","January", TTC.clean$Month)
TTC.clean$Month= gsub("02","February", TTC.clean$Month)
TTC.clean$Month= gsub("03","March", TTC.clean$Month)
TTC.clean$Month= gsub("04","April", TTC.clean$Month)
TTC.clean$Month= gsub("05","May", TTC.clean$Month)
TTC.clean$Month= gsub("06","June", TTC.clean$Month)
TTC.clean$Month= gsub("07","July", TTC.clean$Month)
TTC.clean$Month= gsub("08","August", TTC.clean$Month)
TTC.clean$Month= gsub("09","Spetember", TTC.clean$Month)
TTC.clean$Month= gsub("10","October", TTC.clean$Month)
TTC.clean$Month= gsub("11","November", TTC.clean$Month)
TTC.clean$Month= gsub("12","December", TTC.clean$Month)



TTC.Subway= TTC.clean[, c(3,4,5,6,7,8,10,11)]

head(TTC.Subway)
str(TTC.Subway)
summary(TTC.Subway)

TTC.Subway$Month=as.factor(TTC.Subway$Month)

TTC.Subway=TTC.Subway[!is.na(TTC.Subway$TTC.clean_time.labeled), ]


write.csv(TTC.Subway,file="C:\\Users\\Tina_\\Desktop\\CKME 136_Capstone\\Documents\\Data\\TTCsubway_output.csv")
