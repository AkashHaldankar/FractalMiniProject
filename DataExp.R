#Load necessary Libraries
library(data.table)
#Read Input Files
Output <- read.csv("~/Akash/Fractal_MiniProject/Output.csv", header=FALSE, na.strings="NULL", stringsAsFactors=FALSE)
Groups <- read.table("~/Akash/Fractal_MiniProject/Groups.csv", quote="\"", comment.char="", na.strings="NULL")

#Change col Names
colnames(Output) <- c("ASIN_ID","Customer_ID","Rating","Votes","Votes_Helpful","Date")
colnames(Groups) <- c("Groups")

ASIN_ID <- as.data.frame(unique(Output$ASIN_ID))
colnames(ASIN_ID) <- c("ASIN_ID")
#Map IDs to Group
Groups <- cbind(ASIN_ID,Groups)
colnames(Groups) <- c("ASIN_ID","Group")
summary(Output)
str(Output)
summary(Groups)
str(Groups)

# Convert Column to Date Datatype
Output$Date1 <- as.Date(Output$Date)

#Delete Old column
Output$Date <- NULL

#Eliminate NA values & convert to data table
OutputDataTable <- as.data.table(na.omit(Output))
summary(OutputDataTable)

#Only keep the latest Review into consideration
OutputDataTableFilter <- OutputDataTable[,max(Date1),by = list(ASIN_ID,Customer_ID)]
setnames(OutputDataTableFilter,"V1","Date1")

#Inner Join by 3 columns
Test <- merge(OutputDataTableFilter,OutputDataTable,by=c("ASIN_ID","Customer_ID","Date1"),all = FALSE)

#Remove Entries with Same dates for same user within a review
Test1 <- Test[!duplicated(Test[,1:3,with=FALSE])]
Test1_1 <- Test[1:(199),]
users <- as.data.frame(as.integer(as.factor(Test1_1$Customer_ID)))
colnames(users) <- c("USERIDS")
users <- cbind(Test1_1$Customer_ID,users)

#Set Oder of Asin Id & Date
Test2 <- Test1[order(ASIN_ID,Date1)]

#Find Diff between dates of adjacent rows
Test3 <- Test2[,diffDate:=c(NA,diff(Date1)),by=ASIN_ID]

#Identifying Influncers
Influncers1 <- Test1[,as.numeric(median(Votes_Helpful)),by = list(Customer_ID)]
setorder(Influncers1,-V1)
summary(Influncers1)
Influncers1 <- Influncers1[Influncers1$V1 >= 400]

Influncers2 <- Test1[,sum(Votes_Helpful),by = list(Customer_ID)]
setorder(Influncers2,-V1)
summary(Influncers2$V1)
Influncers2 <- Influncers2[Influncers2$V1 >= 10000]

Influncers3 <- Test1[,max(Votes_Helpful),by = list(Customer_ID)]
setorder(Influncers3,-V1)
summary(Influncers3)
Influncers3 <- Influncers3[Influncers3$V1 >= 250]

Influncers <- union(Influncers1$Customer_ID,Influncers2$Customer_ID)
Influncers <- union(Influncers,Influncers3$Customer_ID)

#Extract ASIN_ID having influncers
TP1 <- Test3[,Customer_ID %in% Influncers, by =ASIN_ID]
TP2 <- subset(TP1,TP1$V1 == "TRUE")
TP3 <- as.data.frame(unique(TP2$ASIN_ID))

#Extract all Reviews having influncers
TP4 <- subset(Test3,ASIN_ID %in% as.character(TP3$`unique(TP2$ASIN_ID)`))
Influncers <- as.data.frame(Influncers)
#Merge with Group Column
TP4 <- merge(TP4,Groups,by = c("ASIN_ID"),all = FALSE)
table(TP4$Group)
TP4$Group <- gsub('\n', '',TP4$Group)

#Identify followers for the influncer Here I have selected a random Influncer
Inf_test <- "A1CNNWWDBNUF0F"
f <- function(DF,ID)
  {
  TestDF <- subset(DF,Customer_ID == ID)
  tempDF <- NULL
  tempDF1 <- NULL
  for(i in 1:nrow(TestDF))
  {
    ASINID <- TestDF$ASIN_ID[i]
    DATE <- as.Date(TestDF$Date1[i])
    tempDF <- subset(DF,ASIN_ID == ASINID & Date1 > DATE)
    tempDF <- tempDF[,diffDateCumSum:=cumsum(diffDate),by=ASIN_ID]
    #print(nrow(tempDF))
    tempDF1 <- rbind(tempDF1,tempDF)
    #print(tempDF1)
  }
  setorder(tempDF1,diffDateCumSum)
  tempDF1
  }
TempDF <- f(TP4,Inf_test)

#Taking only the first recent review into consideration
Latest_Reviewer <- TempDF$Customer_ID[1]
Latest_Review <- as.character(TempDF$ASIN_ID[1])

f1 <- function(DF,Reviewer,Review)
{
  #Extract Reviews of Reviewer
  TestDF1 <- subset(DF,Customer_ID == Reviewer & ASIN_ID != Review)
  IMP_Users <- NULL
  #Extract ASIN IDs
  ASINIDS <- as.character(TestDF1$ASIN_ID)
  print(ASINIDS)
  #For Each Review calc User Similarity
  for(i in 1:length(ASINIDS))
  {
    Reviews <- subset(DF,ASIN_ID %in% ASINIDS[i])
    Ratingbyuser <- Reviews$Rating[Reviews$Customer_ID == Reviewer]
    Reviews$RatingDiff <- abs(Reviews$Rating - Ratingbyuser)
    #print(Reviews)
    IMP_Users <- Reviews[RatingDiff < 1]
    Cust <- as.data.table(IMP_Users$Customer_ID)
    Cust <- rbind(Cust,Cust)
  }
  print(unique(Cust$V1))
  print(length(unique(Cust$V1)))
  #Reviews <- subset(DF,ASIN_ID %in% ASINIDS)
  #print(Reviews)
}
f1(TP4,Latest_Reviewer,Latest_Review)