#Load necessary Libraries
library(data.table)
library(igraph)

#Read Input Files
Output <- read.csv("~/Akash/Fractal_MiniProject/Output.csv", header=FALSE, na.strings="NULL", stringsAsFactors=FALSE)
Groups <- read.table("~/Akash/Fractal_MiniProject/Groups.csv", quote="\"", comment.char="", na.strings="NULL")

#Change col Names
colnames(Output) <- c("ASIN_ID","Customer_ID","Rating","Votes","Votes_Helpful","Date")
colnames(Groups) <- c("Group")
summary(Output)
str(Output)
summary(Groups)
str(Groups)

# Convert Column to Date Datatype
Output$Date1 <- as.Date(Output$Date)

#Delete Old column
Output$Date <- NULL

#Create new column & compute Percentage of Helpful votes
Output$VotesNew <- ifelse(Output$Votes == 0,1,Output$Votes)
Output$Percent_Votes_Helpful <- (Output$Votes_Helpful/Output$VotesNew)*100

#Eliminate NA values & convert to data table
OutputDataTable <- as.data.table(na.omit(Output))
summary(OutputDataTable)

#Only keep the latest Review into consideration
OutputDataTableFilter <- OutputDataTable[,max(Date1),by = list(ASIN_ID,Customer_ID)]
setnames(OutputDataTableFilter,"V1","Date1")

#Inner Join by 3 columns
Test <- merge(OutputDataTableFilter,OutputDataTable,by=c("ASIN_ID","Customer_ID","Date1"),all = FALSE)

#Create a graph based on two columns
g <- graph_from_data_frame(d = subset(Test,select = c(2,1)), directed=TRUE)
summary(g)
no.clusters(g)

# Identify User giving Max Reviews
V(g)$name[max(degree(g, mode="out"))]
graph.density(g)