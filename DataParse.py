import re
import datetime
import csv
from itertools import izip
ASINcol = []
datecol = []
Custcol = []
Ratingcol = []
Votescol = []
Helpfulcol = []
Groupcol = []
ASINID = ""
countreviews = 0
reviewscount = 0
ReviewPresent = False
maincount = 0
NProblem = False
def indent(line):
    strip = line.lstrip()
    return len(line) - len(strip)
def yield_valid_dates(dateStr):
    for match in re.finditer(r"\d{4}-\d{1,2}-\d{1,2}", dateStr):
        try:
            date = datetime.datetime.strptime(match.group(0), "%Y-%m-%d")
            yield date
        except ValueError:
            print "Date Format Error"
            pass
#Program starts here
with open("C:\\Users\\Akash\\Documents\\Akash\\Fractal_MiniProject\\amazon-meta.txt") as file:
    for line in file:
        
        keyvalue = line.split(":")
        indentval = indent(line)
        
        if(indentval == 0 and keyvalue[0]=="ASIN"):
            ASINID = keyvalue[1].strip()
            countreviews = 0
            print "ASIN ID", ASINID
            ASINcol.append(ASINID)
        if(indentval == 2 and (line.strip()=="discontinued product")):
            datecol.append("NULL")
            Custcol.append("NULL")
            Ratingcol.append("NULL")
            Votescol.append("NULL")
            Helpfulcol.append("NULL")
            Groupcol.append("NULL")
        if(indentval == 2 and keyvalue[0].strip() == "reviews"):
            Count = re.sub("downloaded","",keyvalue[2].strip()).strip()
            print "Count-Total", Count
            Count = int(Count)
            Downloaded = re.sub("avg rating","",keyvalue[3].strip()).strip()
            print "Downloaded ", Downloaded
            Downloaded = int(Downloaded)
            if(Count == 0):
                datecol.append("NULL")
                Custcol.append("NULL")
                Ratingcol.append("NULL")
                Votescol.append("NULL")
                Helpfulcol.append("NULL")
            else:
                maincount += Count
            if(Count == Downloaded):
                NProblem = True
            elif(Count != Downloaded):
                NProblem = False
                datecol.append("NULL")
                Custcol.append("NULL")
                Ratingcol.append("NULL")
                Votescol.append("NULL")
                Helpfulcol.append("NULL")
        if(indentval == 2 and line.split(":")[0].strip() == "group"):
            group = line.split(":")[1]
            print "Group", group
            Groupcol.append(group)
        if(indentval == 4 and NProblem == True):
            ReviewPresent = True
            countreviews += 1
            list1 = line.strip().split(":")
            for i in range(len(list1)):
                list1[i] = list1[i].strip()
                for date in yield_valid_dates(list1[i]):
                    datecol.append(date)
                    i+=1
                Customer = re.sub("rating","",list1[i]).strip()
                Custcol.append(Customer)
                i+=1
                Rating = re.sub("votes","",list1[i]).strip()
                Ratingcol.append(Rating)
                i+=1
                Votes = re.sub("helpful","",list1[i]).strip()
                Votescol.append(Votes)
                i+=1
                helpful = list1[i].strip()
                Helpfulcol.append(helpful)
                i+=1
                if(countreviews!=1):
                    ASINcol.append(ASINID)
                break;
print "ASIN COl", len(ASINcol)
print "Customer col", len(Custcol)
print "Rating Col", len(Ratingcol)
print "Votes Col", len(Votescol)
print "Helpful col", len(Helpfulcol)
print "Date col", len(datecol)
print "Group col", len(Groupcol)
print "Main Count", maincount
with open("C:\\Users\\Akash\\Documents\\Akash\\Fractal_MiniProject\\Output.csv", 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(izip(ASINcol,Custcol,Ratingcol,Votescol,Helpfulcol,datecol))
with open("C:\\Users\\Akash\\Documents\\Akash\\Fractal_MiniProject\\Groups.csv", 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(izip(Groupcol))
