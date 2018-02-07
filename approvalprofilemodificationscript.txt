##Load required packages
require(plyr)
require(ggplot2)

##Designate a name for the areas of interest or person responsible for managing them
lib<-c("name")

##Designate relevant sections
sn<-c("LC")
snrange<-c(num:num)

##Create a vector containing all desired call numbers
match<-c(paste(sn, snrange), paste(sn, snrange))

##Read in and merge descriptor files
Sn<-read.csv("descriptor.csv")
descriptors<-rbind(Sn, Sn)

##Read in approval books data
approval<-read.csv("Approval.csv")

##Create a column in the source data with LC Number and a match column
approval$split<-approval$CALL..
approval$split2<-gsub("[A-Z]+", "", approval$split)
approval$LC_Number<-as.numeric(sapply(strsplit(as.character(approval$split2), '\\.'), "[", 1))
approval$match<-paste(approval$LC.Sub, approval$LC_Number)

##Separate out only books that match each of the designated sections
approvalsub<-approval[approval$match %in% match,]

##Create a new data frame with the LC subcategory and minimimun and maximum call number for each descriptor
min<-aggregate(LC_Number ~ Description, data=descriptors, FUN=min)
max<-aggregate(LC_Number ~ Description, data=descriptors, FUN=max)
category<-descriptors[match(unique(descriptors$Description), descriptors$Description),]
category<-merge(category, min, by="Description")
category<-merge(category, max, by="Description")
category<-category[ -c(5)]
descriptors<-descriptors[ -c(1:2)]

##Merge books subset for relevant sections with the descriptor data frame and write the approval subset into a csv file
approvalsub<-merge(approvalsub, descriptors, by="match", all.x=TRUE)
approvalsub2<-approvalsub[ -c(14:15)]
write.csv(approvalsub2, paste(lib, "ApprovalSub.csv", sep=""))

##Read in firm order data from larger collection analysis project
firm<-read.csv("Firm.csv")

##Create a column in the source data with LC Number and a match column
firm$split<-firm$CALL..
firm$split2<-gsub("[A-Z]+", "", firm$split)
firm$LC_Number<-as.numeric(sapply(strsplit(as.character(firm$split2), '\\.'), "[", 1))
firm$match<-paste(firm$LC.Sub, firm$LC_Number)

##Separate out only books that match each of the designated sections
firmsub<-firm[firm$match %in% match,]

##Merge books subset for relevant sections with the descriptor data frame and write the firm order subset into a csv file
firmsub<-merge(firmsub, descriptors, by="match", all.x=TRUE)
firmsub2<-firmsub[ -c(14:15)]
write.csv(firmsub2, paste(lib, "FirmSub.csv", sep=""))

##Count the number of books in each section for firm and approval ordering
acount<-as.data.frame(table(approvalsub$Description))
fcount<-as.data.frame(table(firmsub$Description))

##Sum the total checkouts of aproval and firm order books for each section
acheckouts<-aggregate(TOT.CHKOUT ~ Description, data=approvalsub, FUN="sum")
fcheckouts<-aggregate(TOT.CHKOUT ~ Description, data=firmsub, FUN="sum")

##Merge the counts and total checkouts into one data frame
sum<-merge(acount, acheckouts, by.x="Var1", by.y="Description", all=TRUE)
sum<-merge(sum, fcount, by="Var1", all=TRUE)
sum<-merge(sum, fcheckouts, by.x="Var1", by.y="Description", all=TRUE)

##Approvals - Merge the count and checkout information with the category information and clean up naming conventions
sum<-merge(category, sum, by.x="Description", by.y="Var1", all.x=TRUE)
sum<-sum[ -c(4)]
names(sum)<-c("Description", "LC_Subcategory", "Start_LC_Number", "End_LC_Number", "Approval_Items", "Approval_Circ", "Firm_Items", "Firm_Circ")

##Create total columns for count and circulation
sum$Items<-sum$Approval_Items + sum$Firm_Items
sum$Circ<-sum$Approval_Circ + sum$Firm_Circ

##Read in ILL source data
ill<-read.csv("ILL.csv")

##Create a column in the source data with LC Number and a match column
ill$split<-ill$Call_Number
ill$split2<-gsub("[A-Z]+", "", ill$split)
ill$LC_Number<-as.numeric(sapply(strsplit(as.character(ill$split2), '\\.'), "[", 1))
ill$match<-paste(ill$LC_Subclasses, ill$LC_Number)

##Separate out only ILL requests that match each of the designated sections
illsub<-ill[ill$match %in% match,]

##Merge ILL subset for relevant sections with the descriptor data frame and write the ILL subset into a csv file
illsub<-merge(illsub, descriptors, by="match", all.x=TRUE)
illsub2<-illsub[ -c(18:19)]
write.csv(illsub2, paste(lib, "ILLSub.csv", sep=""))

##Count the number of books requested through ILL for each section and clean up naming conventions
illcount<-as.data.frame(table(illsub$Description))
names(illcount)<-c("Description", "Requests")

##Merge the approvalsum and illsum data sets to create an overall summary data set
sum<-merge(sum, illcount, by="Description", all=TRUE)

##Identify rows with missing data (typically categories in which we have no books), and replace NA values with zero
sum[c("Approval_Circ", "Firm_Circ", "Circ")][is.na(sum[c("Approval_Circ", "Firm_Circ", "Circ")])]<-0

##Calculate use for firm and approvals, and complete ratio calculations for all items combined
  ##Approvals
  sum$aUse<-sum$Approval_Circ/sum$Approval_Items
  sum$aUse<-gsub("NaN", "0", sum$aUse)
  circ<-sum(sum$Circ)
  items<-sum(sum$Items)
  sum$aUse_R<-((sum$Approval_Circ/circ)/(sum$Approval_Items/items))
  sum$aUse_R<-gsub("NaN", "0", sum$aUse_R)
  sum$aRating<-c("Underused")
  sum$aRating[sum$aUse_R>1]<-c("Overused")

  ##Firm Orders
  sum$fUse<-sum$Firm_Circ/sum$Firm_Items
  sum$fUse<-gsub("NaN", "0", sum$fUse)
  sum$fUse_R<-((sum$Firm_Circ/circ)/(sum$Firm_Items/items))
  sum$fUse_R<-gsub("NaN", "0", sum$fUse_R)
  sum$fRating<-c("Underused")
  sum$fRating[sum$fUse_R>1]<-c("Overused")

  ##Combined
  sum$P_Items<-sum$Items/(sum(sum$Items))
  sum$P_Checkouts<-sum$Circ/(sum(sum$Circ))
  sum$Use<-sum$P_Checkouts/sum$P_Items
  sum$Use<-gsub("NaN", "0", sum$Use)
  sum$Rating<-c("Underused")
  sum$Rating[sum$Use>1]<-c("Overused")

##Create percentage columns for the count of ill requests and relating the number of requests to the total number of items (combined)
sum$P_Requests<-sum$Requests/(sum(sum$Requests))
sum$Borrowing<-ifelse(sum$P_Items==0, (sum$P_Requests/(0.1/(sum(sum$Items)))), (sum$P_Requests/(sum$P_Items)))

##Calculate the average and standard deviation for the ill borrowing ratio
m<-mean(sum$Borrowing)
s<-sd(sum$Borrowing)

##Calculate an ill rating based on the ratio, the mean, and the standard deviation:  below mean = low demand, above mean = high demand, above mean+sd = very high demand, above mean+2sd = extremely high demand
sum$ILL_Rating<-c("Low Demand")
sum$ILL_Rating[sum$Borrowing>m]<-c("High Demand")
sum$ILL_Rating[sum$Borrowing>(m+s)]<-c("Very High Demand")
sum$ILL_Rating[sum$Borrowing>(m+2*s)]<-c("Extremely High Demand")

##Calculate a recommendation for firm ordering vs aprovals based on circulation of each
sum$Recommend<-c("No Trend")
sum$aUse<-as.numeric(sum$aUse)
sum$fUse<-as.numeric(sum$fUse)
sum$Recommend[sum$aUse>(sum$fUse+.2)]<-c("Approvals")
sum$Recommend[sum$fUse>(sum$aUse+.2)]<-c("Firm Order")

##Calculate recommendations changes in amount of purchasing based on usage and requests
sum$P_Recommendation<-c("Ease Off")
sum$P_Recommendation[sum$Rating=="Overused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Growth Opportunity")
sum$P_Recommendation[sum$Rating=="Underused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Change in Purchasing")
sum$P_Recommendation[sum$Rating=="Overused" & sum$ILL_Rating=="Low Demand"]<-c("No Changes")

##Reorder the columns
sum<-sum[c(1:11,13:14,16:24,26,12,15,25,27:28)]

##Write to an excel file
write.csv(sum, paste(lib, "Summary.csv", sep=""))

