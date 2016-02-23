# Data Import
data = read.csv("data.csv")
names(data)

# 1. Who has the most openings?
sort(table(data$Agency), decreasing=T)[1]

# 2. Which departments have the highest and the lowest paying positions (based on current job openings)?
summary(data$Salary.Frequency)
min = data$Agency[tapply(data$Salary.Range.From, data$Salary.Frequency, which.min)]
max = data$Agency[tapply(data$Salary.Range.To, data$Salary.Frequency, which.max)]
col_name = c('Annual', 'Daily', 'Hourly')
df_salary = as.data.frame(rbind(as.character(min), as.character(max)))
names(df_salary) = col_name
rownames(df_salary) = c('min', 'max')

# 3. Which jobs do you think are the hardest to fill? (What makes you say that?)
# Looking at reposting rate.
# Check whether repost.
nrow(data)
length(unique(data$Job.ID))

# Reposting exist. Subset only reposted job as a new data frame.
repost_ids = names(sort(table(data$Job.ID), decreasing=T)[sort(table(data$Job.ID), decreasing=T)>1])
repost_ids = as.numeric(repost_ids)
df_repost = subset(data, data$Job.ID %in% repost_ids)

# Check whether Posting.Date is equal to Posting.Updated
sum(as.character(df_repost$Posting.Date) != as.character(df_repost$Posting.Updated))
# Since they are different, it's reasonable to consider posting update.

# Parsing characters into date data.
library(lubridate)
df_repost$Posting.Date = mdy_hms(df_repost$Posting.Date)
df_repost$Posting.Updated = mdy_hms(df_repost$Posting.Updated)

# Creating a function to calculate period between the 1st post time and the last update time.
range = function(alist, blist, group){
    min = as.Date.POSIXct(tapply(alist, group, min))
    max = as.Date.POSIXct(tapply(blist, group, max))
    result = max - min
    result = sort(result, decreasing = T)
}
r = range(df_repost$Posting.Date, df_repost$Posting.Updated, df_repost$Job.ID)

# ID of Top 10 jobs that are hardest to fill.
top_10 = r[1:10]

