# ClinClean

## Install
```
devtools::install_github("yswutan/ClinClean")
library(ClinClean)
```


## Step 1: read clinical raw data
sheet0: follow up; 
sheet1: patient information
```
sheet0 <- read.table("20191121-1611_follow_data_sheet0.txt", header=T, sep="\t", quote="", fileEncoding="UTF-8", fill=T, stringsAsFactors=FALSE)
sheet1 <- read.table("20191121-1611_follow_data_sheet1_part.txt", header=T, sep="\t", quote="", fileEncoding="UTF-8", stringsAsFactors=FALSE)
```
## Step 2: combination
```
sheet <- SheetConvert(sheet0, sheet1)
```
## Step 3: processing
1.OS: 死亡病人：死亡时间-入院时间 （若入院时间比手术日期晚，则选手术日期）；
                有的病人确认死亡，但是没有死亡时间，取最后一次随访时间作为截止日期；
      未死亡病人：最后一次随访时间-入院时间 （若入院时间比手术日期晚，则选手术日期）

2.DFS: 复发病人：复发/转移时间-手术日期；
       无复发病人：最后一次随访时间/死亡时间-手术日期；
       无复发，死亡，但是没有死亡时间：取最后一次随访时间作为截止日期，减去手术日期

3.DFS <= OS
```
ID1 <- unique(sheet1$'住院号')
OS <- OSProcess(sheet, ID1, "max", Ceiling=TRUE)
DFS <- DFSProcess(sheet, ID1, "FollowUp", Ceiling=TRUE)
```
