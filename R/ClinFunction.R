#' SheetConvert
#'
#' @param sheet0 a data.frame of clinical follow up
#' @param sheet1 a data.frame of clinical information
#'
#' @return a combined data.frame
#' @export
#'
#' @examples sheet <- SheetConvert(sheet0, sheet1)
SheetConvert <- function(sheet0, sheet1){
  sheet <- merge(sheet0, sheet1, by="住院号")
  sheet <- sheet[, -grep(".y", colnames(sheet), fixed=T)]
  colnames(sheet) <- gsub(".x", "", colnames(sheet))
  if(length(which(colnames(sheet) %in% c('复发时间.2', '转移时间.1'))) > 0){
    sheet <- sheet[, -which(colnames(sheet) %in% c('复发时间.2', '转移时间.1'))]
    colnames(sheet)[which(colnames(sheet) == '复发时间')] <- '复发待填时间'
    colnames(sheet)[which(colnames(sheet) == '复发时间.1')] <- '复发时间'
  }
  if(length(which(colnames(sheet) %in% c('手术日期'))) > 0){
    sheet$'手术日期' <- as.character(strptime(as.character(sheet$'手术日期'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('死亡时间'))) > 0){
    sheet$'死亡时间' <- as.character(strptime(as.character(sheet$'死亡时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('随访时间'))) > 0){
    sheet$'随访时间' <- as.character(strptime(as.character(sheet$'随访时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('出生年月'))) > 0){
    sheet$'出生年月' <- as.character(strptime(as.character(sheet$'出生年月'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('现况时间'))) > 0){
    sheet$'现况时间' <- as.character(strptime(as.character(sheet$'现况时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('入院日期'))) > 0){
    sheet$'入院日期' <- as.character(strptime(as.character(sheet$'入院日期'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('复发时间'))) > 0){
    sheet$'复发时间' <- as.character(strptime(as.character(sheet$'复发时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('复发后手术时间'))) > 0){
    sheet$'复发后手术时间' <- as.character(strptime(as.character(sheet$'复发后手术时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('转移时间'))) > 0){
    sheet$'转移时间' <- as.character(strptime(as.character(sheet$'转移时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('转移_手术时间'))) > 0){
    sheet$'转移_手术时间' <- as.character(strptime(as.character(sheet$'转移_手术时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('再次手术时间'))) > 0){
    sheet$'再次手术时间' <- as.character(strptime(as.character(sheet$'再次手术时间'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('正常_检查日期'))) > 0){
    sheet$'正常_检查日期' <- as.character(strptime(as.character(sheet$'正常_检查日期'),'%Y/%m/%d %H:%M:%S'))
  }
  if(length(which(colnames(sheet) %in% c('入院日期'))) == 0){
    sheet$'入院日期' <- sheet$'手术日期'
  }
  return(sheet)
}
#
#' FollwUpRelapse
#'
#' @param sheet_individual clinical follow up data of a individual
#'
#' @return a list of relapse data
#' @export
#'
#' @examples RawRelapseList <- FollwUpRelapse(sheet_individual)
FollwUpRelapse <- function(sheet_individual){
  relapse <- union(grep("转移", sheet_individual$'检查后医生意见', fixed=T), grep("复发", sheet_individual$'检查后医生意见', fixed=T))
  normal <- grep("正常", sheet_individual$'检查后医生意见', fixed=T)
  record <- union(grep("病历", sheet_individual$'回复形式', fixed=T), grep("门诊", sheet_individual$'回复形式', fixed=T))
  if(length(normal) > 0 & length(relapse) > 0){
    if(max(normal) > min(relapse)){
      if(length(intersect(relapse, record)) > 0){
        relapse <- min(intersect(relapse, record))
      } else {
        relapse <- relapse[which(relapse > max(normal))]
      }
    } else {
      relapse <- min(relapse)
    }
  } else {
    if(length(relapse) > 0){
      relapse <- min(relapse)
    }
  }
  if(length(relapse) == 1){
    relapseDate <- sheet_individual$'随访时间'[relapse]
    #sheet_individual$'复发待填时间' <- relapseDate
  } else {
    relapse <- c()
    relapseDate <- "NA"
  }
  RawRelapseList <- list(relapse, relapseDate)
  return(RawRelapseList)
}
#' replication2
#'
#' @param Character a character
#'
#' @return a list of replacated index
#' @export
#'
#' @examples ReplicateList <- replication2(Character)
replication2 <- function(Character){
  ReplicateList <- list()
  numNode <- 0
  repnum <- which(duplicated(Character))
  while(length(repnum) > 0){
    numNode <- numNode+1
    num <- which(Character %in% Character[repnum[1]])
    ReplicateList[[numNode]] <- num
    repnum <- setdiff(repnum, unlist(num))
  }
  return(ReplicateList)
}
#' preRelapseRecord
#'
#' @param sheet_individual clinical follow up data of a individual
#'
#' @return pre-Relapse data with record
#' @export
#'
#' @examples preRelapseRecordDate <- preRelapseRecord(sheet_individual)
preRelapseRecord <- function(sheet_individual){
  record <- union(grep("病历", sheet_individual$'回复形式', fixed=T), grep("门诊", sheet_individual$'回复形式', fixed=T))
  RawRelapseList <- FollwUpRelapse(sheet_individual)
  relapse <- RawRelapseList[[1]]
  relapseDate <- RawRelapseList[[2]]
  if(length(relapse) == 1){
    if(length(record) > 0){
      sigrec <- record[which(record <= relapse)]
      if(length(sigrec) > 0){
        preRelapseRecordDate <- sheet_individual$'随访时间'[max(sigrec)]
      } else {
        preRelapseRecordDate <- sheet_individual$'随访时间'[1]
      }
    } else {
      preRelapseRecordDate <- sheet_individual$'随访时间'[1]
    }
  } else {
    preRelapseRecordDate <- sheet_individual$'随访时间'[1]
  }
  return(preRelapseRecordDate)
}
#' timerank
#'
#' @param sheet_individual clinical follow up data of a individual
#' @param times time serise
#'
#' @return ranked time
#' @export
#'
#' @examples sorttimes <- timerank(sheet_individual, times)
timerank <- function(sheet_individual, times){
  TimesinDB <- unlist(sapply(times, function(x){
    timewithname <- unique(sheet_individual[, which(colnames(sheet_individual) == x)])
    timewithname <- timewithname[!is.na(timewithname)]
    names(timewithname) <- NULL
    return(timewithname)
  }))
  names(TimesinDB)[c(grep('复发时间', names(TimesinDB), fixed=T), grep('转移时间', names(TimesinDB), fixed=T))] <- '复发转移时间'
  replist <- replication2(TimesinDB)
  if(length(replist) > 0){
    for(i in 1:length(replist)){
      names(TimesinDB)[replist[[i]][1]] <- paste(names(TimesinDB)[replist[[i]]], collapse = ";")
    }
    TimesinDB <- TimesinDB[-setdiff(unlist(replist), sapply(replist, function(x) x[1]))]
  }

  RelapseTimeFromFU <- FollwUpRelapse(sheet_individual)[[2]]
  names(TimesinDB)[which(TimesinDB %in% RelapseTimeFromFU)] <- paste(names(TimesinDB)[which(TimesinDB %in% RelapseTimeFromFU)], "复发随访时间", sep=";")
  preRelapseRecordTime <- preRelapseRecord(sheet_individual)
  names(TimesinDB)[which(TimesinDB %in% preRelapseRecordTime)] <- paste(names(TimesinDB)[which(TimesinDB %in% preRelapseRecordTime)], "复发前随访病历时间", sep=";")
  recordTime <- sheet_individual$随访时间[union(grep("病历", sheet_individual$'回复形式', fixed=T), grep("门诊", sheet_individual$'回复形式', fixed=T))]
  names(TimesinDB)[which(TimesinDB %in% recordTime)] <- paste(names(TimesinDB)[which(TimesinDB %in% recordTime)], "随访病历时间", sep=";")
  Times <- sort(TimesinDB)
  return(Times)
}
#' OSCalculate
#'
#' @param sheet_individual clinical follow up data of a individual
#' @param type chose max
#'
#' @return a character with overall survival and patient state (0: survival, 1: dead)
#' @export
#'
#' @examples OS <- OSCalculate(sheet_individual, "max")
OSCalculate <- function(sheet_individual, type){
  times <- c('手术日期', '死亡时间', '随访时间', '入院日期', '复发时间', '复发后手术时间', '转移时间', '转移_手术时间', '再次手术时间')
  sorttimes <- timerank(sheet_individual, times)
  if(isTRUE(unique(sheet_individual$'死亡') == "是")){
    status <- 1
    if(length(which(names(sorttimes) %in% '死亡时间')) > 0){
      OS <- as.numeric(difftime(as.POSIXct(sorttimes['死亡时间']), min(as.POSIXct(sorttimes[grep('入院日期', names(sorttimes))]), as.POSIXct(sorttimes[grep('手术日期',  names(sorttimes))])), units="days"))
    } else {
      if(dim(sheet_individual)[1]-1 <= 0){
        lasttwo <- 1
      } else {
        lasttwo <- dim(sheet_individual)[1]-1
      }
      least_OS <- as.numeric(difftime(as.POSIXct(sheet_individual$'随访时间'[lasttwo]), min(as.POSIXct(sorttimes[grep('入院日期', names(sorttimes))]), as.POSIXct(sorttimes[grep('手术日期',  names(sorttimes))])), units="days"))
      most_OS <- as.numeric(difftime(max(as.POSIXct(sheet_individual$'随访时间')), min(as.POSIXct(sorttimes[grep('入院日期', names(sorttimes))]), as.POSIXct(sorttimes[grep('手术日期',  names(sorttimes))])), units="days"))
      if(type == "max"){
        OS <- most_OS
      }
      if(type == "min"){
        OS <- least_OS
      }
      if(type == "mean"){
        OS <- mean(most_OS, least_OS)
      }
      if(type == "no"){
        OS <- "NA"
      }
    }
  } else {
    status <- 0
    OS <- as.numeric(difftime(max(as.POSIXct(sorttimes)), min(as.POSIXct(sorttimes[grep('入院日期', names(sorttimes))]), as.POSIXct(sorttimes[grep('手术日期',  names(sorttimes))])), units="days"))
  }
  OS <- ceiling(OS)
  OSStatus <- data.frame(OS=OS, OSStatus=status)
  return(OSStatus)
}
#' Relapse Data
#'
#' @param sheet_individual clinical follow up data of a individual
#' @param type chose FollowUp
#' @param TimeInterval months
#'
#' @return relapse data and time list
#' @export
#'
#' @examples RelapseAndTimeList <- RelapseDate(sheet_individual, type, TimeInterval)
RelapseDate <- function(sheet_individual, type, TimeInterval){
  times <- c('手术日期', '死亡时间', '随访时间', '入院日期', '复发时间', '复发后手术时间', '转移时间', '转移_手术时间', '再次手术时间')
  sorttimes <- timerank(sheet_individual, times)
  if(type == "FollowUp"){
    relapseDate <- sorttimes[grep('复发随访时间', names(sorttimes), fixed=T)]
    if(length(relapseDate) == 0){
      names(sorttimes)[length(sorttimes)] <- paste(names(sorttimes)[length(sorttimes)], "截止时间", sep=";")
    }
  }
  if(type == "FollowUp&Death"){
    relapseDate <- c(sorttimes[grep('复发随访时间', names(sorttimes), fixed=T)], sorttimes[grep('死亡时间', names(sorttimes), fixed=T)])
    if(length(relapseDate) > 0){
      relapseDate <- min(relapseDate)
    } else {
      names(sorttimes)[length(sorttimes)] <- paste(names(sorttimes)[length(sorttimes)], "截止时间", sep=";")
    }
  }
  if(type == "Recording"){
    if(length(grep('复发转移时间', names(sorttimes), fixed=T)) > 0){
      relapseDate <-  min(sorttimes[grep('复发转移时间', names(sorttimes), fixed=T)])
    } else {
      relapseDate <- sorttimes[grep('复发转移时间', names(sorttimes), fixed=T)]
    }
    if(length(relapseDate) == 0){
      names(sorttimes)[length(sorttimes)] <- paste(names(sorttimes)[length(sorttimes)], "截止时间", sep=";")
    }
  }
  if(type == "FollowUp&Case"){
    prerelapseRecordDate <- sorttimes[grep('复发前随访病历时间', names(sorttimes), fixed=T)]
    relapseDate <- sorttimes[grep('复发随访时间', names(sorttimes), fixed=T)]
    if(length(relapseDate) != 0){
      rangedate <- as.numeric(difftime(as.POSIXct(relapseDate), as.POSIXct(prerelapseRecordDate), units="days"))
      if(rangedate > TimeInterval*30){
        relapseDate <- sorttimes[grep('不存在', names(sorttimes), fixed=T)]
      } else {
        relapseDate <- relapseDate
      }
    }
    if(length(relapseDate) == 0){
      names(sorttimes)[which(sorttimes == prerelapseRecordDate)] <- paste(names(sorttimes)[which(sorttimes == prerelapseRecordDate)], "截止时间", sep=";")
    }
  }
  if(length(relapseDate) != 0){
    names(sorttimes)[which(sorttimes == relapseDate)] <- paste(names(sorttimes)[which(sorttimes == relapseDate)], "截止时间", sep=";")
  }
  RelapseAndTimeList <- list(relapseDate=relapseDate, sorttimes=sorttimes)
  return(RelapseAndTimeList)
}
#' DFSCalculate
#'
#' @param sheet_individual clinical follow up data of a individual
#' @param relapseDate relapse date
#' @param sorttimes sorted times
#' @param type chose FollowUp
#'
#' @return a character with disease-free survival and patient state (0: no relapse, 1: relapse)
#' @export
#'
#' @examples DFS <- DFSCalculate(sheet_individual, relapseDate, sorttimes, "FollowUp")
DFSCalculate <- function(sheet_individual, relapseDate, sorttimes, type){
  DFS <- as.numeric(difftime(as.POSIXct(sorttimes[grep('截止时间', names(sorttimes), fixed=T)]), as.POSIXct(sorttimes[grep('手术日期', names(sorttimes), fixed=T)]), units="days"))
  if(length(relapseDate) != 0){
    DFSStatus <- 1
  } else {
    DFSStatus <- 0
  }
  if(isTRUE(unique(sheet_individual$'死亡' == "是"))){
    if(type == "FollowUp&Death"){
      DFSStatus <- 1
    }
    if(length(grep('死亡时间', names(sorttimes), fixed=T)) > 0){
      DFS <- min(DFS, as.numeric(difftime(as.POSIXct(sorttimes[grep('死亡时间', names(sorttimes), fixed=T)]), as.POSIXct(sorttimes[grep('手术日期', names(sorttimes), fixed=T)]), units="days")))
    }
  }
  DFS <- ceiling(DFS)
  DFSTable <- data.frame(DFS=DFS, DFSStatus=DFSStatus)
  return(DFSTable)
}
#' OSProcess
#'
#' @param sheet a combined sheet
#' @param ID a character of patient IDs
#' @param type chose max
#' @param Ceiling chose TRUE
#'
#' @return a data.frame with overall survival and patient state (0: survival, 1: dead)
#' @export
#'
#' @examples OSAll <- OSProcess(sheet, ID, "max", Ceiling=TRUE)
OSProcess <- function(sheet, ID, type, Ceiling=FALSE){
  OS <- do.call("rbind", lapply(ID, function(x){
    tmp <- sheet[which(sheet$'住院号' == x), , drop=FALSE]
    if(dim(tmp)[1] == 0){
      rs <- NA
    } else {
      rs <- OSCalculate(tmp, type)
    }
    return(rs)
  }))
  rownames(OS) <- ID
  colnames(OS) <- c("OS_days", "Death")
  if(Ceiling==TRUE){
    OS$OS_days <- ceiling(OS$OS_days)
  }
  return(OS)
}
#' RelapseDateProcess
#'
#' @param sheet a combined sheet
#' @param ID a character of patient IDs
#' @param select chose FollowUp
#' @param TimeInterval months
#'
#' @return a character of relapse date of select patients
#' @export
#'
#' @examples relapseDate <- RelapseDateProcess(sheet, ID, "FollowUp")
RelapseDateProcess <- function(sheet, ID, select, TimeInterval=6){
  relapseDate <- sapply(ID, function(x){
    tmp <- sheet[which(sheet$'住院号' == x), , drop=FALSE]
    if(dim(tmp)[1] == 0){
      rs <- "NA"
    } else {
      rs <- RelapseDate(tmp, select, TimeInterval)$relapseDate
      names(rs) <- NULL
    }
    if(length(rs) < 1){
      rs <- "NA"
    }
    return(rs)
  })
  return(relapseDate)
}
#' PreRelapseRecordDateProcess
#'
#' @param sheet a combined sheet
#' @param ID a character of patient IDs
#'
#' @return a character of pre-relapse date with record of select patients
#' @export
#'
#' @examples preRelapseRecordData <- PreRelapseRecordDateProcess(sheet, ID)
PreRelapseRecordDateProcess <- function(sheet, ID){
  preRelapseRecordData <- sapply(ID, function(x){
    tmp <- sheet[which(sheet$'住院号' == x), , drop=FALSE]
    if(dim(tmp)[1] == 0){
      rs <- "NA"
    } else {
      rs <- preRelapseRecord(tmp)
    }
    if(length(rs) < 1){
      rs <- "NA"
    }
    return(rs)
  })
  return(preRelapseRecordData)
}
#' DFSProcess
#'
#' @param sheet a combined sheet
#' @param ID a character of patient IDs
#' @param select chose FollowUp
#' @param TimeInterval months
#' @param Ceiling chose TRUE
#'
#' @return a data.frame with disease-free survival and patient state (0: no relapse, 1: relapse)
#' @export
#'
#' @examples DFSAll <- DFSProcess(sheet, ID, "FollowUp", Ceiling=TRUE)
DFSProcess <- function(sheet, ID, select, TimeInterval=6, Ceiling=FALSE){
  DFS <- do.call("rbind", lapply(ID, function(x){
    tmp <- sheet[which(sheet$'住院号' == x), , drop=FALSE]
    if(dim(tmp)[1] == 0){
      rs <- NA
    } else {
      RelapseAndTimeList <- RelapseDate(tmp, select, TimeInterval)
      rs <- DFSCalculate(tmp, RelapseAndTimeList$relapseDate, RelapseAndTimeList$sorttimes, select)
    }
    return(rs)
  }))
  rownames(DFS) <- ID
  colnames(DFS) <- c("RFS_days", "Relapse")
  if(Ceiling==TRUE){
    DFS$RFS_days <- ceiling(DFS$RFS_days)
  }
  return(DFS)
}


