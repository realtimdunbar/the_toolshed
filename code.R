setwd("/home/timothy/code/the_toolshed/")

season <- 2018

# reads in 2013 retrosheet files, creating two new csv files
download.retrosheet <- function(season){
  # get zip file from retrosheet website
  download.file(
    url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
    , destfile=paste("download.folder", "/zipped/", season, "eve.zip", sep="")
  )
}

unzip.retrosheet <- function(season){
  #unzip retrosheet files
  unzip(paste("download.folder", "/zipped/", season, "eve.zip", sep=""), 
        exdir=paste("download.folder", "/unzipped", sep=""))
}

create.csv.roster = function(year){
  # creates a csv file of the rosters
  filenames <- list.files(path = "download.folder/unzipped/")
  filenames.roster = 
    subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
  read.csv2 = function(file)
    read.csv(paste("download.folder/unzipped/", file, sep=""),header=FALSE)
  R = do.call("rbind", lapply(filenames.roster, read.csv2))
  names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                    "Bats", "Pitches", "Team")
  wd = getwd()
  setwd("download.folder/unzipped")
  write.csv(R, file=paste("roster", year, ".csv", sep=""))
  setwd(wd)
}

cleanup = function(){
  # removes retrosheet files not needed
  wd = getwd()
  setwd("download.folder/unzipped")
  if (.Platform$OS.type == "unix"){
    system("rm *.EVN")
    system("rm *.EVA")
    system("rm *.ROS")
    system("rm TEAM*")} else {
      shell("del *.EVN")
      shell("del *.EVA")
      shell("del *.ROS")
      shell("del TEAM*")
    }       
  setwd(wd)
  setwd("download.folder/zipped")
  if (.Platform$OS.type == "unix"){
    system("rm *.zip")} else {
      shell("del *.zip")
    }
  setwd(wd)
}

compute.runs.expectancy <- function(season){
  # changed -- plyr function replaced with dplyr
  # (increases speed from 114 to 30 sec for 2013 data)
  
  # assume that files "allseason.csv" and "fields.csv"
  # are in current working folder
  # for example, if season = 1961, all1961.csv should be
  # available
  
  # returns play-by-play matrix with new variables
  # RUNS.ROI - runs scored in remainder of inning
  # STATE - current runners/outs state
  # NEW.STATE - new runners/outs state (after play)
  # RUNS.STATE - runs value of current runners/outs state
  # RUNS.NEW.STATE - runs value of new runners/outs state
  # RUNS.VALUE - runs value of play event
  
  data.file <- paste("all", season, ".csv", sep="")
  data <- read.csv(data.file, header=FALSE)
  #  fields <- read.csv("fields.csv")
  #  fields <- read.csv("http://personal.bgsu.edu/~albert/baseball/fields.csv")
  fields <- read.csv("http://www-math.bgsu.edu/~albert/baseball/fields.csv")
  names(data) <- fields[, "Header"]
  
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, 
                           paste(GAME_ID, INN_CT, BAT_HOME_ID))
  
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) +
                             (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, 
                                  list(HALF.INNING = data$HALF.INNING), sum)
  
  RUNS.SCORED.START <- aggregate(data$RUNS, 
                                 list(HALF.INNING = data$HALF.INNING), "[", 1)
  
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  
  data$RUNS.ROI <- data$MAX.RUNS - data$RUNS
  
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                      RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  
  data$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  
  data <- subset(data, (STATE!=NEW.STATE) | (RUNS.SCORED>0))
  
  #  require(plyr)
  #  data.outs <- ddply(data, .(HALF.INNING), summarize,
  #                     Outs.Inning = sum(EVENT_OUTS_CT))
  #  data <- merge(data, data.outs)
  
  require(dplyr)
  data.outs <- summarize(group_by(data, HALF.INNING),
                         Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  
  # for expected runs computation, only consider complete innings
  dataC <- subset(data, Outs.Inning == 3)
  
  RUNS <- summarize(group_by(dataC, STATE), Mean=mean(RUNS.ROI))
  RUNS$Outs <- substr(RUNS$STATE, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  
  RUNS.POTENTIAL <- matrix(c(RUNS$Mean, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$STATE, "000 3","001 3",
                                     "010 3","011 3","100 3","101 3","110 3","111 3") 
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]
  data$RUNS.NEW.STATE <- RUNS.POTENTIAL[data$NEW.STATE, ]
  data$RUNS.VALUE <- data$RUNS.NEW.STATE - data$RUNS.STATE + 
    data$RUNS.SCORED
  
  data
}

runs.expectancy <- function(data){
  RUNS <- with(data, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]  
  RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", 
                               "011", "100", "101", "110", "111")
  RUNS.out
}

download.retrosheet(season)
unzip.retrosheet(season)
system("./process_retrosheet_event.sh")
create.csv.roster(season)
cleanup()
setwd("download.folder/unzipped")
d2018 <- compute.runs.expectancy(2018)
runs.expectancy(d2018)
