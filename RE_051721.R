require(tidyverse)
require(jsonlite)
#season=data from api.blaseball-reference.com/v1/data/events
season <- fromJSON(file.choose())
game_events <- season$game_events
base_runners <- season$base_runners

base_runners$b1 <- ifelse(as.character
			(base_runners[,"base_before_play"])=="1", 1, 0)
base_runners$b2 <- ifelse(as.character
			(base_runners[,"base_before_play"])=="2", 1, 0)
base_runners$b3 <- ifelse(as.character
			(base_runners[,"base_before_play"])=="3", 1, 0)
base_runners$b4 <- ifelse(as.character
			(base_runners[,"base_before_play"])=="4", 1, 0)

base_runners$a1 <- ifelse(as.character
			(base_runners[,"base_after_play"])=="1", 1, 0)
base_runners$a2 <- ifelse(as.character
			(base_runners[,"base_after_play"])=="2", 1, 0)
base_runners$a3 <- ifelse(as.character
			(base_runners[,"base_after_play"])=="3", 1, 0)
base_runners$a4 <- ifelse(as.character
			(base_runners[,"base_after_play"])=="4", 1, 0)
base_runners$RUNS.SCORED <- as.numeric(base_runners$runs_scored)

base_runs<- base_runners %>%
		group_by(game_event_id) %>%
		summarise(across(b1:RUNS.SCORED, sum)) %>%
		rename(id=game_event_id)

base_runs[base_runs$b1>0,"b1"]<- 1
base_runs[base_runs$b2>0,"b2"]<- 1
base_runs[base_runs$b3>0,"b3"]<- 1
base_runs[base_runs$a1>0,"a1"]<- 1
base_runs[base_runs$a2>0,"a2"]<- 1
base_runs[base_runs$a3>0,"a3"]<- 1
base_runs <- as.data.frame(base_runs)

data <- merge(base_runs, game_events, 
		all.x = TRUE, all.y = TRUE)

brnames <- c("b1","b2","b3","b4",
		"a1","a2","a3","a4","RUNS.SCORED")
data[brnames][is.na(data[brnames])] <- 0

data$home_score <- as.numeric(data$home_score)
data$away_score <- as.numeric(data$away_score)
data$runs_batted_in <- as.numeric(data$runs_batted_in)

data_all <- data
data_po <- subset(data, day > 98)
data <- subset(data, day < 99)
base_runners_all <- base_runners
base_runners_po <- subset(base_runners, !(game_event_id %in% data$id))
base_runners <- subset(base_runners, game_event_id %in% data$id)


data <- data[order(data$day,data$game_id, data$event_index),]
data$HALF.INNING <- with(data, paste(game_id, inning, top_of_inning))

data$RUNS <- with(data, away_score + home_score)

RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, 
				list(HALF.INNING = data$HALF.INNING), sum)
RUNS.SCORED.START <- aggregate(data$RUNS,
		list(HALF.INNING = data$HALF.INNING), "[", 1)

MAX <- aggregate(data$RUNS, 
	list(HALF.INNING = data$HALF.INNING), max)

data <- merge(data, MAX)
N <- ncol(data)
names(data)[N] <- "MAX.RUNS"				
data$RUNS.ROI <- data$MAX.RUNS - data$RUNS

get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }

data$STATE <- get.state(data$b1, data$b2, data$b3, data$outs_before_play)

NOUTS <- with(data, outs_before_play + outs_on_play)
  
data$NEW.STATE <- get.state(data$a1, data$a2, data$a3, NOUTS)
			
data.outs <- summarize(group_by(data, HALF.INNING),
                Outs.Inning = sum(outs_on_play))
data <- merge(data, data.outs)
dataC <- subset(data, Outs.Inning = 3)		
dataC <- subset(dataC, STATE != "000 3")		
 
  RUNS <- summarise(group_by(dataC, STATE), Mean=mean(RUNS.ROI))					
  RUNS$Outs <- substr(RUNS$STATE, 5, 5) 									
  RUNS <- RUNS[order(RUNS$Outs), ]										
  
  RUNS.POTENTIAL <- matrix(c(RUNS$Mean, rep(0, 8)), 32, 1)						
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$STATE, "000 3","001 3",					
                                     "010 3","011 3","100 3","101 3","110 3","111 3") 
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]			
  data$RUNS.NEW.STATE <- RUNS.POTENTIAL[data$NEW.STATE, ]	
  data$RUNS.VALUE <- data$RUNS.NEW.STATE - data$RUNS.STATE + 
    data$RUNS.SCORED


runs.expectancy <- function(data){
  RUNS <- with(data, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ] 
  RUNS.out <- matrix(round(RUNS$x, 3), 8, 3) 
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", 
                               "011", "100", "101", "110", "111")
  base.ordered <- c("000", "100", "010", "001", "110", "101", "011", "111")
  RUNS.out <- RUNS.out[match(base.ordered, row.names(RUNS.out)),]
  
  RUNS.out
}

lweights <- aggregate(list(weight = data$RUNS.VALUE), 
			list(event_type = data$event_type), mean)

out.weight <- mean((subset(data, event_type == "FIELDERS_CHOICE"|
	event_type =="OUT"| event_type =="STRIKEOUT"|
	event_type =="UNKNOWN_OUT")$RUNS.VALUE))
lweights <- rbind(lweights,
		data.frame(event_type = "ALL_OUTS", weight = out.weight))  

lweights$scaled  <- lweights$weight - out.weight

events <- data.frame(table(data$event_type, dnn="event_type")) 

all.outs <- (subset(events, event_type == "FIELDERS_CHOICE")$Freq) +
 (subset(events, event_type =="OUT")$Freq) +(subset(events,event_type =="STRIKEOUT")$Freq)
	
events <- rbind(events, data.frame(event_type = "ALL_OUTS", Freq= all.outs))
lweights <- merge(lweights, events)
lweights$weighted <- lweights$scaled * lweights$Freq


lg.obp <- (sum(subset(lweights, event_type== "SINGLE" | event_type=="DOUBLE" | 
event_type=="TRIPLE" |event_type== "HOME_RUN"|event_type== "WALK"|
event_type== "HIT_BY_PITCH"|event_type== "HOME_RUN_5"|event_type== "QUADRUPLE"|
event_type=="CHARM_WALK")$Freq))/
(sum(subset(lweights, event_type== "SINGLE" | event_type=="DOUBLE" | 
event_type=="TRIPLE" |event_type== "HOME_RUN"|event_type== "WALK"|
event_type== "HIT_BY_PITCH"|event_type== "HOME_RUN_5"|event_type== "QUADRUPLE"|
event_type=="CHARM_WALK"|event_type== "FIELDERS_CHOICE"|event_type== "OUT"
|event_type== "SACRIFICE"|event_type== "STRIKEOUT"|event_type== "UNKNOWN_OUT"|
event_type== "CHARM_STRIKEOUT")$Freq))


raw.lg.woba <- (sum(subset(lweights, event_type== "SINGLE" | event_type=="DOUBLE" | 
event_type=="TRIPLE" |event_type== "HOME_RUN"|event_type== "WALK"|
event_type== "HIT_BY_PITCH"|event_type== "HOME_RUN_5"|event_type== "QUADRUPLE"|
event_type=="CHARM_WALK")$weighted))/
(sum(subset(lweights, event_type== "SINGLE" | event_type=="DOUBLE" | 
event_type=="TRIPLE" |event_type== "HOME_RUN"|event_type== "WALK"|
event_type== "HIT_BY_PITCH"|event_type== "HOME_RUN_5"|event_type== "QUADRUPLE"|
event_type=="CHARM_WALK"|event_type== "FIELDERS_CHOICE"|event_type== "OUT"
|event_type== "SACRIFICE"|event_type== "STRIKEOUT"|event_type== "UNKNOWN_OUT"|
event_type== "CHARM_STRIKEOUT")$Freq))

woba.scale <- lg.obp/raw.lg.woba
lweights$final <- lweights$scaled * woba.scale

