library(quantreg)
library(tidyverse)
library(dplyr)
library(stats)
library(grid)
library(vcd) 
combined<-read.csv("combined_clean.csv")
hect<-read.csv("cleaned_hectare.csv")
squirrel <-read.csv("cleaned_squirrel.csv")


#cleaning shifts to binary
nc <-ncol(squirrel)
Y <-squirrel$Shift
new_Y <-numeric(length(Y))
for (i in 1:length(Y)){
  if (Y[i]=="AM"){
    new_Y[i]<-1
  }else{
    new_Y[i]<-0
  }
}
Y<-new_Y
X <-as.matrix(squirrel[, 17:21])
new_X <-as.matrix(squirrel[, 17:21])
X<-new_X
X[, "Running"] <- ifelse(X[, "Running"] == "TRUE", 1, 0)
#fit logistic regression for shift~activity
model <- glm(Y ~ X, family = binomial)
summary(model)

am <- which(combined$Shift=='AM')
pm <-which(combined$Shift=='PM')

run_am<- which(combined[am,]$Running=="TRUE")
run_am_busy <- sum(combined[run_am,]$Conditions=='Busy')
run_am_medium <- sum(combined[run_am,]$Conditions=='Moderate')
run_am_calm <- sum(combined[run_am,]$Conditions=='Calm')

run_pm<-which(combined[pm,]$Running=="TRUE")
run_pm_busy <- sum(combined[run_pm,]$Conditions=='Busy')
run_pm_medium <- sum(combined[run_pm,]$Conditions=='Moderate')
run_pm_calm <- sum(combined[run_pm,]$Conditions=='Calm')

chase_am <- which(combined[am,]$Chasing=="TRUE")
chase_am_busy <-sum( combined[chase_am,]$Conditions=='Busy')
chase_am_medium <- sum(combined[chase_am,]$Conditions=='Moderate')
chase_am_calm <- sum(combined[chase_am,]$onditions=='Calm')
  
chase_pm <- which(combined[pm,]$Chasing=="TRUE")
chase_pm_busy <- sum(combined[chase_pm,]$Conditions=='Busy')
chase_pm_medium <- sum(combined[chase_pm,]$Conditions=='Moderate')
chase_pm_calm <- sum(combined[chase_pm,]$Conditions=='Calm')
  
climb_am <- which(combined[am,]$Climbing=="TRUE")
climb_am_busy <-sum( combined[climb_am,]$Conditions=='Busy')
climb_am_medium <- sum(combined[climb_am,]$Conditions=='Moderate')
climb_am_calm <- sum(combined[climb_am,]$Conditions=='Calm')

climb_pm <- which(combined[pm,]$Climbing=="TRUE")
climb_pm_busy <- sum(combined[climb_pm,]$Conditions=='Busy')
climb_pm_medium <- sum(combined[climb_pm,]$Conditions=='Moderate')
climb_pm_calm <- sum(combined[climb_pm,]$Conditions=='Calm')

eat_am <- which(combined[am,]$Climbing=="TRUE")
eat_am_busy <- sum(combined[eat_am,]$Conditions=='Busy')
eat_am_medium <- sum(combined[eat_am,]$Conditions=='Moderate')
eat_am_calm <- sum(combined[eat_am,]$Conditions=='Calm')

eat_pm <- which(combined[pm,]$Climbing=="TRUE")
eat_pm_busy <-sum(combined[eat_pm,]$Conditions=='Busy')
eat_pm_medium <- sum(combined[eat_pm,]$Conditions=='Moderate')
eat_pm_calm <- sum(combined[eat_pm,]$Conditions=='Calm')

forage_am <- which(combined[am,]$Foraging=="TRUE")
forage_am_busy <- sum(combined[forage_am,]$Conditions=='Busy')
forage_am_medium <- sum(combined[forage_am,]$Conditions=='Moderate')
forage_am_calm <- sum(combined[forage_am,]$Conditions=='Calm')

forage_pm <- which(combined[pm,]$Foraging=="TRUE")
forage_pm_busy <- sum(combined[forage_pm,]$Conditions=='Busy')
forage_pm_medium <- sum(combined[forage_pm,]$Conditions=='Moderate')
forage_pm_calm <- sum(combined[forage_pm,]$Conditions=='Calm')


am_mov_tb <- matrix(
  c(sum(run_am_busy),sum(run_am_medium),sum(run_am_calm),
    sum(chase_am_busy),sum(chase_am_medium),sum(chase_am_calm),
    sum(climb_am_busy),sum(climb_am_medium),sum(climb_am_calm),
    sum(eat_am_busy),sum(eat_am_medium),sum(eat_am_calm),
    sum(forage_am_busy),sum(forage_am_medium),sum(forage_am_calm)
    )
)
am_mov_tb<-data.frame(am_mov_tb)
write_csv(am_mov_tb,"am_mov_tb.csv")

#mosaic plot for condition, activity based on shifts
data_am <- as.table( 
  matrix( 
    as.matrix(am_mov_tb), 
    
    # specifying the number of rows 
    nrow = 3, 
    byrow = TRUE, 
    
    # creating two lists one for rows 
    # and one for columns 
    dimnames = list( 
      Condition = c('Busy','Moderate','Calm'), 
      Movements = c('Running', 'Chasing', 'Climbing','Eating','Foraging') 
    ) 
  ) 
) 


pm_mov_tb <- matrix(
  c(sum(run_pm_busy),sum(run_pm_medium),sum(run_pm_calm),
    sum(chase_pm_busy),sum(chase_pm_medium),sum(chase_pm_calm),
    sum(climb_pm_busy),sum(climb_pm_medium),sum(climb_pm_calm),
    sum(eat_pm_busy),sum(eat_pm_medium),sum(eat_pm_calm),
    sum(forage_pm_busy),sum(forage_pm_medium),sum(forage_pm_calm)
  )
)

pm_mov_tb<-data.frame(pm_mov_tb)
write_csv(pm_mov_tb,"pm_mov_tb.csv")

data_pm <- as.table( 
  matrix( 
    as.matrix(pm_mov_tb), 
    
    # specifying the number of rows 
    nrow = 3, 
    byrow = TRUE, 
    
    # creating two lists one for rows 
    # and one for columns 
    dimnames = list( 
      Condition = c('Busy','Moderate','Calm'), 
      Movements = c('Running', 'Chasing', 'Climbing','Eating','Foraging') 
    ) 
  ) 
) 

par(mfrow=c(1,2))
mosaicplot(data_am, 
       
       # shade is used to plot colored chart 
       shade=TRUE, 
       
       # adding title to the chart 
       main = "AM activity condition plot",
       type="pearson"
) 
mosaicplot(data_pm, 
       
       # shade is used to plot colored chart 
       shade=TRUE, 
       
       # adding title to the chart 
       main = "PM activity condition plot",
       type="pearson"
) 


library(stringr)
n<- nrow(combined)
for (i in 1:n) {
  #Cleaning temperature data
  #cleaning dog data
  combined$temp[i]<-as.numeric(str_extract(combined$Sighter.Observed.Weather.Data[i], "(\\d+)", group = 1))
  combined$dog[i]<-as.numeric(ifelse((str_extract(combined$Other.Animal.Sightings[i], "(Dogs)", group = 1)=='Dogs'),1,0))
  if (is.na(combined$dog[i])){
    combined$dog[i]<-0
  }
}
write.csv(combined,"dog&temp.csv")
#model for activity and dogs
X_da <-as.matrix(combined[, 18:22])
X_da[, "Running"] <- ifelse(X_da[, "Running"] == "TRUE", 1, 0)
Y_da <-as.numeric(combined$dog)
model_da <- lm(Y_da ~ X_da)
summary(model_da)
# When dogs present more probably does no action.
# More probably does climbing and eating instead of other activities that may involve
# ground behavior
dog_y<- which(combined$dog==1)
dog_n <- which(combined$dog==0)
dog_y_busy <-(which(combined[dog_y,]$Conditions=="Busy"))
dog_y_mod <- (which(combined[dog_y,]$Conditions=="Moderate"))
dog_y_calm <-(which(combined[dog_y,]$Conditions=="Calm"))
dog_n_busy <-(which(combined[dog_n,]$Conditions=="Busy"))
dog_n_mod <- (which(combined[dog_n,]$Conditions=="Moderate"))
dog_n_calm <-(which(combined[dog_n,]$Conditions=="Calm"))

run_y_busy <- sum(combined[dog_y_busy,]$Running=="TRUE")
run_n_busy <- sum(combined[dog_n_busy,]$Running=="TRUE")   
chase_y_busy <- sum(combined[dog_y_busy,]$Chasing=="TRUE")
chase_n_busy <- sum(combined[dog_n_busy,]$Chasing=="TRUE")   
climb_y_busy <- sum(combined[dog_y_busy,]$Climbing=="TRUE")
climb_n_busy <- sum(combined[dog_n_busy,]$Climbing=="TRUE")  
eat_y_busy <- sum(combined[dog_y_busy,]$Eating=="TRUE")
eat_n_busy <- sum(combined[dog_n_busy,]$Eating=="TRUE") 
forage_y_busy <- sum(combined[dog_y_busy,]$Foraging=="TRUE")
forage_n_busy <- sum(combined[dog_n_busy,]$Foraging=="TRUE") 

run_y_mod <- sum(combined[dog_y_mod,]$Running=="TRUE")
run_n_mod <- sum(combined[dog_n_mod,]$Running=="TRUE")   
chase_y_mod <- sum(combined[dog_y_mod,]$Chasing=="TRUE")
chase_n_mod <- sum(combined[dog_n_mod,]$Chasing=="TRUE")   
climb_y_mod <- sum(combined[dog_y_mod,]$Climbing=="TRUE")
climb_n_mod <- sum(combined[dog_n_mod,]$Climbing=="TRUE")  
eat_y_mod <- sum(combined[dog_y_mod,]$Eating=="TRUE")
eat_n_mod <- sum(combined[dog_n_mod,]$Eating=="TRUE") 
forage_y_mod <- sum(combined[dog_y_mod,]$Foraging=="TRUE")
forage_n_mod <- sum(combined[dog_n_mod,]$Foraging=="TRUE") 

run_y_calm <- sum(combined[dog_y_calm,]$Running=="TRUE")
run_n_calm <- sum(combined[dog_n_calm,]$Running=="TRUE")   
chase_y_calm <- sum(combined[dog_y_calm,]$Chasing=="TRUE")
chase_n_calm <- sum(combined[dog_n_calm,]$Chasing=="TRUE")   
climb_y_calm <- sum(combined[dog_y_calm,]$Climbing=="TRUE")
climb_n_calm <- sum(combined[dog_n_calm,]$Climbing=="TRUE")  
eat_y_calm <- sum(combined[dog_y_calm,]$Eating=="TRUE")
eat_n_calm <- sum(combined[dog_n_calm,]$Eating=="TRUE") 
forage_y_calm <- sum(combined[dog_y_calm,]$Foraging=="TRUE")
forage_n_calm <- sum(combined[dog_n_calm,]$Foraging=="TRUE") 


y_mov_tb <- matrix(
  c(sum(run_y_busy),sum(chase_y_busy),sum(climb_y_busy),sum(eat_y_busy),sum(forage_y_busy),
    sum(run_y_mod),sum(chase_y_mod),sum(climb_y_mod),sum(eat_y_mod),sum(forage_y_mod),
    sum(run_y_calm),sum(chase_y_calm),sum(climb_y_calm),sum(eat_y_calm),sum(forage_y_calm)
  )
)
#mosaic plot for dog presence
data_dog_y <- as.table( 
  matrix( 
    as.matrix(y_mov_tb), 
    
    # specifying the number of rows 
    nrow = 3, 
    byrow = TRUE, 
    
    # creating two lists one for rows 
    # and one for columns 
    dimnames = list( 
      Condition = c('Busy','Moderate','Calm'), 
      Movements = c('Running', 'Chasing', 'Climbing','Eating','Foraging') 
    ) 
  ) 
) 

n_mov_tb <- matrix(
  c(sum(run_n_busy),sum(chase_n_busy),sum(climb_n_busy),sum(eat_n_busy),sum(forage_n_busy),
    sum(run_n_mod),  sum(chase_n_mod),sum(climb_n_mod),sum(eat_n_mod),sum(forage_n_mod),
    sum(run_n_calm),sum(chase_n_calm),sum(climb_n_calm),sum(eat_n_calm),sum(forage_n_calm)
  )
)

write.csv(n_mov_tb,"n_mov_tb.csv")

data_dog_n <- as.table( 
  matrix( 
    as.matrix(n_mov_tb), 
    
    # specifying the number of rows 
    nrow = 3, 
    byrow = TRUE, 
    
    # creating two lists one for rows 
    # and one for columns 
    dimnames = list( 
      Condition = c('Busy','Moderate','Calm'), 
      Movements = c('Running', 'Chasing', 'Climbing','Eating','Foraging') 
    ) 
  ) 
) 

par(mfrow=c(1,2))
mosaicplot(data_dog_y, 

           shade=FALSE, 
           # adding title to the chart 
           main = "Dog present activity condition plot",
           type="pearson"
) 
mosaicplot(data_dog_n, 
        
           shade = FALSE,
           # adding title to the chart 
           main = "Dog absent activity condition plot",
           type="pearson"
) 

#experiments
#lm_da <-lm(Y_da ~ X_da[, "Running"]+X_da[, "Eating"]+X_da[, "Chasing"]+X_da[, "Climbing"]+X_da[, "Foraging"])
#summary(lm_da)

#glm_da <-glm(X_da[, "Eating"]~Y_da )
             #X_da[, "Running"]++X_da[, "Chasing"]+X_da[, "Climbing"]+X_da[, "Foraging"],family = binomial)
#summary(glm_da$coefficients)

#Fitting against temperature (with activities)
has_temp <-which(!is.na(combined$temp))
temp_d <- combined[has_temp,]
temp_Y <- as.numeric(temp_d$temp)
temp_X <- as.matrix(temp_d[, 18:22])
temp_X[, "Running"] <- ifelse(temp_X[, "Running"] == "TRUE", 1, 0)
m_temp.all <- rq(temp_Y~ temp_X, tau = seq(0.05, 0.95, by = 0.05), data=temp_d)
m_temp.plot <- summary(m_temp.all)
plot(m_temp.plot)
tau_m<-data.frame(m_temp.all$coefficients)
m_pre <-m_temp.all$fitted.values
plot(m_pre[,19])

#lm_temp <- lm(temp_Y~temp_X[, "Running"]+temp_X[, "Eating"]+temp_X[, "Chasing"]+temp_X[, "Climbing"]+temp_X[, "Foraging"])
#summary(lm_temp)


combined$Shift <-as.numeric(ifelse(combined$Shift=="AM",1,0))


combined$Running<-as.numeric(ifelse(combined$Running=="TRUE",1,0))
combined$Chasing <-as.numeric(ifelse(combined$Chasing=="TRUE",1,0))
combined$Climbing<-as.numeric(ifelse(combined$Climbing=="TRUE",1,0))
combined$Eating<-as.numeric(ifelse(combined$Eating=="TRUE",1,0))
combined$Foraging<-as.numeric(ifelse(combined$Foraging=="TRUE",1,0))
#categorizing variables as movement/food
combined$Movement<-as.numeric(ifelse(combined$Movement=="TRUE",1,0))
combined$Food <-as.numeric(ifelse(combined$Food=="TRUE",1,0))
#categorizing hectare, temperature
combined$hec <-as.numeric(str_extract(combined$Hectare,"(\\d+)",group = 1))
combined$hec <- ifelse(combined$hec <=14, "low", ifelse(is.numeric(combined$hec)&combined$hec<=28,"mid","high"))
combined$temp <- ifelse(combined$temp<=30,"cold", ifelse(is.numeric(combined$temp)&combined$temp<=60, "moderate","warm"))

#training for decision tree
ncs <- ncol(combined)
#deleting NAs
combined <- combined[which(!is.na(combined$temp)),]
combined <- combined[which(!combined$Litter==""),]
combined <-combined[which(!is.na(combined$Location)),]
#select condition variables
selected <- combined[,c(34,35,ncs-3,ncs-2,ncs-1,ncs,7,39)]
move <- selected[,c(-2)]
food <- selected[,c(-1)]
#data split
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- (1: total_row)
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
move_train <-create_train_test(move,0.8,train=TRUE)
move_test <- create_train_test(move,0.8, train=FALSE)

food_train <- create_train_test(food,0.8,train = TRUE)
food_test <- create_train_test(food,0.8,train = FALSE)

library(rpart)
library(rpart.plot)
#train movement
fit_move <- rpart(Movement~., data = move_train, method = 'class')
rpart.plot(fit_move, extra = 106)
predict_move <-predict(fit_move, move_test, type = 'class')

#train food
fit_food <-rpart(Food~., data = food_train, method = 'class')
rpart.plot(fit_food,extra = 106)
predict_food <-predict(fit_food, food_test, type = 'class')



