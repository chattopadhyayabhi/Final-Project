Chicago_Crimes_2012_to_2017 <- read.csv("//sttnas/StudentReds/chattop3/Desktop/Chicago_Crimes_2012_to_2017.csv")
crime <- Chicago_Crimes_2012_to_2017
save <- crime
crime <- crime[crime$Year >= 2012,] #Get Year 2010 and Above
crime<- crime[crime$Year <= 2016,]

#crime<-save
#head(save)
drop = c("X","ID","Case.Number","Block","IUCR","Description","Arrest","Domestic","Beat","Ward","Community.Area","Updated.On","Location",
         "X.Coordinate","Y.Coordinate","Location.Description","FBI.Code")
crime1 <- crime[ , !(names(crime) %in% drop)]
class(crime1$Date)
crime.data <- crime1
str(crime.data)
summary(crime.data)
crime.data <- subset(crime.data, !duplicated(crime.data$CASE.))
summary(crime.data)
crime.data <- subset(crime.data, !is.na(crime.data$LATITUDE))
crime.data <- subset(crime.data, !is.na(crime.data$WARD))
which(is.na(crime.data$LOCATION))
 crime.data <- crime.data[-which(is.na(crime.data$LOCATION)), ]
crime.data$date <- as.POSIXlt(crime.data$DATE..OF.OCCURRENCE, format= "%m/%d/%Y %H:%M")
 head(crime.data$date)
 library(chron)
crime.data$time <- times(format(crime.data$date, "%H:%M:%S"))
head(crime.data$time)
 time.tag <- chron(times= c("00:00:00", "06:00:00", "12:00:00", "18:00:00",
                              "23:59:00"))
 crime.data$time.tag <- cut(crime.data$time, breaks= time.tag,
                            labels= c("00-06","06-12", "12-18", "18-00"), include.lowest=
                            TRUE)
 crime.data$date <- as.POSIXlt(strptime(crime.data$date,
                                          format= "%Y-%m-%d"))
crime.data$day <- weekdays(crime.data$date, abbreviate= TRUE)
crime.data$month <- months(crime.data$date, abbreviate= TRUE)
crime.data$crime <- as.character(crime.data$PRIMARY.DESCRIPTION)
crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT",
                                                      "PROSTITUTION", "SEX OFFENSE"), 'SEX', crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"),
                             "MVT", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFEREWITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                                     "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                                     "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), "NONVIO", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime =="CRIMINAL DAMAGE", "DAMAGE",
                              crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL TRESPASS",
                              "TRESPASS", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHER",
                                                       "NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime == "DECEPTIVE PRACTICE",
                              "FRAUD", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER OFFENSE", "OTHER
                                                       OFFENSE"), "OTHER", crime.data$crime)
 crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS
                                                       VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", crime.data$crime)
 table(crime.data$crime)
 crime.data$ARREST <- ifelse(as.character(crime.data$ARREST) == "Y", 1, 0)
 qplot(crime.data$crime, xlab = "Crime", main ="Crimes in Chicago") 
 ggplot(temp, aes(x= crime, y= factor(time.tag))) +
 geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) +
 scale_y_discrete("Time of day", expand = c(0,-2)) +
 scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") +
 theme_bw() + ggtitle("Crimes by time of day") +
 theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
       (colour = NA))
 length(unique(crime.agg$BEAT))
 length(unique(crime.agg$date))
 beats <- sort(unique(crime.agg$BEAT))
 dates <- sort(as.character(unique(crime.agg$date)))
 temp <- expand.grid(beats, dates)
 names(temp) <- c("BEAT", "date")
 temp <- orderBy(~ BEAT, data= temp)
 model.data <- aggregate(crime.agg[, c('count', 'ARREST')], by=
                         list(crime.agg$BEAT, as.character(crime.agg$date)), FUN= sum)
names(model.data) <- c("BEAT", "date", "count", "ARREST")
model.data <- merge(temp, model.data, by= c('BEAT', 'date'), all.x= TRUE)
model.data$count[is.na(model.data$count)] <- 0
 model.data$ARREST[is.na(model.data$ARREST)] <- 0
 model.data$day <- weekdays(as.Date(model.data$date), abbreviate= TRUE)
 model.data$month <- months(as.Date(model.data$date), abbreviate= TRUE)
 pastDays <- function(x) {
   c(0, rep(1, x))
 }
 model.data$past.crime.7 <- ave(model.data$count, model.data$BEAT,
                                FUN= function(x) filter(x, pastDays(7), sides= 1))
 model.data$past.crime.30 <- ave(model.data$count, model.data$BEAT,
                                   FUN= function(x) filter(x, pastDays(30), sides= 1))
 meanNA <- function(x){
   mean(x, na.rm= TRUE)
 }
 model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),
                                     meanNA(model.data$past.crime.1), model.data$past.crime.1)
model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),
                                     meanNA(model.data$past.crime.7), model.data$past.crime.7)
 model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),
                                      meanNA(model.data$past.crime.30), model.data$past.crime.30)
 model.data$past.arrest.30 <- ave(model.data$ARREST, model.data$BEAT,
                                  FUN= function(x) filter(x, pastDays(30), sides= 1))
 model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),
                                       meanNA(model.data$past.arrest.30), model.data$past.arrest.30)
 model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,
                               model.data$past.arrest.30/model.data$past.crime.30)
 model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0,
                                  model.data$past.crime.7/model.data$past.crime.30)
 library(psych)
 model.cor <- cor(model.data[, c('count', 'past.crime.1', 'past.crime.7',
                                  'past.crime.30','policing', 'crime.trend')])
 model.data <- orderBy(~ date, data= model.data)
rows <- c(1:floor(nrow(model.data)*0.9))
test.data <- model.data[-rows, ]
 model.data <- model.data[rows, ]
 crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 +
                       + policing + crime.trend + factor(day) + season, data= model.data)
 crime.model.pred <- predict(crime.model, test.data, type= "response")
 sqrt(mean((test.data$count - crime.model.pred)^2))
 validate <- data.frame(test.data$count, crime.model.pred)
 names(validate) <- c("actual", "predicted")
 validate$bucket <- with(validate, cut(predicted, breaks=
                                         quantile(predicted, probs= seq(0, 1, 0.1)),
                                         include.lowest= TRUE, labels= c(1:10)))
 validate <- aggregate(validate[, c('actual', 'predicted')], by=
                         list(validate$bucket), FUN = mean)
 
 ######
 ns_model <- "model{

 # Likelihood
 for(i in 1:n){
 Y[i]   ~ dpois(N[i]*exp(mu[i]))
 mu[i]<-beta0+inprod(Z[i,],beta[])+inprod(law_t[i,],alpha[])+theta[i]
 }
 # Priors
 beta0 ~ dnorm(0,0.01)
 for(i in 1:n){theta[i] ~ dnorm(0,inv.tau2)}
 for(j in 1:p){beta[j] ~ dnorm(0,0.01)}
 for(k in 1:s){alpha[k]~dnorm(0,0.01)}
 inv.tau2 ~ dgamma(0.01,0.01)
 tau <- 1/sqrt(inv.tau2)
 }"

 require(rjags)
 library(rjags)

 library(purrr)
 library(spdep)
 library(maptools)
 library(stringr)
 library(maps)
 
 chicago1 <- readShapePoly("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
 data.chicago = attr(chicago1, "data")
 polyg <- poly2nb(chicago1)
 chi.nb=nb2INLA("Chicago.graph", polyg)
 A = nb2mat(chi.nb, style="B")

 M <- diag(rowSums(A))
 D<-diag(ncol(crime))
 
 sp_model <- "model{
 
 # Likelihood
 for(i in 1:n){
 Y[i]  ~ dnegbin(exp(mu[i]))
 mu[i]<-beta0+inprod(Z[i,],beta[])+inprod(x[i,],alpha[])+theta[i]+eps[i]
 }
 
 # Priors
 beta0 ~ dnorm(0,0.001)
 for(j in 1:p){beta[j] ~ dnorm(0,0.001)}
 for(k in 1:s){alpha[k]~dnorm(0,0.001)}
 
 theta[1:n] ~ dmnorm(rep(0,n),inv.taus2*Omega[1:n,1:n])
 eps[1:n] ~ dmnorm(rep(0,n),inv.sig2*D[1:n,1:n])
 Omega[1:n,1:n]<- inverse(M[1:n,1:n]-rho*A[1:n,1:n])
 rho  ~ dunif(0,1)
 inv.taus2 ~ dgamma(0.01,0.01)
 taus <- 1/sqrt(inv.taus2)
 inv.sig2 ~ dgamma(0.01,0.01)
 sig <- 1/sqrt(inv.sig2)
 }"

 library(rjags)
 dat    <- list(Y=Y,n=n,Z=Z,N=N,x=x,A=A,M=M,p=p,D=D,s=s)
 init   <- list(beta0=glm_coef[1],beta=glm_coef[2:4],alpha=glm_coef[5:18],theta=rep(0,ncol(crime)))
 model2 <- jags.model(textConnection(sp_model),inits=init,data = dat,n.chains=2,quiet=TRUE)
 update(model2, 1000000, progress.bar="none")
 samp2  <- coda.samples(model2, 
                        variable.names=c("beta0","beta","alpha","theta","rho","taus","sig"), 
                        n.iter=100000,thin=1)
 
 summary(samp2)
 dic.samples(model2,n.iter=20000)
 
######### 
 require(randomForest)
rndfor <- randomForest(crime.data$Primary ~ factor(day) + season+beats , data = train, ntree = 25)
train$Hours <- sapply(train$Dates1, function(x) as.integer(strftime(x, format = "%H")))
rndfor2 <- randomForest(crime.data$Primary ~ factor(day) + season+beats + Hour, data = train, 
                   ntree = 25)
 
rndfor3 <- randomForest(crime.data$Primary ~ factor(day) + season+beats + Hour+ time.stamp, data = train, 
                   ntree = 25)
predictions.result <- predict(rf, test)

#########

library(sp)
library(INLA)
library(rgeos)
library(maptools)
library(spdep)
chicago1 <- readShapePoly("geo_export_b19e9d82-a6d3-44a1-b0be-d35d8c55ab74.shp")
data.chicago = attr(chicago1, "data")
polyg <- poly2nb(chicago1)
nb2INLA("Chicago.graph", polyg)
Chicago1.adj <- paste(getwd(),"/Chicago.graph",sep="")
crime.data$ID.area <- as.numeric(crime.data$beats)
crime.data$ID.time <- crime.data$time
library(INLA)

formula1 <- count ~ 1 + f(ID.area, model="bym", graph=Chicago.adj) + 
  f(ID.area1,ID.time,model="iid") + time

modelst1 <- inla(formula1,
                 family="negativebinomial",
                 data=data,control.compute=list(dic=TRUE,cpo=TRUE),
                 control.predictor=list(link = 1))

                 
 

   
 