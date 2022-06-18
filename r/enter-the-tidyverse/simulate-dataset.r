library(tidyr)

library(simstudy)

narms <- 4
nwaves <- 3

# define a data generator for n=24 observations
set.seed(1234)

## each participant in the study 
participantdef <- (
  defData(varname="age",dist="normal",formula="25",variance=1) %>%
  defDataAdd(varname="sex",dist="binary",formula="0.5") %>%
  defDataAdd(varname="blink0",dist="normal",formula="17.5",variance=1) %>%
  defDataAdd(varname="bmi0",dist="normal",formula="30-0.2*age-2*sex-0.1*blink0",variance=15) %>%
  defDataAdd(varname="hdl0",dist="normal",formula="2+0.006*age+0.016*sex-0.03*bmi0",variance=0.13) %>%
  defDataAdd(varname="trig0",dist="normal",formula="1.5-0.01*age-0.006*sex+0.04*bmi0-0.4*hdl0",variance=0.2) %>%
  defDataAdd(varname="ldl0",dist="normal",formula="3.1-0.012*age-0.45*sex-0.15*hdl0+0.25*trig0+0.016*bmi0",variance=0.6))

## each participant will be visited in nwaves waves for repeated measurements
visitdef <- (
  defDataAdd(varname="month",dist="normal",formula="wave*6",variance=1) %>%
  defDataAdd(varname="blink",dist="normal",formula="blink0-0.01*dose*month-0.005*dose*month*blink0",variance=0.1) %>%
  defDataAdd(varname="bmi",dist="normal",formula="bmi0-0.1*(blink-blink0)+0.5*dose",variance=2) %>%
  defDataAdd(varname="hdl",dist="normal",formula="hdl0-0.03*(bmi-bmi0)",variance=0.01) %>%
  defDataAdd(varname="trig",dist="normal",formula="trig0+0.04*(bmi-bmi0)-0.4*(hdl-hdl0)",variance=0.05) %>%
  defDataAdd(varname="ldl",dist="normal",formula="ldl0-0.15*(hdl-hdl0)+0.25*(trig-trig0)+0.016*(bmi-bmi0)",variance=0.01))

dat <- genData(4*100, id="participant")
dat <- trtAssign(dat, 4, balanced=T, grpName="arm")
# generate baseline data
dat <- addColumns(participantdef, dat)

# define dose for each study arm
dat$dose <- dat$arm-1
dat$dose[dat$arm==narms] <- narms

# generate longitudinal data
dat <- addPeriods(dat, nPeriods=nwaves, idvars="participant", timeid="visitid", perName="wave")
dat <- addColumns(visitdef, dat)
dat <- as.data.frame(dat)

# clean up dataset
is.baseline <- dat$wave == 0
dat$month[is.baseline] <- 0
is.basevar <- grepl("0$", colnames(dat))
for (name in colnames(dat)[is.basevar]) 
  dat[is.baseline,sub("0$","",name)] <- dat[is.baseline,name]
dat <- dat[,!is.basevar]

dat$visitid <- NULL
dat$sex <- factor(c("M","F")[dat$sex+1])
dat$arm <- factor(paste0("arm",dat$arm))
dat$wave <- factor(paste0("wave",dat$wave))

## reorganize the dataset into multiple tables.
participants <- unique(dat[,c("participant","age","sex","arm")])
arms <- unique(dat[,c("arm","dose")])
visits <- unique(dat[,c("participant","wave","month","blink","bmi","hdl","trig","ldl")])

## save data to csv files.
write.csv(participants, file="participants.csv", row.names=F)
write.csv(arms, file="arms.csv", row.names=F)
write.csv(visits, file="visits.csv", row.names=F)

