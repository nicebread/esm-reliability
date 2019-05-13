library(dplyr)
library(psych)

# load preprocessed data sets from both studies
# THESE DYADIC DATA SETS CANNOT BE PROVIDED OPENLY FOR PRIVACY REASONS
load(file="cache/S1.RData")
load(file="cache/S2.RData")

length(unique(S1$couple_uid))
length(unique(S1$person_uid))

length(unique(S2$couple_uid))
length(unique(S2$person_uid))

## ======================================================================
## sample descriptives 
## ======================================================================

# study 1
# Raw data: see https://www.psychdata.de/index.php?main=search&sub=browse&id=zrce16dy99&lang=ger
load("../../Eigene Studien/Pilot 4 (Nov 2016)/R/cache/dat.wide.RData")
S1.sample <- dat.wide %>% group_by(SubjectCode) %>% slice(1) %>% select(SubjectCode, sex, Jahre, stud, bezstatus, jbez, mbez, kinder) %>% mutate(rel.length = (jbez * 12 + mbez) / 12)

prop.table(table(S1.sample$sex)) # (1 = male, 2 = female)
prop.table(table(S1.sample$stud))
describe(S1.sample$Jahre)

describe(S1.sample$rel.length)
S1.sample[, "bezstatus"] <-
  fXactor(
    S1.sample$bezstatus,
    levels = c(1:5),
    labels = c(
      "offene Beziehung",
      "feste Beziehung",
      "verlobt",
      "verheiratet / in eingetragener Lebenspartnerschaft",
      "andere"
    )
  )
prop.table(table(S1.sample$bezstatus))
1 - unname(prop.table(table(S1.sample$bezstatus))[4])
table(S1.sample$kinder)

# study 2
# raw data will be published as a scientific use file

load("../../Eigene Studien/Hauptstudie/R/cache/preq.dat.full.RData")
S2.preqESM <- merge(S2, preq.dat.full, by = c("pid", "studyVariant"))

S2.sample <- S2.preqESM %>% group_by(person_uid) %>% slice(1) %>% select(person_uid, sex, age, stud, schule, schule_o, bezstatus, bezdauer, kinder)

prop.table(table(S2.sample$sex)) # (1 = male, 2 = female)
prop.table(table(S2.sample$stud))
table(S2.sample$schule)
S2.sample$schule_o[S2.sample$schule_o != ""] # 5 additional "Abitur" (-> university degree)
(327+5)/510
describe(S2.sample$age)

describe(S2.sample$bezdauer/12)
prop.table(table(S2.sample$bezstatus))
1 - unname(prop.table(table(S2.sample$bezstatus))[4])
prop.table(table(S2.sample$kinder))


## ======================================================================
## scheduled vs. answered pings, compliance rate
## ======================================================================

# scheduled pings: 

# S1:
130*14*5

# S2:
510*28*4 #(S2: only 4 pings for motivation)
510*28*5 #(S2: 5 pings for relsat)

# moments in data set:
length(unique(S1.C.long$person_moment_uid))  # 100%. This is not 9100 (the full numer of scheduled pings, because we had some a priori exclusions due to Christmas break, and one person starting later than expected)
length(unique(S2.C.long$person_moment_uid))  # 100%

# non-NA moments in data set
S2.C.pings <- S2.C.long[!is.na(S2.C.long$value), "person_moment_uid"] %>% unique %>% nrow
S2.Ind.pings <- S2.Ind.long[!is.na(S2.Ind.long$value), "person_moment_uid"] %>% unique %>% nrow
S2.Pow.pings <- S2.Pow.long[!is.na(S2.Pow.long$value), "person_moment_uid"] %>% unique %>% nrow
S2.A.pings <- S2.A.long[!is.na(S2.A.long$value), "person_moment_uid"] %>% unique %>% nrow
S2.RS2.pings <- S2.RS2.long[!is.na(S2.RS2.long$value), "person_moment_uid"] %>% unique %>% nrow
S2.RS3.pings <- S2.RS3.long[!is.na(S2.RS3.long$value), "person_moment_uid"] %>% unique %>% nrow

S1.C.pings <- S1.C.long[!is.na(S1.C.long$value), "person_moment_uid"] %>% unique %>% nrow
S1.Ind.pings <- S1.Ind.long[!is.na(S1.Ind.long$value), "person_moment_uid"] %>% unique %>% nrow
S1.Pow.pings <- S1.Pow.long[!is.na(S1.Pow.long$value), "person_moment_uid"] %>% unique %>% nrow
S1.A.pings <- S1.A.long[!is.na(S1.A.long$value), "person_moment_uid"] %>% unique %>% nrow
S1.RS.pings <- S1.RS.long[!is.na(S1.RS.long$value), "person_moment_uid"] %>% unique %>% nrow


save(S2.C.pings, S2.Ind.pings, S2.Pow.pings, S2.A.pings, S2.RS2.pings, S2.RS3.pings, S1.C.pings, S1.Ind.pings, S1.Pow.pings, S1.A.pings, S1.RS.pings, file="cache/n_survey.RData")


## ======================================================================
## Sanity check: Why do not all scales have the exact same count of answered surveys?
## Answer: Incomplete/interrupted surveys where participants submitted only partly complete data.
## ======================================================================

inc <- which(is.na(dat.wide$Mot.A3) & !is.na(dat.wide$Mot.A1))

dat.wide[inc, ] %>% select(qMot.A1, Mot.A3, EventStartedTime, EventCompletedTime)
