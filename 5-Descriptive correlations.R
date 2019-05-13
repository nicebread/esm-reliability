## ======================================================================
## Compute scale inter-correlations on three levels:
## Between-person, within-person/between-days, within-person between-moments
## ======================================================================

library(dplyr)
library(psych) # for disattenuated correlations

load(file="cache/S1.RData")
load(file="cache/S2.RData")
load(file="cache/varDecomp.RData")   # contains the reliabilities for disattenuation

# load all files in the /helpers directory
R.utils::sourceDirectory("helpers/", modifiedOnly=TRUE)

# Helper functions: (back)transform correlations to Fisher's Z
r2Z <- function(r) {
	# tweak to avoid infinite results:
	r[r == 1] <- 1-1e-10
	r[r == -1] <- -(1-1e-10)
	return(0.5 * log((1 + r)/(1 - r)))
}

Z2r <- function(Z) {
	return((exp(2*Z)-1)/(exp(2*Z)+1))
}


# day_ID must be nested under person (i.e., running from 1 to 14, for example)
# rel.person, rel.day, and rel.moment are vectors with the reliabilities of the scale values.
# Must be in the same order as the variable names in `varList`


computeCorOnLevels <- function(dat, varList = c("RS", "Ind", "Pow", "A", "C"), person_ID = "person_uid", day_ID = "day_id", rel.person=NA, rel.day=NA, rel.moment=NA) {

	longData <- dat[, c(varList, person_ID, day_ID)] %>% as.data.frame()

	## ======================================================================
	##  aggregate on moment level
	## ======================================================================

	# within person correlations for each person
	# remove day mean level differences to get rid of between-day effects
	
	cors.moment <- data.frame()
	
	# cycle through persons
	for (i in unique(longData[, person_ID])) {
		 person_moment <- longData[longData[, person_ID]==i, ]

		 # cycle through variables, compute their daymean, and the centered values
		 for (v in 1:length(varList)) {
			 DM <- aggregate(person_moment[, varList[v]], list(person_moment[, day_ID]), mean, na.rm=TRUE)
			 colnames(DM) <- c(day_ID, paste0(varList[v], ".daymean"))
			 person_moment <- merge(person_moment, DM, by=day_ID)
			 person_moment[, paste0(varList[v], ".c")] <- person_moment[, varList[v]] - person_moment[, paste0(varList[v], ".daymean")]
		 }
		 
		 C1 <- cor(person_moment[, paste0(varList, ".c")], use="pairwise")			 

		 # vectorize correlation matrix
		 C1.vector <- c(C1)
 
		 cors.moment <- rbind(cors.moment, C1.vector)
	}
	

	# average within-person (moment-level) correlations; use Fisher's-Z (back)transformation for proper average correlations
	cors.moment.avg <- cors.moment %>% r2Z() %>% colMeans(na.rm=TRUE) %>% Z2r()

	# bring back to tabular format
	res.moment <- matrix(cors.moment.avg, nrow=length(varList), ncol=length(varList), dimnames=list(varList, varList))


	## ======================================================================
	##  aggregate on day level
	## ======================================================================

	longData.day <- longData %>% 
		group_by_at(c(person_ID, day_ID)) %>% 
		summarise_at(varList, mean, na.rm=TRUE) %>% 
		ungroup() %>% 
		as.data.frame()
		
	# within person correlations for each person (day-level)
	cors.day <- data.frame()
	for (i in unique(longData.day[, person_ID])) {
		 person_day <- longData.day[longData.day[, person_ID]==i, ]
		 C2 <- cor(person_day[, varList], use="pairwise")
	 
		 # vectorize correlation matrix
		 C2.vector <- c(C2)
	 
		 cors.day <- rbind(cors.day, C2.vector)
	}

	# average within-person (day-level) correlations; use Fisher's-Z (back)transformation for proper average correlations
	cors.day.avg <- cors.day %>% r2Z() %>% colMeans(na.rm=TRUE) %>% Z2r()

	# bring back to tabular format
	res.day <- matrix(cors.day.avg, nrow=length(varList), ncol=length(varList), dimnames=list(varList, varList))

	## ======================================================================
	##  aggregate on person level
	## ======================================================================

	longData.person <- longData %>% 
		group_by_at(c(person_ID)) %>% 
		summarise_at(varList, mean, na.rm=TRUE) %>% 
		ungroup() %>% 
		as.data.frame()	
	
	C3 <- cor(longData.person[, varList], use="pairwise")

	# bring back to tabular format
	res.person <- C3
	dimnames(res.person) <- list(varList, varList)

	return(list(
			person=res.person,
			day=res.day,
			moment=res.moment
		)
	)
}

# ---------------------------------------------------------------------
# Compute correlations

S1.cors <- computeCorOnLevels(dat=S1, varList = c("RS2", "Ind", "Pow", "A", "C"), person_ID = "person_uid", day_ID = "day_id")
S2.cors <- computeCorOnLevels(dat=S2, varList = c("RS3", "Ind", "Pow", "A", "C"), person_ID = "person_uid", day_ID = "day_id")


# ---------------------------------------------------------------------
# Compute disattenuated correlations

# Disattenuation can result in correlations > 1, these are set to 1.
clamp <- function(x, MIN=-1, MAX=1) {
	x[x < MIN] <- MIN
	x[x > MAX] <- MAX
	x
}

# for S1
rel.person.S1 <- reliabilities.avg[c("RS2", "Ind", "Pow", "A", "C"), "R_BP.S1"]
rel.day.S1 <- reliabilities.avg[c("RS2", "Ind", "Pow", "A", "C"), "R_WPD.S1"]
rel.moment.S1 <- reliabilities.avg[c("RS2", "Ind", "Pow", "A", "C"), "R_WPM.S1"]

S1.cors.disatt <- S1.cors
S1.cors.disatt$person <- correct.cor(S1.cors$person, rel.person.S1) %>% clamp()
S1.cors.disatt$day <- correct.cor(S1.cors$day, rel.day.S1) %>% clamp()
S1.cors.disatt$moment <- correct.cor(S1.cors$moment, rel.moment.S1) %>% clamp()


# for S2
rel.person.S2 <- reliabilities.avg[c("RS3", "Ind", "Pow", "A", "C"), "R_BP.S2"]
rel.day.S2 <- reliabilities.avg[c("RS3", "Ind", "Pow", "A", "C"), "R_WPD.S2"]
rel.moment.S2 <- reliabilities.avg[c("RS3", "Ind", "Pow", "A", "C"), "R_WPM.S2"]

S2.cors.disatt <- S2.cors
S2.cors.disatt$person <- correct.cor(S2.cors$person, rel.person.S2) %>% clamp()
S2.cors.disatt$day <- correct.cor(S2.cors$day, rel.day.S2) %>% clamp()
S2.cors.disatt$moment <- correct.cor(S2.cors$moment, rel.moment.S2) %>% clamp()


# ---------------------------------------------------------------------
#  RAW CORRELATIONS: combine upper half of S1 with upper half of S2; transpose to flip upper tri to lower tri

cor.personlevel <- S1.cors$person
cor.personlevel[lower.tri(cor.personlevel)] <- t(S2.cors$person[lower.tri(S2.cors$person)])

cor.daylevel <- S1.cors$day
cor.daylevel[lower.tri(cor.daylevel)] <- t(S2.cors$day)[lower.tri(S2.cors$day)]

cor.momentlevel <- S1.cors$moment
cor.momentlevel[lower.tri(cor.momentlevel)] <- t(S2.cors$moment)[lower.tri(S2.cors$moment)]

# format for printing
cor.personlevel.string <- f2(cor.personlevel, digits=2, skipZero=TRUE, trimToZero=.005)
cor.daylevel.string <- f2(cor.daylevel, digits=2, skipZero=TRUE, trimToZero=.005)
cor.momentlevel.string <- f2(cor.momentlevel, digits=2, skipZero=TRUE, trimToZero=.005)
diag(cor.personlevel.string) <- diag(cor.daylevel.string) <- diag(cor.momentlevel.string) <- ""
rownames(cor.personlevel.string) <- rownames(cor.daylevel.string) <- rownames(cor.momentlevel.string) <- c("RS", "Ind", "Pow", "A", "C")


# ---------------------------------------------------------------------
#  DISATTENUATED: combine upper half of S1 with upper half of S2 (which contains the disattenuated correlations); transpose to flip upper tri to lower tri

cor.personlevel.disatt <- S1.cors.disatt$person
cor.personlevel.disatt[lower.tri(cor.personlevel.disatt)] <- t(S2.cors.disatt$person)[lower.tri(S2.cors.disatt$person)]
diag(cor.personlevel.disatt) <- NA

cor.daylevel.disatt <- S1.cors.disatt$day
cor.daylevel.disatt[lower.tri(cor.daylevel.disatt)] <- t(S2.cors.disatt$day)[lower.tri(S2.cors.disatt$day)]
diag(cor.daylevel.disatt) <- NA

cor.momentlevel.disatt <- S1.cors.disatt$moment
cor.momentlevel.disatt[lower.tri(cor.momentlevel.disatt)] <- t(S2.cors.disatt$moment)[lower.tri(S2.cors.disatt$moment)]
diag(cor.momentlevel.disatt) <- NA

# format for printing
cor.personlevel.disatt.string <- f2(cor.personlevel.disatt, digits=2, skipZero=TRUE, trimToZero=.005)
cor.daylevel.disatt.string <- f2(cor.daylevel.disatt, digits=2, skipZero=TRUE, trimToZero=.005)
cor.momentlevel.disatt.string <- f2(cor.momentlevel.disatt, digits=2, skipZero=TRUE, trimToZero=.005)
diag(cor.personlevel.disatt.string) <- diag(cor.daylevel.disatt.string) <- diag(cor.momentlevel.disatt.string) <- ""
rownames(cor.personlevel.disatt.string) <- rownames(cor.daylevel.disatt.string) <- rownames(cor.momentlevel.disatt.string) <- c("RS", "Ind", "Pow", "A", "C")


rownames(cor.personlevel.string) <- rownames(cor.daylevel.string) <- rownames(cor.momentlevel.string) <- rownames(cor.personlevel.disastt.string) <- rownames(cor.daylevel.disastt.string) <- rownames(cor.momentlevel.disastt.string) <- c("RS", "Ind", "Pow", "A", "C")

save(cor.personlevel, cor.daylevel, cor.momentlevel,
		 cor.personlevel.string, cor.daylevel.string, cor.momentlevel.string, 
		 cor.personlevel.disatt.string, cor.daylevel.disatt.string, cor.momentlevel.disatt.string, 
		 S1.cors, S2.cors, S1.cors.disatt, S2.cors.disatt,
		 file="cache/corTabs.RData")
		 

 # ---------------------------------------------------------------------
 # Compare disattenauted correlations between levels - signs for ecological fallacy, or generally a pattenr of ergodicity?
 
# difference between disattenuated correlations
p.d.diffs <- as.vector(abs(cor.personlevel.disatt - cor.daylevel.disatt))
summary(p.d.diffs)
hist(p.d.diffs)

# no substantial difference between day- and moment-level:
d.m.diffs <- as.vector(abs(cor.momentlevel.disatt - cor.daylevel.disatt))
summary(d.m.diffs)
hist(d.m.diffs)
