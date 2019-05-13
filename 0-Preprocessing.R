library(dplyr)
library(psych)

# load preprocessed data sets from both studies
# THESE DYADIC DATA SETS CANNOT BE PROVIDED OPENLY FOR PRIVACY REASONS
load(file="cache/S1.RData")
load(file="cache/S2.RData")


# ---------------------------------------------------------------------
# Helper function: Turn wide data frame into long, make variable selection.
# Many things are hard-coded for this specific study.
longify <- function(dat.wide, itemCols) {
	
	# print a summary of the inter-item-correlations
	cor(dat.wide %>% select(one_of(itemCols)), use="p") %>% round(2) %>% print()

	dat.long <- dat.wide %>% 
		select(
			couple_uid, person_uid, weekend, 
			person_moment_uid, person_day_uid, moment_id,
			couple_moment_uid, couple_day_uid,
			moment_uid, day_id,
			one_of(itemCols)
			) %>% 
		gather(key="item", value="value", one_of(itemCols)) %>% 
		arrange(couple_uid, person_uid, person_day_uid, person_moment_uid)
		
	return(dat.long)	
}


## ======================================================================
## Study 1 / S1
## ======================================================================

# create scale from z-items at each moment
S1$C <- rowMeans(S1[, c("C_1.z", "C_2.z", "C_3.z", "C_4.z")], na.rm=TRUE)	
S1$A <- rowMeans(S1[, c("Ind_1.z", "Ind_2.z", "Pow_1.z", "Pow_2.z")], na.rm=TRUE)
S1$Pow <- rowMeans(S1[, c("Pow_1.z", "Pow_2.z")], na.rm=TRUE)
S1$Ind <- rowMeans(S1[, c("Ind_1.z", "Ind_2.z")], na.rm=TRUE)
S1$RS2 <- rowMeans(S1[, c("RS_1.z", "RS_2.r.z")], na.rm=TRUE)
	
save(S1, file="cache/S1.RData")


# ---------------------------------------------------------------------
# For reliability estimation: Bring S1 into long format, where each item gets its row.

## Communion
S1.C.long <- longify(S1, itemCols <- c("C_1.z", "C_2.z", "C_3.z", "C_4.z"))

## Supplemental analysis: two-item communion motivation scale from Zygar et al. (2018), EJP
## Non-standardized item responses were used there. (Both items had the same response scale)
S1.C2.long <- longify(S1, itemCols <- c("C_1", "C_2"))

## Agency
# Independence
S1.Ind.long <- longify(S1, itemCols <- c("Ind_1.z", "Ind_2.z"))

# Power
S1.Pow.long <- longify(S1, itemCols <- c("Pow_1", "Pow_2"))

# Agency: Independence + Power
S1.A.long <- longify(S1, itemCols <- c("Ind_1.z", "Ind_2.z", "Pow_1.z", "Pow_2.z"))

## Relationship satisfaction
S1.RS.long <- longify(S1, itemCols <- c("RS_1.z", "RS_2.r.z"))

## Save all as a bundle
save(S1.RS.long, S1.C.long, S1.C2.long, S1.A.long, S1.Pow.long, S1.Ind.long, file="cache/S1.long.RData")



## ======================================================================
## ----------------------------------------------------------------------
## main study --> Henceforward: "Study 2"/S2
## ----------------------------------------------------------------------
## ======================================================================
	
# create scales from z-items
S2$C <- rowMeans(S2[, c("C_1.z", "C_2.z", "C_3.z", "C_4.z")], na.rm=TRUE)	
S2$A <- rowMeans(S2[, c("Ind_1.z", "Ind_2.z", "Pow_1.z", "Pow_2.z", "Pow_3.z")], na.rm=TRUE)
S2$Pow <- rowMeans(S2[, c("Pow_1.z", "Pow_2.z", "Pow_3.z")], na.rm=TRUE)
S2$Ind <- rowMeans(S2[, c("Ind_1.z", "Ind_2.z")], na.rm=TRUE)
S2$RS2 <- rowMeans(S2[, c("RS_1.z", "RS_3")], na.rm=TRUE)
S2$RS3 <- rowMeans(S2[, c("RS_1.z", "RS_2.r.z", "RS_3")], na.rm=TRUE)

save(S2, file="cache/S2.RData")


# ---------------------------------------------------------------------
# For reliability estimation: Bring S2 into long format, where each item gets its row.

## Communion
# the evening ping did not contain the questions for state motivation, therefore filter
S2.C.long <- longify(S2 %>% filter(moment_id <= 3), itemCols <- c("C_1.z", "C_2.z", "C_3.z", "C_4.z"))

## Agency
# the evening ping did not contain the questions for state motivation, therefore filter

# Independence
S2.Ind.long <- longify(S2 %>% filter(moment_id <= 3), itemCols <- c("Ind_1.z", "Ind_2.z"))

# Power
S2.Pow.long <- longify(S2 %>% filter(moment_id <= 3), itemCols <- c("Pow_1.z", "Pow_2.z", "Pow_3.z"))

# Agency: Independence + Power
S2.A.long <- longify(S2 %>% filter(moment_id <= 3), itemCols <- c("Ind_1.z", "Ind_2.z", "Pow_1.z", "Pow_2.z", "Pow_3.z"))

## Relationship satisfaction - 3 items (including the "annoyance" item)
S2.RS3.long <- longify(S2, itemCols <- c("RS_1.z", "RS_2.r.z", "RS_3.z"))

## Relationship satisfaction - 2 items (excluding the "annoyance" item)
S2.RS2.long <- longify(S2, itemCols <- c("RS_1.z", "RS_3.z"))


## Save all as a bundle
save(S2.RS2.long, S2.RS3.long, S2.C.long, S2.A.long, S2.Pow.long, S2.Ind.long, file="cache/S2.long.RData")
