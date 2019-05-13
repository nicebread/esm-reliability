## ======================================================================
## 1. Build tables of variance decomposition
## 2. Compute reliabilities
## ======================================================================

library(dplyr)
library(ascii)
options(scipen=999)
load(file="cache/VDC.S1.RData")
load(file="cache/VDC.S2.RData")

# load all files in the /helpers directory
R.utils::sourceDirectory("helpers/", modifiedOnly=TRUE)

# varDecomp1 function from /helpers directory: Extract variance components from all lme4 fits.
VAR.S1.abs <- sapply(
	list(VDC.S1.RS, VDC.S1.Ind, VDC.S1.Pow, VDC.S1.A, VDC.S1.C, VDC.S1.C2),
	varDecomp1,
	digits=5, include_error = TRUE, relative=FALSE, add_overall=FALSE
) %>% as.data.frame()

VAR.S2.abs <- sapply(
	list(VDC.S2.RS2, VDC.S2.RS3, VDC.S2.Ind, VDC.S2.Pow, VDC.S2.A, VDC.S2.C),
	varDecomp1,
	digits=5, include_error = TRUE, relative=FALSE, add_overall=FALSE
) %>% as.data.frame()

colnames(VAR.S1.abs) <- c("RS", "Ind", "Pow", "A", "C", "C2")
colnames(VAR.S2.abs) <- c("RS2", "RS3", "Ind", "Pow", "A", "C")

VAR.S1.rel <- sapply(
	list(VDC.S1.RS, VDC.S1.Ind, VDC.S1.Pow, VDC.S1.A, VDC.S1.C, VDC.S1.C2),
	varDecomp1,
	digits=5, include_error = FALSE, relative=TRUE, add_overall=FALSE
) %>% as.data.frame()

VAR.S2.rel <- sapply(
	list(VDC.S2.RS2, VDC.S2.RS3, VDC.S2.Ind, VDC.S2.Pow, VDC.S2.A, VDC.S2.C),
	varDecomp1,
	digits=5, include_error = FALSE, relative=TRUE, add_overall=FALSE
) %>% as.data.frame()

colnames(VAR.S1.rel) <- c("RS", "Ind", "Pow", "A", "C", "C2")
colnames(VAR.S2.rel) <- c("RS2", "RS3", "Ind", "Pow", "A", "C")


# sanity check: All coefficients in the same order?
all.equal(rownames(VAR.S1.abs), rownames(VAR.S2.abs))
all.equal(rownames(VAR.S1.rel), rownames(VAR.S2.rel))
all.equal(rownames(VAR.S1.abs)[1:22], rownames(VAR.S2.rel))

# add ID column
VAR.S1.abs$COEF <- rownames(VAR.S1.abs)
VAR.S2.abs$COEF <- rownames(VAR.S2.abs)
VAR.S1.rel$COEF <- rownames(VAR.S1.rel)
VAR.S2.rel$COEF <- rownames(VAR.S2.rel)

# ---------------------------------------------------------------------
# Create explanations and labels for the variance components
# Note: The order of terms is not the same as the order from the varDecomp function
# Will later be merged (and put into the same order) using the COEF column

COEF <- c(
"couple_uid:day_id:moment_id:item",
"person_uid:day_id:moment_id",
"person_uid:day_id:item",
"couple_uid:day_id:moment_id",
"couple_uid:day_id:item",
"person_uid:moment_id:item",
"person_uid:day_id",
"couple_uid:moment_id:item",
"couple_uid:day_id",
"person_uid:moment_id",
"person_uid:item",
"couple_uid:moment_id",
"day_id:moment_id:item",
"couple_uid:item",
"person_uid",
"day_id:moment_id",
"couple_uid",
"day_id:item",
"moment_id:item",
"day_id",
"moment_id",
"item",
"Error"
)

symbols <- c(
"CDMI",
"PDM",
"PDI",
"CDM",
"CDI",
"PMI",
"PD",
"CMI",
"CD",
"PM",
"PI",
"CM",
"DMI",
"CI",
"P",
"DM",
"C",
"DI",
"MI",
"D",
"M",
"I",
"e"
)

labels <- c(
"couple:day:moment:item (CDMI)",
"person:day:moment (PDM)",
"person:day:item (PDI)",
"couple:day:moment (CDM)",
"couple:day:item (CDI)",
"person:moment:item (PMI)",
"person:day (PD)",
"couple:moment:item (CMI)",
"couple:day (CD)",
"person:moment (PM)",
"person:item (PI)",
"couple:moment (CM)",
"day:moment:item (DMI)",
"couple:item (CI)",
"person (P)",
"day:moment (DM)",
"couple (C)",
"day:item (DI)",
"moment:item (MI)",
"day (D)",
"moment (M)",
"item (I)",
"Error (e)"
)


comment <- c(
"Do couples have a stable, differential understanding of items at specific time points of specific days?",		# "couple:day:moment:item (CDMI)",
"Variance between moments (each moment of each person is unique)",		# "person:day:moment (PDM)",
"Do persons have a differential understanding of items at specific days (1 to 14/28)?",		# "person:day:item (PDI)",
"Event-level variance between couples",		# "couple:day:moment (CDM)",
"Do couples have a stable, differential understanding of items at specific days?",		# "couple:day:item (CDI)",
"Do person have a differential understanding of items on specific time points (1 to 5) across all days?",		# "person:moment:item (PMI)",
"Variance between days (each day of each person is a unique day)",		# "person:day (PD)",
"Do couples have a stable, differential understanding of items at specific time points across all days?",		# "couple:moment:item (CMI)",
"Do specific days have different meanings for each couple? (days 1 to 14/28)",		# "couple:day (CD)",
"Variance between time points of a day (pooled within each person)",		# "person:moment (PM)",
"Do persons have a stable, differential understanding of items?",		# "person:item (PI)",
"couple:moment",		# "couple:moment (CM)",
"Do specific items have a specific meaning on specific moments of specific days (across all persons)?",		# "day:moment:item (DMI)",
"Do couples have a stable, differential understanding of items?",		# "couple:item (CI)",
"Variance between persons",		# "person (P)",
"Do certain events (e.g., moment 4 on day 9) have a special meaning across all persons?",		# "day:moment (DM)",
"Variance between couples",		# "couple (C)",
"Do specific items have a specific meaning on specific days, across all persons?",		# "day:item (DI)",
"Do specific items have a specific meaning on specific time points of the day, pooled across all days of all persons?",		# "moment:item (MI)",
"Variance between days 1 to 14/28 (pooled acrossed all persons)",		# "day (D)",
"Variance between time points 1 to 5 (moments are pooled within and across all persons)",		# "moment (M)",
"Do the mean level of items differ?",		# "item (I)",
"Residual error variance"		# "Error (e)"
)

example <- c(
	"Different understanding of items after a conflict between the partners",	#"couple:day:moment:item (CDMI)",
	"",	#"person:day:moment (PDM)",
	"Some persons change the interpretations of items on specific days",	#"person:day:item (PDI)",
	"Shared momentary environment",	#"couple:day:moment (CDM)",
	"Some couples change the interpretation of some items at specific days",	#"couple:day:item (CDI)",
	"Some persons change the interpretation of some items in the evening",	#"person:moment:item (PMI)",
	"",	#"person:day (PD)",
	"Couples differ in their shared understanding of items in the morning vs. in the evening.",	#"couple:moment:item (CMI)",
	"Shared daily characteristics (e.g. being together on a family gathering)",	#"couple:day (CD)",
	"Systematic effects of morning vs. evening for some persons",	#"person:moment (PM)",
	"Differential item functioning for men and women, or for specific persons",	#"person:item (PI)",
	"Systematic effects of morning vs. evening for some couples",	#"couple:moment (CM)",
	"All persons change the interpretation of an item on the evening of ESM day 6.",	#"day:moment:item (DMI)",
	"Couples agree on a common understanding of specific items",	#"couple:item (CI)",
	"",	#"person (P)",
	"All persons report higher values on all items on the first moment of the first ESM day.",	#"day:moment (DM)",
	"",	#"couple (C)",
	"All persons change the interpretation of some items on fridays (assumed that all participant started on a Monday).",	#"day:item (DI)",
	"All persons change the interpretation of some items in the evening.",	#"moment:item (MI)",
	"Time trends across the study, or effects of weekend vs. weekday.",	#"day (D)",
	"Systematic effects of morning vs. evening",	#"moment (M)",
	"Items are z-standardized, therefore we expect only small values",	#"item (I)",
	""	#"Error (e)"
	)



VDC_labels <- data.frame(
	COEF = as.character(COEF),
	sym = symbols,
	source = labels,
	comment = comment,
	example = example,
	stringsAsFactors=FALSE
)

# do not include RS2 from S2 (the two-item scale)
# do not include C2 from S1 (the two-item C scale from EJP - this is just a supplemental analysis not reported in this table)
VDC_results <- data.frame(
	COEF=as.character(VAR.S1.abs$COEF),
	round(VAR.S1.abs %>% select(-COEF, -C2), 2),
	round(VAR.S2.abs %>% select(-COEF, -RS2), 2),
	rbind(paste.matrix(round(VAR.S1.rel %>% select(-COEF, -C2)), "%", sep=""), rep("", 5)),  #add an empty row for the missing error term
	rbind(paste.matrix(round(VAR.S2.rel %>% select(-COEF, -RS2)), "%", sep=""), rep("", 5)),  #add an empty row for the missing error term	
	stringsAsFactors=FALSE
)

VDC_table <- merge(VDC_labels, VDC_results, by="COEF")

# define which terms are theoretically meaningful and which are nuisance terms
# (the result tables will be ordered along these subsets)
theoretically_meaningful <- c("C", "P", "D", "M", "CD", "PD", "CDM", "PDM")
nuisance <- VDC_table$sym[!VDC_table$sym %in% c(theoretically_meaningful, "e")] %>% as.character()
nuisance <- nuisance[order(nchar(as.character(nuisance)))]

VDC_meaningful <- VDC_table[match(theoretically_meaningful, VDC_table$sym), ] %>% select(-sym)
VDC_nuisance <- VDC_table[match(nuisance, VDC_table$sym), ] %>% select(-sym)
VDC_error <- VDC_table %>% filter(source=="Error (e)") %>% select(-sym)

VDC_table_ordered <- rbind(VDC_meaningful, VDC_nuisance, VDC_error)
rownames(VDC_table_ordered) <- NULL
VDC_table_ordered


# ---------------------------------------------------------------------
# Reliability analysis

## notation: 
# n_i = number of items (in the manuscript, this is j)
# n_d = number of days within person (in the manuscript, this is k)
# n_m = number of moments within day (in the manuscript, this is l)

# tab is the variance table (only use the absolute variances!), name is the label of the variable
computeReliabilities <- function(tab, name, n_i, n_m, n_d) {
	s_P <- tab["person_uid", name]
	s_PI <- tab["person_uid:item", name]
	s_D <- tab["day_id", name]
	s_PD <- tab["person_uid:day_id", name]
	s_PDI <- tab["person_uid:day_id:item", name]
	s_PDM <- tab["person_uid:day_id:moment_id", name]
	s_e <- tab["Error", name]

	# Between person
	R_BP <- (s_P + s_PI/n_i) / (s_P + s_PI/n_i + s_D/n_d + s_PD/n_d + s_PDI/(n_d*n_i) + s_e/(n_i*n_m*n_d))

	# Between days
	R_WPD <- (s_PD + s_PDI/n_i) / (s_PD + s_D + s_PDI/n_i + s_e/(n_i*n_m))

	# Between moments
	R_WPM <- (s_PDM) / (s_PDM + s_e/n_i)
	
	return(list(R_BP = R_BP, R_WPD = R_WPD, R_WPM = R_WPM))
}


# ---------------------------------------------------------------------
# Reliabilities when the maximum number (ie., all scheduled pings) is entered into the formulas

R.S1.RS <- computeReliabilities(VAR.S1.abs, name="RS", n_i=2, n_m=5, n_d=14)
R.S2.RS2 <- computeReliabilities(VAR.S2.abs, name="RS2", n_i=2, n_m=5, n_d=28)
R.S2.RS3 <- computeReliabilities(VAR.S2.abs, name="RS3", n_i=3, n_m=5, n_d=28)

R.S1.C <- computeReliabilities(VAR.S1.abs, name="C", n_i=4, n_m=5, n_d=14)
R.S1.C2 <- computeReliabilities(VAR.S1.abs, name="C2", n_i=2, n_m=5, n_d=14) # only reported in footnote
R.S2.C <- computeReliabilities(VAR.S2.abs, name="C", n_i=4, n_m=4, n_d=28)

R.S1.A <- computeReliabilities(VAR.S1.abs, name="A", n_i=4, n_m=5, n_d=14)
R.S2.A <- computeReliabilities(VAR.S2.abs, name="A", n_i=5, n_m=4, n_d=28)

R.S1.Pow <- computeReliabilities(VAR.S1.abs, name="Pow", n_i=2, n_m=5, n_d=14)
R.S2.Pow <- computeReliabilities(VAR.S2.abs, name="Pow", n_i=3, n_m=4, n_d=28)

R.S1.Ind <- computeReliabilities(VAR.S1.abs, name="Ind", n_i=2, n_m=5, n_d=14)
R.S2.Ind <- computeReliabilities(VAR.S2.abs, name="Ind", n_i=2, n_m=4, n_d=28)

# glue together for table

reliabilities <- rbind(
	unlist(c(R.S1.RS, R.S2.RS2)),
	c(rep(NA, 3), unlist(R.S2.RS3)),
	unlist(c(R.S1.Ind, R.S2.Ind)),
	unlist(c(R.S1.Pow, R.S2.Pow)),
	unlist(c(R.S1.A, R.S2.A)),
	unlist(c(R.S1.C, R.S2.C))
) %>% as.data.frame()

rownames(reliabilities) <- c("RS2", "RS3", "Ind", "Pow", "A", "C")
colnames(reliabilities) <- c("R_BP.S1", "R_WPD.S1", "R_WPM.S1", "R_BP.S2", "R_WPD.S2", "R_WPM.S2")

# ---------------------------------------------------------------------
# Collect results in table, format for printing

relTable <- data.frame(
	"Scale" = c("RS2", "RS3", "Ind", "Pow", "A", "C"),
	f2(reliabilities, digits=2, skipZero=TRUE)
)

relTable[relTable=="NA"] <- ""

colnames(relTable) <- c("Scale", "R_BP.S1", "R_WPD.S1", "R_WPM.S1", "R_BP.S2", "R_WPD.S2", "R_WPM.S2")


## ======================================================================
## Additional analysis: Insert the average *actual* number of items, moments, and days.
## This varies because the response rate was not 100% for all persons.
## cf. Scott et al. (2018), Footnote 5, and Shrout & Lane (2012): 
## "However, if different persons have different numbers of days or items, then the reliability of the measurements will vary by respondent. In circumstances such as this, we recommend that investigators report the range of reliability values assuming more or fewer replicate items and diary days."
## ======================================================================

# ---------------------------------------------------------------------
# Compute average number of days and moments per person (i.e., maximum possible days/moments x response rate)

# use power motivation as lower bound, because this has the smallest number of measurements
S1.days.avg <- S1.Pow.long %>% 
	filter(!is.na(value)) %>% # remove rows without answers
	group_by(person_uid) %>% 
	summarise(
		n_days=length(unique(person_day_uid))
	) %>% 
	ungroup() %>% 
	summarize(
		n_days_mean=mean(n_days)
	) %>% pull(n_days_mean)


S2.days.avg <- S2.Pow.long %>% 
	filter(!is.na(value)) %>% 
	group_by(person_uid) %>% 
	summarise(
		n_days=length(unique(person_day_uid))
	) %>% 
	ungroup() %>% 
	summarize(
		n_days_mean=mean(n_days)
	) %>% pull(n_days_mean)


S1.moments.avg <- S1.Pow.long %>% 
	filter(!is.na(value)) %>% 
	group_by(person_uid, day_id) %>% 
	summarise(
		n_moments=length(unique(moment_uid))
	) %>% 
	ungroup() %>% 
	summarize(
		n_moments_mean=mean(n_moments)
	) %>% pull(n_moments_mean)
	
# Relationship satisfaction had up to 5 measurements	
S2.moments.avg.5 <- S2.RS3.long %>% 
	filter(!is.na(value)) %>% 
	group_by(person_uid, day_id) %>% 
	summarise(
		n_moments=length(unique(moment_uid))
	) %>% 
	ungroup() %>% 
	summarize(
		n_moments_mean=mean(n_moments)
	) %>% pull(n_moments_mean)
	
	
# motivation items only had 4 measurements	
S2.moments.avg.4 <- S2.Pow.long %>% 
	filter(!is.na(value)) %>% 
	group_by(person_uid, day_id) %>% 
	summarise(
		n_moments=length(unique(moment_uid))
	) %>% 
	ungroup() %>% 
	summarize(
		n_moments_mean=mean(n_moments)
	) %>% pull(n_moments_mean)	


# enter these average numbers into n_M and n_d
R.S1.RS.avg <- computeReliabilities(VAR.S1.abs, name="RS", n_i=2, n_m=S1.moments.avg, n_d=S1.days.avg)
R.S2.RS2.avg <- computeReliabilities(VAR.S2.abs, name="RS2", n_i=2, n_m=S2.moments.avg.5, n_d=S2.days.avg)
R.S2.RS3.avg <- computeReliabilities(VAR.S2.abs, name="RS3", n_i=3, n_m=S2.moments.avg.5, n_d=S2.days.avg)

R.S1.C.avg <- computeReliabilities(VAR.S1.abs, name="C", n_i=4, n_m=S1.moments.avg, n_d=S1.days.avg)
R.S1.C2.avg <- computeReliabilities(VAR.S1.abs, name="C2", n_i=2, n_m=S1.moments.avg, n_d=S1.days.avg) # .96, .83, .72
R.S2.C.avg <- computeReliabilities(VAR.S2.abs, name="C", n_i=4, n_m=S2.moments.avg.4, n_d=S2.days.avg)

R.S1.A.avg <- computeReliabilities(VAR.S1.abs, name="A", n_i=4, n_m=S1.moments.avg, n_d=S1.days.avg)
R.S2.A.avg <- computeReliabilities(VAR.S2.abs, name="A", n_i=5, n_m=S2.moments.avg.4, n_d=S2.days.avg)

R.S1.Pow.avg <- computeReliabilities(VAR.S1.abs, name="Pow", n_i=2, n_m=S1.moments.avg, n_d=S1.days.avg)
R.S2.Pow.avg <- computeReliabilities(VAR.S2.abs, name="Pow", n_i=3, n_m=S2.moments.avg.4, n_d=S2.days.avg)

R.S1.Ind.avg <- computeReliabilities(VAR.S1.abs, name="Ind", n_i=2, n_m=S1.moments.avg, n_d=S1.days.avg)
R.S2.Ind.avg <- computeReliabilities(VAR.S2.abs, name="Ind", n_i=2, n_m=S2.moments.avg.4, n_d=S2.days.avg)

# glue together for table

reliabilities.avg <- rbind(
	unlist(c(R.S1.RS.avg, R.S2.RS2.avg)),
	c(rep(NA, 3), unlist(R.S2.RS3.avg)),
	unlist(c(R.S1.Ind.avg, R.S2.Ind.avg)),
	unlist(c(R.S1.Pow.avg, R.S2.Pow.avg)),
	unlist(c(R.S1.A.avg, R.S2.A.avg)),
	unlist(c(R.S1.C.avg, R.S2.C.avg))
) %>% as.data.frame()

rownames(reliabilities.avg) <- c("RS2", "RS3", "Ind", "Pow", "A", "C")
colnames(reliabilities.avg) <- c("R_BP.S1", "R_WPD.S1", "R_WPM.S1", "R_BP.S2", "R_WPD.S2", "R_WPM.S2")

# ---------------------------------------------------------------------
# Collect results in table, format for printing

relTable.avg <- data.frame(
	"Scale" = c("RS2*", "RS3", "Ind", "Pow*", "A*", "C"),
	f2(reliabilities.avg, digits=2, skipZero=TRUE)
)

relTable.avg[relTable.avg=="NA"] <- ""

colnames(relTable.avg) <- c("Scale", "R_BP.S1", "R_WPD.S1", "R_WPM.S1", "R_BP.S2", "R_WPD.S2", "R_WPM.S2")

# ---------------------------------------------------------------------
# Differences between maximal reliability (with maximal number of days and moments) and average reliability

reliabilities - reliabilities.avg
summary(reliabilities - reliabilities.avg)


save(VDC_table, VAR.S1.abs, VAR.S2.abs, VAR.S1.rel, VAR.S2.rel, reliabilities, reliabilities.avg, relTable, relTable.avg, theoretically_meaningful, nuisance, file="cache/varDecomp.RData")
#load(file="cache/varDecomp.RData")
