## ======================================================================
## VARIANCE DECOMPOSITION, treating dyad members as indistinguishable
## ======================================================================

library(lme4)
options(scipen=999)

# THESE DYADIC DATA SETS CANNOT BE PROVIDED OPENLY FOR PRIVACY REASONS
load(file="cache/S1.long.RData")
load(file="cache/S2.long.RData")


## ======================================================================
## The model formula
## ----------------------------
## We print the full model with all interactions, and comment out all terms
## that make no sense / cannot be estimated.
## ======================================================================

VDC.indist.formula <- formula(
	# main effects
	value ~ 1 + 
		# main effects
		(1|couple_uid) +
		(1|person_uid) +
		(1|day_id) +
		(1|moment_id) +
		(1|item) +
	
		# two-way IA
		# (1|couple_uid:person_uid) +		# makes no sense: persons are nested under couples
		(1|couple_uid:day_id) +
		(1|couple_uid:moment_id) +
		(1|couple_uid:item) +
	
		(1|person_uid:day_id) +
		(1|person_uid:moment_id) +
		(1|person_uid:item) +	
	
		(1|day_id:moment_id) +	
		(1|day_id:item) +	
	
		(1|moment_id:item)+
	
		# three-way IA	
		#(1|couple_uid:person_uid:day_id) +						# makes no sense: persons are nested under couples
		#(1|couple_uid:person_uid:moment_id) +					# makes no sense: persons are nested under couples
		#(1|couple_uid:person_uid:item) +								# makes no sense: persons are nested under couples
		(1|couple_uid:day_id:moment_id) +
		(1|couple_uid:day_id:item) +
		(1|couple_uid:moment_id:item) +
	
		(1|person_uid:day_id:moment_id) +
		(1|person_uid:day_id:item) +
		(1|person_uid:moment_id:item) +
		(1|day_id:moment_id:item) +
	
		# four-way IA	
		#(1|couple_uid:person_uid:day_id:moment_id) +			# makes no sense: persons are nested under couples
		#(1|couple_uid:person_uid:day_id:item) +						# makes no sense: persons are nested under couples
		#(1|couple_uid:person_uid:moment_id:item) +					# makes no sense: persons are nested under couples
		(1|couple_uid:day_id:moment_id:item)
		# (1|person_uid:day_id:moment_id:item)			# not defined (only one data point in this factor)
)
	

# generic starting values
SV <- rep(0.3, 22)

# settings for optimizer: NLOPT_LN_BOBYQA is faster, and allows more finegrained control
ctrl <- lmerControl(
	optimizer="nloptwrap", calc.derivs = FALSE, 
	optCtrl=list(algorithm="NLOPT_LN_BOBYQA", xtol_abs=.005, xtol_rel=.005, print_level=3)
)	
	
# ---------------------------------------------------------------------
# Compute variance decomposition (VDC) for Study 1

VDC.S1.RS  <- lmer(formula=VDC.indist.formula, data=S1.RS.long, control=ctrl, verbose=3, start=SV)
VDC.S1.C   <- lmer(formula=VDC.indist.formula, data=S1.C.long, control=ctrl, verbose=3, start=SV)
VDC.S1.C2  <- lmer(formula=VDC.indist.formula, data=S1.C2.long, control=ctrl, verbose=3, start=SV)
VDC.S1.A   <- lmer(formula=VDC.indist.formula, data=S1.A.long, control=ctrl, verbose=3, start=SV)
VDC.S1.Pow <- lmer(formula=VDC.indist.formula, data=S1.Pow.long, control=ctrl, verbose=3, start=SV)
VDC.S1.Ind <- lmer(formula=VDC.indist.formula, data=S1.Ind.long, control=ctrl, verbose=3, start=SV)

save(VDC.S1.RS, VDC.S1.Ind, VDC.S1.Pow, VDC.S1.A, VDC.S1.C, VDC.S1.C2, file="cache/VDC.S1.RData")

# ---------------------------------------------------------------------
# Compute variance decomposition (VDC) for Study 2

VDC.S2.RS2  <- lmer(formula=VDC.indist.formula, data=S2.RS2.long, control=ctrl, verbose=3, start=SV)
VDC.S2.RS3  <- lmer(formula=VDC.indist.formula, data=S2.RS3.long, control=ctrl, verbose=3, start=SV)
VDC.S2.C    <- lmer(formula=VDC.indist.formula, data=S2.C.long, control=ctrl, verbose=3, start=SV)
VDC.S2.A    <- lmer(formula=VDC.indist.formula, data=S2.A.long, control=ctrl, verbose=3, start=SV)
VDC.S2.Pow  <- lmer(formula=VDC.indist.formula, data=S2.Pow.long, control=ctrl, verbose=3, start=SV)
VDC.S2.Ind  <- lmer(formula=VDC.indist.formula, data=S2.Ind.long, control=ctrl, verbose=3, start=SV)
