# Extract variance components from a lme4 fit called "model"
# decomposition of the stable variance (excluding error variance)

#' @param model The lme4 results object
#' @param digits Digits to round to
#' @param include_error Should the residual error variance be included in the results table?
#' @param relative Should variance components be normalized to 100%?
#' @param add_overall Should the overall variance (i.e., sum of all random variance components) be added to the output?
varDecomp1 <- function(model, digits=3, include_error = FALSE, relative=TRUE, add_overall=TRUE) {
	model_summary <- summary(model)
	res_var <- attributes(model_summary$varcor)$sc^2	# residual variance on L1
	variances <- unlist(model_summary$varcor)
	stable_variance <- sum(variances)
	full_variance <- stable_variance+res_var
	if (include_error == FALSE) {
		if (relative == TRUE) {
			res <- variances/stable_variance*100
		} else {
			res <- variances
		}
		
		# Danger zone!! lmer changes the order of coefficients, even if the model syntax is the same. Bring into alphabetical order, except "error" which is last.
		res <- res[order(names(res))]
		
	} else {
		variances <- c(variances, Error=res_var)
		if (relative == TRUE) {
			res <- variances/full_variance*100
		} else {
			res <- variances
		}
		
		# Danger zone!! lmer changes the order of coefficients, even if the model syntax is the same. Bring into alphabetical order, except "error" which is last.
		res0 <- res[order(names(res))]
		res <- c(res0[names(res0) != "Error"], res["Error"])
	}
	
	res <- round(res, digits)
	
	if (add_overall == TRUE) {
		res <- c(res, "Overall error variance"=round(stable_variance + res_var, 3))
	}
	
	return(res)
}
