#  predict.felm.R
#  ------------------------------------------------------------------------------------------------
#  Guenter J. Hitsch, January 29, 2016
#
#  Adds a predict method to the lfe package.
#  Warning: This is a fix, not a feature.

require(lfe)
require(data.table)



# Usage:
# -------------------------------------------------------------------------------------------------
# fit must be the output from an felm regression
# newdata must be a data frame or data.table with all the variables contained in the 
# original regression!

predict.felm <- function(fit, newdata) {
   
   if (class(fit) != "felm") stop("'fit' is not a felm object")
   if (!("data.frame" %in% class(newdata))) stop("'newdata' must be a data.frame or data.table")

   setDT(newdata)
   uses_FEs = length(fit$fe) > 0

   # Predict output based on estimated coefficients, not inclucing fixed effects
   formula_string = as.character(fit$terms)
   if (uses_FEs) original_formula = paste("~ 0 +", formula_string[2])
   else original_formula = paste("~ ", formula_string[2])
   X = model.matrix(formula(original_formula), newdata)

   # Only retain columns in X if among the original inputs
   X = X[, rownames(fit$coefficients)]

   y = as.vector(X %*% fit$coefficients)

   # Add fixed effect values to prediction
   if (uses_FEs) {
      FE = as.data.table(getfe(fit))
      cols = c("fe", "idx")
      FE[, (cols) := lapply(.SD, as.character), .SDcols = cols]

      for (name in unique(FE$fe)) {
         fe_DT = newdata[, name, with = FALSE]
         fe_DT[, obs_no := .I]
         setnames(fe_DT, name, "idx")
         fe_DT[, idx := as.character(idx)]
         
         fe_DT = merge(fe_DT, FE[fe == name, .(idx, effect)], by = "idx")
         fe_DT = fe_DT[order(obs_no)]
         
         y = y + as.vector(fe_DT$effect)
      }
   }
   
   return(y)
} 
