# ------------------------------------------------------------------------------
# CREATION OF PREVALENCE FORMULA
# ------------------------------------------------------------------------------

# PURPOSE: map desired input variables for STM topical prevalence to formula

make_prevalence_formula <- function(data, 
                                    categorical_vars, 
                                    smooth_vars,
                                    smooth_df = 5L) {
  
  # Check whether input is of character type
  
  cv <- unlist(categorical_vars)
  sv <- unlist(smooth_vars)
  
  checkmate::assert_character(cv)
  checkmate::assert_character(sv)
  
  # Make sure columns exist in docvars and do not contain NA (not allowed in 
  # STM prevalence formula)
  
  data_dt <- as.data.table(data)
  cols_docvars <- c(cv, sv)
  
  if(any(!(cols_docvars %in% names(data_dt)))) {
    
    col_not_found <- paste(
      cols_docvars[which(!(cols_docvars %in% names(data_dt)))],
      collapse = ", ")
    
    stop(sprintf("column(s) %s not found in data", col_not_found))
    
  }
  
  if(any(is.na(data_dt[, .(cols_docvars)]))) {
    
    col_with_na <- paste(
      cols_docvars[which(is.na(data_dt[, .(cols_docvars)]))],
      collapse = ", ")
    
    stop(sprintf("column(s) %s contain(s) NA values", col_not_found))
    
  }
  
  # Add smoothing terms and collapse to formula
  
  cv_formula <- paste0(cv, collapse = "+")
  sv_formula <- paste0("s(", sv, ", df = ", smooth_df, ")", collapse = "+")
  formula_right <- paste0(c(cv_formula, sv_formula), collapse = "+")
  
  as.formula(paste("", formula_right, sep = "~"))
  
}
