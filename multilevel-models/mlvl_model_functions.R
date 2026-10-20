#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   model_functions.R
#
#   Purpose   Functions that tabulate the multilevel models and compute the
#             intraclass correlation.
#   Paper     Section 2.4
#   Used by   06_null_model.R, 07_random_intercept_slope.R,
#             08_cross_level.R (with source()).
#   Packages  nlme, msm, janitor, dplyr
#   Note      This script only defines functions; it does not produce output.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 TABLE FOR MODELS WITH A RANDOM INTERCEPT ONLY ------------------------------


# Returns fixed effects (estimate, SE, 95% CI, p-value), random-effect
# variances with their 95% CI, and fit statistics (log-likelihood,
# deviance, AIC, BIC). Variance SEs are obtained by the delta method from
# the log-SD parameterization used by nlme.
table_mixed_model <- function(model, digits = 3) {
  
  # Confidence intervals
  int.aux       <- nlme::intervals(model)
  non.resid     <- as.data.frame(int.aux$reStruct$ciudad)
  non.resid <- non.resid[grep("^sd\\(", rownames(non.resid)), , drop = FALSE]
  
  resid.sd      <- as.data.frame(t(int.aux$sigma))
  colnames(resid.sd) <- colnames(non.resid)
  rownames(resid.sd) <- "Residual"
  sd_int        <- rbind(non.resid, resid.sd)
  sd_int$variables <- rownames(sd_int)
  
  int.aux.fixed <- as.data.frame(int.aux$fixed)
  
  # SE of the variances by the delta method (parameters in log(sd))
  apVar <- model$apVar
  Vpar  <- as.matrix(apVar)
  par   <- attr(apVar, "Pars")
  
  # Log(sd) parameters of the random intercept (reStruct) and of the
  # residual (lSigma)
  idx_re   <- grep("^reStruct\\.", names(par))
  idx_sig  <- which(names(par) == "lSigma")
  idx_keep <- c(idx_re, idx_sig)
  
  par_k <- par[idx_keep]
  V_k   <- Vpar[idx_keep, idx_keep, drop = FALSE]
  
  # Variance = exp(2 * theta)
  se_var_par <- sapply(seq_along(par_k), function(i) {
    msm::deltamethod(as.formula(paste0("~ exp(2*x", i, ")")), mean = par_k, cov = V_k)
  })
  names(se_var_par) <- names(par_k)
  
  # Variances reported by VarCorr
  vcorr   <- nlme::VarCorr(model)
  var.num <- as.numeric(vcorr[, "Variance"])
  names(var.num) <- rownames(vcorr)
  
  # SEs aligned with VarCorr: lSigma for the residual, reStruct for the
  # random intercept
  se_var_vcorr <- rep(NA_real_, length(var.num))
  names(se_var_vcorr) <- names(var.num)
  
  if ("Residual" %in% names(se_var_vcorr) && "lSigma" %in% names(se_var_par)) {
    se_var_vcorr["Residual"] <- se_var_par["lSigma"]
  }
  
  re_names <- names(se_var_par)[grepl("^reStruct\\.", names(se_var_par))]
  if (length(re_names) > 0) {
    for (rn in re_names) {
      idx_nonres <- names(se_var_vcorr) != "Residual"
      se_var_vcorr[idx_nonres] <- se_var_par[rn]
    }
  }
  
  # Names of intervals() ("sd((Intercept))", "Residual") matched to VarCorr
  to_vcorr_name <- function(x) {
    if (x == "Residual") return("Residual")
    sub("^sd\\(\\((.*)\\)\\)$", "\\1", x)
  }
  
  vc_names <- vapply(sd_int$variables, to_vcorr_name, character(1))
  vc_names[vc_names == "Intercept"] <- "(Intercept)"
  
  var.num2 <- var.num[vc_names]
  names(var.num2) <- sd_int$variables
  
  se2 <- se_var_vcorr[vc_names]
  names(se2) <- sd_int$variables
  
  # Wald statistics for the variances (not reported in the paper)
  wald_z2 <- var.num2 / se2
  wald_p2 <- 2 * (1 - pnorm(abs(wald_z2)))
  
  df.random <- data.frame(
    type      = "random",
    variables = sd_int$variables,
    b         = round(var.num2[sd_int$variables], digits),
    se        = round(se2[sd_int$variables], digits),
    lower     = round(sd_int$lower^2, digits),
    upper     = round(sd_int$upper^2, digits),
    z_value   = round(wald_z2[sd_int$variables], digits),
    p_value   = wald_p2[sd_int$variables],
    row.names = NULL
  )
  
  # Fixed effects
  smry  <- summary(model)
  
  s.aux <- smry$tTable %>%
    as.data.frame() %>%
    janitor::clean_names()
  
  df.fixed <- data.frame(
    type      = "fixed",
    variables = rownames(s.aux),
    b         = round(s.aux$value, digits),
    se        = round(s.aux$std_error, digits),
    lower     = round(int.aux.fixed$lower, digits),
    upper     = round(int.aux.fixed$upper, digits),
    z_value   = round(s.aux$t_value, digits),
    p_value   = s.aux$p_value,
    row.names = NULL
  )
  
  # Fit statistics
  df.stats <- data.frame(
    type      = "statistics",
    variables = c("logLik", "Deviance", "AIC", "BIC"),
    b         = c(
      as.numeric(smry$logLik),
      as.numeric(stats::deviance(model)),
      smry$AIC,
      smry$BIC
    ),
    se      = NA_real_,
    lower   = NA_real_,
    upper   = NA_real_,
    z_value = NA_real_,
    p_value = NA_real_,
    row.names = NULL
  )
  
  dplyr::bind_rows(df.fixed, df.random, df.stats)
}



# 2 TABLE FOR MODELS WITH A RANDOM SLOPE ---------------------------------------


# Same output as table_mixed_model(). The random-effect variances come from
# the log-Cholesky parameterization of the random intercept and slope, and
# their SEs are obtained by the delta method with a numerical gradient.
table_mixed_model_rslope <- function(model, digits = 3, group = "ciudad",
                                     include_wald = TRUE, eps = 1e-6) {
  
  # Confidence intervals (sd)
  int.aux <- nlme::intervals(model)
  
  re_ci <- int.aux$reStruct[[group]]
  if (is.null(re_ci)) stop("int.aux$reStruct[['", group, "']] not found. Check the group name.")
  
  non.resid <- as.data.frame(re_ci)
  non.resid <- non.resid[grep("^sd\\(", rownames(non.resid)), , drop = FALSE]
  
  resid.sd <- as.data.frame(t(int.aux$sigma))
  colnames(resid.sd) <- colnames(non.resid)
  rownames(resid.sd) <- "Residual"
  
  sd_int <- rbind(non.resid, resid.sd)
  sd_int$variables <- rownames(sd_int)
  
  int.aux.fixed <- as.data.frame(int.aux$fixed)
  
  # Variances reported by VarCorr
  vcorr   <- nlme::VarCorr(model)
  var.num <- as.numeric(vcorr[, "Variance"])
  names(var.num) <- rownames(vcorr)
  
  re_names_vcorr <- setdiff(names(var.num), "Residual")
  k <- length(re_names_vcorr)
  if (k < 2) stop("This function is for models with a random slope (k >= 2).")
  
  # Delta method for the log-Cholesky parameterization (k x k)
  apVar <- model$apVar
  Vpar  <- as.matrix(apVar)
  par   <- attr(apVar, "Pars")
  
  idx_re  <- grep(paste0("^reStruct\\.", group), names(par))
  idx_sig <- which(names(par) == "lSigma")
  if (length(idx_sig) != 1) stop("'lSigma' not found in attr(apVar, 'Pars').")
  
  par_re <- par[idx_re]
  V_re   <- Vpar[idx_re, idx_re, drop = FALSE]
  
  m_expected <- k * (k + 1) / 2
  if (length(par_re) != m_expected) {
    stop("Length of reStruct parameters = ", length(par_re),
         " but ", m_expected, " expected for k = ", k,
         ". Check the random-effects structure.")
  }
  
  # Lower Cholesky factor L from theta (log diagonal + off-diagonal)
  build_L <- function(theta, k) {
    L <- matrix(0, k, k)
    diag(L) <- exp(theta[1:k])
    off <- theta[(k + 1):length(theta)]
    idx <- 1
    if (k >= 2) {
      for (i in 2:k) {
        for (j in 1:(i - 1)) {
          L[i, j] <- off[idx]
          idx <- idx + 1
        }
      }
    }
    L
  }
  
  # Random-effect variances, diag(Sigma)
  f_var_re <- function(theta) {
    L <- build_L(theta, k)
    Sigma <- L %*% t(L)
    diag(Sigma)
  }
  
  # Numerical gradient (central differences) of each variance
  grad_var_re <- function(theta, eps) {
    p <- length(theta)
    G <- matrix(0, nrow = k, ncol = p)
    for (j in 1:p) {
      th_plus  <- theta; th_plus[j]  <- th_plus[j]  + eps
      th_minus <- theta; th_minus[j] <- th_minus[j] - eps
      f_plus  <- f_var_re(th_plus)
      f_minus <- f_var_re(th_minus)
      G[, j] <- (f_plus - f_minus) / (2 * eps)
    }
    G
  }
  
  theta <- as.numeric(par_re)
  G <- grad_var_re(theta, eps = eps)
  
  # SE of the random-effect variances: sqrt(g' V g)
  se_var_re <- vapply(1:k, function(i) {
    gi <- matrix(G[i, ], ncol = 1)
    as.numeric(sqrt(t(gi) %*% V_re %*% gi))
  }, numeric(1))
  
  # Residual variance = exp(2 * lSigma), univariate delta method
  ls <- as.numeric(par[idx_sig])
  var_ls <- as.numeric(Vpar[idx_sig, idx_sig])
  se_var_res <- abs(2 * exp(2 * ls)) * sqrt(var_ls)
  
  se_var_vcorr <- rep(NA_real_, length(var.num))
  names(se_var_vcorr) <- names(var.num)
  se_var_vcorr[re_names_vcorr] <- se_var_re
  se_var_vcorr["Residual"]     <- se_var_res
  
  # Names of intervals() ("sd((Intercept))", "sd(larea_c)", "Residual")
  # matched to VarCorr
  to_vcorr_name <- function(x) {
    if (x == "Residual") return("Residual")
    
    if (grepl("^sd\\(\\(.*\\)\\)$", x)) {
      nm <- sub("^sd\\(\\((.*)\\)\\)$", "\\1", x)
      if (nm == "Intercept" && "(Intercept)" %in% names(var.num)) return("(Intercept)")
      return(nm)
    }
    
    if (grepl("^sd\\(.*\\)$", x)) {
      nm <- sub("^sd\\((.*)\\)$", "\\1", x)
      return(nm)
    }
    
    x
  }
  
  vc_names <- vapply(sd_int$variables, to_vcorr_name, character(1))
  
  if (any(!vc_names %in% names(var.num))) {
    bad <- vc_names[!vc_names %in% names(var.num)]
    stop("These names do not match VarCorr(): ", paste(bad, collapse = ", "))
  }
  
  var.num2 <- var.num[vc_names]
  names(var.num2) <- sd_int$variables
  
  se2 <- se_var_vcorr[vc_names]
  names(se2) <- sd_int$variables
  
  # Wald statistics for the variances (not reported in the paper)
  if (isTRUE(include_wald)) {
    wald_z2 <- var.num2 / se2
    wald_p2 <- 2 * (1 - pnorm(abs(wald_z2)))
  } else {
    wald_z2 <- rep(NA_real_, length(var.num2))
    wald_p2 <- rep(NA_real_, length(var.num2))
    names(wald_z2) <- names(var.num2)
    names(wald_p2) <- names(var.num2)
  }
  
  df.random <- data.frame(
    type      = "random",
    variables = sd_int$variables,
    b         = round(var.num2[sd_int$variables], digits),
    se        = round(se2[sd_int$variables], digits),
    lower     = round(sd_int$lower^2, digits),
    upper     = round(sd_int$upper^2, digits),
    z_value   = round(wald_z2[sd_int$variables], digits),
    p_value   = wald_p2[sd_int$variables],
    row.names = NULL
  )
  
  # Fixed effects
  smry <- summary(model)
  s.aux <- smry$tTable |>
    as.data.frame() |>
    janitor::clean_names()
  
  df.fixed <- data.frame(
    type      = "fixed",
    variables = rownames(s.aux),
    b         = round(s.aux$value, digits),
    se        = round(s.aux$std_error, digits),
    lower     = round(int.aux.fixed$lower, digits),
    upper     = round(int.aux.fixed$upper, digits),
    z_value   = round(s.aux$t_value, digits),
    p_value   = s.aux$p_value,
    row.names = NULL
  )
  
  # Fit statistics
  df.stats <- data.frame(
    type      = "statistics",
    variables = c("logLik", "Deviance", "AIC", "BIC"),
    b         = c(
      as.numeric(smry$logLik),
      as.numeric(stats::deviance(model)),
      smry$AIC,
      smry$BIC
    ),
    se      = NA_real_,
    lower   = NA_real_,
    upper   = NA_real_,
    z_value = NA_real_,
    p_value = NA_real_,
    row.names = NULL
  )
  
  dplyr::bind_rows(df.fixed, df.random, df.stats)
}



# 3 INTRACLASS CORRELATION -----------------------------------------------------


# rho = tau0^2 / (tau0^2 + sigma^2)
rho_f <- function(model) {
  vc <- nlme::VarCorr(model)
  
  # Variance of the random intercept (first row)
  tau0 <- as.numeric(vc[1, "Variance"])
  
  # Residual variance (last row)
  sigma2 <- as.numeric(vc[nrow(vc), "Variance"])
  
  rho <- tau0 / (tau0 + sigma2)
  return(rho)
}
