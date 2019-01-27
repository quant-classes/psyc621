# A function to compute standardized effect size (analogous to Cohen's d) for 
# multilevel models, based on: 
# Hedges, L. V. (2009). Effect sizes in nested designs. In H. Cooper, 
# L. V. Hedges, & J. C. Valentine (Eds.), The handbook of research synthesis 
# and meta-analysis (2nd ed., pp. 337–355). New York, NY: Russell Sage 
# Foundation.
#
# Function by Hok Chio (Mark) Lai, January 26, 2019

mlm_d <- function(x, ci = TRUE, nsim = 999, 
                  .progress = "txt", ...) {
  # Compute standardized effect size for mixed models. Confidence interval is
  #   obtained with parametric bootstrap
  # 
  # Args:
  #   x: a fitted object from lme4::lmer().
  #   nsim: number of bootstrap samples to approximate the standard error
  #   
  # Returns:
  #   A named numeric vector: 
  #     d = effect size, 
  #     se = standard error, 
  #     ci_ll = lower bound of confidence interval (default is 95% confidence)
  #     ci_ul = upper bound of confidence interval (default is 95% confidence)
  # Error handling
  if (length(x@theta) > 1) {
    stop("Currently the function only computes effect size for", 
         "\na two-level model with no random slopes.")
  }
  if (length(x@beta) != 2L) {
    stop("The model must include only the intercept and the treatment variable.")
  }
  if (length(unique(x@pp$X[ , 2])) != 2L) {
    stop("The treatment variable must have two levels (values).")
  }
  compute_d <- function(x) x@beta[2] / 
    sqrt(sum((sigma(x) * c(1, x@theta))^2))
  if (ci) {
    boot_d <- lme4::bootMer(x, 
                            compute_d, 
                            nsim = nsim, .progress = .progress, ...)
    boot_d_ci <- boot::boot.ci(boot_d, type = "basic")$basic[4:5]
    cat("\nCompleted", length(boot_d$t), "bootstrap samples\n")
    boot_d$R <- length(boot_d$t)
    setNames(c(boot_d$t0, sd(boot_d$t), boot_d_ci[1], boot_d_ci[2]), 
             c("d", "se", "ci_ul", "ci_ll"))
  } else {
    c(d = compute_d(x))
  }
}

# Example:
# m1 <- lme4::lmer(mathach ~ sector + (1 | id), data = hsball)
# mlm_d(m1)  # may take a few minutes