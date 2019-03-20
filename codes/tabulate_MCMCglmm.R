# The function requires installation of the following packages:
# install.packages(c("tidyverse", "broom.mixed", "stargazer"))
# Example:
# library(MCMCglmm)
# data("PlodiaPO")
# model1 <- MCMCglmm(PO ~ 1, random = ~ FSfamily, data = PlodiaPO)
# tabulate_MCMCglmm(model1)

tabulate_MCMCglmm <- function(x, type = c("text", "latex", "html"), digits = 2L, 
                              ess = TRUE, conf.int = TRUE, ...) {
  library(tidyverse)
  library(stargazer)
  library(broom.mixed)
  type <- match.arg(type)
  broom.mixed::tidy(x, ess = ess, conf.int = conf.int) %>% 
    mutate(term = if_else(group == "Residual" & effect == "ran_pars", 
                          "residual", term), 
           term = str_replace(term, "\\(Intercept\\)", "int"),
           term = str_replace(term, "__", "("), 
           term = if_else(effect == "ran_pars" & group != "Residual", 
                          paste0(term, ")"), term)) %>% 
    add_row(term = "Fixed", .before = 1) %>% 
    add_row(term = "Random", .before = which(.$effect == "ran_pars")[1]) %>% 
    select(-effect, -group) %>% 
    rename(se = `std.error`) %>% 
    as.data.frame() %>% 
    stargazer(type = type, 
              summary = FALSE, digits = digits, rownames = FALSE, ...)
}