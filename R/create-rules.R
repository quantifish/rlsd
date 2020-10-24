#' Set MP rules
#'
#' @export
#'
create_rules <- function()
{
    rule1 <- c(4, 0, 0.3, 0.5, 200, 0.1, 0.1, 0.05, 0, 0)
    rules <- expand.grid("Par1" = 4,
                         "Par2" = c(0.1, 0.2), 
                         "Par3" = c(0.35, 0.45), 
                         "Par4" = c(0.5, 0.6, 0.7, 0.8), 
                         "Par5" = c(100, 125, 150, 175, 200), 
                         "Par6" = 0.1,
                         "Par7" = 0.1,
                         "Par8" = 0.05,
                         "Par9" = 0,
                         "Par10" = c(0, 2))
    names(rule1) <- names(rules)
    all_rules <- rbind.data.frame(rule1, rules)
    write.csv(all_rules, file.path("data/rules.csv"), row.names = FALSE)
}
