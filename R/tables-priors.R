priors.table <- function(model, cap = ""){
  ## Make a table of the priors used in the model given

  model <- model[[1]]

  p.names <- data.frame(name = c("Uniform",
                                 "Normal",
                                 "Log-Normal",
                                 "Beta",
                                 "Gamma"),
                        prior = c(0, 1, 2, 3, 4))
  p.names$name = as.character(p.names$name)
  p.names <- as.tibble(p.names)

  prior.specs <- as.data.frame(model$ctl$params)
  ## Remove fixed parameters
  prior.specs <- prior.specs[prior.specs$phz > 0,]
  ## Remove upper and lower bound, and phase information, but keep initial
  ##  value
  prior.specs <- prior.specs[, -c(2:4)]
  ## Remove kappa
  prior.specs <- prior.specs[rownames(prior.specs) != "kappa",]
  prior.names <- rownames(prior.specs)

  ## Add the q parameters to the prior specs table
  q.params <- model$ctl$surv.q
  num.q.params <- ncol(q.params)

  q.specs <- lapply(1:num.q.params,
                    function(x){
                      c(q.params[2, x],
                        q.params[1, x],
                        q.params[2, x],
                        q.params[3, x])
                    })
  q.specs <- as.data.frame(do.call(rbind, q.specs))
  rownames(q.specs) <- paste0("log_q", 1:num.q.params)
  colnames(q.specs) <- colnames(prior.specs)
  prior.specs <- rbind(prior.specs, q.specs)

  ## Remove log part for q's with uniform priors
  j <- prior.specs
  non.q <- j[-grep("q", rownames(j)),]
  non.q <- non.q %>%
    rownames_to_column() %>%
    as.tibble()

  q <- j[grep("q", rownames(j)),]

  q <- q %>%
    rownames_to_column() %>%
    mutate(rowname = if_else(!prior,
                             gsub("log_", "", rowname),
                             rowname))

  prior.specs <- as.data.frame(rbind(non.q, q))
  rownames(prior.specs) <- prior.specs$rowname
  prior.specs <- prior.specs %>% select(-rowname)
  rownames(prior.specs)[rownames(prior.specs) == "steepness"] = "h"

  prior.specs <- prior.specs %>%
    mutate(Parameter = rownames(prior.specs)) %>%
    left_join(p.names, by = "prior") %>%
    select(-prior) %>%
    rename(`Initial value` = ival, Distribution = name)

  prior.specs <- prior.specs[,c("Parameter",
                                "Distribution",
                                "Initial value",
                                "p1",
                                "p2")]
  prior.specs$`Initial value` <- round(prior.specs$`Initial value`, 2)
  prior.specs$p1 <- round(prior.specs$p1, 2)

  prior.specs$Parameter <- sapply(prior.specs$Parameter,
                                  function(x){
                                    get.rmd.name(x)
                                  })

  kable(prior.specs,
        caption = cap,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE,
        align = c("l", "r", "r", "r", "r")) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
}
