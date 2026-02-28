## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(idiolect)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# corpus <- create_corpus("path/to/folder")

## -----------------------------------------------------------------------------
corpus <- enron.sample

## -----------------------------------------------------------------------------
corpus

## ----eval=FALSE, include=TRUE-------------------------------------------------
# posnoised.corpus <- contentmask(corpus, model = "en_core_web_sm", algorithm = "POSnoise")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# install.packages("spacyr")
# spacyr::install_spacy()

## -----------------------------------------------------------------------------
Q <- corpus_subset(corpus, author == "Kimberly_w" & textname == "Mail_3")
K <- corpus_subset(corpus, author == "Kimberly_w" & textname != "Mail_3")
R <- corpus_subset(corpus, author != "Kimberly_w")

## -----------------------------------------------------------------------------
vectorize(Q, tokens = "word", remove_punct = FALSE, remove_symbols = TRUE, remove_numbers = TRUE,
          lowercase = TRUE, n = 1, weighting = "rel", trim = FALSE) |> 
  print(max_nfeat = 3)

## -----------------------------------------------------------------------------
vectorize(Q, tokens = "character", remove_punct = FALSE, remove_symbols = TRUE, remove_numbers = TRUE,
          lowercase = TRUE, n = 4, weighting = "rel", trim = TRUE, threshold = 1000) |> 
  print(max_nfeat = 3)

## -----------------------------------------------------------------------------
validation <- K + R

## ----include=FALSE------------------------------------------------------------
set.seed(22)

## -----------------------------------------------------------------------------
validation.Q <- corpus_sample(validation, size = 1, by = author)
validation.K <- corpus_subset(validation, !docnames(validation) %in% docnames(validation.Q))

## ----include=FALSE------------------------------------------------------------
set.seed(5)

## ----warning=FALSE------------------------------------------------------------
res <- impostors(validation.Q, validation.K, validation.K, algorithm = "RBI", k = 10)

## ----paged.print=TRUE---------------------------------------------------------
res[1:10,]

## ----message=FALSE, warning=FALSE, paged.print=TRUE---------------------------
p <- performance(res, progress = FALSE)
p$evaluation

## ----fig.height=4, fig.width=6, fig.dpi=110-----------------------------------
density_plot(res)

## ----include=FALSE------------------------------------------------------------
set.seed(10)

## ----warning=FALSE------------------------------------------------------------
q.res <- impostors(Q, K, R, algorithm = "RBI", k = 10)

## -----------------------------------------------------------------------------
q.res

## ----include=FALSE------------------------------------------------------------
set.seed(2)

## ----message=FALSE, warning=FALSE---------------------------------------------
q.res2 <- impostors(Q, K, R, algorithm = "RBI", k = 10, features = TRUE)
strwrap(q.res2$features, width = 70)

## -----------------------------------------------------------------------------
concordance(Q, K, R, search = ", her", token.type = "character") |> 
  dplyr::select(pre, node, post, authorship)

## -----------------------------------------------------------------------------
concordance(Q, K, R, search = ", here is", token.type = "word") |> 
  dplyr::select(pre, node, post, authorship)

## -----------------------------------------------------------------------------
concordance(Q, K, R, search = "lso ,", token.type = "character") |> 
  dplyr::select(pre, node, post, authorship)

## ----fig.height=4, fig.width=6, fig.dpi=110-----------------------------------
density_plot(res, q = q.res$score)

## -----------------------------------------------------------------------------
q.llr <- calibrate_LLR(res, q.res, latex = T)
q.llr$`Verbal label`
strwrap(q.llr$Interpretation)

## -----------------------------------------------------------------------------
posterior(q.llr$LLR) |> 
  dplyr::select(prosecution_prior_probs, prosecution_post_probs)

