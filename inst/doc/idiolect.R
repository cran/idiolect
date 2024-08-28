## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(idiolect)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  corpus <- create_corpus("path/to/folder")

## -----------------------------------------------------------------------------
corpus <- enron.sample

## -----------------------------------------------------------------------------
corpus

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  posnoised.corpus <- contentmask(corpus, model = "en_core_web_sm", algorithm = "POSnoise")

## -----------------------------------------------------------------------------
Q <- corpus_subset(corpus, author == "Kw")[1]
K <- corpus_subset(corpus, author == "Kw")[2:5]
R <- corpus_subset(corpus, author != "Kw")

## -----------------------------------------------------------------------------
vectorize(Q, tokens = "word", remove_punct = F, remove_symbols = T, remove_numbers = T,
          lowercase = T, n = 1, weighting = "rel", trim = F) |> 
  print(max_nfeat = 3)

## -----------------------------------------------------------------------------
vectorize(Q, tokens = "character", remove_punct = F, remove_symbols = T, remove_numbers = T,
          lowercase = T, n = 4, weighting = "rel", trim = T, threshold = 1000) |> 
  print(max_nfeat = 3)

## -----------------------------------------------------------------------------
validation <- K + R

## -----------------------------------------------------------------------------
validation.Q <- corpus_subset(validation, grepl("^unknown", docnames(validation)))
validation.K <- corpus_subset(validation, grepl("^known", docnames(validation)))

## ----include=FALSE------------------------------------------------------------
set.seed(2)

## ----warning=FALSE------------------------------------------------------------
res <- impostors(validation.Q, validation.K, validation.K, algorithm = "RBI", k = 50)

## ----paged.print=TRUE---------------------------------------------------------
res[1:10,]

## ----message=FALSE, warning=FALSE, paged.print=TRUE---------------------------
p <- performance(res)
p$evaluation

## ----fig.height=4, fig.width=6, fig.dpi=110-----------------------------------
density_plot(res)

## ----include=FALSE------------------------------------------------------------
set.seed(10)

## ----warning=FALSE------------------------------------------------------------
q.res <- impostors(Q, K, R, algorithm = "RBI", k = 50)

## -----------------------------------------------------------------------------
q.res

## ----include=FALSE------------------------------------------------------------
set.seed(2)

## ----message=FALSE, warning=FALSE---------------------------------------------
q.res2 <- impostors(Q, K, R, algorithm = "RBI", k = 50, features = T)
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

