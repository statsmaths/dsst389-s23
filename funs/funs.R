#############################################################################
# Settings and functions for working with the class notes and assignements
# Note that this file will be overwritten. Do not modify directly!
#
# Date: 24 November 2022

#############################################################################
# load a few required packages; others will be referenced with :: and :::
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(hms))
suppressPackageStartupMessages(library(RcppRoll))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(cleanNLP))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyverse))

#############################################################################
# some standard settings
theme_set(theme_minimal())
if (.Platform$OS.type == "unix")
{
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
} else {
  Sys.setlocale("LC_ALL", "English")
}
Sys.setenv(LANG = "en")
options(width = 76L)
options(pillar.min_character_chars = 15)
options(dplyr.summarise.inform = FALSE)
options(readr.show_col_types = FALSE)
options(ggrepel.max.overlaps = Inf)
options(sparse.colnames = TRUE)
options(lubridate.week.start = 1)

#############################################################################
# spatial functions
sm_centroid <- function(data) {
  suppressWarnings({ z <- st_coordinates(st_centroid(data)) })
  return(tibble(lon = z[,1], lat = z[,2]))
}
spatial_join <- function(...) {
  return(st_as_sf(as_tibble(st_join(...))))
}

#############################################################################
# change default parameter in the arrange function
arrange <- function(.data, ...) { dplyr::arrange(.data, ..., .by_group = TRUE) }
null_to_na <- function(x) { ifelse(is.null(x), NA, x) }

#############################################################################
# custom theme; mimics Tufte theme
theme_tufte <- function()
{
    ret <- ggplot2::theme_bw(base_family = "sans", base_size = 11) +
        ggplot2::theme(
          legend.background = ggplot2::element_blank(),
          legend.key        = ggplot2::element_blank(),
          panel.background  = ggplot2::element_blank(),
          panel.border      = ggplot2::element_blank(),
          strip.background  = ggplot2::element_blank(),
          plot.background   = ggplot2::element_blank(),
          axis.line         = ggplot2::element_blank(),
          panel.grid        = ggplot2::element_blank(),
          axis.ticks        = ggplot2::element_blank()
        )
    ret
}


#############################################################################
# S3 print methods for model classes
print.enet <- function(x, ...)
{
  cat("\nNumber of lambda: ", length(x$model$lambda), "\n\n")
}

print.knnm <- function(x, ...)
{
  cat("\nk: ", x$k, "\n\n")
}

print.gbmm <- function(x, ...)
{
  cat("\nk: ", x$ntree, "\n\n")
}

print.nb <- function(x, ...)
{
  cat("\n\n")
}

print.ldam <- function(x, ...)
{
  cat("\nntopics: ", max(x$docs$topic), "\n\n")
}

#############################################################################
# hidden (start with ".") helper functions
.assert <- function (statement, msg = "")
{
    if (!statement) {
        stop(msg, call. = (msg == ""))
    }
}

.tidy_matrix <- function(
  x, rows_to = "document", cols_to = "term", values_to = "count"
)
{
  if (is.null(rownames(x)))
      stop("input must have row names")
  if (is.null(colnames(x)))
      stop("input must have column names")
  x <- as.matrix(x)
  out <- tibble::tibble(var1 = rownames(x)[row(x)], var2 = colnames(x)[col(x)],
      var3 = as.numeric(x))
  names(out) <- c(rows_to, cols_to, values_to)
  out
}

#############################################################################
# function to download project data
dsst_project_docs <- function(dname, sname)
{
  url_base <- sprintf("https://github.com/statsmaths/data-%s/raw/main/class", dname)
  f_corpus <- file.path("..", "data_project", sprintf("%s_%s.csv.bz2", dname, sname))
  u_corpus <- sprintf("%s/%s.csv.bz2", url_base, sname)
  if (!file.exists(f_corpus)) { download.file(u_corpus, f_corpus) }
  docs <- read_csv(f_corpus)
  docs
}

dsst_project_anno <- function(dname, sname)
{
  url_base <- sprintf("https://github.com/statsmaths/data-%s/raw/main/class", dname)
  f_token <- file.path("..", "data_project", sprintf("%s_%s_token.csv.bz2", dname, sname))
  u_token <- sprintf("%s/%s_token.csv.bz2", url_base, sname)
  if (!file.exists(f_token)) { download.file(u_token, f_token) }
  anno <- read_csv(f_token)
  anno
}

#############################################################################
# copy to clipboard

dsst_clipboard <- function(input) {
  pck <- attr(class(input), "package")
  if (!is.null(pck)) { if (pck == "Matrix") { input <- as.matrix(input) }  }

  clipr::write_clip(input, allow_non_interactive = TRUE)
}

dsst_save_plot <- function(
  filename = NA, width = 9, height = 15, units = "cm", ...
)
{
  if (is.na(filename))
  {
    filename <- file.path(
      "output", paste0("image-", as.integer(Sys.time()), ".png")
    )
  }
}

#############################################################################
# build predictive classification models: elastic net, knn, boosted trees

dsst_enet_build <- function(
  anno, docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  alpha = 0.9,
  nfolds = 3,
  trace_it = getOption("dsst.traceit", TRUE),
  lambda_min_ratio = 0.05,
  nlambda = 100,
  seed = 1,
  embed = NULL,
  add_terms = FALSE
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix, if needed
  if (is.null(embed) | add_terms)
  {
    X <- cleanNLP::cnlp_utils_tf(
      anno,
      doc_set = docs[[doc_var]],
      min_df = min_df,
      max_df = max_df,
      max_features = max_features,
      doc_var = doc_var,
      token_var = token_var
    )
  }

  # add or select the embedding matrix
  if (!is.null(embed))
  {
    .assert(nrow(embed) == nrow(docs),
            "embedding must have the same number of rows as docs")
    if (add_terms)
    {
      X <- cbind(X, as.matrix(embed))
    } else {
      X <- as.matrix(embed)
    }
  }

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- docs[[label_var]][docs[[train_var]] == "train"]

  model <- glmnet::cv.glmnet(
    X_train,
    y_train,
    alpha = alpha,
    family = "multinomial",
    nfolds = nfolds,
    trace.it = trace_it,
    lambda.min.ratio = lambda_min_ratio,
    nlambda = nlambda
  )

  # create the predictions
  docs$pred_label <- as.vector(
    glmnet:::predict.cv.glmnet(model, newx = X, type = "class")
  )
  docs$pred_value <- apply(
    glmnet:::predict.cv.glmnet(model, newx = X, type = "response")[,,1], 1, max
  )
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    model = model,
    docs = ungroup(docs)
  ), class = c('enet'))

  output
}

dsst_knn_build <- function(
  anno,
  docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  k = 1,
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix
  X <- cleanNLP::cnlp_utils_tfidf(
    anno,
    doc_set = docs[[doc_var]],
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )
  y <- docs[[label_var]]

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- y[docs[[train_var]] == "train"]

  model <- FNN::knn(X_train, X, y_train, k = k)
  probs <- attr(model, "nn.dist")
  probs <- 1 - probs / max(probs)

  # create the predictions
  docs$pred_label <- as.character(model)
  docs$pred_value <- probs
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    docs = ungroup(docs),
    k = 1
  ), class = c('knnm'))

  output
}

dsst_gbm_build <- function(
  anno, docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  trace_it = getOption("dsst.traceit", TRUE),
  max_depth = 3,
  eta = 0.05,
  nrounds = 10,
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix
  X <- cleanNLP::cnlp_utils_tf(
    anno,
    doc_set = docs[[doc_var]],
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  # need to integer encode the classes
  y_set <- unique(docs[[label_var]])
  y <- (match(docs[[label_var]], y_set) - 1L)

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- y[docs[[train_var]] == "train"]
  X_valid <- X[docs[[train_var]] == "valid", ]
  y_valid <- y[docs[[train_var]] == "valid"]

  data_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  data_valid <- xgboost::xgb.DMatrix(data = X_valid, label = y_valid)
  watchlist <- list(train=data_train, valid=data_valid)

  model <- xgboost::xgb.train(data = data_train,
                     max_depth = max_depth,
                     eta = eta,
                     nrounds = nrounds,
                     nthread = 2,
                     objective = "multi:softmax",
                     eval_metric = "merror",
                     watchlist = watchlist,
                     verbose = trace_it,
                     print_every_n = 25,
                     num_class = length(y_set))

  y_hat <- y_set[xgboost:::predict.xgb.Booster(model, newdata = X) + 1]

  # create the predictions
  docs$pred_label <- y_hat
  docs$pred_value <- 1
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    model = model,
    docs = ungroup(docs)
  ), class = c('gbmm'))

  output
}

dsst_metrics <- function(
  anno, docs,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  higher_only = FALSE,
  train_only = TRUE
)
{
  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create copies with the correct names (this is messy, but less error prone)
  docs <- docs[,match(c(doc_var, train_var, label_var), names(docs))]
  names(docs) <- c("doc_id", "train_id", "label")
  anno <- anno[,match(c(doc_var, token_var), names(anno))]
  names(anno) <- c("doc_id", "token")

  if (train_only)
  {
    docs <- filter(docs, train_id == "train")
  }

  counts <- anno %>%
    inner_join(docs, by = "doc_id") %>%
    group_by(token, label) %>%
    summarise(o11 = n()) %>%
    group_by(token) %>%
    mutate(o12 = sum(o11) - o11) %>%
    group_by(label) %>%
    mutate(o21 = sum(o11) - o11) %>%
    ungroup() %>%
    mutate(o22 = sum(o11) - o11 - o12 - o21)

  N <- counts$o11 + counts$o12 + counts$o21 + counts$o22
  e11 <- (counts$o11 + counts$o12) * ((counts$o11 + counts$o21) / N)
  e12 <- (counts$o11 + counts$o12) * ((counts$o12 + counts$o22) / N)
  e21 <- (counts$o21 + counts$o22) * ((counts$o11 + counts$o21) / N)
  e22 <- (counts$o21 + counts$o22) * ((counts$o12 + counts$o22) / N)

  counts$gscore <- 2 * (
    counts$o11 * log(counts$o11 / e11) +
    counts$o12 * log(counts$o12 / e12) +
    counts$o21 * log(counts$o21 / e21) +
    counts$o22 * log(counts$o22 / e22)
  )

  counts$chi2 <- (
    (counts$o11 - e11)^2 / e11 + (counts$o12 - e12)^2 / e12 +
    (counts$o21 - e21)^2 / e21 + (counts$o22 - e22)^2 / e22
  )

  counts$pval <- pchisq(counts$chi2, df = 1, lower.tail = FALSE)

  counts$count_word <- counts$o11 + counts$o12
  counts$expected <- e11
  counts <- select(counts, label, token, count = o11, expected, count_word, gscore, chi2)
  counts <- arrange(counts, label, desc(gscore))

  if (higher_only)
  {
    counts <- filter(counts, count > expected)
  }

  counts <- group_by(counts, label) %>% group_split() %>% as.list()
  counts
}

dsst_nb_build <- function(
  anno, docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  alpha = 0.9,
  nfolds = 3
)
{

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create copies with the correct names (this is messy, but less error prone)
  docs2 <- docs[,match(c(doc_var, train_var, label_var), names(docs))]
  names(docs2) <- c("doc_id", "train_id", "label")
  anno <- anno[,match(c(doc_var, token_var), names(anno))]
  names(anno) <- c("doc_id", "token")

  # create vocabulary
  vocab <- anno %>%
    filter(!is.na(token)) %>%
    inner_join(filter(docs2, train_id == "train"), by = "doc_id") %>%
    mutate(n_docs = length(unique(doc_id))) %>%
    group_by(token) %>%
    mutate(n_distinct = length(unique(doc_id))) %>%
    summarise(n = n(), df = first(n_distinct) / first(n_docs)) %>%
    arrange(desc(n))

  vocab <- vocab %>%
    slice_head(n = max_features) %>%
    filter(df <= max_df) %>%
    filter(df >= min_df)

  # create Naive Bayes estimator
  nb <- expand_grid(token = vocab$token, label = unique(docs2$label))

  lprobs <- docs2 %>%
    filter(train_id == "train") %>%
    mutate(n = n()) %>%
    group_by(label) %>%
    summarise(log_prob = log(n()) - log(first(n)))

  tprobs <- anno %>%
    distinct() %>%
    mutate(ndocs = n_distinct(doc_id)) %>%
    left_join(filter(docs2, train_id == "train"), by = "doc_id") %>%
    group_by(token, label) %>%
    summarise(n = n(), ndocs = first(ndocs)) %>%
    ungroup() %>%
    right_join(nb, by = c("token", "label")) %>%
    mutate(n = if_else(is.na(n), 1, n + 1)) %>%
    mutate(ndocs = max(ndocs, na.rm = TRUE)) %>%
    mutate(log_prob = log(n) - log(ndocs))

  # fit the naive bayes model on the documents
  pred <- anno %>%
    distinct() %>%
    inner_join(tprobs, by = "token") %>%
    group_by(doc_id, label) %>%
    summarise(prob = sum(log_prob)) %>%
    inner_join(lprobs, by = "label") %>%
    mutate(prob = prob + log_prob) %>%
    select(-log_prob) %>%
    rename(pred_label = label) %>%
    arrange(desc(prob)) %>%
    mutate(diffs = prob - max(prob)) %>%
    mutate(pred_value = 1 / sum(exp(diffs))) %>%
    slice_head(n = 1) %>%
    inner_join(docs2, by = "doc_id") %>%
    rename(real_label = label, pred_index = train_id)

  docs <- left_join(docs, pred, by = "doc_id")

  # create the output and return the values
  output <- structure(list(
    docs = ungroup(docs)
  ), class = c('nb'))

  output
}


#############################################################################
# functions to evaluate performance of the model; these should work on all
# models

dsst_erate <- function(model, segmented = FALSE)
{
  if (segmented)
  {
    res <- model$docs %>%
      group_by(train_id, real_label) %>%
      summarize(
        class_rate = mean(pred_label != real_label), .groups = "drop"
      ) %>%
      pivot_wider(values_from = class_rate, names_from = train_id)
  } else {
    res <- model$docs %>%
      group_by(train_id) %>%
      summarize(
        class_rate = mean(pred_label != real_label), .groups = "drop"
      ) %>%
      pivot_wider(values_from = class_rate, names_from = train_id)
  }

  return(res)
}

dsst_confusion_matrix <- function(model, colnames = TRUE)
{
  tab <- table(
    model$docs$real_label[model$docs$train_id == "valid"],
    model$docs$pred_label[model$docs$train_id == "valid"]
  )

  if (!colnames) { colnames(tab) <- NULL }

  return(tab)
}


dsst_neg_examples <- function(model, n = 10L, real_label = NULL, seed = 1)
{
  if (!is.na(seed)) { set.seed(seed) }

  if (is.null(real_label))
  {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  } else if (real_label == "ALL") {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      group_by(real_label) %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  } else {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(.data$real_label == .env$real_label) %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  }

  return(res)
}

dsst_max_prob <- function(model, n = 10L, real_label = NULL, seed = 1)
{
  if (!is.na(seed)) { set.seed(seed) }

  if (is.null(real_label))
  {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  } else if (real_label == "ALL") {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      group_by(real_label) %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  } else {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(.data$real_label == .env$real_label) %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  }

  return(res)
}

#############################################################################
# helper function to print text; pairs with the function dsst_max_prob,
# dsst_neg_examples, etc.

dsst_print_text <- function(res, max_chars = 500L)
{
  if ("real_label" %in% names(res))
  {
    res %>%
      mutate(text = stringi::stri_sub(text, 1L, max_chars)) %>%
      mutate(response = sprintf("%s => %s (%f) \n %s\n",
                                real_label, pred_label, pred_value, text)) %>%
      magrittr::use_series(response) %>%
      stringi::stri_split(fixed = "\n") %>%
      unlist() %>%
      stringi::stri_wrap(width = options()$width - 1L) %>%
      cat(sep = "\n")
  } else {
    res %>%
      mutate(text = stringi::stri_sub(text, 1L, max_chars)) %>%
      mutate(response = sprintf("%s \n %s\n",
                                doc_id, text)) %>%
      magrittr::use_series(response) %>%
      stringi::stri_split(fixed = "\n") %>%
      unlist() %>%
      stringi::stri_wrap(width = 79) %>%
      cat(sep = "\n")
  }
}

#############################################################################
# functions to evaluate the coefficents from the elastic net model

dsst_coef <- function(model, lambda_num = "lambda.1se", include_min_lambda = TRUE)
{
  # select the value of lambda to use
  if (!is.character(lambda_num))
  {
    lambda <- model$model$lambda[lambda_num]
  } else {
    lambda <- model$model[[lambda_num]]
  }

  # take the coefficents for the selected value of lambda and turn this into a
  # matrix with one row for each term and one column for each class
  temp <- coef(model$model, s = lambda)
  beta <- Reduce(cbind, temp)
  colnames(beta) <- names(temp)

  # now, we will take all of the coefficents for all lambda to determine the
  # ordering of the variables; a rough form of variable importance is given by
  # the smallest value of lambda for which we have a variable in the model; ties
  # can be broken by the absolute value of the coefficent's sign
  beta_all <- coef(model$model$glmnet.fit)
  nlambda <- length(model$model$lambda)
  min_s <- matrix(0, ncol = length(beta_all), nrow = nrow(beta))
  for (j in seq_len(ncol(min_s)))
  {
      min_s[,j] <- apply(beta_all[[j]], 1, function(v) {
        u <- which(v != 0)[1] ; ifelse(is.na(u), nlambda + 1L, u)
      })
  }
  min_s <- apply(min_s, 1, min)
  min_s[1] <- 0   # make sure intercept is first

  # determine the ordering of the rows of beta
  tsize <- -1 * apply(abs(beta), 1, max)
  coef_order <- order(min_s, tsize)
  beta <- beta[coef_order, ]
  non_zero_rows <- apply(beta != 0, 1, any)

  # if desired, add the minimal lambda number
  if (include_min_lambda)
  {
    min_s <- min_s[coef_order]
    beta <- cbind(beta, min_s)
    colnames(beta)[ncol(beta)] <- "MLN"
  }

  # now, remove non-zero terms
  beta <- beta[non_zero_rows,,drop=FALSE]

  return(beta)
}

dsst_coef_positive <- function(model, lambda_num = "lambda.1se")
{
  beta <- dsst_coef(model, lambda_num = lambda_num, include_min_lambda = FALSE)
  beta_df <- tibble(
    cname = colnames(beta)[as.integer(col(beta))],
    rname = rownames(beta)[as.integer(row(beta))],
    value = as.numeric(beta)
  )
  beta_df <- filter(beta_df, value > 0)
  beta_df <- filter(beta_df, rname != "(Intercept)")
  beta_df <- group_by(beta_df, cname)
  terms <- summarize(beta_df, terms = paste(rname, collapse = "; "))

  nmax <- max(stringi::stri_length(terms$cname))
  smax <- sprintf("%%%ds : %%s", nmax)
  terms <- sprintf(smax, terms$cname, terms$terms)

  cat(terms, sep = "\n")

  invisible(NULL)
}

#############################################################################
# function to evaluate variable importance for gradiant boosted trees

dsst_gbm_imp <- function(model)
{
  importance_matrix <- xgboost::xgb.importance(model = model$model)
  df <- tibble(
    feature = importance_matrix$Feature,
    gain = importance_matrix$Gain,
    cover = importance_matrix$Cover,
    frequency = importance_matrix$Frequency
  )
  df
}

#############################################################################
# functions to create new sets of features from the annotations

dsst_ngram <- function (
  object, n_min = 1, n = 3, doc_var = "doc_id", token_var = "lemma"
)
{
  .assert(doc_var %in% names(object),
          sprintf("doc_var '%s' not found in object", doc_var))
  .assert(token_var %in% names(object),
          sprintf("token_var '%s' not found in object", token_var))

  words <- split(object[[token_var]], object[[doc_var]])
  ngrams <- tokenizers:::generate_ngrams_batch(words, ngram_min = n_min,
    ngram_max = n, stopwords = character(), ngram_delim = " ")
  out <- tibble::tibble(
    doc_id = rep(names(words), sapply(ngrams, length)),
    lemma = unlist(ngrams)
  )
  out <- out[!is.na(out$lemma), ]
  out
}

dsst_skip_gram <- function (
  object, n_min = 1, n = 3, k = 1, doc_var = "doc_id", token_var = "lemma"
)
{
  .assert(doc_var %in% names(object),
          sprintf("doc_var '%s' not found in object", doc_var))
  .assert(token_var %in% names(object),
          sprintf("token_var '%s' not found in object", token_var))

  words <- split(object[[token_var]], object[[doc_var]])
  skips <- unique(unlist(lapply(n_min:n, tokenizers:::get_valid_skips,
      k), recursive = FALSE, use.names = FALSE))
  ngrams <- tokenizers:::skip_ngrams_vectorised(words, skips,
      character())
  out <- tibble::tibble(doc_id = rep(names(words), sapply(ngrams,
      length)), lemma = unlist(ngrams))
  out <- out[!is.na(out$lemma), ]
  out
}

#############################################################################
# functions to perform corpus linguistic operations

dsst_kwic <- function(
  terms,
  anno,
  n = 20L,
  width = 20L,
  seed = 1,
  upos_set = NULL,
  xpos_set = NULL,
  use_token = FALSE
)
{
  if (!is.na(seed)) { set.seed(seed) }
  if (is.null(upos_set)) { upos_set <- unique(anno$upos) }
  if (is.null(xpos_set)) { xpos_set <- unique(anno$xpos) }
  if (use_token) { anno$lemma <- anno$token }

  temp <- anno %>%
    group_by(doc_id, sid) %>%
    mutate(kwic_flag =
      (lemma %in% terms) & (upos %in% upos_set) & (xpos %in% xpos_set)
    ) %>%
    mutate(kwic_flag = kwic_flag & !duplicated(kwic_flag)) %>%
    filter(any(kwic_flag)) %>%
    mutate(part = cumsum(kwic_flag) - 0.5 * kwic_flag) %>%
    group_by(doc_id, sid, part) %>%
    summarise(text = paste(token, collapse = " ")) %>%
    mutate(text =
      if_else(part == 0, stringi::stri_pad_left(text, width = width), text)
    ) %>%
    mutate(text =
      if_else(part == 1, stringi::stri_pad_right(text, width = width), text)
    ) %>%
    mutate(text =
      if_else(part == 0, stringi::stri_sub(text, -1 * width, -1), text)
    ) %>%
    mutate(text =
      if_else(part == 1, stringi::stri_sub(text, 1, width), text)
    )

  nw <- max(stringi::stri_length(unique(temp$text[temp$part == 0.5])))
  temp$text[temp$part == 0.5] <- stringi::stri_pad_right(
    temp$text[temp$part == 0.5], nw
  )

  temp <- temp %>%
    group_by(doc_id, sid) %>%
    mutate(text = if_else(part == 0.5, paste0("|", text, "|"), text)) %>%
    mutate(text =
      if_else(
        min(part) > 0 & part == 0.5,
        stringi::stri_pad_left(
          text, width = width + stringi::stri_length(text) + 1L
        ),
        text)) %>%
    summarise(text = paste(text, collapse = " ")) %>%
    ungroup() %>%
    slice_sample(n = n) %>%
    arrange(doc_id)

  temp$doc_id <- sprintf("[%s]", temp$doc_id)
  nmax <- max(stringi::stri_length(unique(temp$doc_id)))
  temp$doc_id <- stringi::stri_pad_right(temp$doc_id, nmax + 1L)

  these <- paste0(temp$doc_id, temp$text)
  cat(these, sep = "\n")
  invisible(these)
}

dsst_tfidf <- function(
  anno,
  n_terms = 5,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma"
)
{
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  # create temporary data
  doc_id <- token <- tf <- NULL
  x <- data.frame(doc_id = anno[[doc_var]], token = anno[[token_var]],
      stringsAsFactors = FALSE)
  N <- length(unique(x$doc_id))

  # build the vocabulary
  possible_vocab <- table(x[!duplicated(x), ]$token)/N
  possible_vocab <- possible_vocab[possible_vocab >= min_df &
      possible_vocab <= max_df]
  possible_vocab <- sort(possible_vocab, decreasing = TRUE)
  vocabulary <- names(possible_vocab[seq(1, min(max_features,
      length(possible_vocab)))])

  # build term frequency data
  .assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")
  x <- x[x$token %in% vocabulary, ]
  tf_tibble <- dplyr::group_by(x, doc_id, token)
  tf_tibble <- dplyr::summarize(tf_tibble, tf = dplyr::n(), .groups = "drop")
  tf_tibble <- dplyr::group_by(tf_tibble, token)
  tf_tibble <- dplyr::mutate(tf_tibble, tfidf = (1 + log2(tf)) *
      log2(N/dplyr::n()))
  tf_tibble <- dplyr::ungroup(tf_tibble)

  # summarize
  res <- tf_tibble %>%
    group_by(doc_id) %>%
    arrange(desc(tfidf)) %>%
    slice_head(n = n_terms) %>%
    summarize(tokens = paste(token, collapse = "; "), .groups = "drop")

  return(res)
}

#############################################################################
# functions to perform clustering

dsst_angle_dist <- function(
  anno,
  docs = NULL,
  n_docs = 1,
  type = c("each", "overall"),
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  item_name = "document"
)
{
  type <- match.arg(type)

  if (!is.null(docs))
  {
    anno <- inner_join(anno, select(docs, -text), by = "doc_id", suffix = c("", "_new"))
  }

  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  x <- as.matrix(x)
  sim <- x/sqrt(rowSums(x * x))
  sim <- sim %*% t(sim)
  out <- .tidy_matrix(sim, sprintf("%s1", item_name), sprintf("%s2",
      item_name), "distance")
  out$distance[out$distance > 1] <- 1
  out$distance[out$distance < -1] <- -1
  out$distance <- acos(out$distance)/pi

  if (type == "each")
  {
    res <- out %>%
      filter(document1 < document2) %>%
      group_by(document1) %>%
      arrange(distance) %>%
      slice_head(n = n_docs) %>%
      mutate(min_dist = min(distance)) %>%
      ungroup() %>%
      arrange(min_dist) %>%
      select(-min_dist)
  } else {
    res <- out %>%
      filter(document1 < document2) %>%
      arrange(distance) %>%
      slice_head(n = n_docs) %>%
      arrange(distance)
  }

  res
}

dsst_kmeans <- function(
  anno,
  docs = NULL,
  n_dims = 2,
  n_clusters = 5L,
  output_type = c("vector", "df"),
  invert = FALSE,
  min_df = 0.1,
  max_df = 0.9,
  max_features = ifelse(invert, 100, 10000),
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  if (!is.null(docs))
  {
    anno <- inner_join(anno, select(docs, -text), by = "doc_id", suffix = c("", "_new"))
  }

  output_type <- match.arg(output_type)

  df <- dsst_pca(
    anno = anno,
    n_dims = n_dims,
    invert = invert,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var,
    seed = seed
  )

  X <- as.matrix(df[, -1L, drop = FALSE])
  df$cluster <- as.numeric(stats::kmeans(
    X, centers = n_clusters, nstart = 3L
  )$cluster)

  if (output_type == "vector")
  {
    names(df)[1] <- "document"
    df <- df %>%
      group_by(cluster) %>%
      summarize(docs = paste(document, collapse = "; "))
    df <- df$docs
  }
  return(df)
}

dsst_hclust <- function(
  anno,
  docs = NULL,
  n_dims = 2,
  invert = FALSE,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{

  if (!is.null(docs))
  {
    anno <- inner_join(anno, select(docs, -text), by = "doc_id", suffix = c("", "_new"))
  }

  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  df <- dsst_pca(
    anno = anno,
    n_dims = n_dims,
    invert = invert,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var,
    seed = seed
  )

  X <- as.matrix(df[, -1L, drop = FALSE])
  rownames(X) <- df[[1L]]
  out <- hclust(dist(X), method = "single")
  out
}

dsst_hclust_plot <- function(model)
{
  plot(model)
}

dsst_hclust_cut <- function(
  model, nclust = NULL, height = NULL, output_type = c("vector", "df")
)
{
  output_type <- match.arg(output_type)
  clust <- cutree(model, k = nclust, h = height)
  df <- tibble(
    document = names(clust),
    cluster = as.numeric(clust)
  ) %>% arrange(cluster)

  if (output_type == "vector")
  {
    df <- df %>%
      group_by(cluster) %>%
      summarize(docs = paste(document, collapse = "; "))
    df <- df$docs
  }
  return(df)
}

#############################################################################
# functions to perform dimensionality reduction

dsst_pca <- function(
  anno,
  docs = NULL,
  n_dims = 2,
  invert = FALSE,
  min_df = 0.1,
  max_df = 0.9,
  max_features = ifelse(invert, 100, 10000),
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  if (!is.null(docs))
  {
    anno <- inner_join(anno, select(docs, -text), by = "doc_id")
  }

  if (!is.na(seed)) { set.seed(seed) }
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  x <- as.matrix(x)
  if (invert) { x <- t(x) }

  # determine if we will need any missing values due to zero variation
  sigma <- apply(x, 1, sd)
  keep_these <- which(sigma != 0)
  z <- x[keep_these, , drop=FALSE]

  # compute the rotation
  rot <- irlba::prcomp_irlba(t(z), n = n_dims, scale. = TRUE)$rotation
  colnames(rot) <- sprintf("v%d", seq_len(ncol(rot)))

  # build a tibble of the results
  df <- matrix(NA_real_, nrow = nrow(x), ncol = n_dims)
  colnames(df) <- sprintf("v%d", seq_len(ncol(df)))
  df <- tibble::as_tibble(df)
  df[keep_these,] <- tibble::as_tibble(rot)

  # create and return the output
  df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
  names(df)[1] <- doc_var
  df
}

dsst_umap <- function(
  anno,
  docs = NULL,
  type = c("each", "overall"),
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  if (!is.null(docs))
  {
    anno <- inner_join(anno, select(docs, -text), by = "doc_id")
  }

  if (!is.na(seed)) { set.seed(seed) }
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  # produce the features
  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  # fit the UMAP model
  x <- as.matrix(x)
  df <- umap::umap(x, n_components = 2L, random_state = seed)$layout

  # create and return the output
  colnames(df) <- sprintf("v%d", seq_len(ncol(df)))
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
  names(df)[1] <- doc_var

  df
}

#############################################################################
# functions to plot dimensionality reduction

dsst_plot_dred <- function(object, label_flag = (nrow(object) <= 100L))
{
  object$document <- object[[1L]]
  p <- object %>%
    ggplot(aes(x = v1, y = v2)) +
      geom_point(color = "grey90") +
      theme_void()

  if (label_flag) { p <- p + ggrepel::geom_text_repel(aes(label = document)) }

  p
}

dsst_json_drep <- function(
  object,
  docs,
  path = file.path("..", "output", "dim_reduction.json"),
  nchar = 500L,
  color_var = NULL,
  title_vars = "doc_id"
)
{
  doc_var <- names(object)[1]
  names(object)[1] <- "doc_id"

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert('v1' %in% names(object),
          "variable 'v1' not found in object")
  .assert('v2' %in% names(object),
          "variable 'v2' not found in object")

  cdata <- docs
  if (any(duplicated(docs[[doc_var]])))
  {
    cdata$doc_id <- cdata[[doc_var]]
    cdata <- cdata %>%
      group_by(doc_id) %>%
      summarise(text = stringi::stri_paste(text, collapse = "\n\n"))
  }
  cdata$text <- stringi::stri_sub(cdata$text, 1, nchar)
  cdata <- inner_join(object, cdata, by = c("doc_id" = doc_var))
  cdata <- filter(cdata, !is.na(v1), !is.na(v2))

  # figure out the colors
  if (!is.null(color_var))
  {
    value_set <- unique(cdata[[color_var]])
    color_set <- scales::hue_pal()(length(value_set))
    index <- match(cdata[[color_var]], value_set)
    cdata$color <- color_set[index]
  } else {
    cdata$color <- "#bdae93"
  }

  # figure out the title
  these <- cdata[, which(names(cdata) %in% title_vars)]
  cdata$doc_id <- apply(these, 1, paste, collapse = "; ")

  # scale values for v1 and v2 and set values for size
  cdata$v1 <- (cdata$v1 - min(cdata$v1)) / (max(cdata$v1) - min(cdata$v1))
  cdata$v2 <- (cdata$v2 - min(cdata$v2)) / (max(cdata$v2) - min(cdata$v2))
  cdata$size <- ifelse(nrow(cdata) > 100, "0.2", "1.0")

  cdata <- nest(cdata, meta = -c(color, text, v1, v2, size))
  res <- as.character(jsonlite::toJSON(cdata, pretty = TRUE))
  write_lines(res, path)
}

#############################################################################
# functions to build and work with topic models

dsst_lda_build <- function(
  anno,
  num_topics = 16,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  trace_it = getOption("dsst.traceit", TRUE),
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  lda_model <- topicmodels::LDA(
    x = x, k = num_topics, control = list(seed = seed, verbose = trace_it)
  )

  docs <- tibble::tibble(
    doc_id = rep(rownames(x), num_topics),
    topic = rep(seq_len(num_topics), each = nrow(x)),
    prob = as.numeric(lda_model@gamma)
  )
  terms <- tibble::tibble(
    token = rep(colnames(x), each = num_topics),
    topic = rep(seq_len(num_topics), ncol(x)),
    beta = as.numeric(lda_model@beta)
  )

  # create the output and return the values
  output <- structure(list(
    docs = docs,
    terms = terms,
    model = lda_model
  ), class = c('ldam'))

  return(output)
}

dsst_json_lda <- function(
  object,
  docs,
  path = file.path("..", "output", "lda_model.json"),
  truncate = -1
)
{
  tnames <- object$terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 5) %>%
    group_by(topic) %>%
    summarize(name = paste(token, collapse = "; "))

  topic_words <- object$terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 100) %>%
    mutate(weight = round(100 * exp(beta) / max(exp(beta)))) %>%
    filter(weight > 0) %>%
    ungroup()

  topic_weights <- object$docs %>%
    group_by(topic) %>%
    summarize(proportion = sum(prob)) %>%
    mutate(proportion = proportion / sum(proportion) * 100)

  top_docs <- object$docs %>%
    arrange(topic, desc(prob)) %>%
    group_by(topic) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  dset <- sort(unique(object$docs$doc_id))
  top_docs$id <- match(top_docs$doc_id, dset) - 1L

  tset <- sort(unique(object$terms$topic))
  topics <- vector("list", length(tset))
  for (j in seq_along(topics))
  {
    topics[[j]] <- list(
      "short" = jsonlite::unbox(sprintf("Cluster %d", j)),
      "long" = jsonlite::unbox(sprintf("Cluster %d: %s", j, tnames$name[tnames$topic == tset[j]])),
      "proportion" = jsonlite::unbox(round(topic_weights$proportion[topic_weights$topic == tset[j]])),
      "top_docs_ids" = top_docs$id[top_docs$topic == tset[j]],
      "doc_perc" = top_docs$prob[top_docs$topic == tset[j]],
      "top_word" = topic_words$token[topic_words$topic == tset[j]],
      "word_wgt" = topic_words$weight[topic_words$topic == tset[j]]
    )
  }

  top_topics <- object$docs %>%
    arrange(doc_id, desc(prob)) %>%
    group_by(doc_id) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  top_topics$id <- match(top_topics$topic, tset) - 1L

  lout <- vector("list", length(dset))
  if (truncate > 0) { docs$text <- stringi::stri_sub(docs$text, 1, truncate) }
  for (j in seq_along(lout))
  {
    lout[[j]] <- list(
      "top_topics_ids" = top_topics$id[top_topics$doc_id == dset[j]],
      "topic_weights" = top_topics$prob[top_topics$doc_id == dset[j]],
      "title" = jsonlite::unbox(dset[j]),
      "text" = docs$text[docs$doc_id == dset[j]],
      "meta" = list()
    )
  }

  res <- list(topics = topics, docs = lout)
  jsonlite::write_json(res, path)
}

#############################################################################
# functions to create wikipedia data

dsst_cache_get <- function(url, cache_dir, page = NULL, force = FALSE)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)

  # create a cache of the query
  cache_file <- file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  # check if file exists and either load or query and save
  if (file.exists(cache_file) & !force)
  {
    res <- readRDS(cache_file)
  } else {
    res <- httr::GET(url)
    saveRDS(res, cache_file)
    if (!is.null(page)) { message(sprintf("Downloading %s", page)) }
  }

  return(res)
}

dsst_wiki_load <- function(
  page, cache_dir = file.path("..", "output", "cache"), lang = "en"
)
{
  # get the page
  url <- httr::modify_url(
    sprintf("https://%s.wikipedia.org/w/api.php", lang),
    query = list(
      action = "parse",
      format = "json",
      redirects = TRUE,
      page = utils::URLdecode(page)
    )
  )
  res <- dsst_cache_get(url, cache_dir = cache_dir, page = page)
  httr::stop_for_status(res)
  obj <- httr::content(res, type = "application/json")

  # return the entire object
  return(obj)
}

dsst_wiki_get_links <- function(
  obj,
  xpath = ".//p//a",
  table_num = NA_integer_,
  column_num = NA_integer_
)
{
  tree <- xml2::read_html(obj$parse$text[[1]])
  links <- xml2::xml_find_all(tree, xpath = xpath)
  links <- xml2::xml_attr(links, "href")
  links <- links[stringi::stri_sub(links, 1L, 6L) == "/wiki/"]
  links <- links[stringi::stri_sub(links, 1L, 16L) != "/wiki/Wikipedia:"]
  links <- stringi::stri_sub(links, 7L, -1L)
  links <- links[!stringi::stri_detect(links, fixed = "#")]
  links <- unique(links)
  tibble(links = links)
}

dsst_wiki_get_links_table <- function(
  obj,
  table_num = 1L,
  column_num = NULL,
  print_first_rows = FALSE
)
{
  tree <- xml2::read_html(obj$parse$text[[1]])
  tables <- xml2::xml_find_all(tree, xpath = ".//table")

  .assert(length(tables) >= table_num,
          sprintf("Asking for table %d of %d", table_num, length(tables)))

  this_table <- tables[[table_num]]
  rows <- xml2::xml_find_all(this_table, ".//tr")

  if (print_first_rows)
  {
    fr <- map_chr(tables, ~ xml2::xml_text(xml2::xml_find_first(..1, ".//tr")))
    fr <- stringi::stri_sub(fr, 1L, options()$width - 5L)
    print(fr)
  }

  # grab links for all columns or specified column
  if (is.null(column_num))
  {
    links <- xml2::xml_find_all(rows, xpath = ".//a")
    links <- xml2::xml_attr(links, "href")
  } else {
    links <- map(rows, function(u) {
      td <- xml2::xml_find_all(u, ".//td")
      res <- ifelse(length(td) < column_num, "",
                    xml2::xml_attr(xml2::xml_find_all(td[[column_num]], ".//a"),
                                   "href"))
      res
    })
    links <- flatten_chr(links)
  }

  # process the links
  links <- links[!is.na(links)]
  links <- links[stringi::stri_sub(links, 1L, 6L) == "/wiki/"]
  links <- links[stringi::stri_sub(links, 1L, 16L) != "/wiki/Wikipedia:"]
  links <- stringi::stri_sub(links, 7L, -1L)
  links <- links[!stringi::stri_detect(links, fixed = "#")]
  links <- unique(links)
  tibble(links = links)
}

dsst_wiki_make_data <- function(
  links, cache_dir = file.path("..", "output", "cache"), lang = "en"
)
{
  links <- links$links
  nl <- length(links)
  res <- tibble(doc_id = rep(NA_character_, nl), text = rep(NA_character_, nl))
  for (j in seq_len(nl))
  {
    obj_json <- dsst_wiki_load(links[j], cache_dir = cache_dir, lang = lang)
    obj_html <- xml2::read_html(obj_json$parse$text[[1]])

    refs <- xml2::xml_find_all(obj_html, ".//sup/a")
    xml2::xml_text(refs) <- ""
    probs <- xml2::xml_find_all(obj_html, ".//span[not(@class)]")
    xml2::xml_text(probs) <- ""

    inst <- xml2::xml_find_all(obj_html, ".//p[not(@class)]")
    text <- stringi::stri_replace_all(
      xml2::xml_text(inst), "", regex = "\\[[\\W\\w]+\\]"
    )
    text <- text[stringi::stri_length(text) > 50]
    text <- text[stringi::stri_sub(text, 1L, 1L) %in% LETTERS]
    text <- stringi::stri_replace_all(text, "", regex = "\\([^\\)]+\\)")
    text <- stringi::stri_replace_all(text, '"', regex = "'")
    text <- stringi::stri_replace_all(text, "", regex = "[^\\w\\.;\\-\\' ]")
    text <- stringi::stri_replace_all(text, " ", regex = "[ ]+")
    text <- stringi::stri_paste(text, collapse = "\n\n")
    res$doc_id[j] <- obj_json$parse$displaytitle
    res$text[j] <- text
  }

  return(res)
}
