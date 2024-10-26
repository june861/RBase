lin_reg <- function(formula, data, subset, save_model = TRUE,
                    weights = NULL, na.action = na.fail,
                    model_save_path = NULL, ...) {

  mf <- match.call(expand.dots = FALSE)
  mf <- mf[c(1L, match(c("formula", "data", "subset", "save_model", "weights", "na.action", "model_save_path"), names(mf), 0L))]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())

  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")

  if (!is.null(weights)) {
    if (!is.numeric(weights))
      stop("'weights' must be a numeric vector")
  }

  x <- model.matrix(mt, mf)

  if (is.null(weights)) {
    coef <- lm.fit(x, y)$coefficients
  } else {
    coef <- lm.wfit(x, y, weights)$coefficients
  }

  model <- structure(list(coefficients = coef, residuals = y - x %*% coef,
                          call = match.call(), terms = mt, model = mf,
                          na.action = attr(mf, "na.action"),
                          xlevels = .getXlevels(mt, mf)),
                     class = "lin_reg")

  if (save_model) {
    if (is.null(model_save_path)) {
      curr_dir <- getwd()
      model_save_path <- paste(curr_dir, "linear_model.rda", sep = "/")
    }

    # check model_save_path fairness
    dir_path <- dirname(model_save_path)
    if (!file.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    # Check if file already exists, modify filename if needed
    base_name <- basename(model_save_path)
    pattern <- paste0("^", gsub("[.]", "\\.", base_name), "$")
    dir_files <- list.files(dir_path, pattern = pattern, full.names = FALSE)

    i <- 1
    basename_omit <- strsplit(base_name, "\\.")[[1L]][[1L]]
    while (base_name %in% dir_files) {
      base_name <- paste0(basename_omit, i, ".rda")
      i <- i + 1
    }

    model_save_path <- file.path(dir_path, base_name)
    print(cat("model will be save in ", model_save_path))
    save(model, file = model_save_path)
  }

  model
}

# 创建一些数据
x <- rnorm(100)
y <- 20 * x + 23 + rnorm(100)

# 使用 lin_reg 训练模型
model <- lin_reg(y ~ x, data = data.frame(x = x, y = y))