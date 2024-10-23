if (!requireNamespace("animation")) install.packages("animation")
library(animation)

# 设置动画参数
interval <- 20
ani.options(interval = interval) # 设置帧间隔时间

# 动态拟合线性模型
dynamic_fit <- function(n_points_max = 100, step = 20) {
  set.seed(123) # 设置随机种子确保可重复性
  x <- rnorm(n_points_max)
  y <- 20 * x + 23 + rnorm(n_points_max) # 生成带有噪声的线性数据
  
  saveGIF({
    for (n in seq(step, n_points_max, by = step)) {
      # 使用前n个点拟合模型
      model <- lm(y ~ x, data = data.frame(x = x[1:n], y = y[1:n]))
      # 预测整个x范围内的y值
      predictions <- predict(model, data.frame(x = seq(min(x), max(x), length.out = 100)))
      # 绘制当前拟合线
      plot(x[1:n], y[1:n], pch=19, col="blue", main=paste0("Fitting with ", n, " points"),
           xlab = "x", ylab = "y")
      lines(seq(min(x), max(x), length.out = 100), predictions, col="red", lwd=2)
      
      Sys.sleep(interval) # 暂停一下，让动画效果更明显
    }
  }, movie.name = "dynamic_fit.gif", interval = 0.1)
}

dynamic_fit()