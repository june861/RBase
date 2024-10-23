# library(mylin2024111211)
# # build a linear random datas
# x <- rnorm(100)
# y <- 20 * x + 23 + rnorm(100)
# data <- data.frame(x, y)
# # draw plot
# library(ggplot2)
# # 绘制散点图
# ggplot(data1, aes(x = x, y = y)) +
#   geom_point(color = "blue", size = 2) +  # 绘制散点
#   labs(title = "Scatter Plot", x = "X Values", y = "Y Values") +  # 添加标题和标签
#   theme_minimal()
# # fit model
# model <- lin_reg(y ~ x, data = data)
# abline(model, col = "red")
# print(model)
# model_save_dir <- getwd()
# model_save_name <- "my_linear_mode.rda"
# filename <- paste(model_save_dir, model_save_name, sep = '/')
# save_model(model = model, filename = filename)
# load(filename)
# data2 <- data.frame(x = 1:10)
# predictions <- predict(model, data2)
# print(predictions)



library(mylin2024111211)

# 创建一些数据
set.seed(123) # 保证随机数可重复
x <- rnorm(100)
y <- 20 * x + 23 + rnorm(100)

# 使用 lin_reg 训练模型
model <- lin_reg(y ~ x, data = data.frame(x = x, y = y))

# 输出模型信息
print(model)

# # 预测值
predictions <- predict(model, data.frame(x = seq(min(x),
                                                 max(x), length.out = 10)))

# 绘制原始数据与预测线
plot(x, y, pch = 19, col = "blue", main = "Linear Regression Model",
     xlab = "x", ylab = "y")
abline(model, col = 'orange')
lines(seq(min(x), max(x), length.out = 10), predictions, col = "red", lwd = 2)
legend("topleft", legend = c("Actual Data", "Fitted Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = 2)