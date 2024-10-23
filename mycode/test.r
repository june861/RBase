library(mylin2024111211)
# build a linear random datas
x <- rnorm(100)
y <- 20 * x + 23 + rnorm(100)
data <- data.frame(x, y)
# draw plot
library(ggplot2)
# 绘制散点图
ggplot(data1, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 2) +  # 绘制散点
  labs(title = "Scatter Plot", x = "X Values", y = "Y Values") +  # 添加标题和标签
  theme_minimal()
# fit model
model <- lin_reg(y ~ x, data = data)
abline(model, col = "red")
print(model)
model_save_dir <- getwd()
model_save_name <- "my_linear_mode.rda"
filename <- paste(model_save_dir, model_save_name, sep = '/')
save_model(model = model, filename = filename)
load(filename)
data2 <- data.frame(x = 1:10)
predictions <- predict(model, data2)
print(predictions)
