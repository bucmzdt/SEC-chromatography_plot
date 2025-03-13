# 加载必要的包
library(ggplot2)
library(dplyr)
library(svglite)

folder_path <- "F:\\20250117SEC"  # 例如 "C:/Users/yourname/Documents/CSV_Files"
setwd(folder_path)
# 获取所有 CSV 文件的列表
#file_list <- list.files(path=folder_path, pattern="*.csv", full.names=TRUE)
file_list <- list.files(path=folder_path, pattern="\\.csv$", full.names=TRUE, ignore.case=TRUE)
#file_list <- list.files(path=folder_path, pattern="*.CSV|*.csv", full.names=TRUE, ignore.case=TRUE)
#更准确：确保只匹配 真正的 CSV 文件，而不会错误地匹配类似 "myfile_csv.txt" 这种文件（因为 _csv.txt 里也有 csv）。
#支持大小写匹配（通过 ignore.case=TRUE）：可以自动匹配 .CSV 和 .csv。
# 创建空的 data.frame 以存储所有数据
all_data <- data.frame()

#归一化
## Min-Max 归一化函数

normalize_minmax <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100)
}
#设置颜色向量
color_vector <- c("red", "blue", "green", "purple", "orange", "cyan", "pink", "brown", "gray", "black")
# 读取所有 CSV 文件，并合并到 `all_data`
for (i in seq_along(file_list)) {
  df <- read.csv(file_list[i], sep="\t", header=FALSE, stringsAsFactors=FALSE, fileEncoding="UTF-16", skip = 300, nrows = 600)
  
  colnames(df) <- c("Retention_Time", "UV_Absorbance")
  
  df$Retention_Time <- as.numeric(df$Retention_Time)
  df$UV_Absorbance <- as.numeric(df$UV_Absorbance)
  df <- na.omit(df)
  
  # 归一化 UV 吸收度
  df$UV_Absorbance_MaxMin_Normalized <- normalize_minmax(df$UV_Absorbance)
  
  # **去掉 .csv 后缀**
  df$file_name <- as.character(gsub("\\.CSV$", "", basename(file_list[i]), ignore.case=TRUE))
  
  
  #df$Group <- i  # 用于 3D 分层
  df$Color <- color_vector[i %% length(color_vector) ]  # 按索引循环使用颜色
  
  all_data <- bind_rows(all_data, df)
}



# 绘制不同颜色的平滑曲线
ggplot1=ggplot(all_data, aes(x=Retention_Time, y=UV_Absorbance, color=file_name)) +
  #geom_smooth(method="loess", span=0.1, se=FALSE, size=0.7) + # 手动指定颜色# Loess 平滑曲线
  geom_point(size=0, alpha=0.0) +  # 绘制散点
  geom_line(size=0.6, linetype="solid")+ # 手动指定颜色# Loess 平滑曲线
  scale_color_manual(values=color_vector) + 
  xlim(2, 6) +  # 设置 X 轴显示范围
  ylim(-1, 10) + # 设置 Y 轴显示范围
  labs(title="Chromatography Curves for Multiple Files",
       x="Retention Time (minutes)",
       y="280nm UV Absorbance(mA) ",
       color="test") +  # 图例标题
  theme_minimal() +
  theme(legend.position="right")  # 将图例放在右侧
ggplot1
#ggsave("chromatography_plot.png", plot = ggplot1, width = 8, height = 5, dpi = 300,bg = "white")
#ggsave("chromatography_plot.eps", plot = ggplot1, width = 12, height = 8, dpi = 600, device = "eps")
#透明色
#ggsave("chromatography_plot.svg", plot = ggplot1, width = 8, height = 5, dpi = 300, device = "svg", bg = "transparent")
ggsave("chromatography_plot.pdf", plot = ggplot1, width = 8, height = 5, dpi = 300, device = "pdf")
# 绘制不同颜色的Normalized平滑曲线
ggplot2=ggplot(all_data, aes(x=Retention_Time, y=UV_Absorbance_MaxMin_Normalized, color=file_name)) +
  #geom_smooth(method="loess", span=0.1, se=FALSE, size=0.7) +
  geom_point(size=0, alpha=0.0) +  # 绘制散点
  geom_line(size=0.6, linetype="solid")+ # 手动指定颜色# Loess 平滑曲线
  scale_color_manual(values=color_vector) +  
  labs(title="Chromatography Curves for Multiple Files",
       x="Retention Time (minutes)",
       y="UV Absorbance Normalized (100%)",
       color="test") +  # 图例标题
  theme_minimal() +
  theme(legend.position="right")  # 将图例放在右侧
ggplot2
#ggsave("chromatography_plot.png", plot = ggplot2, width = 8, height = 5, dpi = 300,bg = "white")

#ggsave("chromatography_plot.eps", plot = ggplot2, width = 12, height = 8, dpi = 600, device = "eps")
ggsave("chromatography_plot.pdf", plot = ggplot2, width = 8, height = 5, dpi = 300, device = "pdf")



library(plotly)  # 3D 可视化



# 设置 CSV 文件所在的文件夹路径（请替换为你的文件夹路径）



# 使用 plotly 绘制 3D 立体曲线图
p <- plot_ly(all_data, x = ~Retention_Time, y = ~UV_Absorbance, z = ~as.character(all_data$file_name), 
             type = 'scatter3d', mode = 'lines',
             color = ~file_name,
             colors = all_data$Color,# 让不同组用不同颜色
             line = list(width = 4)) %>%
  layout(title = "3D Chromatography Curves",
         scene = list(xaxis = list(title = "Retention Time (minutes)"),
                      yaxis = list(title = "280nM UV Absorbance (mA)"),
                      zaxis = list(title = "Group ")))

# 显示 3D 图像
p
htmlwidgets::saveWidget(p, "chromatography_3D_plot.html")#保存图像


# 进行数据的Normalize

#all_data$Group <- all_data$file_name

# 使用 plotly 绘制 3D 立体曲线图
D3PlotNm<- plot_ly(all_data, x = ~Retention_Time, y = ~UV_Absorbance_MaxMin_Normalized, z = ~file_name, 
             type = 'scatter3d', mode = 'lines',
             color = ~file_name,
             colors = all_data$Color,# 让不同组用不同颜色
             line = list(width = 4)) %>%
  layout(title = "3D Chromatography Curves Normalized",
         scene = list(xaxis = list(title = "Retention Time (minutes)"),
                      yaxis = list(title = "UV Absorbance Normalized (100%)"),
                      zaxis = list(title = "Group ")))

# 显示 3D 图像
D3PlotNm
htmlwidgets::saveWidget(D3PlotNm, "chromatography_3D_plot_Normalized.html")#保存图像

