# 加载必要的包
library(readxl)
library(ggplot2)
library(ggrepel)
library(scales)

# 读取数据
file_path <- "/Users/wangyixuan/Desktop/数据处理/Fig3G-差异蛋白火山图/7.（糖蛋白）Aged_Young_merged_合并后_Ttest结果.xlsx"
data <- read_excel(file_path)

# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")

# 去除缺失值
plot_data <- data[!is.na(data$Log2FC) & !is.na(data$P_value), ]

# 定义显著性分组
plot_data$Group <- "Not significant"
plot_data$Group[plot_data$Significant == "up"] <- "Up"
plot_data$Group[plot_data$Significant == "down"] <- "Down"

# 标记目标蛋白
target_protein <- "sp|P04233|HG2A_HUMAN"
plot_data$IsTarget <- plot_data$Protein == target_protein

# 统计信息
cat("\n绘图数据统计:\n")
cat("总蛋白数:", nrow(plot_data), "\n")
cat("上调蛋白 (红色):", sum(plot_data$Group == "Up"), "\n")
cat("下调蛋白 (蓝色):", sum(plot_data$Group == "Down"), "\n")
cat("不显著蛋白 (灰色):", sum(plot_data$Group == "Not significant"), "\n")
cat("目标蛋白是否在数据中:", ifelse(any(plot_data$IsTarget), "是", "否"), "\n")

if(any(plot_data$IsTarget)) {
  target_data <- plot_data[plot_data$IsTarget, ]
  cat("目标蛋白信息:\n")
  print(target_data[, c("Protein", "Log2FC", "P_value", "Significant")])
}

# 创建火山图（修正边框问题）
p <- ggplot(plot_data, aes(x = Log2FC, y = -log10(P_value))) +
  # 添加点 - 去掉 stroke 参数，使用 shape = 16（实心圆无边框）
  geom_point(aes(color = Group, size = Group, alpha = Group), shape = 16) +
  # 设置颜色：上调=红色，下调=蓝色，不显著=灰色
  scale_color_manual(values = c("Up" = "#E31A23",      # 红色
                                "Down" = "#2171B5",    # 蓝色
                                "Not significant" = "#999999")) +  # 灰色
  # 设置点的大小和透明度
  scale_size_manual(values = c("Up" = 2.5, 
                               "Down" = 2.5, 
                               "Not significant" = 2.5)) +
  scale_alpha_manual(values = c("Up" = 0.8, 
                                "Down" = 0.8, 
                                "Not significant" = 0.5)) +
  # 添加阈值线
  geom_vline(xintercept = c(-1, 1), 
             linetype = "dashed", 
             color = "black", 
             size = 0.5,
             alpha = 0.7) +
  geom_hline(yintercept = -log10(0.05), 
             linetype = "dashed", 
             color = "black", 
             size = 0.5,
             alpha = 0.7) +
  # 标记目标蛋白（黄色大点）- 使用 shape = 16 去掉边框
  geom_point(data = subset(plot_data, IsTarget),
             aes(x = Log2FC, y = -log10(P_value)),
             color = "#FFD700",  # 金黄色
             size = 4,
             shape = 16,  # 实心圆，无边框
             alpha = 1) +
  # 添加目标蛋白标签
  geom_text_repel(data = subset(plot_data, IsTarget),
                  aes(x = Log2FC, y = -log10(P_value), label = Protein),
                  size = 5,
                  color = "black",
                  fontface = "bold",
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = "black",
                  segment.size = 0.5,
                  arrow = arrow(length = unit(0.01, "npc"))) +
  # 设置主题
  theme_minimal(base_size = 14) +
  theme(
    # 面板背景
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#F0F0F0", size = 0.3),
    panel.grid.minor = element_line(color = "#F5F5F5", size = 0.2),
    panel.border = element_rect(fill = NA, color = "black", size = 0.8),
    # 图例
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    # 坐标轴
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black", size = 0.5),
    # 标题
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    # 边距
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  # 坐标轴标签
  labs(x = expression(Log[2] ~ "Fold Change (Aged/Young)"),
       y = expression(-Log[10] ~ "(P value)"),
       title = "Volcano Plot of Protein Expression",
       subtitle = "Aged vs Young",
       color = "Regulation",
       size = "Regulation",
       alpha = "Regulation") +
  # 设置坐标轴范围（可选，根据数据调整）
  scale_x_continuous(limits = c(min(plot_data$Log2FC, na.rm = TRUE) - 0.5,
                                max(plot_data$Log2FC, na.rm = TRUE) + 0.5)) +
  # 添加阈值标签
  annotate("text", x = 1.2, y = -log10(0.05) + 0.3, 
           label = "Log2FC = 1", size = 3.5, color = "black") +
  annotate("text", x = -1.2, y = -log10(0.05) + 0.3, 
           label = "Log2FC = -1", size = 3.5, color = "black") +
  annotate("text", x = max(plot_data$Log2FC, na.rm = TRUE) - 1, 
           y = -log10(0.05) + 0.5, 
           label = "P = 0.05", size = 3.5, color = "black")

# 显示图形
print(p)

# 保存高清图片
# PDF格式（矢量图，适合论文投稿）
pdf_path <- "/Users/wangyixuan/Desktop/Volcano_Plot_Publication.pdf"
ggsave(pdf_path, plot = p, width = 10, height = 8, device = "pdf", bg = "white")
cat("✓ PDF矢量图已保存:", pdf_path, "\n")

# PNG格式（高清）
png_path <- "/Users/wangyixuan/Desktop/Volcano_Plot_Publication.png"
ggsave(png_path, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ PNG图片已保存:", png_path, "\n")

# 输出统计信息
cat("\n" %>% rep(60) %>% paste(collapse=""), "\n")
cat("火山图统计摘要:\n")
cat("=" %>% rep(60) %>% paste(collapse=""), "\n")
cat(sprintf("上调蛋白 (Log2FC > 1, P < 0.05): %d (%.1f%%)\n", 
            sum(plot_data$Group == "Up"), 
            sum(plot_data$Group == "Up")/nrow(plot_data)*100))
cat(sprintf("下调蛋白 (Log2FC < -1, P < 0.05): %d (%.1f%%)\n", 
            sum(plot_data$Group == "Down"), 
            sum(plot_data$Group == "Down")/nrow(plot_data)*100))
cat(sprintf("总显著差异蛋白: %d (%.1f%%)\n", 
            sum(plot_data$Group %in% c("Up", "Down")),
            sum(plot_data$Group %in% c("Up", "Down"))/nrow(plot_data)*100))

if(any(plot_data$IsTarget)) {
  cat("\n✓ 目标蛋白 sp|P04233|HG2A_HUMAN 已用黄色高亮标记\n")
}

# 导出统计表格
summary_stats <- data.frame(
  指标 = c("总蛋白数", "上调蛋白", "下调蛋白", "不显著蛋白", "目标蛋白"),
  数量 = c(nrow(plot_data), 
         sum(plot_data$Group == "Up"),
         sum(plot_data$Group == "Down"),
         sum(plot_data$Group == "Not significant"),
         sum(plot_data$IsTarget)),
  百分比 = c("100%",
          sprintf("%.1f%%", sum(plot_data$Group == "Up")/nrow(plot_data)*100),
          sprintf("%.1f%%", sum(plot_data$Group == "Down")/nrow(plot_data)*100),
          sprintf("%.1f%%", sum(plot_data$Group == "Not significant")/nrow(plot_data)*100),
          sprintf("%.1f%%", sum(plot_data$IsTarget)/nrow(plot_data)*100))
)

write.csv(summary_stats, "/Users/wangyixuan/Desktop/Volcano_Plot_Statistics.csv", row.names = FALSE)
cat("\n✓ 统计摘要已保存: Volcano_Plot_Statistics.csv\n")

cat("\n所有文件已保存至桌面！\n")