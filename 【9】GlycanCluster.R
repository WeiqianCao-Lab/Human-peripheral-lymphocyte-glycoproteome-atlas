# 加载必要的包
library(readxl)
library(pheatmap)

# 设置桌面路径（自动识别不同操作系统）
if (Sys.info()["sysname"] == "Windows") {
  desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
} else {
  desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
}

# 读取数据
file_path <- file.path(desktop_path, "Fig3C", "六组糖型强度汇总.xlsx")
data <- read_excel(file_path, sheet = 1)

# 设置行名（糖型名称）并移除Glycan列
data_matrix <- as.data.frame(data)
rownames(data_matrix) <- data_matrix$Glycan_Type
data_matrix$Glycan_Type <- NULL

# 定义糖型的顺序（Y轴顺序）
glycan_order <- c("Oligomannose",
                  "Both fucosylation and sialylation",
                  "Sialylation only",
                  "Fucosylation only",
                  "Complex/Hybrid without fucosylation and sialylation")

# 提取三个数据集（按列索引）
# 列1-6：Raw数据，列7-12：Significance数据，列13-18：Normalized数据
all_data <- data_matrix[glycan_order, 1:6]      # Raw数据
sig_data <- data_matrix[glycan_order, 7:12]     # Significance数据
norm_data <- data_matrix[glycan_order, 13:18]   # Normalized数据

# 简化列名（A1,A2,A3对应Group1-3 Aged组，Y1,Y2,Y3对应Group4-6 Young组）
colnames(all_data) <- c("A1", "A2", "A3", "Y1", "Y2", "Y3")
colnames(sig_data) <- c("A1", "A2", "A3", "Y1", "Y2", "Y3")
colnames(norm_data) <- c("A1", "A2", "A3", "Y1", "Y2", "Y3")

# 添加数据集前缀
colnames(all_data) <- paste0("Raw_", colnames(all_data))
colnames(sig_data) <- paste0("Sig_", colnames(sig_data))
colnames(norm_data) <- paste0("Norm_", colnames(norm_data))

# 横向合并成一个大矩阵
merged_data <- cbind(all_data, sig_data, norm_data)

# ========== 创建列注释（Aged和Young分组） ==========
# 每个数据集的前3列是Aged组，后3列是Young组
group_type <- rep(c(rep("Aged", 3), rep("Young", 3)), 3)

# 创建注释数据框
annotation_col <- data.frame(
  Group = factor(group_type, levels = c("Young", "Aged"))
)
rownames(annotation_col) <- colnames(merged_data)

# ========== 定义注释颜色 ==========
annotation_colors <- list(
  Group = c(
    "Young" = "#3498DB",      # 蓝色
    "Aged" = "#9B59B6"        # 紫色
  )
)

# ========== 定义颜色渐变（从深蓝到深红） ==========
color_palette <- colorRampPalette(c("#313695", "#4575B4", "#74ADD1", 
                                    "#ABD9E9", "#E0F3F8", "#FFFFBF", 
                                    "#FEE090", "#FDAE61", "#F46D43", 
                                    "#D73027", "#A50026"))(100)

# ========== 绘制热图并保存为PDF ==========
pdf_path <- file.path(desktop_path, "B细胞糖型热图.pdf")

pheatmap(as.matrix(merged_data),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = color_palette,
         main = "B细胞糖型分组强度统计热图",
         fontsize = 10,
         fontsize_row = 11,
         fontsize_col = 9,
         angle_col = "45",
         border_color = "grey80",
         annotation_col = annotation_col,
         annotation_colors = annotation_colors,
         annotation_names_col = TRUE,
         gaps_col = c(6, 12),
         cellwidth = 28,
         cellheight = 32,
         filename = pdf_path,
         width = 14,
         height = 7
)

cat("\n✅ 完成！热图已保存到：", pdf_path, "\n\n")

# ========== 输出更美观的PDF版本（带数值标注） ==========
pdf_path_with_numbers <- file.path(desktop_path, "B细胞糖型热图_带数值.pdf")

pheatmap(as.matrix(merged_data),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = color_palette,
         main = "B细胞糖型分组强度统计热图\n(显示数值)",
         fontsize = 10,
         fontsize_row = 11,
         fontsize_col = 9,
         angle_col = "45",
         border_color = "grey80",
         annotation_col = annotation_col,
         annotation_colors = annotation_colors,
         annotation_names_col = TRUE,
         gaps_col = c(6, 12),
         display_numbers = TRUE,
         number_color = "black",
         fontsize_number = 6,
         cellwidth = 28,
         cellheight = 32,
         filename = pdf_path_with_numbers,
         width = 14,
         height = 7
)

cat("✅ 带数值的热图已保存到：", pdf_path_with_numbers, "\n\n")

# ========== 输出详细信息 ==========
cat("📊 数据信息：\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat(sprintf("  • 糖型数量：%d\n", nrow(merged_data)))
cat(sprintf("  • 样本数量：%d\n", ncol(merged_data)))
cat("  • 数据集结构：\n")
cat("    - Raw数据：列1-6 (Aged: A1-A3, Young: Y1-Y3)\n")
cat("    - Significance数据：列7-12 (Aged: A1-A3, Young: Y1-Y3)\n")
cat("    - Normalized数据：列13-18 (Aged: A1-A3, Young: Y1-Y3)\n\n")

cat("🎨 颜色渐变（从低值到高值）：\n")
cat("  • 深蓝 (#313695) → 蓝 → 浅蓝 → 浅黄 → 橙 → 红 → 深红 (#A50026)\n")
cat("  • 低表达 ←────────────────────────────────────────→ 高表达\n\n")

cat("📌 图例说明：\n")
cat("  • 🔵 蓝色顶栏：Young组（年轻组）\n")
cat("  • 🟣 紫色顶栏：Aged组（老年组）\n")
cat("  • 竖线分隔：Raw | Significance | Normalized\n")
cat("  • Y轴糖型顺序（从上到下）：\n")
for(i in 1:length(glycan_order)) {
  cat(sprintf("    %d. %s\n", i, glycan_order[i]))
}
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("\n📁 文件保存位置：", desktop_path, "\n")
cat("📄 生成的文件：\n")
cat("  • B细胞糖型热图.pdf（标准版本）\n")
cat("  • B细胞糖型热图_带数值.pdf（显示数值版本）\n")