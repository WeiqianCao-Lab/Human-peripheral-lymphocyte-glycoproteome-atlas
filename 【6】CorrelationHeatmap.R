# 简化版本 - 专注于修复错误
library(readxl)
library(pheatmap)

# 读取和处理数据
file_path <- "/Users/wangyixuan/Desktop/数据处理/糖蛋白热图/四种细胞NGP-PCA-HeatMap.xlsx"
data <- read_excel(file_path,sheet=3)

# 处理行名
if(ncol(data) > 0) {
  if(any(duplicated(data[[1]]))) {
    data[[1]] <- make.unique(as.character(data[[1]]), sep = "_")
  }
  rownames(data) <- data[[1]]
  data <- data[-1]
}

data <- as.data.frame(lapply(data, as.numeric))

# 处理列名：将点号替换为连字符
colnames(data) <- gsub("\\.", "-", colnames(data))

# 检查数据维度
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")

# 计算相关性矩阵
cor_matrix <- cor(data, use = "pairwise.complete.obs")

# 设置热图颜色
red_palette <- colorRampPalette(c("#FFE5E5", "#FF0000"))(100)

# 从列名提取细胞类型前缀
# 修改为只移除末尾的数字
extract_prefix <- function(col_names) {
  # 只移除末尾的数字，保留中间的字母和数字组合
  prefixes <- gsub("\\d+$", "", col_names)
  return(prefixes)
}

# 获取唯一的细胞类型
cell_types <- unique(extract_prefix(colnames(data)))
cat("检测到的细胞类型:", cell_types, "\n")

# 创建自定义标签
n_samples <- ncol(data)
custom_labels <- rep("", n_samples)

# 为每个细胞类型的第二个样本设置标签
for(cell_type in cell_types) {
  # 找到属于当前细胞类型的所有列
  cell_cols <- grep(paste0("^", cell_type, "[0-9]*$"), colnames(data))
  if(length(cell_cols) >= 2) {
    # 在第二个样本位置显示细胞类型名称
    custom_labels[cell_cols[2]] <- cell_type
  }
}

# 创建分组注释 - 根据实际样本名称自动分配Young和Old
# 这里需要根据您的实际样本命名规则来调整
annotation_col <- data.frame(
  AgeGroup = ifelse(grepl("Y", colnames(data), ignore.case = TRUE), "Young", "Aged")
)
rownames(annotation_col) <- colnames(data)

# 创建颜色映射
annotation_colors <- list(
  AgeGroup = c(
    "Young" = "blue",
    "Aged" = "purple"
  )
)

# 绘制热图
pheatmap(cor_matrix,
         color = red_palette,
         breaks = seq(0.5, 1, length.out = 101),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         labels_row = custom_labels,
         labels_col = custom_labels,
         annotation_col = annotation_col,
         annotation_row = annotation_col,
         annotation_colors = annotation_colors,
         annotation_legend = TRUE,
         legend = TRUE,
         main = "Heat map of intercellular correlations",
         fontsize = 10,
         border_color = NA)

# 保存图片
dev.copy(png, "/Users/wangyixuan/Desktop/correlation_heatmap_simple.png", 
         width = 1000, height = 800)
dev.off()