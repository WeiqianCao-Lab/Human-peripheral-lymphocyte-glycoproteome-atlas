# 安装并加载必要的包
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

# 读取Excel文件的第三个sheet
data <- read_excel("/Users/wangyixuan/Desktop/数据处理/糖蛋白PCA/四种细胞NGP-PCA-HeatMap.xlsx", 
                   sheet = 3)  # 添加sheet参数

# 检查数据结构和列名
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
cat("前几行数据预览:\n")
print(head(data))

# 设置行名为蛋白代号（假设第一列是蛋白代号）
if (ncol(data) >= 1) {
  rownames(data) <- data[[1]]
  protein_data <- data[, -1]  # 移除第一列（蛋白代号）
  
  # 检查并处理缺失值
  if (any(is.na(protein_data))) {
    cat("发现缺失值，正在处理...\n")
    protein_data <- na.omit(protein_data)
  }
  
  # 识别并移除常数列（注意：prcomp需要转置后的数据，所以这里按行检查）
  constant_columns <- apply(protein_data, 1, function(x) length(unique(x)) == 1)
  if (any(constant_columns)) {
    cat("发现", sum(constant_columns), "个常数列，正在移除...\n")
    protein_data <- protein_data[!constant_columns, ]
  }
  
  # 检查数据是否为空
  if (nrow(protein_data) == 0) {
    stop("所有蛋白质列都是常数，无法进行PCA分析")
  }
  
  # 打印数据基本信息
  cat("处理后的数据维度:", dim(protein_data), "\n")
  cat("样本数量:", ncol(protein_data), "\n")
  
  # 创建样本分组信息
  # 这里假设每个样本有3个重复，但第三个sheet的数据结构可能不同
  # 你可以根据实际情况调整分组逻辑
  n_samples <- ncol(protein_data)
  sample_names <- c("B-A", "B-Y", "NK-A", "NK-Y", "Tc-A", "Tc-Y", "Th-A", "Th-Y")
  
  # 检查样本数量是否匹配
  if (n_samples %% 3 == 0) {
    # 如果样本数是3的倍数，使用原分组逻辑
    actual_n_samples <- n_samples / 3
    if (actual_n_samples == length(sample_names)) {
      groups <- rep(sample_names, each = 3)
    } else {
      warning("样本数量与提供的名称数量不匹配，将使用自动生成的名称")
      groups <- rep(paste0("Sample", 1:actual_n_samples), each = 3)
    }
  } else {
    # 如果样本数不是3的倍数，使用简单分组
    warning("样本数量不是3的倍数，使用简单分组")
    if (n_samples <= length(sample_names)) {
      groups <- sample_names[1:n_samples]
    } else {
      groups <- paste0("Sample", 1:n_samples)
    }
  }
  
  cat("分组信息:", unique(groups), "\n")
  cat("每组样本数:", table(groups), "\n")
  
  # 使用prcomp执行PCA分析
  # 注意：prcomp要求样本在行，变量在列，所以需要转置
  pca_result <- prcomp(t(protein_data), scale. = TRUE)
  
  # 提取PCA坐标
  pca_coord <- as.data.frame(pca_result$x[, 1:2])
  colnames(pca_coord) <- c("PC1", "PC2")
  pca_coord$Group <- groups
  
  # 计算方差解释百分比
  variance_explained <- round(summary(pca_result)$importance[2, 1:2] * 100, 1)
  
  # 自定义函数计算置信椭圆
  calculate_ellipse <- function(x, y, level = 0.95) {
    if (length(x) < 3) {
      return(data.frame(x = NA, y = NA))
    }
    df <- data.frame(x = x, y = y)
    cov_mat <- cov(df)
    eigen_decomp <- eigen(cov_mat)
    eigenvalues <- eigen_decomp$values
    eigenvectors <- eigen_decomp$vectors
    
    # 计算椭圆参数
    theta <- seq(0, 2 * pi, length.out = 100)
    circle <- cbind(cos(theta), sin(theta))
    
    # 计算缩放因子
    scale <- sqrt(qchisq(level, 2))
    
    # 计算椭圆坐标
    ellipse <- t(eigenvectors %*% (t(circle) * scale * sqrt(eigenvalues)) + 
                   c(mean(x), mean(y)))
    
    return(data.frame(x = ellipse[, 1], y = ellipse[, 2]))
  }
  
  # 为每个组计算置信椭圆
  ellipse_data <- data.frame()
  for (group in unique(groups)) {
    group_data <- pca_coord[pca_coord$Group == group, ]
    if (nrow(group_data) >= 3) {
      ellipse <- calculate_ellipse(group_data$PC1, group_data$PC2)
      ellipse$Group <- group
      ellipse_data <- rbind(ellipse_data, ellipse)
    }
  }
  
  # 使用ggplot2绘制PCA图
  p <- ggplot(pca_coord, aes(x = PC1, y = PC2, color = Group, fill = Group)) +
    geom_point(size = 0.8) +
    geom_path(data = ellipse_data, aes(x = x, y = y, color = Group), 
              alpha = 0.7, size = 0.8) +
    geom_polygon(data = ellipse_data, aes(x = x, y = y, fill = Group), 
                 alpha = 0.2) +
    labs(title = "PCA Analysis of Glycoprotein Expression",
         x = paste0("PC1 (", variance_explained[1], "%)"),
         y = paste0("PC2 (", variance_explained[2], "%)")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")
  
  # 显示图形
  print(p)
  
  # 保存图形
  ggsave("PCA_plot_sheet3_confidence.png", p, width = 10, height = 8, dpi = 300)
  
  # 可选：查看PCA结果的摘要信息
  cat("\nPCA结果摘要:\n")
  print(summary(pca_result))
  
  # 可选：查看前几个主成分的载荷
  cat("\n前5个变量在PC1和PC2上的载荷:\n")
  loadings <- pca_result$rotation[, 1:2]
  print(head(loadings, 5))
  
} else {
  stop("数据列数不足，无法进行分析")
}