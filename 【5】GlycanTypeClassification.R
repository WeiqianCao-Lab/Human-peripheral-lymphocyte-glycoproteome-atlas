# 加载必要的包
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)

# ========== 1. 文件路径 ==========
file_path <- "/Users/wangyixuan/Desktop/五种细胞糖肽定量（拆分group）_with_glycotype.xlsx"
output_path <- "/Users/wangyixuan/Desktop/五种细胞糖肽定量（拆分group）_with_glycotype_classified.xlsx"

# ========== 2. 定义糖型分类函数 ==========

# 从字符串解析五个数字
parse_glycan_string <- function(glycan_string) {
  if (is.na(glycan_string) || glycan_string == "" || glycan_string == "NA") {
    return(rep(NA, 5))
  }
  
  # 移除方括号并分割字符串
  numbers <- gsub("\\[|\\]", "", glycan_string) %>%
    strsplit("\\s+") %>%
    unlist() %>%
    as.numeric()
  
  if (length(numbers) != 5) {
    return(rep(NA, 5))
  }
  
  return(numbers)
}

# 判断是否为Complex
is_complex <- function(H, N, A, F, pH) {
  return(N > 2 && N >= (H - 1))
}

# 判断是否为Hybrid
is_hybrid <- function(H, N, A, F, pH) {
  return((N > 2 && N < (H - 1)) || (N == 2 && (A > 0 || F > 0)))
}

# 完整糖型分类函数
classify_glycan <- function(H, N, A, F, pH) {
  # 处理NA值
  if (any(is.na(c(H, N, A, F, pH)))) {
    return(NA)
  }
  
  # 第一步：判断是否为Oligomannose
  if (H >= 3 && N == 2 && A == 0 && F == 0) {
    return("Oligomannose")
  }
  
  # 第二步：判断是否为Complex或Hybrid
  is_comp <- is_complex(H, N, A, F, pH)
  is_hyb <- is_hybrid(H, N, A, F, pH)
  
  if (!is_comp && !is_hyb) {
    return("Other")  # 理论上不应出现，作为兜底
  }
  
  # 第三步：根据A和F细分
  if (A == 0 && F == 0) {
    return("Complex/Hybrid without fucosylation and sialylation")
  } else if (A == 0 && F > 0) {
    return("Fucosylation only")
  } else if (A > 0 && F == 0) {
    return("Sialylation only")
  } else if (A > 0 && F > 0) {
    return("Both fucosylation and sialylation")
  }
  
  return("Other")
}

# 从字符串直接分类
classify_from_string <- function(glycan_string) {
  nums <- parse_glycan_string(glycan_string)
  if (any(is.na(nums))) {
    return(NA)
  }
  return(classify_glycan(nums[1], nums[2], nums[3], nums[4], nums[5]))
}

# ========== 3. 定义处理单个sheet的函数 ==========
process_sheet <- function(data, sheet_name) {
  cat("\n========================================\n")
  cat("处理 sheet:", sheet_name, "\n")
  cat("========================================\n")
  
  # 获取D列（第4列）
  if (ncol(data) < 4) {
    cat("警告：", sheet_name, "列数不足，跳过\n")
    return(NULL)
  }
  
  d_col <- data[[4]]
  
  # 对每个单元格进行分类
  classifications <- sapply(d_col, classify_from_string)
  
  # 将分类结果添加到数据框的F列（第6列）
  # 如果原数据列数不足6列，先补充
  while (ncol(data) < 6) {
    data <- cbind(data, NA)
  }
  data[[6]] <- classifications
  
  # 统计分类结果
  class_table <- table(classifications, useNA = "ifany")
  class_df <- as.data.frame(class_table)
  colnames(class_df) <- c("Glycotype", "Count")
  
  # 计算百分比
  total_valid <- sum(class_df$Count[class_df$Glycotype != "NA"], na.rm = TRUE)
  class_df$Percentage <- round(class_df$Count / total_valid * 100, 2)
  
  # 按数量降序排列
  class_df <- class_df %>% arrange(desc(Count))
  
  cat("\n分类统计:\n")
  print(class_df)
  
  return(list(
    data = data,
    statistics = class_df
  ))
}

# ========== 4. 读取所有sheet并处理 ==========
# 获取所有sheet名称
sheets <- excel_sheets(file_path)
cat("文件中的sheet:", paste(sheets, collapse = ", "), "\n")
cat("共有", length(sheets), "个sheet\n")

# 存储处理结果
processed_sheets <- list()
statistics_sheets <- list()

for (i in 1:length(sheets)) {
  sheet_name <- sheets[i]
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("读取 sheet", i, "/", length(sheets), ":", sheet_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # 读取当前sheet
  data <- read_excel(file_path, sheet = sheet_name)
  
  # 处理
  result <- process_sheet(data, sheet_name)
  
  if (!is.null(result)) {
    processed_sheets[[sheet_name]] <- result$data
    statistics_sheets[[paste0(sheet_name, "_统计")]] <- result$statistics
  }
}

# ========== 5. 创建总体统计表 ==========
cat("\n\n")
cat("########################################\n")
cat("#         创建总体统计表              #\n")
cat("########################################\n")

# 定义所有可能的糖型类别
all_types <- c("Oligomannose", 
               "Complex/Hybrid without fucosylation and sialylation",
               "Fucosylation only", 
               "Sialylation only", 
               "Both fucosylation and sialylation",
               "Other", "NA")

# 创建总体统计表
overall_stats <- data.frame(Glycotype = all_types)

for (sheet_name in names(statistics_sheets)) {
  # 提取sheet名称（去掉"_统计"后缀）
  original_name <- gsub("_统计", "", sheet_name)
  stats <- statistics_sheets[[sheet_name]]
  
  # 创建计数列
  count_col <- paste0(original_name, "_Count")
  pct_col <- paste0(original_name, "_Percentage")
  
  overall_stats[[count_col]] <- sapply(all_types, function(gt) {
    if (gt %in% stats$Glycotype) {
      return(stats$Count[stats$Glycotype == gt])
    } else {
      return(0)
    }
  })
  
  overall_stats[[pct_col]] <- sapply(all_types, function(gt) {
    if (gt %in% stats$Glycotype) {
      return(stats$Percentage[stats$Glycotype == gt])
    } else {
      return(0)
    }
  })
}

# 添加总计行
total_row <- data.frame(Glycotype = "TOTAL", stringsAsFactors = FALSE)
for (sheet_name in names(statistics_sheets)) {
  original_name <- gsub("_统计", "", sheet_name)
  count_col <- paste0(original_name, "_Count")
  pct_col <- paste0(original_name, "_Percentage")
  
  total_row[[count_col]] <- sum(overall_stats[[count_col]][overall_stats$Glycotype != "TOTAL"], na.rm = TRUE)
  total_row[[pct_col]] <- 100
}
overall_stats <- rbind(overall_stats, total_row)

# ========== 6. 保存结果到Excel ==========
cat("\n保存结果到Excel文件...\n")

# 创建输出列表
output_sheets <- list()

# 添加处理后的原始数据sheet
for (sheet_name in names(processed_sheets)) {
  output_sheets[[sheet_name]] <- processed_sheets[[sheet_name]]
}

# 添加每个sheet的统计表
for (sheet_name in names(statistics_sheets)) {
  output_sheets[[sheet_name]] <- statistics_sheets[[sheet_name]]
}

# 添加总体统计表
output_sheets[["总体统计"]] <- overall_stats

# 保存到Excel文件
write_xlsx(output_sheets, output_path)

cat("\n结果已保存至:", output_path, "\n")

# ========== 7. 输出统计摘要 ==========
cat("\n========================================\n")
cat("分析完成！统计摘要:\n")
cat("========================================\n")

for (sheet_name in names(statistics_sheets)) {
  original_name <- gsub("_统计", "", sheet_name)
  cat("\n", original_name, ":\n")
  stats <- statistics_sheets[[sheet_name]]
  print(stats)
}

cat("\n总体统计表:\n")
print(overall_stats)

cat("\n========================================\n")
cat("输出文件包含以下sheet:\n")
cat("  - 原始数据sheet（每个sheet的F列为糖型分类结果）\n")
cat("  - 各sheet统计表（每个sheet的糖型统计）\n")
cat("  - 总体统计（所有sheet汇总对比）\n")
cat("========================================\n")