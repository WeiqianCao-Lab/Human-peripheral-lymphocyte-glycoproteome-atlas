# 加载必要的包
if (!require("readxl")) install.packages("readxl")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
library(readxl)
library(openxlsx)
library(dplyr)

# 设置文件路径
file_path <- '/Users/wangyixuan/Desktop/数据处理/Fig2A蛋白/Fig2A 蛋白.xlsx'

# 1. 获取所有sheet名称
sheet_names <- excel_sheets(file_path)

# 2. 创建汇总数据框
summary_data <- data.frame(
  Sheet_Name = character(),
  Group_Name = character(),
  Column2_Total = integer(),
  Column3_Total = integer(),
  Overlap_Count = integer(),
  Column2_Unique = integer(),
  Column3_Unique = integer(),
  Overlap_Ratio_Col2 = numeric(),
  Overlap_Ratio_Col3 = numeric(),
  stringsAsFactors = FALSE
)

# 3. 创建存储具体元素的工作簿（可选）
detail_wb <- createWorkbook()
addWorksheet(detail_wb, "汇总统计")

# 4. 循环处理每个sheet
all_overlap_list <- list()
all_col2_unique_list <- list()
all_col3_unique_list <- list()

for (sheet in sheet_names) {
  # 读取当前sheet的数据
  data <- read_excel(file_path, sheet = sheet)
  
  # 检查数据是否有至少三列
  if (ncol(data) >= 3) {
    # 获取第二列和第三列的数据（跳过标题行，去除NA值）
    col2_data <- na.omit(data[[2]])
    col3_data <- na.omit(data[[3]])
    
    # 去重处理
    col2_unique <- unique(col2_data)
    col3_unique <- unique(col3_data)
    
    # 计算交集
    common_elements <- intersect(col2_unique, col3_unique)
    
    # 计算特有元素
    col2_only <- setdiff(col2_unique, col3_unique)
    col3_only <- setdiff(col3_unique, col2_unique)
    
    # 计算统计量
    col2_total <- length(col2_unique)
    col3_total <- length(col3_unique)
    overlap_count <- length(common_elements)
    col2_unique_count <- length(col2_only)
    col3_unique_count <- length(col3_only)
    
    # 计算交叠比例
    overlap_ratio_col2 <- ifelse(col2_total > 0, round(overlap_count/col2_total * 100, 2), 0)
    overlap_ratio_col3 <- ifelse(col3_total > 0, round(overlap_count/col3_total * 100, 2), 0)
    
    # 添加到汇总数据框
    summary_data <- rbind(summary_data, data.frame(
      Sheet_Name = sheet,
      Group_Name = sheet,
      Column2_Total = col2_total,
      Column3_Total = col3_total,
      Overlap_Count = overlap_count,
      Column2_Unique = col2_unique_count,
      Column3_Unique = col3_unique_count,
      Overlap_Ratio_Col2 = overlap_ratio_col2,
      Overlap_Ratio_Col3 = overlap_ratio_col3,
      stringsAsFactors = FALSE
    ))
    
    # 存储具体元素（可选，用于创建详细结果）
    all_overlap_list[[sheet]] <- common_elements
    all_col2_unique_list[[sheet]] <- col2_only
    all_col3_unique_list[[sheet]] <- col3_only
    
    # 打印处理进度
    cat(sprintf("已处理 sheet: %s\n", sheet))
    cat(sprintf("  第二列总数: %d, 第三列总数: %d\n", col2_total, col3_total))
    cat(sprintf("  交集个数: %d, 第二列特有: %d, 第三列特有: %d\n\n", 
                overlap_count, col2_unique_count, col3_unique_count))
    
  } else {
    # 如果列数不足，添加错误信息
    summary_data <- rbind(summary_data, data.frame(
      Sheet_Name = sheet,
      Group_Name = sheet,
      Column2_Total = NA,
      Column3_Total = NA,
      Overlap_Count = NA,
      Column2_Unique = NA,
      Column3_Unique = NA,
      Overlap_Ratio_Col2 = NA,
      Overlap_Ratio_Col3 = NA,
      stringsAsFactors = FALSE
    ))
    
    cat(sprintf("警告: sheet '%s' 数据列数不足，无法计算\n\n", sheet))
  }
}

# 5. 创建最终结果工作簿
final_wb <- createWorkbook()

# 添加汇总统计表
addWorksheet(final_wb, "汇总统计")
writeData(final_wb, sheet = "汇总统计", summary_data, rowNames = FALSE)

# 可选：添加格式美化
# 设置列宽
setColWidths(final_wb, sheet = "汇总统计", cols = 1:9, widths = c(15, 15, 12, 12, 12, 12, 12, 15, 15))

# 添加百分比格式
addStyle(final_wb, sheet = "汇总统计", 
         style = createStyle(numFmt = "0.00%"),
         rows = 2:(nrow(summary_data) + 1), cols = 8:9, gridExpand = TRUE)

# 6. 可选：创建详细元素列表sheet（包含交集元素）
if (length(all_overlap_list) > 0) {
  # 找出最长交集列表的长度
  max_length <- max(sapply(all_overlap_list, length))
  
  # 创建数据框
  overlap_df <- data.frame(matrix(NA, nrow = max_length, ncol = length(sheet_names)))
  colnames(overlap_df) <- sheet_names
  
  # 填充数据
  for (sheet in sheet_names) {
    elements <- all_overlap_list[[sheet]]
    if (length(elements) > 0) {
      overlap_df[1:length(elements), sheet] <- elements
    }
  }
  
  addWorksheet(final_wb, "交集元素详情")
  writeData(final_wb, sheet = "交集元素详情", overlap_df, rowNames = FALSE)
}

# 7. 可选：创建特有元素列表sheet
# 第二列特有元素
if (length(all_col2_unique_list) > 0) {
  max_length <- max(sapply(all_col2_unique_list, length))
  col2_unique_df <- data.frame(matrix(NA, nrow = max_length, ncol = length(sheet_names)))
  colnames(col2_unique_df) <- sheet_names
  
  for (sheet in sheet_names) {
    elements <- all_col2_unique_list[[sheet]]
    if (length(elements) > 0) {
      col2_unique_df[1:length(elements), sheet] <- elements
    }
  }
  
  addWorksheet(final_wb, "第二列特有元素")
  writeData(final_wb, sheet = "第二列特有元素", col2_unique_df, rowNames = FALSE)
}

# 第三列特有元素
if (length(all_col3_unique_list) > 0) {
  max_length <- max(sapply(all_col3_unique_list, length))
  col3_unique_df <- data.frame(matrix(NA, nrow = max_length, ncol = length(sheet_names)))
  colnames(col3_unique_df) <- sheet_names
  
  for (sheet in sheet_names) {
    elements <- all_col3_unique_list[[sheet]]
    if (length(elements) > 0) {
      col3_unique_df[1:length(elements), sheet] <- elements
    }
  }
  
  addWorksheet(final_wb, "第三列特有元素")
  writeData(final_wb, sheet = "第三列特有元素", col3_unique_df, rowNames = FALSE)
}

# 8. 保存结果到新文件
output_path <- '/Users/wangyixuan/Desktop/Fig2A_蛋白_交叠分析汇总.xlsx'
saveWorkbook(final_wb, output_path, overwrite = TRUE)

# 9. 打印统计摘要
cat("\n====== 统计摘要 ======\n")
cat(sprintf("总处理sheet数: %d\n", length(sheet_names)))
cat(sprintf("成功处理sheet数: %d\n", sum(!is.na(summary_data$Overlap_Count))))
cat("\n汇总统计包含以下列:\n")
cat("1. Sheet_Name: sheet名称\n")
cat("2. Group_Name: 组别名称\n")
cat("3. Column2_Total: 第二列不重复元素总数\n")
cat("4. Column3_Total: 第三列不重复元素总数\n")
cat("5. Overlap_Count: 第二列和第三列交集元素个数\n")
cat("6. Column2_Unique: 第二列特有元素个数\n")
cat("7. Column3_Unique: 第三列特有元素个数\n")
cat("8. Overlap_Ratio_Col2: 交集占第二列比例(百分比)\n")
cat("9. Overlap_Ratio_Col3: 交集占第三列比例(百分比)\n")
cat(sprintf("\n分析完成！结果已保存到: %s\n", output_path))
cat("Excel文件包含以下sheet:\n")
cat("1. 汇总统计: 所有组的统计结果汇总\n")
cat("2. 交集元素详情: 各组的交集元素列表\n")
cat("3. 第二列特有元素: 各组的第二列特有元素列表\n")
cat("4. 第三列特有元素: 各组的第三列特有元素列表\n")

# 10. 在控制台显示汇总统计
cat("\n====== 汇总统计表 ======\n")
print(summary_data)