# 加载必要的包
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

# ========== 1. 读取文件 ==========
file_path <- "/Users/wangyixuan/Desktop/五种细胞糖肽定量（拆分group）_副本.xlsx"

# 获取所有sheet名称
sheets <- excel_sheets(file_path)
cat("文件中的sheet:", paste(sheets, collapse = ", "), "\n")
cat("共有", length(sheets), "个sheet\n\n")

# ========== 2. 定义分析函数 ==========
analyze_protein_frequency <- function(data, sheet_name) {
  cat("\n========================================\n")
  cat("正在分析 sheet:", sheet_name, "\n")
  cat("========================================\n")
  
  # 获取A列（第一列）
  a_col <- data[[1]]
  
  # 清理数据：去除空值、NA和空格
  proteins <- a_col[!is.na(a_col) & a_col != "" & trimws(a_col) != ""]
  proteins <- trimws(proteins)
  
  cat("总蛋白条目数（包括重复）:", length(proteins), "\n")
  cat("唯一蛋白数量:", length(unique(proteins)), "\n")
  
  # 统计每个蛋白出现的次数
  protein_counts <- as.data.frame(table(proteins))
  colnames(protein_counts) <- c("Protein_ID", "Frequency")
  
  # 按频率降序排列
  protein_counts <- protein_counts %>% arrange(desc(Frequency))
  
  # 统计出现1次、2次、3次、4次、>=5次的蛋白数量
  freq_1 <- sum(protein_counts$Frequency == 1)
  freq_2 <- sum(protein_counts$Frequency == 2)
  freq_3 <- sum(protein_counts$Frequency == 3)
  freq_4 <- sum(protein_counts$Frequency == 4)
  freq_5plus <- sum(protein_counts$Frequency >= 5)
  
  # 计算百分比
  total_unique <- nrow(protein_counts)
  
  cat("\n频率统计:\n")
  cat("  出现1次的蛋白数量:", freq_1, "(", round(freq_1/total_unique*100, 2), "%)\n")
  cat("  出现2次的蛋白数量:", freq_2, "(", round(freq_2/total_unique*100, 2), "%)\n")
  cat("  出现3次的蛋白数量:", freq_3, "(", round(freq_3/total_unique*100, 2), "%)\n")
  cat("  出现4次的蛋白数量:", freq_4, "(", round(freq_4/total_unique*100, 2), "%)\n")
  cat("  出现>=5次的蛋白数量:", freq_5plus, "(", round(freq_5plus/total_unique*100, 2), "%)\n")
  
  # 显示出现次数最多的前10个蛋白
  cat("\n出现次数最多的前10个蛋白:\n")
  top10 <- head(protein_counts, 10)
  print(top10)
  
  # 如果有出现>=5次的蛋白，显示详细信息
  if (freq_5plus > 0) {
    cat("\n出现>=5次的蛋白列表:\n")
    high_freq <- protein_counts %>% filter(Frequency >= 5) %>% arrange(desc(Frequency))
    print(high_freq)
  }
  
  # 返回结果列表
  return(list(
    sheet_name = sheet_name,
    total_entries = length(proteins),
    unique_proteins = total_unique,
    freq_table = protein_counts,
    freq_summary = data.frame(
      Frequency = c("1次", "2次", "3次", "4次", ">=5次"),
      Count = c(freq_1, freq_2, freq_3, freq_4, freq_5plus),
      Percentage = c(
        round(freq_1/total_unique*100, 2),
        round(freq_2/total_unique*100, 2),
        round(freq_3/total_unique*100, 2),
        round(freq_4/total_unique*100, 2),
        round(freq_5plus/total_unique*100, 2)
      )
    )
  ))
}

# ========== 3. 遍历所有sheet进行分析 ==========
all_results <- list()
all_summaries <- list()

for (i in 1:length(sheets)) {
  sheet_name <- sheets[i]
  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat("读取 sheet", i, "/", length(sheets), ":", sheet_name, "\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  
  # 读取当前sheet
  data <- read_excel(file_path, sheet = sheet_name)
  
  # 分析
  result <- analyze_protein_frequency(data, sheet_name)
  
  # 存储结果
  all_results[[sheet_name]] <- result
  all_summaries[[sheet_name]] <- result$freq_summary
}

# ========== 4. 汇总所有sheet的结果 ==========
cat("\n\n")
cat("########################################\n")
cat("#         汇总所有sheet的结果          #\n")
cat("########################################\n")

# 创建汇总表
master_summary <- data.frame(
  Sheet = character(),
  Total_Entries = integer(),
  Unique_Proteins = integer(),
  Freq_1 = integer(),
  Freq_2 = integer(),
  Freq_3 = integer(),
  Freq_4 = integer(),
  Freq_5plus = integer(),
  stringsAsFactors = FALSE
)

for (sheet_name in sheets) {
  result <- all_results[[sheet_name]]
  summary <- result$freq_summary
  
  master_summary <- rbind(master_summary, data.frame(
    Sheet = sheet_name,
    Total_Entries = result$total_entries,
    Unique_Proteins = result$unique_proteins,
    Freq_1 = summary$Count[summary$Frequency == "1次"],
    Freq_2 = summary$Count[summary$Frequency == "2次"],
    Freq_3 = summary$Count[summary$Frequency == "3次"],
    Freq_4 = summary$Count[summary$Frequency == "4次"],
    Freq_5plus = summary$Count[summary$Frequency == ">=5次"]
  ))
}

# 添加百分比列
master_summary$Freq_1_Pct <- round(master_summary$Freq_1 / master_summary$Unique_Proteins * 100, 2)
master_summary$Freq_2_Pct <- round(master_summary$Freq_2 / master_summary$Unique_Proteins * 100, 2)
master_summary$Freq_3_Pct <- round(master_summary$Freq_3 / master_summary$Unique_Proteins * 100, 2)
master_summary$Freq_4_Pct <- round(master_summary$Freq_4 / master_summary$Unique_Proteins * 100, 2)
master_summary$Freq_5plus_Pct <- round(master_summary$Freq_5plus / master_summary$Unique_Proteins * 100, 2)

# 打印汇总表
cat("\n汇总表:\n")
print(master_summary)

# ========== 5. 可视化 ==========
# 创建堆叠条形图数据
plot_data <- master_summary %>%
  select(Sheet, Freq_1, Freq_2, Freq_3, Freq_4, Freq_5plus) %>%
  tidyr::pivot_longer(cols = -Sheet, names_to = "Frequency", values_to = "Count")

plot_data$Frequency <- factor(plot_data$Frequency, 
                              levels = c("Freq_1", "Freq_2", "Freq_3", "Freq_4", "Freq_5plus"),
                              labels = c("1次", "2次", "3次", "4次", "≥5次"))

# 绘制堆叠条形图
p <- ggplot(plot_data, aes(x = Sheet, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a23", "#ff7f00", "#6a3d9a")) +
  labs(
    x = NULL,
    y = "蛋白数量",
    title = "各细胞类型中蛋白出现频率分布",
    subtitle = "A列蛋白在每条糖肽中出现的次数统计",
    fill = "出现次数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold")

print(p)

# 保存图形
ggsave("/Users/wangyixuan/Desktop/Protein_Frequency_Distribution.png", p, width = 10, height = 6, dpi = 300)

# ========== 6. 保存详细结果到Excel ==========
# 创建输出列表
output_sheets <- list()

# 添加汇总表
output_sheets[["Summary"]] <- master_summary

# 为每个sheet添加详细频率表
for (sheet_name in sheets) {
  result <- all_results[[sheet_name]]
  output_sheets[[paste0(sheet_name, "_详细频率")]] <- result$freq_table
}

# 保存到Excel文件
output_path <- "/Users/wangyixuan/Desktop/Protein_Frequency_Analysis.xlsx"
write_xlsx(output_sheets, output_path)
cat("\n详细结果已保存至:", output_path, "\n")

# ========== 7. 输出总结 ==========
cat("\n========================================\n")
cat("分析完成！\n")
cat("========================================\n")
cat("\n各sheet汇总:\n")
for (sheet_name in sheets) {
  result <- all_results[[sheet_name]]
  cat("\n", sheet_name, ":\n")
  cat("  总条目数:", result$total_entries, "\n")
  cat("  唯一蛋白数:", result$unique_proteins, "\n")
  cat("  出现1次的蛋白:", result$freq_summary$Count[1], 
      "(", result$freq_summary$Percentage[1], "%)\n")
  cat("  出现2次的蛋白:", result$freq_summary$Count[2], 
      "(", result$freq_summary$Percentage[2], "%)\n")
  cat("  出现3次的蛋白:", result$freq_summary$Count[3], 
      "(", result$freq_summary$Percentage[3], "%)\n")
  cat("  出现4次的蛋白:", result$freq_summary$Count[4], 
      "(", result$freq_summary$Percentage[4], "%)\n")
  cat("  出现>=5次的蛋白:", result$freq_summary$Count[5], 
      "(", result$freq_summary$Percentage[5], "%)\n")
}