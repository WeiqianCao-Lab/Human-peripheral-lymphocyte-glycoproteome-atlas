# 加载必要的包
library(readxl)
library(writexl)
library(stringr)

# 设置文件路径
file_path <- '/Users/wangyixuan/Desktop/数据处理/Fig2B-Glycosite相关/蛋白&糖蛋白交集与序列.xlsx'

# 读取Excel文件，不自动转换列名，以保留原始列名
data <- read_excel(file_path, col_names = FALSE)

# 检查数据维度
cat("数据维度:", dim(data), "\n")
cat("前几行数据预览:\n")
print(head(data))

# 提取A列和D列数据（列索引从1开始）
# 使用第一行作为列标题
protein_names <- data[[1]]  # A列
protein_sequences <- data[[4]]  # D列

# 初始化结果向量
counts <- integer(length(protein_sequences))
positions_list <- character(length(protein_sequences))

# 修改正则表达式模式：N后面跟着不是P的任意字符，然后是S或T或C
pattern <- "N[^P][STC]"

# 遍历所有行，包括标题行
for(i in 1:length(protein_sequences)) {
  seq <- as.character(protein_sequences[i])
  
  # 处理标题行
  if(i == 1) {
    counts[i] <- NA
    positions_list[i] <- "N-位置"
    next
  }
  
  if(is.na(seq) || seq == "") {
    counts[i] <- 0
    positions_list[i] <- ""
    next
  }
  
  # 将序列转换为大写以确保匹配
  seq_upper <- toupper(seq)
  
  # 查找所有匹配的位置
  # 使用gregexpr获取所有匹配的起始位置
  matches <- gregexpr(pattern, seq_upper, perl = TRUE)
  match_positions <- as.numeric(matches[[1]])
  
  # 过滤掉-1（无匹配的情况）
  if(length(match_positions) == 1 && match_positions[1] == -1) {
    counts[i] <- 0
    positions_list[i] <- ""
  } else {
    counts[i] <- length(match_positions)
    
    # 生成位置字符串
    if(counts[i] > 0) {
      pos_str <- paste0("N-", match_positions, collapse = "; ")
      positions_list[i] <- pos_str
    } else {
      positions_list[i] <- ""
    }
  }
}

# 创建结果数据框
result_df <- data.frame(
  `蛋白名称` = protein_names,
  `蛋白序列` = protein_sequences,
  `NXS/NXT/NXC数量` = counts,  # 修改列名以反映新的匹配模式
  `N-位置` = positions_list,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 设置列名
colnames(result_df) <- c("蛋白名称", "蛋白序列", "NXS/NXT/NXC数量", "N-位置")

# 输出结果到新的Excel文件
output_path <- '/Users/wangyixuan/Desktop/蛋白_NXS_NXT_NXC_分析结果.xlsx'  # 修改文件名
write_xlsx(result_df, output_path)

cat("分析完成！结果已保存到:", output_path, "\n")

# 显示分析统计信息
total_proteins <- length(protein_sequences) - 1  # 减去标题行
matched_proteins <- sum(counts > 0, na.rm = TRUE)
total_matches <- sum(counts, na.rm = TRUE)

cat("\n=== 分析统计 ===\n")
cat(sprintf("总蛋白数: %d\n", total_proteins))
cat(sprintf("包含NXS/NXT/NXC的蛋白数: %d\n", matched_proteins))
cat(sprintf("总匹配数: %d\n", total_matches))

# 显示前几行结果用于验证
cat("\n=== 结果预览（前10行）===\n")
print(head(result_df, 10))

# 特别检查P01042|KNG1_HUMAN这个蛋白
cat("\n=== 检查P01042|KNG1_HUMAN ===")
kng1_index <- which(grepl("P01042|KNG1_HUMAN", protein_names, ignore.case = TRUE))
if(length(kng1_index) > 0) {
  for(idx in kng1_index) {
    cat(sprintf("\n在第 %d 行找到P01042|KNG1_HUMAN\n", idx))
    cat(sprintf("蛋白名称: %s\n", protein_names[idx]))
    cat(sprintf("序列长度: %d\n", nchar(protein_sequences[idx])))
    cat(sprintf("NXS/NXT/NXC数量: %d\n", counts[idx]))
    cat(sprintf("N-位置: %s\n", positions_list[idx]))
    
    # 显示部分序列和匹配位置
    if(counts[idx] > 0) {
      seq <- protein_sequences[idx]
      matches <- gregexpr(pattern, toupper(seq), perl = TRUE)
      match_positions <- as.numeric(matches[[1]])
      cat("匹配的序列片段: ")
      for(pos in match_positions) {
        if(pos > 0) {
          fragment <- substr(seq, pos, min(pos+2, nchar(seq)))
          cat(sprintf("位置%d: %s ", pos, fragment))
        }
      }
      cat("\n")
    }
  }
} else {
  cat("\n未找到P01042|KNG1_HUMAN，请检查蛋白名称是否正确\n")
}

# 显示更多验证示例（更新测试用例以包含C）
cat("\n=== 更多验证示例 ===\n")
test_sequences <- list(
  "测试1" = "MANLSTNPTNSSK",
  "测试2" = "MNNSTNPTNCS",  # 添加包含C的测试
  "测试3" = "ABCDEFG",
  "测试4" = "ANLSMNTSK",
  "测试5" = "ANPSMNTS",     # 包含P，不应该匹配
  "测试6" = "NATNSSNVST",
  "测试7" = "NACNSC",       # 专门测试C
  "测试8" = "NLCNTC",       # 测试各种组合
  "测试9" = "NNCNPC"        # 包含NPC，不应该匹配
)

for(test_name in names(test_sequences)) {
  seq <- test_sequences[[test_name]]
  matches <- gregexpr(pattern, toupper(seq), perl = TRUE)
  match_positions <- as.numeric(matches[[1]])
  
  if(length(match_positions) == 1 && match_positions[1] == -1) {
    match_count <- 0
    pos_str <- ""
  } else {
    match_count <- length(match_positions)
    pos_str <- paste0("N-", match_positions, collapse = "; ")
  }
  
  cat(sprintf("\n%s: %s\n", test_name, seq))
  cat(sprintf("匹配数: %d\n", match_count))
  cat(sprintf("位置: %s\n", pos_str))
  
  # 显示匹配的具体内容
  if(match_count > 0) {
    cat("匹配片段: ")
    for(pos in match_positions) {
      if(pos > 0) {
        fragment <- substr(seq, pos, min(pos+2, nchar(seq)))
        cat(sprintf("%s(位置%d) ", fragment, pos))
      }
    }
    cat("\n")
  }
}

# 显示一些统计信息
cat("\n=== 修改说明 ===")
cat("\n正则表达式模式已从 'N[^P][ST]' 修改为 'N[^P][STC]'")
cat("\n现在匹配: N + (不是P的任意字符) + (S或T或C)")
cat("\n即匹配模式: NXS, NXT, NXC (其中X不能是P)")