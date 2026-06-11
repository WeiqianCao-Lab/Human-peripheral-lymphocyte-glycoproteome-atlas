install_packages<-function(){ 
  metr_pkgs<-c("WebGestaltR","corrplot","ggvenn","VennDiagram","purrr","reshape2","Biostrings",
               "data.table","UpSetR","DECIPHER","ggbreak",
               "openxlsx","ggrepel","seqinr","readr","tidyr","dplyr","stringr","ggseqlogo",
               "ggplot2","statnet","circlize","tibble","corrplot","pheatmap","psych",
               "ComplexHeatmap","RColorBrewer","circlize","org.Hs.eg.db","clusterProfiler",
               "readxl","limma","ggraph","igraph","tidyverse","RColorBrewer")
  list_installed<-installed.packages()
  new_pkgs<-subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  if(length(new_pkgs)!=0){
    #source("https://bioconductor.org/biocLite.R")
    options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
    BiocManager::install(new_pkgs, dependencies = TRUE, ask = FALSE)
    print(c(new_pkgs, " packages added..."))
  }
  if((length(new_pkgs)<1)){
    print("No new packages added...")
  }
  for(i in 1:length(metr_pkgs)){
    library(metr_pkgs[i],character.only = TRUE)
  }
}

install_packages()

#path<-readline()
#path2<-gsub("\\\\", "/", path)
setwd("/Users/wangyixuan/Desktop/")
#C:\Users\IBS\Documents\R\Serum_Atypical\Serum_Structure
#---------------------------------------------------------------------------------------
# 读取Excel文件中的主数据表（"Gps_heatmap_data_with_atypical-"）
data <- read.xlsx("Th-处理.xlsx", sheet = "Sheet1")

# 数据预处理：提取所需列并去重，类似原始代码中的data1
# 选择Protein、Site和Glycan(H,N,A,F,pH)列
data1 <- data %>%
  select(Protein, Site) %>%
  distinct()

# 提取UniProt accession号（从Protein列中提取，如"sp|P01024|CO3_HUMAN"中的"P01024"）
# 并将Site列转换为数值型
data1$Proteins <- str_split(data1$Protein, "\\|", simplify = T)[, 2]
data1$Prosite <- as.numeric(str_split(data1$Site, "\\;", simplify = T)[, 1])


# 人类蛋白质组FASTA文件准备
structure_fasta <- "SP__Homo+sapiens+(Human)+[9606]_20210225.fasta"

# 读取FASTA文件，获取蛋白质序列
fasta.data <- readAAStringSet(structure_fasta)

# 使用protr包的predictHEC函数预测蛋白质二级结构（H: α-Helix, C: Loop/Turn, E: β-Sheet）
structure.predict.result <- PredictHEC(fasta.data)

# 从FASTA文件名称中提取UniProt accession号（格式：>sp|P01024|CO3_HUMAN ...）
fasta.name <- names(fasta.data)
uniprot.accession <- str_split(fasta.name, "\\|", simplify = T)[, 2]

# 创建唯一蛋白质-位点数据框，用于二级结构标注（类似data2）
data2 <- distinct(data1, Proteins, Prosite)
data2$Structure <- ""  # 初始化Structure列，用于存储二级结构标签

# 为每个位点标注二级结构（H, C, E）
for (j in 1:nrow(data2)) {
  prot_index <- match(data2$Proteins[j], uniprot.accession)  # 查找蛋白质在FASTA中的索引
  if (!is.na(prot_index)) {  # 确保蛋白质匹配成功
    struct_seq <- structure.predict.result[prot_index]  # 获取该蛋白质的二级结构序列
    if (nchar(struct_seq) >= data2$Prosite[j]) {  # 确保位点编号在序列范围内
      data2$Structure[j] <- substr(struct_seq, data2$Prosite[j], data2$Prosite[j])  # 提取位点的二级结构
    }
  }
}

# 将二级结构标注合并回data1（如果需要进一步分析）
data3 <- left_join(data1, data2, by = c("Proteins", "Prosite"))

# 保存标注后的数据
write.xlsx(data3, "annotated_data_Glycoprotein.xlsx")

# 计算整个蛋白质组的二级结构统计
proteome.total.structure.site <- sum(width(fasta.data))  # 蛋白质组总残基数
proteome.total.structure.site.H <- sum(str_count(structure.predict.result, "H"))  # α-Helix残基数
proteome.total.structure.site.C <- sum(str_count(structure.predict.result, "C"))  # Loop/Turn残基数
proteome.total.structure.site.E <- sum(str_count(structure.predict.result, "E"))  # β-Sheet残基数

# 计算输入蛋白质（即数据集中涉及的蛋白质）的二级结构统计
unique_prots <- unique(data1$Proteins)  # 提取数据集中唯一的蛋白质
input_indices <- match(unique_prots, uniprot.accession)  # 查找这些蛋白质在FASTA中的索引
input_indices <- input_indices[!is.na(input_indices)]  # 移除未匹配的索引
structure.input.result <- structure.predict.result[input_indices]  # 获取输入蛋白质的二级结构序列

proteome.input.structure.site <- sum(str_length(structure.input.result))  # 输入蛋白质总残基数
proteome.input.structure.site.H <- sum(str_count(structure.input.result, "H"))  # 输入蛋白质中α-Helix残基数
proteome.input.structure.site.C <- sum(str_count(structure.input.result, "C"))  # 输入蛋白质中Loop/Turn残基数
proteome.input.structure.site.E <- sum(str_count(structure.input.result, "E"))  # 输入蛋白质中β-Sheet残基数

# 设置structure_protein_data为标注后的位点数据
structure_protein_data <- data2

# 计算二级结构比例和富集比
structure_result <- data.frame(
  Structure = c("α-Helix", "Loop/Turn", "β-Sheet"),  # 二级结构类型
  proportions = c(  # 计算糖基化位点中各二级结构的比例（百分比）
    sum(structure_protein_data$Structure == "H", na.rm = T) / sum(!is.na(structure_protein_data$Structure)) * 100,
    sum(structure_protein_data$Structure == "C", na.rm = T) / sum(!is.na(structure_protein_data$Structure)) * 100,
    sum(structure_protein_data$Structure == "E", na.rm = T) / sum(!is.na(structure_protein_data$Structure)) * 100
  ),
  EnrichmentRatio = c(  # 计算糖基化位点中二级结构的富集比
    (sum(structure_protein_data$Structure == "H", na.rm = T) / sum(!is.na(structure_protein_data$Structure))) / 
      (proteome.total.structure.site.H / proteome.total.structure.site),
    (sum(structure_protein_data$Structure == "C", na.rm = T) / sum(!is.na(structure_protein_data$Structure))) / 
      (proteome.total.structure.site.C / proteome.total.structure.site),
    (sum(structure_protein_data$Structure == "E", na.rm = T) / sum(!is.na(structure_protein_data$Structure))) / 
      (proteome.total.structure.site.E / proteome.total.structure.site)
  ),
  EnrichmentRatio2 = c(  # 计算输入蛋白质中二级结构的富集比
    (proteome.input.structure.site.H / proteome.input.structure.site) / 
      (proteome.total.structure.site.H / proteome.total.structure.site),
    (proteome.input.structure.site.C / proteome.input.structure.site) / 
      (proteome.total.structure.site.C / proteome.total.structure.site),
    (proteome.input.structure.site.E / proteome.input.structure.site) / 
      (proteome.total.structure.site.E / proteome.total.structure.site)
  )
)

# 打印结果
print(structure_result)

# 保存结果到Excel文件
write.xlsx(structure_result, "structure_proportions_enrichment_Glycoprotein.xlsx")




























