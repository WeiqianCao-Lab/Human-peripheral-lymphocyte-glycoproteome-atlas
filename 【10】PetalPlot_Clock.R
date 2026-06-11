# 加载必要的包
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

# 1. 设置保存路径 --------------------------------------------------------
desktop_path <- "/Users/wangyixuan/Desktop/"

# ============ 花瓣图参数 ============
petal_max_radius <- 0.9  # 修改为0.9
petal_angle_width <- 50
petal_gap_angle <- -25
petal_smoothness <- 100
petal_alpha <- 0.85
errorbar_width_deg <- 5

# ============ 三次重复数据（用于花瓣图）============
site_replicates <- data.frame(
  B = c("136", "136", "136", "136", "136", "136",
        "270", "270", "270", "270", "270", "270",
        "130", "130", "130", "130", "130", "130"),
  Group = c(rep("Aged", 3), rep("Young", 3),
            rep("Aged", 3), rep("Young", 3),
            rep("Aged", 3), rep("Young", 3)),
  Value = c(534524232.9, 515092944.2, 647916052.9,
            187166171.3, 251368332.8, 187822385.5,
            153956114.9, 173527223, 144128715.7,
            107602153.3, 97522445.99, 95520213.5,
            13948007.47, 12704967.07, 13926673.39,
            9112183.059, 5471909.918, 6568390.501)
)

# 计算花瓣图的均值和标准差（基于原始数据）
site_stats <- site_replicates %>%
  group_by(B, Group) %>%
  summarise(Mean = mean(Value), SD = sd(Value), .groups = 'drop')

# 位点中心角度
site_angles <- data.frame(
  B = c("130", "136", "270"),
  mid_angle = c(27.69, 138.46, 290.77),
  color = c("#4DAF4A", "#E41A1C", "#377EB8")
)

site_stats <- site_stats %>% left_join(site_angles, by = "B")

# 分离Aged和Young
aged_data <- site_stats %>% filter(Group == "Aged") %>% dplyr::select(B, Mean, SD, mid_angle, color)
young_data <- site_stats %>% filter(Group == "Young") %>% dplyr::select(B, Mean, SD, mid_angle, color)
colnames(aged_data)[2:3] <- c("Mean_Aged", "SD_Aged")
colnames(young_data)[2:3] <- c("Mean_Young", "SD_Young")

site_final <- aged_data %>% left_join(young_data, by = c("B", "mid_angle", "color"))

# log2转换和半径映射
# 均值进行log2转换
site_final$Mean_Aged_log2 <- log2(site_final$Mean_Aged)
site_final$Mean_Young_log2 <- log2(site_final$Mean_Young)

# 误差棒：基于原始数据的标准差，计算log2尺度上的误差范围
# 改进的方法：log2((Mean+SD)/Mean) 更精确
site_final$SD_Aged_log2 <- log2((site_final$Mean_Aged + site_final$SD_Aged) / site_final$Mean_Aged)
site_final$SD_Young_log2 <- log2((site_final$Mean_Young + site_final$SD_Young) / site_final$Mean_Young)

# 半径映射
max_val <- max(site_final$Mean_Aged_log2, site_final$Mean_Young_log2)
min_val <- min(site_final$Mean_Aged_log2, site_final$Mean_Young_log2)
min_radius <- 0.1
radius_scale <- (petal_max_radius - min_radius) / (max_val - min_val)
radius_offset <- min_radius - min_val * radius_scale

site_final$Aged_radius <- site_final$Mean_Aged_log2 * radius_scale + radius_offset
site_final$Young_radius <- site_final$Mean_Young_log2 * radius_scale + radius_offset
site_final$Aged_sd_radius <- abs(site_final$SD_Aged_log2 * radius_scale)
site_final$Young_sd_radius <- abs(site_final$SD_Young_log2 * radius_scale)

# 创建圆润花瓣函数
create_smooth_petal <- function(center_angle, radius, petal_angle_width, n_points) {
  half_angle_rad <- (petal_angle_width / 2) * pi / 180
  center_rad <- center_angle * pi / 180
  t <- seq(-half_angle_rad, half_angle_rad, length.out = n_points)
  angle_ratio <- abs(t) / half_angle_rad
  radial_factor <- cos(angle_ratio * pi / 2)
  radial_factor <- pmax(radial_factor, 0.05)
  current_radius <- radius * radial_factor
  point_angle <- center_rad + t
  x <- current_radius * cos(point_angle)
  y <- current_radius * sin(point_angle)
  x <- c(0, x, 0)
  y <- c(0, y, 0)
  return(data.frame(x = x, y = y))
}

# 生成花瓣
all_petals <- list()
for(i in 1:nrow(site_final)) {
  aged_center <- site_final$mid_angle[i] - (petal_angle_width/2 + petal_gap_angle/2)
  young_center <- site_final$mid_angle[i] + (petal_angle_width/2 + petal_gap_angle/2)
  
  aged_petal <- create_smooth_petal(aged_center, site_final$Aged_radius[i], petal_angle_width, petal_smoothness)
  young_petal <- create_smooth_petal(young_center, site_final$Young_radius[i], petal_angle_width, petal_smoothness)
  
  aged_petal$type <- "Aged"; young_petal$type <- "Young"
  aged_petal$group <- paste0(i, "_aged"); young_petal$group <- paste0(i, "_young")
  
  all_petals[[length(all_petals) + 1]] <- aged_petal
  all_petals[[length(all_petals) + 1]] <- young_petal
}
petals_df <- do.call(rbind, all_petals)

# 生成误差棒
errorbar_list <- list()
half_width_rad <- (errorbar_width_deg / 2) * pi / 180
for(i in 1:nrow(site_final)) {
  aged_center <- site_final$mid_angle[i] - (petal_angle_width/2 + petal_gap_angle/2)
  young_center <- site_final$mid_angle[i] + (petal_angle_width/2 + petal_gap_angle/2)
  
  aged_rad <- aged_center * pi / 180
  aged_lower_r <- max(0.02, site_final$Aged_radius[i] - site_final$Aged_sd_radius[i])
  aged_upper_r <- site_final$Aged_radius[i] + site_final$Aged_sd_radius[i]
  aged_left_angle <- aged_rad - half_width_rad; aged_right_angle <- aged_rad + half_width_rad
  
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(aged_lower_r * cos(aged_rad), aged_upper_r * cos(aged_rad)),
    y = c(aged_lower_r * sin(aged_rad), aged_upper_r * sin(aged_rad)),
    group = paste0(i, "_aged_line"), type = "Aged", element = "line"
  )
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(aged_upper_r * cos(aged_left_angle), aged_upper_r * cos(aged_right_angle)),
    y = c(aged_upper_r * sin(aged_left_angle), aged_upper_r * sin(aged_right_angle)),
    group = paste0(i, "_aged_top"), type = "Aged", element = "bar"
  )
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(aged_lower_r * cos(aged_left_angle), aged_lower_r * cos(aged_right_angle)),
    y = c(aged_lower_r * sin(aged_left_angle), aged_lower_r * sin(aged_right_angle)),
    group = paste0(i, "_aged_bottom"), type = "Aged", element = "bar"
  )
  
  young_rad <- young_center * pi / 180
  young_lower_r <- max(0.02, site_final$Young_radius[i] - site_final$Young_sd_radius[i])
  young_upper_r <- site_final$Young_radius[i] + site_final$Young_sd_radius[i]
  young_left_angle <- young_rad - half_width_rad; young_right_angle <- young_rad + half_width_rad
  
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(young_lower_r * cos(young_rad), young_upper_r * cos(young_rad)),
    y = c(young_lower_r * sin(young_rad), young_upper_r * sin(young_rad)),
    group = paste0(i, "_young_line"), type = "Young", element = "line"
  )
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(young_upper_r * cos(young_left_angle), young_upper_r * cos(young_right_angle)),
    y = c(young_upper_r * sin(young_left_angle), young_upper_r * sin(young_right_angle)),
    group = paste0(i, "_young_top"), type = "Young", element = "bar"
  )
  errorbar_list[[length(errorbar_list) + 1]] <- data.frame(
    x = c(young_lower_r * cos(young_left_angle), young_lower_r * cos(young_right_angle)),
    y = c(young_lower_r * sin(young_left_angle), young_lower_r * sin(young_right_angle)),
    group = paste0(i, "_young_bottom"), type = "Young", element = "bar"
  )
}
errorbars_df <- do.call(rbind, errorbar_list)

# 花瓣数值标签
petal_labels <- data.frame()
for(i in 1:nrow(site_final)) {
  aged_center <- site_final$mid_angle[i] - (petal_angle_width/2 + petal_gap_angle/2)
  young_center <- site_final$mid_angle[i] + (petal_angle_width/2 + petal_gap_angle/2)
  aged_rad <- aged_center * pi / 180; young_rad <- young_center * pi / 180
  aged_label_r <- site_final$Aged_radius[i] + site_final$Aged_sd_radius[i] + 0.04
  young_label_r <- site_final$Young_radius[i] + site_final$Young_sd_radius[i] + 0.04
  
  petal_labels <- rbind(petal_labels,
                        data.frame(x = aged_label_r * cos(aged_rad), y = aged_label_r * sin(aged_rad),
                                   label = paste0(round(site_final$Mean_Aged[i]/1e6, 1), "M"),
                                   type = "Aged", text_color = "#D73027"),
                        data.frame(x = young_label_r * cos(young_rad), y = young_label_r * sin(young_rad),
                                   label = paste0(round(site_final$Mean_Young[i]/1e6, 1), "M"),
                                   type = "Young", text_color = "#313695"))
}

# ==================================================
# 2. 环形热图数据 - 交换130位点的5200和6200顺序 ----------------
# 定义136位点糖型的正确顺序
glycan_order_136 <- c("54030", "76000", "92000", "82000", "72000", "62000")

# 定义130位点糖型的正确顺序（6200和5200交换位置）
# 原来的顺序是 ["52000", "62000"]，现在交换为 ["62000", "52000"]
glycan_order_130 <- c("62000", "52000")

# 完整的数据
data <- data.frame(
  A = rep("sp|P04233|HG2A_HUMAN", 13),
  B = c(136, 136, 136, 136, 136, 136, 130, 130, 270, 270, 270, 270, 270),
  C = c("[5 4 0 3 0]", "[7 6 0 0 0]", "[9 2 0 0 0]", "[8 2 0 0 0]", "[7 2 0 0 0]", "[6 2 0 0 0]",
        "[6 2 0 0 0]", "[5 2 0 0 0]",
        "[9 2 0 0 0]", "[8 2 0 0 0]", "[7 2 0 0 0]", "[6 2 0 0 0]", "[5 2 0 0 0]"),
  A1 = c(18312.27769, 17779.52905, 458606905.5, 44802999.51, 1542270.231, 29535965.78,
         12574342.62, 1373664.849,
         741321.9924, 37520503.98, 25600433.84, 87993255.33, 2100599.717),
  A2 = c(74169.53004, 16422.60108, 427878968, 51583240.11, 1920541.456, 33619602.52,
         11733667.93, 971299.1416,
         521222.0348, 40071328.07, 28087098.66, 104419067.3, 428506.8931),
  A3 = c(45465.53244, 21807.71302, 555430991.6, 49983471.62, 1637409.752, 40796906.64,
         12554659.86, 1372013.532,
         452956.4949, 24222581.48, 35988360.57, 82569265.44, 895551.7355),
  Y1 = c(294297.4983, 18334.92829, 163767866.5, 9595050.277, 86535.22001, 13404086.87,
         8678622.582, 433560.4773,
         173592.0286, 26183458.23, 22768387.35, 56538932.86, 1937782.819),
  Y2 = c(48068.33581, 19769.99708, 210070681.6, 19477164.24, 892385.0574, 20860263.52,
         5134579.74, 337330.1787,
         123963.4681, 14324086.4, 19741339.5, 62430074.75, 902981.8622),
  Y3 = c(172309.599, 18464.92903, 132908767.6, 11020030.48, 278284.8199, 43424528.06,
         6199583.47, 368807.0312,
         146717.3846, 16311379.33, 17259186.64, 60730128.3, 1072801.838)
)

# 数据预处理
plot_data <- data %>%
  mutate(B = as.character(B)) %>%
  mutate(Glycan_Simple = case_when(
    C == "[9 2 0 0 0]" ~ "92000",
    C == "[8 2 0 0 0]" ~ "82000",
    C == "[7 2 0 0 0]" ~ "72000",
    C == "[6 2 0 0 0]" ~ "62000",
    C == "[7 6 0 0 0]" ~ "76000",
    C == "[5 4 0 3 0]" ~ "54030",
    C == "[5 2 0 0 0]" ~ "52000",
    TRUE ~ "other"
  ))

# 分离并重新排列
data_130 <- plot_data %>% filter(B == "130")
data_136 <- plot_data %>% filter(B == "136")
data_270 <- plot_data %>% filter(B == "270")

# 按指定顺序排列130位点（交换6200和5200的位置）
data_130_ordered <- data_130[match(glycan_order_130, data_130$Glycan_Simple), ]

# 按指定顺序排列136位点
data_136_ordered <- data_136[match(glycan_order_136, data_136$Glycan_Simple), ]

# 合并并按顺序排列行（顺序：130 -> 136 -> 270）
plot_data <- bind_rows(data_130_ordered, data_136_ordered, data_270) %>%
  mutate(Peptide_ID = row_number(),
         Display_Label = case_when(
           B == "130" & Glycan_Simple == "62000" ~ "130_6200",
           B == "130" & Glycan_Simple == "52000" ~ "130_5200",
           B == "136" & Glycan_Simple == "54030" ~ "136_54030",
           B == "136" & Glycan_Simple == "76000" ~ "136_7600",
           B == "136" & Glycan_Simple == "92000" ~ "136_9200",
           B == "136" & Glycan_Simple == "82000" ~ "136_8200",
           B == "136" & Glycan_Simple == "72000" ~ "136_7200",
           B == "136" & Glycan_Simple == "62000" ~ "136_6200",
           B == "270" ~ paste0("270_", Glycan_Simple),
           TRUE ~ paste0(B, "_", Glycan_Simple)
         )) %>%
  dplyr::select(-Glycan_Simple)

# 转换成长格式
aged_data <- plot_data %>%
  dplyr::select(Peptide_ID, Display_Label, A1, A2, A3) %>%
  pivot_longer(cols = c(A1, A2, A3), names_to = "Repeat", values_to = "Expression") %>%
  mutate(Group = "Aged", Repeat_Num = as.numeric(gsub("A", "", Repeat)), Log2_Expr = log2(Expression + 1))

young_data <- plot_data %>%
  dplyr::select(Peptide_ID, Display_Label, Y1, Y2, Y3) %>%
  pivot_longer(cols = c(Y1, Y2, Y3), names_to = "Repeat", values_to = "Expression") %>%
  mutate(Group = "Young", Repeat_Num = as.numeric(gsub("Y", "", Repeat)), Log2_Expr = log2(Expression + 1))

all_data <- bind_rows(aged_data, young_data)

# 计算角度位置
n_peptides <- max(plot_data$Peptide_ID)
n_repeats <- 3
angle_per_peptide <- 360 / n_peptides
angle_per_repeat <- angle_per_peptide / n_repeats

all_data <- all_data %>%
  mutate(
    Start_Angle = (Peptide_ID - 1) * angle_per_peptide + (Repeat_Num - 1) * angle_per_repeat,
    End_Angle = Start_Angle + angle_per_repeat,
    Mid_Angle = (Start_Angle + End_Angle) / 2
  )

# 创建平滑扇形坐标函数
create_smooth_sector_coords <- function(start_angle, end_angle, r_inner, r_outer, n_points = 50) {
  angles <- seq(start_angle, end_angle, length.out = n_points)
  angles_rad <- angles * pi / 180
  x_outer <- r_outer * cos(angles_rad)
  y_outer <- r_outer * sin(angles_rad)
  angles_rev <- rev(angles)
  angles_rev_rad <- angles_rev * pi / 180
  x_inner <- r_inner * cos(angles_rev_rad)
  y_inner <- r_inner * sin(angles_rev_rad)
  return(data.frame(x = c(x_outer, x_inner), y = c(y_outer, y_inner)))
}

# 生成热图多边形
polygon_list <- list()
for (i in 1:nrow(all_data)) {
  row <- all_data[i, ]
  if (row$Group == "Aged") { r_inner <- 1.0; r_outer <- 1.2
  } else { r_inner <- 1.2; r_outer <- 1.4 }
  coords <- create_smooth_sector_coords(row$Start_Angle, row$End_Angle, r_inner, r_outer, n_points = 30)
  polygon_list[[i]] <- data.frame(x = coords$x, y = coords$y, Log2_Expr = row$Log2_Expr,
                                  Display_Label = row$Display_Label, Group = row$Group,
                                  Peptide_ID = row$Peptide_ID, Repeat_Num = row$Repeat_Num,
                                  Mid_Angle = row$Mid_Angle)
}
polygon_df <- do.call(rbind, polygon_list)

# 外层夹层
highlight_peptide <- plot_data %>% filter(Display_Label == "136_54030") %>% pull(Peptide_ID)
outer_layer_list <- list()
for (i in 1:nrow(plot_data)) {
  for (rep in 1:3) {
    start_angle <- (i - 1) * angle_per_peptide + (rep - 1) * angle_per_repeat
    end_angle <- start_angle + angle_per_repeat
    is_highlight <- (i == highlight_peptide)
    coords <- create_smooth_sector_coords(start_angle, end_angle, 1.4, 1.8, n_points = 30)
    outer_layer_list[[length(outer_layer_list) + 1]] <- data.frame(
      x = coords$x, y = coords$y, Peptide_ID = i, Repeat_Num = rep, Highlight = is_highlight)
  }
}
outer_layer_df <- do.call(rbind, outer_layer_list)

# 彩色边界
b_groups <- plot_data %>%
  group_by(B) %>%
  summarise(n_peptides = n(), .groups = 'drop') %>%
  mutate(ratio = n_peptides, angle = ratio / sum(ratio) * 360,
         start_angle = cumsum(lag(angle, default = 0)), end_angle = start_angle + angle,
         color = case_when(B == "136" ~ "#E41A1C", B == "270" ~ "#377EB8", B == "130" ~ "#4DAF4A"),
         label = paste0(B, " (", ratio, "个)"))

create_colored_arc <- function(start_angle, end_angle, radius, color, linewidth = 2) {
  theta <- seq(start_angle, end_angle, length.out = 200) * pi / 180
  return(data.frame(x = radius * cos(theta), y = radius * sin(theta), color = color))
}
colored_arcs_list <- list()
for(i in 1:nrow(b_groups)) {
  colored_arcs_list[[i]] <- create_colored_arc(b_groups$start_angle[i], b_groups$end_angle[i], 1.0, b_groups$color[i])
}
colored_arcs_df <- do.call(rbind, colored_arcs_list)

# 标签
peptide_labels <- plot_data %>%
  mutate(Mid_Angle = (Peptide_ID - 0.5) * angle_per_peptide) %>%
  dplyr::select(Peptide_ID, Display_Label, Mid_Angle)

# 计算表达量范围
expr_min <- min(polygon_df$Log2_Expr)
expr_max <- max(polygon_df$Log2_Expr)
expr_mid <- mean(polygon_df$Log2_Expr)

# 颜色梯度
custom_colors <- colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9",
                                    "#FFFFBF", 
                                    "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"))(100)

# 最终图形 - 调整中心标签位置
p_final <- ggplot() +
  geom_polygon(data = polygon_df, aes(x = x, y = y, fill = Log2_Expr, group = interaction(Peptide_ID, Repeat_Num, Group)), color = NA) +
  geom_polygon(data = outer_layer_df %>% filter(!Highlight), aes(x = x, y = y, group = interaction(Peptide_ID, Repeat_Num)), fill = "#A1C2B1", alpha = 0.5, color = NA) +
  geom_polygon(data = outer_layer_df %>% filter(Highlight), aes(x = x, y = y, group = interaction(Peptide_ID, Repeat_Num)), fill = "#CBAF98", alpha = 0.5, color = NA) +
  geom_path(data = colored_arcs_df, aes(x = x, y = y, color = color), linewidth = 2) +
  
  # 花瓣图
  geom_polygon(data = petals_df %>% filter(type == "Aged"), aes(x = x, y = y, group = group), fill = "#D73027", alpha = petal_alpha, color = "black", size = 0.3) +
  geom_polygon(data = petals_df %>% filter(type == "Young"), aes(x = x, y = y, group = group), fill = "#313695", alpha = petal_alpha, color = "black", size = 0.3) +
  
  # 误差棒
  geom_line(data = errorbars_df %>% filter(element == "line"), aes(x = x, y = y, group = group), color = "black", linewidth = 0.5) +
  geom_line(data = errorbars_df %>% filter(element == "bar"), aes(x = x, y = y, group = group), color = "black", linewidth = 0.5) +
  
  geom_text(data = petal_labels, aes(x = x, y = y, label = label), color = petal_labels$text_color, size = 3, fontface = "bold") +
  
  scale_fill_gradientn(colors = custom_colors, 
                       name = expression(Log[2] ~ "(Expression)"),
                       breaks = c(expr_min, expr_mid, expr_max),
                       labels = c(round(expr_min, 1), round(expr_mid, 1), round(expr_max, 1)),
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                              barwidth = unit(1.2, "cm"), barheight = unit(8, "cm"),
                                              ticks.colour = "black", ticks.linewidth = 0.5,
                                              frame.colour = "black", frame.linewidth = 0.5)) +
  scale_color_identity() +
  geom_text(data = b_groups %>% mutate(mid_angle = start_angle + angle/2, x = 0.85 * cos(mid_angle * pi / 180), y = 0.85 * sin(mid_angle * pi / 180)),
            aes(x = x, y = y, label = label), size = 3, fontface = "bold", color = "#333333") +
  geom_text(data = peptide_labels, aes(x = 1.95 * cos(Mid_Angle * pi / 180), y = 1.95 * sin(Mid_Angle * pi / 180),
                                       label = Display_Label, angle = ifelse(Mid_Angle > 90 & Mid_Angle < 270, Mid_Angle + 180, Mid_Angle)),
            size = 3.2, fontface = "bold", color = "#333333", hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 0, y = -0.15, label = "Aged", size = 7, fontface = "bold", color = "#D73027", alpha = 0.85) +  # 调整位置
  annotate("text", x = 0, y = 0.25, label = "Young", size = 7, fontface = "bold", color = "#313695", alpha = 0.85) +   # 调整位置和大小
  annotate("text", x = 0, y = 1.62, label = "Outer Layer", size = 6, fontface = "bold", color = "#666666", alpha = 0.7) +
  geom_text(data = all_data %>% filter(Group == "Aged") %>% group_by(Peptide_ID, Repeat_Num, Mid_Angle) %>% summarise(Log2_Expr = mean(Log2_Expr), .groups = 'drop'),
            aes(x = 1.07 * cos(Mid_Angle * pi / 180), y = 1.07 * sin(Mid_Angle * pi / 180), label = paste0("R", Repeat_Num)),
            size = 2.0, color = "white", alpha = 0.7, fontface = "bold") +
  geom_text(data = all_data %>% filter(Group == "Young") %>% group_by(Peptide_ID, Repeat_Num, Mid_Angle) %>% summarise(Log2_Expr = mean(Log2_Expr), .groups = 'drop'),
            aes(x = 1.27 * cos(Mid_Angle * pi / 180), y = 1.27 * sin(Mid_Angle * pi / 180), label = paste0("R", Repeat_Num)),
            size = 2.0, color = "white", alpha = 0.7, fontface = "bold") +
  coord_fixed() + theme_void() +
  theme(legend.position = c(0.88, 0.5), legend.title = element_text(size = 11, face = "bold", color = "#222222", margin = margin(b = 8)),
        legend.text = element_text(size = 9, color = "#444444"),
        legend.background = element_rect(fill = "white", color = "#CCCCCC", size = 0.3),
        legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
        plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(t = 20, r = 25, b = 20, l = 20))

print(p_final)

# 保存
pdf(paste0(desktop_path, "Circular_Heatmap_Swapped_130.pdf"), width = 12, height = 11, bg = "white")
print(p_final)
dev.off()

ggsave(paste0(desktop_path, "Circular_Heatmap_Swapped_130.png"), p_final, width = 12, height = 11, dpi = 600, bg = "white")

# 打印顺序确认
cat("\n")
cat("================================================================================\n")
cat("130位点糖型显示顺序（从0度开始顺时针）:\n")
ordered_labels_130 <- plot_data %>% filter(B == "130") %>% pull(Display_Label)
for(i in 1:length(ordered_labels_130)) {
  cat(paste0("  ", i, ". ", ordered_labels_130[i], "\n"))
}
cat("\n136位点糖型显示顺序:\n")
ordered_labels_136 <- plot_data %>% filter(B == "136") %>% pull(Display_Label)
for(i in 1:length(ordered_labels_136)) {
  cat(paste0("  ", i, ". ", ordered_labels_136[i], "\n"))
}
cat("================================================================================\n")
cat(paste0("✓ PDF已保存: ", desktop_path, "Circular_Heatmap_Swapped_130.pdf\n"))
cat(paste0("✓ PNG已保存: ", desktop_path, "Circular_Heatmap_Swapped_130.png\n"))
cat("================================================================================\n")