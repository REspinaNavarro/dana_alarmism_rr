# ==== MDS FIGURES =====
# Alarmism Detection in Disaster News Headlines

# 1. PACKAGES

packages <- c(
  "readxl",
  "tidyverse",
  "proxy",
  "ggrepel"
)

new_packages <- packages[
  !(packages %in% installed.packages()[,"Package"])
]

if(length(new_packages)){
  install.packages(new_packages)
}

library(readxl)
library(tidyverse)
library(proxy)
library(ggrepel)


# 2. DATA
df_raw <- read_excel(
  "data/AI_classification_train.xlsx"
)


# 3. HELPER FUNCTIONS

# MDS
run_mds <- function(df){
  
  distance_matrix <- proxy::dist(
    t(df),
    method = "Jaccard"
  )
  
  cmdscale(
    distance_matrix,
    k = 2,
    eig = TRUE,
    add = TRUE
  )
}

# Get variance from MDS
get_variance <- function(mds_fit){
  
  eig <- mds_fit$eig[
    mds_fit$eig > 0
  ]
  
  round(
    eig / sum(eig) * 100,
    1
  )
}

# Get coordinates for variables
get_coordinates <- function(mds_fit){
  
  as.data.frame(
    mds_fit$points
  ) %>%
    tibble::rownames_to_column(
      "Variable"
    ) %>%
    rename(
      Dim1 = V1,
      Dim2 = V2
    )
}


# Figure export
save_figure <- function(plot, file_name){
  
  ggsave(
    filename = paste0("figures/", file_name, ".svg"),
    plot = plot,
    width = 10,
    height = 8,
    dpi = 600
  )
  
}

# 4. PLOT SETTINGS

# Color paletts
pal_llms <- c(
  ChatGPT = "#D95F02",
  Gemini  = "#1B9E77",
  Claude  = "#7570B3"
)

pal_methods <- c(
  LLM        = "#D95F02",
  Dictionary = "#2C7FB8"
)

pal_samples <- c(
  Training   = "#2C7FB8",
  Validation = "#D95F02"
)

# Shapes

shape_alarmism <- c(
  "Global alarmism"     = 17,
  "Rhetorical resource" = 16
)

shape_method <- c(
  "Alarmism" = 17,
  "Resource" = 16
)

# Theme

theme_alarmism <- function(){
  
  theme_minimal(base_size = 13) +
    
    theme(
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major = element_line(
        colour = "grey90",
        linewidth = 0.3
      ),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 16
      ),
      
      plot.subtitle = element_text(
        colour = "grey40",
        hjust = 0.5,
        size = 12
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      ),
      
      plot.margin = margin(
        15, 30, 15, 30
      )
    )
}

# Labels
label_layer <- geom_text_repel(
  size = 4,
  fontface = "bold",
  box.padding = 0.8,
  point.padding = 0.6,
  max.overlaps = Inf,
  segment.color = NA,
  show.legend = FALSE,
  seed = 123
)


# FIGURE 1
# INTER-MODEL FRAGMENTATION

# Data
df_llm_panel <- df_raw %>%
  select(
    ChatGPT_Alarm,
    ChatGPT_V1,
    ChatGPT_V2,
    ChatGPT_V3,
    ChatGPT_V4,
    Gemini_Alarm,
    Gemini_V1,
    Gemini_V2,
    Gemini_V3,
    Gemini_V4,
    Claude_Alarm,
    Claude_V1,
    Claude_V2,
    Claude_V3,
    Claude_V4
  )

# MDS
mds_fit <- run_mds(df_llm_panel)
gof <- round(
  mds_fit$GOF[1],
  2
)

variance <- get_variance(mds_fit)


percent_dim1 <- variance[1]
percent_dim2 <- variance[2]

# Coordinates
mds_results <- get_coordinates(mds_fit) %>%
  mutate(
    LLM = case_when(
      str_detect(Variable, "^ChatGPT_") ~ "ChatGPT",
      str_detect(Variable, "^Gemini_")  ~ "Gemini",
      str_detect(Variable, "^Claude_")  ~ "Claude"
    ),
    
    Type = if_else(
      str_detect(Variable, "_Alarm$"),
      "Global alarmism",
      "Rhetorical resource"
    )
  )

# Alarmism nodes
references <- mds_results %>%
  filter(Type == "Global alarmism") %>%
  select(
    LLM,
    x_start = Dim1,
    y_start = Dim2
  )

# Connections
segments <- mds_results %>%
  filter(Type == "Rhetorical resource") %>%
  left_join(
    references,
    by = "LLM"
  )

# Figure 1
figure1 <- ggplot(
  mds_results,
  aes(
    x = Dim1,
    y = Dim2,
    label = Variable,
    color = LLM,
    shape = Type
  )
) +
  
  geom_segment(
    data = segments,
    aes(
      x = x_start,
      y = y_start,
      xend = Dim1,
      yend = Dim2,
      color = LLM
    ),
    inherit.aes = FALSE,
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.65,
    show.legend = FALSE
  ) +
  
  geom_point(size = 5) +
  
  label_layer +
  
  coord_equal(
    clip = "off"
  ) +
  
  scale_color_manual(
    values = pal_llms
  ) +
  
  scale_shape_manual(
    values = shape_alarmism
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  labs(
    title = "Global Alarmism and Rhetoric Resources",
    subtitle = paste0(
      "Classical MDS based on Jaccard distances (GOF = ",
      round(gof, 2),
      ")"
    ),
    x = paste0(
      "Dimension 1 (",
      percent_dim1,
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      percent_dim2,
      "%)"
    ),
    color = "Language Model",
    shape = "Variable Type"
  ) +
  
  theme_alarmism()

#figure1

# FIGURE 2
# HOLISTIC VS STRUCTURED ALARMISM

# Data
df_consensus <- df_raw %>%
  select(
    LLM_Alarm,
    LLM_V1,
    LLM_V2,
    LLM_V3,
    LLM_V4,
    LLM_Alarm_rr
  )

# MDS
mds_fit <- run_mds(df_consensus)

gof <- round(
  mds_fit$GOF[1],
  3
)

variance <- get_variance(mds_fit)

percent_dim1 <- variance[1]
percent_dim2 <- variance[2]

# Coordinates
mds_results <- get_coordinates(mds_fit) %>%
  mutate(
    Type = case_when(
      Variable %in% c(
        "LLM_Alarm",
        "LLM_Alarm_rr"
      ) ~ "Alarmism",
      
      TRUE ~ "Rhetorical resource"
    )
  )

# Connections from structured alarmism
center <- mds_results %>%
  filter(
    Variable == "LLM_Alarm_rr"
  ) %>%
  select(
    x_start = Dim1,
    y_start = Dim2
  )

segments <- mds_results %>%
  filter(
    Variable %in% c(
      "LLM_V1",
      "LLM_V2",
      "LLM_V3",
      "LLM_V4"
    )
  ) %>%
  mutate(
    x_start = center$x_start,
    y_start = center$y_start
  )

# Figure
figure2 <- ggplot(
  mds_results,
  aes(
    x = Dim1,
    y = Dim2,
    label = Variable,
    color = Type,
    shape = Type
  )
) +
  
  geom_segment(
    data = segments,
    aes(
      x = x_start,
      y = y_start,
      xend = Dim1,
      yend = Dim2
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  geom_point(size = 5) +
  
  label_layer +
  
  coord_equal(
    clip = "off"
  ) +
  
  scale_color_manual(
    values = c(
      "Alarmism" = "#D95F02",
      "Rhetorical resource" = "#2C7FB8"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Alarmism" = 17,
      "Rhetorical resource" = 16
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  labs(
    title = "Holistic and Structured Alarmism",
    subtitle = paste0(
      "Classical MDS based on Jaccard distances (GOF = ",
      gof,
      ")"
    ),
    x = paste0(
      "Dimension 1 (",
      percent_dim1,
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      percent_dim2,
      "%)"
    ),
    color = "Variable Type",
    shape = "Variable Type"
  ) +
  
  theme_alarmism()

#figure2


# FIGURE 3 - LLM Consensus and Refined Dictionary

# Data
df_llm_vs_dictionary <- df_raw %>%
  select(
    LLM_V1,
    LLM_V2,
    LLM_V3,
    LLM_V4,
    LLM_Alarm_rr,
    Dicc_ref_V1,
    Dicc_ref_V2,
    Dicc_ref_V3,
    Dicc_ref_V4,
    Dicc_ref_Alarm_rr
  )

# MDS
mds_fit <- run_mds(
  df_llm_vs_dictionary
)

gof <- round(
  mds_fit$GOF[1],
  3
)

variance <- get_variance(
  mds_fit
)

percent_dim1 <- variance[1]
percent_dim2 <- variance[2]

# Coordinates
mds_results <- get_coordinates(
  mds_fit
) %>%
  mutate(
    Method = case_when(
      str_detect(
        Variable,
        "^LLM_"
      ) ~ "LLM",
      
      str_detect(
        Variable,
        "^Dicc_ref_"
      ) ~ "Dictionary"
    ),
    
    Type = case_when(
      
      Variable %in% c(
        "LLM_Alarm_rr",
        "Dicc_ref_Alarm_rr"
      ) ~ "Center",
      
      TRUE ~ "Rhetorical resource"
    )
  )

# Centers
centers <- mds_results %>%
  filter(
    Type == "Center"
  ) %>%
  select(
    Method,
    x_start = Dim1,
    y_start = Dim2
  )

# Connect resources with the corresponding center
segments <- mds_results %>%
  filter(
    Type == "Rhetorical resource"
  ) %>%
  left_join(
    centers,
    by = "Method"
  )

# Figure 
figure3 <- ggplot(
  mds_results,
  aes(
    x = Dim1,
    y = Dim2,
    label = Variable,
    color = Method,
    shape = Type
  )
) +
  
  geom_segment(
    data = segments,
    aes(
      x = x_start,
      y = y_start,
      xend = Dim1,
      yend = Dim2,
      color = Method
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.7,
    show.legend = FALSE
  ) +
  
  geom_point(size = 5) +
  
  geom_text_repel(
    size = 4,
    fontface = "bold",
    box.padding = 0.8,
    point.padding = 0.6,
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.color = "grey60",
    show.legend = FALSE,
    seed = 123
  ) +
  
  coord_equal(
    clip = "off"
  ) +
  
  scale_color_manual(
    values = c(
      "LLM" = "#D95F02",
      "Dictionary" = "#2C7FB8"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Center" = 17,
      "Rhetorical resource" = 16
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  labs(
    title = "LLM Consensus and Refined Dictionary",
    subtitle = paste0(
      "Classical MDS based on Jaccard distances (GOF = ",
      gof,
      ")"
    ),
    x = paste0(
      "Dimension 1 (",
      percent_dim1,
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      percent_dim2,
      "%)"
    ),
    color = "Method",
    shape = "Variable Type"
  ) +
  
  theme_alarmism()

#figure3


# FIGURE 4
# SPATIAL CONFIGURATION OF THE REFINED DICTIONARY

# Data
df_dictionary <- df_raw %>%
  select(
    Dicc_ref_V1,
    Dicc_ref_V2,
    Dicc_ref_V3,
    Dicc_ref_V4,
    Dicc_ref_Alarm_rr
  )

# MDS
mds_fit <- run_mds(
  df_dictionary
)

gof <- round(
  mds_fit$GOF[1],
  3
)

variance <- get_variance(
  mds_fit
)

percent_dim1 <- variance[1]
percent_dim2 <- variance[2]

# Coordinates
mds_results <- get_coordinates(
  mds_fit
) %>%
  mutate(
    Type = if_else(
      Variable == "Dicc_ref_Alarm_rr",
      "Alarmism",
      "Rhetorical resource"
    )
  )

# Alarmism centroid
center <- mds_results %>%
  filter(
    Variable == "Dicc_ref_Alarm_rr"
  ) %>%
  select(
    x_start = Dim1,
    y_start = Dim2
  )

# Connections
segments <- mds_results %>%
  filter(
    Variable %in% c(
      "Dicc_ref_V1",
      "Dicc_ref_V2",
      "Dicc_ref_V3",
      "Dicc_ref_V4"
    )
  ) %>%
  mutate(
    x_start = center$x_start,
    y_start = center$y_start
  )

# Figure
figure4 <- ggplot(
  mds_results,
  aes(
    x = Dim1,
    y = Dim2,
    label = Variable,
    color = Type,
    shape = Type
  )
) +
  
  geom_segment(
    data = segments,
    aes(
      x = x_start,
      y = y_start,
      xend = Dim1,
      yend = Dim2
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    colour = "grey60"
  ) +
  
  geom_point(size = 5) +
  
  label_layer +
  
  coord_equal(
    clip = "off"
  ) +
  
  scale_color_manual(
    values = c(
      "Alarmism" = "#D95F02",
      "Rhetorical resource" = "#2C7FB8"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Alarmism" = 17,
      "Rhetorical resource" = 16
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = 0.25)
  ) +
  
  labs(
    title = "Spatial Configuration of the Refined Dictionary",
    subtitle = paste0(
      "Classical MDS based on Jaccard distances (GOF = ",
      gof,
      ")"
    ),
    x = paste0(
      "Dimension 1 (",
      percent_dim1,
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      percent_dim2,
      "%)"
    ),
    color = "Variable Type",
    shape = "Variable Type"
  ) +
  
  theme_alarmism()

#figure4



# Export figures
save_figure(figure1, "figure_1")
save_figure(figure2, "figure_2")
save_figure(figure3, "figure_3")
save_figure(figure4, "figure_4")

print('Script runs successfuly')

