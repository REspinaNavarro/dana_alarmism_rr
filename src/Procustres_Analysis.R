# PROCRUSTES VALIDATION
# Training Sample vs Blind Validation Sample


# 1. Packages setup


required_packages <- c(
  "readxl",
  "tidyverse",
  "proxy",
  "vegan",
  "ggrepel",
  "svglite"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

library(readxl)
library(tidyverse)
library(proxy)
library(vegan)
library(ggrepel)


# 2. Data

df_sample <- read_excel(
  "data/AI_sample_corpus.xlsx"
)

df_blind <- read_excel(
  "data/AI_blind_corpus.xlsx"
)

# 3. Helper Functions

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
  "Sample Corpus" = "#2C7FB8",
  "Blind Corpus"  = "#D95F02"
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


# FIGURE 5
# Transferability Across Sample and Blind Corpus


# Sample corpus

df_train_mds <- df_sample %>%
  select(
    Dicc_ref_V1,
    Dicc_ref_V2,
    Dicc_ref_V3,
    Dicc_ref_V4,
    Dicc_ref_Alarm_rr
  )


# Blind corpus

df_valid_mds <- df_blind %>%
  select(
    V1_dicc_ref_resto,
    V2_dicc_ref_resto,
    V3_dicc_ref_resto,
    V4_dicc_ref_resto,
    Alarm_rr_dicc_ref_resto
  )


# Jaccard distances

dist_train <- proxy::dist(
  t(df_train_mds),
  method = "Jaccard"
)

dist_valid <- proxy::dist(
  t(df_valid_mds),
  method = "Jaccard"
)

# MDS

mds_train <- cmdscale(
  dist_train,
  k = 2,
  eig = TRUE,
  add = TRUE
)

mds_valid <- cmdscale(
  dist_valid,
  k = 2,
  eig = TRUE,
  add = TRUE
)

gof_train <- round(
  mds_train$GOF[1],
  3
)

gof_valid <- round(
  mds_valid$GOF[1],
  3
)


# Procrustes

proc_fit <- vegan::procrustes(
  X = mds_train$points,
  Y = mds_valid$points,
  scale = FALSE,
  symmetric = FALSE
)

coords_train <- as.data.frame(
  proc_fit$X
)

colnames(coords_train) <- c(
  "Dim1",
  "Dim2"
)

coords_train$Variable <- rownames(
  proc_fit$X
)

coords_train <- coords_train %>%
  relocate(Variable) %>%
  mutate(
    Sample = "Sample Corpus"
  )


coords_valid <- as.data.frame(
  proc_fit$Yrot
)

colnames(coords_valid) <- c(
  "Dim1",
  "Dim2"
)

coords_valid$Variable <- rownames(
  proc_fit$Yrot
)

coords_valid <- coords_valid %>%
  relocate(Variable) %>%
  mutate(
    Sample = "Blind Corpus"
  )

figure5_data <- bind_rows(
  coords_train,
  coords_valid
)

# Labels
figure5_data <- figure5_data %>%
  mutate(
    Resource = case_when(
      grepl("V1", Variable)    ~ "V1",
      grepl("V2", Variable)    ~ "V2",
      grepl("V3", Variable)    ~ "V3",
      grepl("V4", Variable)    ~ "V4",
      grepl("Alarm", Variable) ~ "Alarm_RR",
      TRUE                     ~ NA_character_
    ),

    Label = paste0(
      Resource,
      "_",
      if_else(
        Sample == "Sample Corpus",
        "sample",
        "blind"
      )
    ),

    Type = if_else(
      Resource == "Alarm_RR",
      "Alarmism",
      "Rhetorical resource"
    )
  )


# Centroid

centers <- figure5_data %>%
  filter(
    grepl("Alarm", Variable)
  ) %>%
  select(
    Sample,
    x_start = Dim1,
    y_start = Dim2
  )


# Segments
segments <- figure5_data %>%
  filter(
    !grepl("Alarm", Variable)
  ) %>%
  left_join(
    centers,
    by = "Sample"
  )

# Figure
figure5 <- ggplot(
  figure5_data,
  aes(
    x = Dim1,
    y = Dim2,
    color = Sample,
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
      color = Sample
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    alpha = 0.7,
    linetype = "dashed"
  ) +

  geom_point(size = 5) +

  geom_text_repel(
    aes(label = Label),
    size = 4,
    fontface = "bold",
    segment.color = "grey60",
    show.legend = FALSE
  ) +

  coord_equal(
    clip = "off"
  ) +

  scale_color_manual(
    values = pal_samples,
    labels = c(
      "Sample Corpus" = "Sample Corpus (n = 200)",
      "Blind Corpus"  = "Blind Corpus (n = 949)"
    )
  ) +

  scale_shape_manual(
    values = c(
      "Alarmism"           = 17,
      "Rhetorical resource" = 16
    )
  ) +

  labs(
    title    = "Transferability Across Sample and Blind Corpus",
    subtitle = paste0(
      "Procrustes-aligned MDS configurations | GOFsample = ",
      gof_train,
      " | GOFblind = ",
      gof_valid
    ),
    x     = "Aligned Dimension 1",
    y     = "Aligned Dimension 2",
    color = "Sample",
    shape = "Variable Type"
  ) +

  theme_alarmism()

#figure5
save_figure(figure5, "figure5")

# TABLE 3: PREVALENCE COMPARISON

# Sample corpus
sample_data <- df_sample %>%
  transmute(
    V1       = Dicc_ref_V1,
    V2       = Dicc_ref_V2,
    V3       = Dicc_ref_V3,
    V4       = Dicc_ref_V4,
    Alarm_rr = Dicc_ref_Alarm_rr
  )

# Blind corpus
blind_data <- df_blind %>%
  transmute(
    V1       = V1_dicc_ref_resto,
    V2       = V2_dicc_ref_resto,
    V3       = V3_dicc_ref_resto,
    V4       = V4_dicc_ref_resto,
    Alarm_rr = Alarm_rr_dicc_ref_resto
  )

variables <- c(
  "V1",
  "V2",
  "V3",
  "V4",
  "Alarm_rr"
)

table3 <- purrr::map_dfr(
  variables,
  function(v){

    x_sample <- sum(
      sample_data[[v]] == 1,
      na.rm = TRUE
    )

    n_sample <- nrow(sample_data)

    x_blind <- sum(
      blind_data[[v]] == 1,
      na.rm = TRUE
    )

    n_blind <- nrow(blind_data)

    test <- prop.test(
      x = c(x_sample, x_blind),
      n = c(n_sample, n_blind),
      correct = FALSE
    )

    tibble(
      Variable                 = v,
      Sample_Corpus_Prevalence = round(100 * x_sample / n_sample, 2),
      Blind_Corpus_Prevalence  = round(100 * x_blind  / n_blind,  2),
      Difference_pp            = round(100 * (x_sample / n_sample - x_blind / n_blind), 2),
      p_value                  = test$p.value
    )
  }
)

table3 <- table3 %>%
  mutate(
    Holm_Adjusted_p = p.adjust(
      p_value,
      method = "holm"
    )
  ) %>%
  mutate(
    p_value         = round(p_value,         4),
    Holm_Adjusted_p = round(Holm_Adjusted_p, 4)
  )

table3