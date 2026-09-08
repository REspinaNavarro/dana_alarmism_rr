
# Krippendorff's Alpha for Inter-LLM Agreement


# 1. Packages installing if needed
required_packages <- c(
  "readxl",
  "tidyverse",
  "irrCAC"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

library(readxl)
library(tidyverse)
library(irrCAC)


# 2. Data

df_raw <- read_excel(
  "data/AI_sample_corpus.xlsx"
)


# 3. Helper functions

compute_alpha <- function(df){
  
  alpha_result <- irrCAC::krippen.alpha.raw(
    ratings = df
  )
  
  round(
    alpha_result$est$coeff.val,
    3
  )
}


# 4. Krippendorff's Alpha

# Global alarmism (holistic)
alpha_alarmism <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_Alarm,
      Gemini_Alarm,
      Claude_Alarm
    )
)

# Structured alarmism
alpha_alarmism_rr <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_Alarm_rr,
      Gemini_Alarm_rr,
      Claude_Alarm_rr
    )
)

# V1 Exceptionalism
alpha_v1 <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_V1,
      Gemini_V1,
      Claude_V1
    )
)

# V2 Systemic Collapse
alpha_v2 <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_V2,
      Gemini_V2,
      Claude_V2
    )
)

# V3 Civil Vulnerability
alpha_v3 <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_V3,
      Gemini_V3,
      Claude_V3
    )
)

# V4 Future Escalation
alpha_v4 <- compute_alpha(
  df_raw %>%
    select(
      ChatGPT_V4,
      Gemini_V4,
      Claude_V4
    )
)


# 5. TABLE 2 - Inter-LLM Agreement: Krippendorff's Alpha
table2_alpha <- tibble(
  Construct = c(
    "Alarmism (Global/Holistic)",
    "Alarmism_RR (Structured)",
    "V1: Exceptionalism",
    "V2: Systemic Collapse",
    "V3: Civil Vulnerability",
    "V4: Future Escalation"
  ),
  
  Krippendorff_Alpha = c(
    alpha_alarmism,
    alpha_alarmism_rr,
    alpha_v1,
    alpha_v2,
    alpha_v3,
    alpha_v4
  )
)

print(table2_alpha)


# LLM Prevalence

native_prevalence <- tibble(
  Construct = c(
    "Alarmism",
    "Alarmism_RR",
    "V1",
    "V2",
    "V3",
    "V4"
  ),
  
  Prevalence = round(
    c(
      mean(df_raw$LLM_Alarm),
      mean(df_raw$LLM_Alarm_rr),
      mean(df_raw$LLM_V1),
      mean(df_raw$LLM_V2),
      mean(df_raw$LLM_V3),
      mean(df_raw$LLM_V4)
    ) * 100,
    2
  )
)

native_prevalence


# JACCARD Distances: Table 2

library(proxy)
library(tidyverse)

jaccard_distance <- function(x, y){
  
  dist_matrix <- proxy::dist(
    t(data.frame(x, y)),
    method = "Jaccard"
  )
  
  as.matrix(dist_matrix)[1, 2]
}

table2_jaccard <- tibble(
  Construct = c(
    "Alarmism_RR",
    "V1",
    "V2",
    "V3",
    "V4"
  ),
  
  Jaccard_Distance = round(
    c(
      
      jaccard_distance(
        df_raw$LLM_Alarm_rr,
        df_raw$Dicc_ref_Alarm_rr
      ),
      
      jaccard_distance(
        df_raw$LLM_V1,
        df_raw$Dicc_ref_V1
      ),
      
      jaccard_distance(
        df_raw$LLM_V2,
        df_raw$Dicc_ref_V2
      ),
      
      jaccard_distance(
        df_raw$LLM_V3,
        df_raw$Dicc_ref_V3
      ),
      
      jaccard_distance(
        df_raw$LLM_V4,
        df_raw$Dicc_ref_V4
      )
      
    ),
    4
  )
)

table2_jaccard


print('Script processed succesfully')