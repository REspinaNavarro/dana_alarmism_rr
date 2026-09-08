# Alarmism Detection in Disaster News Headlines

This repository contains the complete reproducible workflow accompanying the study:

> *From Concept to Detection: Operationalizing Alarmist Rhetorical Resources in Disaster News Headlines: The 2024 Valencia DANA Floods as a Case Study*

The project develops and validates a structured framework for detecting alarm-related framing in disaster news headlines using Large Language Models (LLMs), expert-guided dictionary construction, and spatial validation methods. The empirical application is based on a corpus of 1,149 news headlines related to the October 2024 Valencia DANA floods.

## Repository Workflow

### Krippendorf Alpha

Computes inter-model reliability among ChatGPT, Gemini, and Claude.

**Techniques:**

- Krippendorff's Alpha

**Outputs:**

- Inter-LLM agreement coefficients for:
  - Alarmism (Holistic)
  - Alarmism_RR (Structured)
  - V1 Exceptionalism
  - V2 Systemic Collapse
  - V3 Civil Vulnerability
  - V4 Future Escalation

This script reproduces the reliability metrics reported in Table 2.

---

### Dictionary Training

Applies the Refined Alarmism Dictionary to the training sample (n = 200).

**Techniques:**

- Dictionary-based classification
- Rule-based pattern matching
- Keyword and expression detection

**Outputs:**

- Classification of rhetorical resources:
  - V1 Exceptionalism
  - V2 Systemic Collapse
  - V3 Civil Vulnerability
  - V4 Future Escalation
- Evidence traceability for each classification
- Resource prevalence estimates

---

### Dictionary Validation

Applies the Refined Alarmism Dictionary to the blind validation corpus (n = 949).

**Techniques:**

- Dictionary-based classification
- Rule-based pattern matching
- Keyword and expression detection

**Outputs:**

- Structured classification of the validation corpus
- Resource prevalence estimates
- Classification evidence and traceability

---

### MDS Analysis

Generates the Multidimensional Scaling (MDS) analyses and spatial representations reported in the manuscript.

**Techniques:**

- Jaccard Distance
- Classical Multidimensional Scaling (MDS)
- Goodness-of-Fit (GOF)

**Outputs:**

- Figure 1: Inter-model Fragmentation
- Figure 2: Holistic vs Structured Alarmism
- Figure 3: LLM Consensus vs Refined Dictionary
- Figure 4: Spatial Configuration of the Refined Dictionary


### Procrustes Analysis

Evaluates the transferability and spatial stability of the Refined Dictionary between the training corpus (n = 200) and the blind validation corpus (n = 949).

**Techniques:**

- Jaccard Distance
- Classical Multidimensional Scaling (MDS)
- Asymmetric Procrustes Alignment
- Two-sample Proportion Tests
- Holm Multiple Comparison Correction

**Outputs:**

- Figure 5: Transferability Across Training and Validation Samples
- Table 3: Comparison of prevalence rates between training and validation corpora

---

## Statistical Methods

The repository implements the following analytical procedures:

- Krippendorff's Alpha
- Jaccard Distance
- Classical Multidimensional Scaling (MDS)
- Procrustes Alignment
- Two-sample Proportion Tests
- Holm Multiple Comparison Correction
- PROTEST for validation in rotation similarities.

The complete workflow reproduces the statistical analyses, figures, and tables reported in the manuscript.
