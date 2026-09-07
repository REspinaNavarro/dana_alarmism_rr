# DANA Alarmism Repository
Reproducible statistical analysis for:  "From Concept to Detection: Operationalizing Alarmist Rhetorical Resources in Disaster News Headlines"

## Repository Workflow
The repository contains the complete statistical workflow used to develop, validate, and evaluate the proposed alarmism framework.

### Krippendorf Alpha
Computes inter-model agreement metrics for the training corpus.

Main tasks:

- Estimates Krippendorff's Alpha for:
  - Alarmism (Global/Holistic)
  - Alarmism_RR (Structured)
  - V1: Exceptionalism
  - V2: Systemic Collapse
  - V3: Civil Vulnerability
  - V4: Future Escalation
- Generates the inter-LLM reliability results reported in Table 2.
- Exports agreement statistics for reproducibility.


### Dictionary Training
Applies the Refined Alarmism Dictionary to the training corpus (n = 200).

Main tasks:

- Headline preprocessing and normalization.
- Dictionary-based detection of rhetorical resources.
- Identification of:
  - V1: Exceptionalism
  - V2: Systemic Collapse
  - V3: Civil Vulnerability
  - V4: Future Escalation
- Evidence extraction and traceability.
- Estimation of rhetorical resource prevalence rates.
- Export of the fully classified training dataset.

### Dictionary Validation
Applies the Refined Alarmism Dictionary to the blind validation corpus (n = 949).

Main tasks:

- Replication of the dictionary classification procedure.
- Evidence extraction and traceability.
- Estimation of rhetorical resource prevalence rates.
- Export of the classified validation dataset.

### MDS Analysis Figures
Generates the multidimensional scaling analyses and graphical representations reported in the manuscript.

Main tasks:

- Computation of Jaccard distance matrices.
- Classical Multidimensional Scaling (MDS).
- Goodness-of-Fit (GOF) estimation.
- Generation of:
  - Figure 1: Inter-model Fragmentation
  - Figure 2: Holistic vs Structured Alarmism
  - Figure 3: LLM Consensus vs Refined Dictionary
  - Figure 4: Spatial Configuration of the Refined Dictionary

### Procrustres Analysis
Evaluates the transferability and stability of the Refined Dictionary.

Main tasks:

- Construction of MDS configurations for training and validation samples.
- Procrustes alignment between both configurations.
- Assessment of spatial stability and transferability.
- Generation of:
  - Figure 5: Transferability Across Training and Validation Samples
- Computation of prevalence comparison statistics.
- Generation of Table 3 using proportion tests with Holm correction.

## Supporting Methods
The repository implements the following analytical techniques:

- Krippendorff's Alpha
- Jaccard Distance
- Classical Multidimensional Scaling (MDS)
- Procrustes Alignment
- Proportion Tests
- Holm Multiple Comparison Correction