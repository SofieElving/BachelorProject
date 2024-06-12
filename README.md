
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Improving Cancer Prediction by Optimizing cfDNA Error Model

Through our bachelor's project (Bachelor of Data Science), we've gotten the opportunity to work with Molekylær Medicinsk Afdeling (MOMA) to improve an aspect of the DREAMS pipeline, by utilizing same data used in the paper detailing UMIseq. Both papers can be found as 
- DREAMS: deep read-level error model for sequencing data applied to low-frequency variant calling and circulating tumor DNA detection. MBC. By Christensen et al. 2023
- Error-Corrected Deep Targeted Sequencing of Circulating Cell-Free DNA from Colorectal Cancer Patients for Sensitive Detection of Circulating Tumor DNA. MDPI. By Frydendahl et al. 2024. 

## About The Project
Circulating tumor DNA (ctDNA) is released from cancer cells into the bloodstream and constitutes a fraction of the total circulating cell-free DNA (cfDNA) in cancer patients. ctDNA measurements can therefore be used to identify and characterize tumors. Due to its short half-life, ctDNA is also ideal for monitoring treatment response and follow-up in cancer patients. However, the fraction of ctDNA in the blood is often very small, making it challenging to distinguish cancer mutations from somatic mutations that have no clinical significance.
In this project, we are working to optimize an error model, in the form of a neural network, that can distinguish cancer-specific mutations from Errors. Our work builds on the DREAMS method, which uses a neural network for low-frequency variant calling and detection of ctDNA from sequencing data. Specifically, we focus on modeling INDEL variations. We successfully improved the sensitivity of cancer predictions by 4.75% at a specificity of 95%, corresponding to 21.6% of the detectable cases. This underscores the potential of developing accurate error models for use in the DREAMS setup to effectively detect cancer.

For a detailed report see the PDF "Bachelor Project".

## Installation

You can install the development version of dreams from
[GitHub](https://github.com/JakobSkouPedersenLab/dreams) with:

``` r
# install.packages("devtools")
devtools::install_github("JakobSkouPedersenLab/dreams")
```


## About DREAMS

For technical details describing how DREAMS works please see the
[article](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-023-02920-1).
