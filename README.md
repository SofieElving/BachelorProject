
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Improving Cancer Prediction by Optimizing cfDNA Error Model

Through our bachelor's project (Bachelor of Data Science), we've gotten the opportunity to work with Molekylær Medicinsk Afdeling (MOMA) to improve an aspect of the DREAMS pipeline, by utilizing same data used in the paper detailing UMIseq. Both papers can be found as 
- DREAMS: deep read-level error model for sequencing data applied to low-frequency variant calling and circulating tumor DNA detection. MBC. By Christensen et al. 2023
- Error-Corrected Deep Targeted Sequencing of Circulating Cell-Free DNA from Colorectal Cancer Patients for Sensitive Detection of Circulating Tumor DNA. MDPI. By Frydendahl et al. 2024. 

## Installation

You can install the development version of dreams from
[GitHub](https://github.com/JakobSkouPedersenLab/dreams) with:

``` r
# install.packages("devtools")
devtools::install_github("JakobSkouPedersenLab/dreams")
```

### Additional Setup (If needed)

If you encounter any issues related to TensorFlow integrations within R,
install Keras within the correct python environment to ensure a proper
setup:

``` r
keras::install_keras(envname = "<ENVIRONMENT_NAME>")
```

### Usage

After installation, set the environment at the start of each R session:

``` r
reticulate::use_condaenv("<ENVIRONMENT_NAME>", required = TRUE)
```

## Basic Functions

This section provides an overview of the basic functions available in
the dreams library.

### Data Preparation and Model Training

The first steps in using DREAMS involve preparing your data and setting
up the model for training, crucial for effective variant calling and
cancer detection.

``` r
library(dreams)

# For training, DREAMS requires one or more BAM files and a reference genome.
training_data = get_training_data(
  bam_paths = "/path/bam_file",
  reference_path = "/path/hg38.fa",
  ...)


# Training the DREAMS Model using a Neural Network
# Basic settings for Keras are required.

model = train_dreams_model(
  training_data,
  layers = c(128, 64, 32),
  model_features = c("read_index", "strand", "trinucleotide_ctx", "first_in_pair", 
                     "umi_count", "seq_length", "fragment_size", "n_other_errors", 
                     "local_GC"),
  lr = 0.01,
  batch_size = 32000,
  epochs = 750,
  model_file_path = NULL,
  ...)
```

#### Feature Selection

The DREAMS model supports a variety of features categorized into
numeric, categorical, and embedded types:

##### Numeric Features

- `read_index`, `fragment_size`, `local_GC`, `umi_count`, `umi_errors`,
  `local_complexity_1`, `local_complexity_2`, `n_other_errors`,
  `prior_error`, `seq_length`

##### Categorical Features

- `ref`, `strand`, `first_in_pair`, `ctx_minus1`, `ctx_plus1`, `chr`,
  `genomic_pos`

##### Embedded Feature

- `trinucleotide_ctx`

Ensure that the dataset used aligns with the selected features and
adjust the parameters such as `layers`, `lr`, `batch_size`, and `epochs`
as needed.

### Variant Calling and Cancer Detection with DREAMS

The statistical methods `dreams_vc` and `dreams_cc` can be used for
variant calling and cancer detection, respectively.

``` r
# Call variants using DREAMS-vc

variant_calls = dreams_vc(
  mutations_df = mutations_df,
  bam_file_path = "/path/test_bam_file",
  reference_path = "/path/hg38.fa",
  model = model,
  ...)

# Call cancer using DREAMS-cc

cancer_calls = dreams_cc(
  mutations_df = mutations_df,
  bam_file_path = "/path/test_bam_file",
  reference_path = "/path/hg38.fa",
  model = model,
  ...)
```

### Saving and Loading Models

You can save your trained models for later use and load them as needed.
To save a trained model, use the `save_model_hdf5` function from the
`keras` package. Specify the file path where you want to save the model.
Similarly, when training a model using the `train_dreams_model`
function, you can directly specify a file path where to save the model
using the `model_file_path` argument. This allows for automatic saving
of the model upon training completion.

As default `model_file_path = NULL`, and the model won’t be saved
automatically. You can then manually save the model using
`save_model_hdf5`.

To load a previously saved model, use the `load_model_hdf5` function.

``` r
library(keras)

# Save the model
save_model_hdf5(model, filepath = "path/to/your_model.h5")

# Load the model
loaded_model <- load_model_hdf5(filepath = "path/to/your_model.h5")
```

## About DREAMS

For technical details describing how DREAMS works please see our
[article](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-023-02920-1).
