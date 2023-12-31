# Acoustics Data Analysis Repository

Welcome to the **Acoustics Data Analysis GitHub repository**! This repository focuses on the analysis of acoustic data, with the goal of isolating the soundscattering signal from datasets reconstructed from screenshots. The processing steps outlined in the accompanying R Markdown document aim to extract the acoustic signal, remove noise, and prepare the data for further analysis.

## Purpose

The primary purpose of this repository is to provide a comprehensive tutorial and resources for anyone interested to replicate the workflow of isolating biomass signals from the surrounding. Whether you're studying marine life, analyzing swarm behavior, or investigating soundscattering phenomena, this repository aims to guide you through the essential steps to process and analyze acoustic datasets. In this particular case the data has been reconstructed from screenshots.

## Content Overview

The repository contains the following components:

1. **R Markdown Document**: The heart of the analysis, the R Markdown document is the detailed step-by-step guide for isolating the soundscattering signal. It covers the entire workflow, including loading functions, sea floor detection, denoising, and isolating the biomass signal.

2. **Functions Folder**: This folder holds all the custom R scripts necessary for the analysis. These functions are used in the R Markdown document and provide essential tools for the data processing.

3. **Data Folder**: The "data" directory contains metadata in xslx format. The input data are too large for this repository but can be found in this following [Zenodo Repository](https://sandbox.zenodo.org/record/1184381).

4. **Export Folder**: This directory holds exported files, including visualizations (PNG files) and exported RDS files based on the calculations.

5. **README.md**: The main README file for the repository, providing an overview, setup instructions, and links to relevant resources.

## Workflow

The analysis workflow, as outlined in the R Markdown document, includes the following major steps:

1. **Setup**: Loading functions and setting up the analysis framework.
2. **Loading**: Preparing the input data and creating a tibble to store file-specific input parameters.
3. **Sea Floor Detection**: Identifying the sea floor in the acoustic data.
4. **Denoising**: Removing noise and isolating the biomass signal from the dataset.
5. **Isolating the Biomass Signal**: Refining the isolation of the biomass signal.
6. **Final Data**: Exporting the processed data, including visualizations and metadata.

## Getting Started

To replicate the analysis, follow these steps:

1. Clone or download this repository to your local machine.
2. Get the data Input from the [Zenodo Repository](https://sandbox.zenodo.org/record/1184381) and store it in the "/data" folder
3. Open the R Markdown document and follow the step-by-step instructions.
4. Access the R scripts in the "functions" folder for details on each function used in the analysis.
5. The processed data and visualizations will be saved in the "export" folder.
6. Modify parameters as needed for your specific dataset and research goals.

## Access the Tutorial Materials

All the necessary files to replicate the tutorial, including the R Markdown document, R scripts, and data files, can be found in the following Zenodo repository:

[Acoustics Data Analysis Tutorial - Zenodo Repository](https://sandbox.zenodo.org/record/1184381)

Feel free to explore the contents, make use of the provided resources, and adapt the analysis for your purposes.

Happy data analysis! 📊🔊