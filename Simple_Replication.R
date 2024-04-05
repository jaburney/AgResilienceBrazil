# -----------------------------------------------------------------
# Simple_Replication.R 
# -----------------------------------------------------------------
# This is the simple replication file for 
# Burney, McIntosh, Lopez-Videla, Samphantharak, Gori Maia
# "Empirical Modeling of Agricultural Climate Risk", PNAS (2024)
# DOI: XXX

# Outputs analysis and figures / tables from the manuscript:
# Figures 1-3, Figures S1-S2
# Descriptive Figures: Figure 1, Figure S1-S4
# Tables: S3-S8
#
# Not Included due to Data Sharing Restrictions:
# Code for Figure 4, Figures S5-S9, Table S9-S12 (FI data include PII)

# Does not include code to write Tables S1, S2 (descriptive)

# Assumption: Run from "Replication/" folder
# Written on a Mac; May need to alter directory names for PC

# -----------------------------------------------------------------
# Required Packages 
# -----------------------------------------------------------------
library(tidyverse)
library(fixest)
library(modelsummary)
library(magick)
library(pander)
library(glue)
library(margins)
library(ggeffects)
library(ggpubr)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(rworldmap)
library(sf)
library(zoo)

# May have to install tinytex, magick
#devtools::install_github('talgalili/installr')
#require(installr)
#install.ImageMagick()

# -----------------------------------------------------------------
# Create directory for output if it doesn't exist
# -----------------------------------------------------------------
if (dir.exists("Output/")==FALSE) {
  dir.create("Output/")
  dir.create("Output/Figures/")
  dir.create("Output/Figures/AgCoefPlots/")
  dir.create("Output/Figures/AgMIPlots/")
  dir.create("Output/Figures/FIPlots/")
  dir.create("Output/Figures/Projections")
  dir.create("Output/Tables/")
  dir.create("Output/Tables/Ag/")
  dir.create("Output/Tables/FI/")
}

# -----------------------------------------------------------------
# Ag Regression (Historical) Analysis
# -----------------------------------------------------------------

# Load ag and climate data
df = readRDS(file="Ag_Climate_Data_SimpleRep.RDS")

# Run analysis - Data file with models and input data frames saved to Output directory
# Creates coefficient tables for Yield, Revenue, Price by crop (with covariate at national or state level), with comparison to standard FE model (Tables S3-S8 in Manuscript)
# Creates coefficient plots for Yield, Revenue, Price by crop (with covariate at national or state level)
# Creates marginal effects plots for Yield, Revenue, Price by crop (with covariate shock at national level - configurable within file) (Figure 2 and S3-S4 in Manuscript)
# Saves models and data in: Output/Ag_Analysis.RData
source(file="Ag_Analysis.R")

# Figure 2 (Fancier version of Ag plots)
source(file="Fancier_Fig2_SimpleRep.R")

# -----------------------------------------------------------------
# Financial Institution (FI) & Climate Data
# FI Regression (Historical) Analysis
# -----------------------------------------------------------------
# Note: Individual loan data cannot be included in this respository
# due to data sharing agreements (PII). 

# -----------------------------------------------------------------
# Load spatial data for plots and projections
# -----------------------------------------------------------------
data("countriesCoarse")
brazil = countriesCoarse[countriesCoarse$NAME=="Brazil" & !is.na(countriesCoarse$NAME),]

# Load admin level 2 data
municipalities = readRDS("rBRA_adm2.rds")
ba_mun = subset(municipalities,municipalities$NAME_1=="Bahia")

# -----------------------------------------------------------------
# Future Projections
# -----------------------------------------------------------------

# Load ag, FI and climate data
source("Projections_SimpleRep.R")

# Produce Figures (Fig 3, Figs S10,S12)
# NOTE: Have to manually make sure significance markers
# plot correctly on figs. If you change anything, beware!
source("Projections_Figs_SimpleRep.R")

