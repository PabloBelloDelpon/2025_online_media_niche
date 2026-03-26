# Replication Package

**Arvidsson, Bello & Keuschnigg — *The Structural Origins of the Conservative Online Media Niche, US Twitter 2022***

This repository contains all data and code necessary to reproduce the figures in the main article and appendix. Raw data cannot be shared due to privacy constraints and practical infeasibility (raw data are on the order of hundreds of gigabytes).

## Data

The replication data are available at [Repository Name] ([DOI/URL]). Please download the data files and place them in the `data/` folder before running any scripts.

## Replication

Running scripts `01` through `07` in order will reproduce all figures in the main article and appendix.

## Other

**`scripts/99_randomize_networks.R`** contains the core network simulation described in the Data and Methods section. It generates simulated networks between the users and egos in our data, based on the homophily function described in the article. Full replication of the end results as presented in the paper additionally requires tweet-level data for egos and alters, which we are unable to share publicly due to data volume and privacy considerations.

**`scripts/99_function_sim_homophily.R`** contains a helper function used to produce Figure A2.