# Heritable response to high CO<sub>2</sub>

This repository contains the code to reproduce the results on the article 

## Links to Arabidopsis raw images:
Dataset 1: https://doi.org/10.5281/zenodo.7688927<br>
Dataset 2: https://doi.org/10.5281/zenodo.7693935<br>
Dataset 3: https://doi.org/10.5281/zenodo.7693946


## Segmentation of individual plants

Instance segmentation labels for individual plants were obtained from the RGB images using the package [Plant Segmentation with Graph Cuts](https://github.com/ayanc/plseg). The region of interest for the images on each camera (tray containing 24 plants) was defined right around the edge of the tray on a single time point. The scale was set to 50% per side, which reduces the resolution to 25% of the original. All the Graph Cuts segmentation parameters were left as default.

## Area and shape analysis

The plant segmentation file for each tray contains the labels for all the image time points in the form of a 3D Numpy array. For better compatibility with the PlantCV parallelization we process the labels file to generate a 2D array file per image time point using the script <code>split_labels.py</code>.

We analyze the cropped RGB images and the corresponding labels jointly using the PlantCV workflow in <code>Analyze_from_labels_workflow.py</code>. We obtain area and shape measurement per individual plant at all the different time points.


## Data processing

We consolidate and process the resulting area data in R using the script <code>ROL_r3_shape_plots.R</code>. There we discard outlier data points that are outside of two standard deviations from the mean computed for each day.

## Area analysis

First we used the script <code>ChamberVariabilityfactors.R</code> to calculate the chamber variability factors to perform batch correction and normalize the data in different rounds of growth. The input for the R script is <code>AT_areas.tsv</code> containting the area of individual plants as mentioned above. This step must be done before the other R scripts to generate the figures.

After performing the batch correction, we used the script <code>Figure1C-D.R</code> to generate the plant growth figures in Figure 1C and 1D of the manuscript.

To generate the plant growth figures for the mutants in Figure 5B and 5D, we used the script <code>Figure5B-C.R</code>

and finally we used the script <code>Figure6F.R</code> to generate the differential plant growth plots in Figure 6F of the manuscript.
