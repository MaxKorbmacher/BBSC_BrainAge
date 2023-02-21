# Densly Sampled BrainAge

Here, I analysed the brain age for particpants of 2 data sets:

1) the Bergen Breakfast Scanning Club

Wang, M. Y., Korbmacher, M., Eikeland, R., & Specht, K. (2022). Deep brain imaging of three participants across 1 year: The Bergen breakfast scanning club project. Frontiers in Human Neuroscience, 16. https://doi.org/10.3389%2Ffnhum.2022.1021503

2) the Travelling Human Phantom data set

Opfer, R., Krüger, J., Spies, L., Ostwaldt, A. C., Kitzler, H. H., Schippling, S., & Buchert, R. (2022). Automatic segmentation of the thalamus using a massively trained 3D convolutional neural network: higher sensitivity for the detection of reduced thalamus volume by improved inter-scanner stability. European Radiology, 1-10. https://doi.org/10.1007/s00330-022-09170-y

Data available here: https://www.kaggle.com/datasets/ukeppendorf/frequently-traveling-human-phantom-fthp-dataset

## Data set 1 (Bergen Breakfast Scanning Club)

Data files:
df.csv >> brain age predictions for data set 1
group_T1w.tsv >> MRICQ outputs for data set 1

## Data set 2 (Travelling Human Phantom)

Data files:
out.csv >> the MRIQC outputs for data set 2
inclusion_list.csv >> list to exclude repeat scans
predictions.csv >> brain age predictions and acquisition parameters

## Analyses for both data sets can be found here:
new_analyses.R
