# VisualizeHealthcareData
Using R shiny to create interactive visualizations

## Introduction

This project aims to look at the crude prevalence of diseases and disease indicators in the US by state, gender and race in order to gain a better understanding of geography impacts diseases and their indicators.

## Data

* This dataset is available for public use at: [disease data](https://catalog.data.gov/dataset/u-s-chronic-disease-indicators-cdi-e50c9)
* The dataset spans various years - our project focuses on the most recent data from 2014.


### Tab 1:

1. Starts with the chloropleth colored by Mortality Rates.
2. Moves on to the indicators related to the state clicked on.
3. The topic selected then yields a treemap of indicators for the state in question.
4. The indicator from the treemap yields a dropdown menu which has options for stratification.
5. Based on the selected stratification, a donut chart showing the breakup is created.

<img src="https://github.com/zdanish1/VisualizeHealthcareData/blob/master/healthcare_viz.gif" alt="healthcare viz" width="800" height="400">

### Tab 2:

This tab shows a cartogram of the deadliest of all diseases by inidcator. The cartogram shows rates both by percentage and by 100,000 people of both different types of cancers as well as the preventative measures such as mammograms that can be taken against it.

The drop down menu on the right includes different indicators for the various types of cancer.

<img src="https://github.com/zdanish1/VisualizeHealthcareData/blob/master/topogram.gif" alt="alt text" width="800" height="400">

It would be interesting to see how the states rates differed and whether higher rates of precautionary and preventative measures resulted in reduced death rates.
 
