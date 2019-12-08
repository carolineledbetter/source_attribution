---
geometry: margin=1in
output:
  html_document:
    df_print: paged
    keep_md: yes
  pdf_document:
    fig_caption: yes
  word_document: default
---





**Project:**P1330White
**PI:**Alice White  
**Prepared By:**Caroline Ledbetter  
**Date:** 12/08/2019  
  
# Methods  
## Data
Data were collected from 1998-2016 from NORS. Food-borne outbreaks were 
grouped into categories based on the food source as 
identified in NORS. Outbreaks missing IFSAC information, those caused by 
multiple sources, unclassified outbreaks, outbreaks of undetermined source 
and outbreaks from a source other than animal or plant were removed. Food-borne 
outbreak caused by dairy, eggs, fruits, meat, poultry, or vegetables were 
included.
Food-borne outbreaks caused by other sources were excluded. 
Non food-borne outbreaks caused by animal contact were included. (Fig 1)  
The final analysis data was split into training (75%) and test (25%) 
stratified 
by outbreak source to ensure balance. A validation data set consisting of 
2017 outbreak data was used to evaluate the final selected model. 

![Fig.1 Cohort Flow Chart](StatisticalSummary_files/figure-html/flow_chart-1.png)

## Predictors  
The month of the first illness, the geography of the outbreak (multi state, 
multi county, single county), the etiology of the outbreak (STEC or Salmonella 
serotype) and the gender and ages of cases were used as predictors. Missing 
predictors were imputed using k-nearest neighbors using the training data. 

*Gender and Age*  
The number of female cases as a proportion of cases whose gender was known was 
used for predicting. If gender was unknown for all cases, the predictor was 
missing. The number of cases for each age group (under 1, 1 to 4, 5 to 19, 
20 to 49, 50 plus) as a proportion of cases whose age was known was used for 
predicting. If age was unknown for all cases, the age predictors were missing. 

*Salmonella Serotypes*  
Only serotypes with ten or more outbreaks were included as is. Salmonella 
serotypes 
with less than ten outbreaks but more than three were clustered into three 
groups based on there association with plant or animal 
outbreaks using logistic regression. 
Serotypes with three or fewer outbreaks were categorized as rare. 
Missing Salmonella serotypes were treated as missing.


## Model Selection  
We selected six algorithmic methods for 
prediction based on their ability to predict 
multiple class probabilities well - adaptive boosting classification 
trees (AdaBoost.M1), classification and 
regression trees (CART), weighted k nearest neighbors (knn), 
boosted trees (using xgboost), random forest (using ranger) and multivariate 
adaptive regression splines (MARS). 
A non-informative model that uses no information from predictors was also 
generated for comparison purposes. 
The final model was chosen based on Brier 
Scores (a measure of the difference in the predicted probability and the actual 
event). All analysis was done in R version 3.6.1 (2019-07-05). Data cleaning 
was done using the tidyverse. rsample v(0.0.5) and
recipes v(0.1.7) 
(part of tidymodels) were used for data splitting, imputation and 
preprocessing. The parsnip v(0.0.4)
(null model), adabag v(4.2) (Adaboost.M1), 
C50 v(0.1.2) (CART), 
kknn v(1.3.1) (weighted knn),
xgboost v(0.90.0.2) (boosted trees), 
and ranger v(0.11.2) (random forest) 
packages were used. 
The caret Package v(6.0.84) was used for tuning and test 
set prediction. The final model was selected based on the brier score of 
the test set. The brier score is a measurement of the accuracy of probabilistic
predictions of mutually exclusive outcomes. It is the mean of the square of the 
differences 
between the predicted probability and the observed outcome (equal to zero if 
the outcome did not occur, one if it did). 
(Fig 2) $$BS = \frac{1}{N} \sum^{N}_{t=1} \sum^{N}_{i=1} (f_{ti} - o_{ti})^2$$  
where $f$ is the predicted probability of the category for an event, 
$o$ is the observed outcome for a category for an event, $R$ is number of
possible 
categories and $N$ is the total number of observed outcomes. 

![Figure 2. Graphical Representation of Brier Score for One Outbreak](StatisticalSummary_files/figure-html/plot_bs-1.png)

# Results  
There were 4,059 non-waterborne 
outbreaks in the NORS database. 
748 outbreaks that were not
food-borne or animal contact were excluded. 
1,936 food-borne outbreaks were 
excluded because they did not have a single identifiable food source. 
The final data set included 1,224 outbreaks that were identified as 
dairy, eggs, fruits, meat, poultry, vegetables and animal contact.

Characteristics of outbreaks in the analysis are given in table 1. Fruit 
and vegetable outbreaks had the highest average percentage of females, 
meat and dairy had the lowest. Animal contact outbreaks had much higher 
average proportions of children than all other outbreaks. Egg meat and 
dairy outbreaks were more common in the summer and much less common in the
winter, animal contacts occurred most in winter and spring. Most outbreaks 
are single county 
(63%
overall), but egg and poultry are particularly likely to be single county. Fruit 
and vegetable outbreaks have the highest average total cases. 


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Table 1. Outbreak Characteristics by Source</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Animal Contact <br> N= 187 </th>
   <th style="text-align:left;"> Dairy <br> N= 79 </th>
   <th style="text-align:left;"> Eggs <br> N= 155 </th>
   <th style="text-align:left;"> Fruits <br> N= 75 </th>
   <th style="text-align:left;"> Meat <br> N= 312 </th>
   <th style="text-align:left;"> Poultry <br> N= 224 </th>
   <th style="text-align:left;"> Vegetables <br> N= 192 </th>
   <th style="text-align:left;"> Other <br> N= 151 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> Season </td>
   <td style="text-align:left;"> Winter </td>
   <td style="text-align:left;"> 65(35) </td>
   <td style="text-align:left;"> 16(20) </td>
   <td style="text-align:left;"> 27(17) </td>
   <td style="text-align:left;"> 8(11) </td>
   <td style="text-align:left;"> 41(13) </td>
   <td style="text-align:left;"> 27(12) </td>
   <td style="text-align:left;"> 37(19) </td>
   <td style="text-align:left;"> 25(17) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Spring </td>
   <td style="text-align:left;"> 51(27) </td>
   <td style="text-align:left;"> 19(24) </td>
   <td style="text-align:left;"> 37(24) </td>
   <td style="text-align:left;"> 27(36) </td>
   <td style="text-align:left;"> 104(33) </td>
   <td style="text-align:left;"> 61(27) </td>
   <td style="text-align:left;"> 57(30) </td>
   <td style="text-align:left;"> 39(26) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Summer </td>
   <td style="text-align:left;"> 43(23) </td>
   <td style="text-align:left;"> 27(34) </td>
   <td style="text-align:left;"> 59(38) </td>
   <td style="text-align:left;"> 24(32) </td>
   <td style="text-align:left;"> 112(36) </td>
   <td style="text-align:left;"> 74(33) </td>
   <td style="text-align:left;"> 50(26) </td>
   <td style="text-align:left;"> 56(37) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Fall </td>
   <td style="text-align:left;"> 28(15) </td>
   <td style="text-align:left;"> 17(22) </td>
   <td style="text-align:left;"> 32(21) </td>
   <td style="text-align:left;"> 16(21) </td>
   <td style="text-align:left;"> 55(18) </td>
   <td style="text-align:left;"> 62(28) </td>
   <td style="text-align:left;"> 48(25) </td>
   <td style="text-align:left;"> 31(21) </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> Geography </td>
   <td style="text-align:left;"> Multi County </td>
   <td style="text-align:left;"> 25(13) </td>
   <td style="text-align:left;"> 30(38) </td>
   <td style="text-align:left;"> 12(8) </td>
   <td style="text-align:left;"> 12(16) </td>
   <td style="text-align:left;"> 49(16) </td>
   <td style="text-align:left;"> 14(6) </td>
   <td style="text-align:left;"> 34(18) </td>
   <td style="text-align:left;"> 9(6) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Multi State </td>
   <td style="text-align:left;"> 71(38) </td>
   <td style="text-align:left;"> 8(10) </td>
   <td style="text-align:left;"> 4(3) </td>
   <td style="text-align:left;"> 33(44) </td>
   <td style="text-align:left;"> 55(18) </td>
   <td style="text-align:left;"> 14(6) </td>
   <td style="text-align:left;"> 97(51) </td>
   <td style="text-align:left;"> 33(22) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Single County </td>
   <td style="text-align:left;"> 87(47) </td>
   <td style="text-align:left;"> 41(52) </td>
   <td style="text-align:left;"> 137(88) </td>
   <td style="text-align:left;"> 28(37) </td>
   <td style="text-align:left;"> 206(66) </td>
   <td style="text-align:left;"> 195(87) </td>
   <td style="text-align:left;"> 60(31) </td>
   <td style="text-align:left;"> 107(71) </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> Missing </td>
   <td style="text-align:left;"> 4(2) </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:left;"> 2(1) </td>
   <td style="text-align:left;"> 2(3) </td>
   <td style="text-align:left;"> 2(1) </td>
   <td style="text-align:left;"> 1(0) </td>
   <td style="text-align:left;"> 1(1) </td>
   <td style="text-align:left;"> 2(1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total Cases </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 30.99(55.88) </td>
   <td style="text-align:left;"> 24.00(35.96) </td>
   <td style="text-align:left;"> 36.94(158.52) </td>
   <td style="text-align:left;"> 54.28(119.66) </td>
   <td style="text-align:left;"> 22.46(38.42) </td>
   <td style="text-align:left;"> 26.08(58.45) </td>
   <td style="text-align:left;"> 56.57(139.31) </td>
   <td style="text-align:left;"> 47.23(127.68) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Male </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 42.12(23.19) </td>
   <td style="text-align:left;"> 49.11(22.41) </td>
   <td style="text-align:left;"> 49.81(23.39) </td>
   <td style="text-align:left;"> 36.21(17.70) </td>
   <td style="text-align:left;"> 50.61(23.51) </td>
   <td style="text-align:left;"> 49.02(26.53) </td>
   <td style="text-align:left;"> 37.94(19.08) </td>
   <td style="text-align:left;"> 47.17(21.30) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Female </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 57.88(23.19) </td>
   <td style="text-align:left;"> 50.89(22.41) </td>
   <td style="text-align:left;"> 50.19(23.39) </td>
   <td style="text-align:left;"> 63.79(17.70) </td>
   <td style="text-align:left;"> 49.39(23.51) </td>
   <td style="text-align:left;"> 50.98(26.53) </td>
   <td style="text-align:left;"> 62.06(19.08) </td>
   <td style="text-align:left;"> 52.83(21.30) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Under 1yr </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 8.92(13.20) </td>
   <td style="text-align:left;"> 2.45(8.56) </td>
   <td style="text-align:left;"> 0.68(4.63) </td>
   <td style="text-align:left;"> 1.73(3.60) </td>
   <td style="text-align:left;"> 0.76(4.06) </td>
   <td style="text-align:left;"> 0.69(3.16) </td>
   <td style="text-align:left;"> 0.69(2.43) </td>
   <td style="text-align:left;"> 1.30(6.53) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent 1yr to 4yr </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 27.24(27.23) </td>
   <td style="text-align:left;"> 19.65(21.22) </td>
   <td style="text-align:left;"> 2.77(6.73) </td>
   <td style="text-align:left;"> 11.37(16.50) </td>
   <td style="text-align:left;"> 7.55(16.47) </td>
   <td style="text-align:left;"> 5.64(12.53) </td>
   <td style="text-align:left;"> 4.63(11.09) </td>
   <td style="text-align:left;"> 6.82(14.16) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent 20yr to 49yr </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 20.36(21.92) </td>
   <td style="text-align:left;"> 27.52(24.82) </td>
   <td style="text-align:left;"> 46.73(30.64) </td>
   <td style="text-align:left;"> 25.71(20.86) </td>
   <td style="text-align:left;"> 41.72(29.70) </td>
   <td style="text-align:left;"> 50.53(30.04) </td>
   <td style="text-align:left;"> 50.53(22.96) </td>
   <td style="text-align:left;"> 49.59(29.52) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent 5yr to 19yr </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 31.53(29.37) </td>
   <td style="text-align:left;"> 36.16(26.06) </td>
   <td style="text-align:left;"> 17.12(24.49) </td>
   <td style="text-align:left;"> 25.91(25.29) </td>
   <td style="text-align:left;"> 27.35(28.99) </td>
   <td style="text-align:left;"> 19.33(25.60) </td>
   <td style="text-align:left;"> 19.71(18.48) </td>
   <td style="text-align:left;"> 19.00(22.71) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent 50yr or older </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 11.95(17.62) </td>
   <td style="text-align:left;"> 14.22(19.83) </td>
   <td style="text-align:left;"> 32.70(31.06) </td>
   <td style="text-align:left;"> 35.29(31.76) </td>
   <td style="text-align:left;"> 22.63(24.66) </td>
   <td style="text-align:left;"> 23.81(28.02) </td>
   <td style="text-align:left;"> 24.44(20.83) </td>
   <td style="text-align:left;"> 23.29(26.57) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent Hospitalized </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 0.28(0.26) </td>
   <td style="text-align:left;"> 0.31(0.28) </td>
   <td style="text-align:left;"> 0.18(0.24) </td>
   <td style="text-align:left;"> 0.27(0.19) </td>
   <td style="text-align:left;"> 0.31(0.30) </td>
   <td style="text-align:left;"> 0.23(0.28) </td>
   <td style="text-align:left;"> 0.27(0.23) </td>
   <td style="text-align:left;"> 0.24(0.25) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Length (in days) of Outbreak </td>
   <td style="text-align:left;"> Mean (Sd) </td>
   <td style="text-align:left;"> 43.92(125.49) </td>
   <td style="text-align:left;"> 33.53(57.55) </td>
   <td style="text-align:left;"> 8.77(20.34) </td>
   <td style="text-align:left;"> 7.23(11.26) </td>
   <td style="text-align:left;"> 7.48(19.54) </td>
   <td style="text-align:left;"> 5.88(42.01) </td>
   <td style="text-align:left;"> 10.47(18.43) </td>
   <td style="text-align:left;"> 8.13(15.06) </td>
  </tr>
</tbody>
</table>

Final selected tuning parameters are shown in table 2. 
All models performed better than the null model on the test set (table 3). 
Calibration curves are shown in figure 3. 

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2. Tuning Parameters</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Model </th>
   <th style="text-align:left;"> Parameters </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Random Forest </td>
   <td style="text-align:left;"> mtry = 2; split rule = gini; min node size = 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Weighted k-nearest neighbors </td>
   <td style="text-align:left;"> k = 44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Multivariate Adaptive Regression Spline </td>
   <td style="text-align:left;"> degree = 2; nprune = 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Boosted Trees </td>
   <td style="text-align:left;"> eta = 0.3; max depth = 6; gamma = 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CART </td>
   <td style="text-align:left;"> trials = 20; model type = 'rules'; no winnowing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Adaboost.M1 </td>
   <td style="text-align:left;"> mfinal = 9, max depth = 1, coefficient type = 'Zhu' </td>
  </tr>
</tbody>
</table>



<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 3. Brier Scores for each model</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Model </th>
   <th style="text-align:right;"> Brier Score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Random Forest </td>
   <td style="text-align:right;"> 0.098 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Weighted k-Nearest Neighbors </td>
   <td style="text-align:right;"> 0.098 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Multivariate Adaptive Regression Spline </td>
   <td style="text-align:right;"> 0.100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Boosted Trees </td>
   <td style="text-align:right;"> 0.103 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CART </td>
   <td style="text-align:right;"> 0.106 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Adaboost.M1 </td>
   <td style="text-align:right;"> 0.110 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Null </td>
   <td style="text-align:right;"> 0.118 </td>
  </tr>
</tbody>
</table>

![](StatisticalSummary_files/figure-html/calibration_curves-1.png)<!-- -->





*Validation*  
The validation data set consisted of 98 food-borne and 
animal contact outbreaks including 
7 outbreaks whose sources was not 
one of our predicted categories. These were not excluded from evaluation 
metrics to more accurately reflect real world performance. The selected 
random forest model had a brier score of 
0.098. 
The calibration plot is shown in figure 4. Fruit and animal contact 
outbreaks were the most over-predicted. Dairy, and egg outbreaks
were the most under-predicted. 


![](StatisticalSummary_files/figure-html/validation_calplot-1.png)<!-- -->

Overall, the correct outbreak 
was in the top two predicted for 64 
(65%) of outbreaks. The percentage of outbreaks
where the correct category was in the top two predictions by actual 
outbreak source is shown in figure 5. 

![](StatisticalSummary_files/figure-html/accuracy_graph-1.png)<!-- -->




