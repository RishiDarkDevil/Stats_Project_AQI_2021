---
title: "AIR QUALITY INDEX-INDIA DATA ANALYSIS"
subtitle: "Statistical Methods II Spring Project"
author: "RishiDarkDevil"
date: "6/27/2021"
output: 
  html_document:
    keep_md: TRUE
---







# INTRODUCTION

In this Data Analysis Project, I am going to work with **Air Quality Index Data of India**. I will be using several Statistical Tools to Analyze the Data which includes Exploratory Data Analysis, Techniques and methodologies used for Inference and will be Modelling the Data to summarize any pattern or general trend.

I will also try to find out if there was any significant drop in the levels pollutant gases in the Atmosphere due to imposing lockdowns in the year 2020 and 2021, when several industries, factories, transportation facilities were suspended to work. I will be comparing the data for 2020 and 2021 with the previous years.

### UNDERSTANDING THE DATASET

Let's take a look at the Data

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>First few rows of the Air Quality Index Data</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Year </th>
   <th style="text-align:right;"> Month </th>
   <th style="text-align:right;"> Day </th>
   <th style="text-align:left;"> City </th>
   <th style="text-align:left;"> Specie </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> variance </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> pm25 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 296.0 </td>
   <td style="text-align:right;"> 460.0 </td>
   <td style="text-align:right;"> 394.0 </td>
   <td style="text-align:right;"> 27226.40 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Hyderabad </td>
   <td style="text-align:left;"> pm25 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 159.0 </td>
   <td style="text-align:right;"> 162.0 </td>
   <td style="text-align:right;"> 161.0 </td>
   <td style="text-align:right;"> 8.59 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> pm10 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 79.0 </td>
   <td style="text-align:right;"> 999.0 </td>
   <td style="text-align:right;"> 218.0 </td>
   <td style="text-align:right;"> 634717.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> o3 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 87.4 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 2324.38 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> so2 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 231.83 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> pm25 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 139.0 </td>
   <td style="text-align:right;"> 747.0 </td>
   <td style="text-align:right;"> 307.0 </td>
   <td style="text-align:right;"> 215149.00 </td>
  </tr>
</tbody>
</table>

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Last few rows of the Air Quality Index Data</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Year </th>
   <th style="text-align:right;"> Month </th>
   <th style="text-align:right;"> Day </th>
   <th style="text-align:left;"> City </th>
   <th style="text-align:left;"> Specie </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> variance </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> o3 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 105.7 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 4611.99 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> pm25 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:right;"> 104.0 </td>
   <td style="text-align:right;"> 63.0 </td>
   <td style="text-align:right;"> 1398.61 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> pressure </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 996.9 </td>
   <td style="text-align:right;"> 1007.5 </td>
   <td style="text-align:right;"> 999.3 </td>
   <td style="text-align:right;"> 67.94 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> wind-speed </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 10.87 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> dew </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2021 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:left;"> co </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 16.41 </td>
  </tr>
</tbody>
</table>

-   This Data set contains 263890 rows and 10 columns.

-   The Year ranges from 2014 to 2021 (till June), with observations recorded on each of the 30 /31 days of the month for 12 months.

-   The Data is generated from the 22 City Stations for Real-Time Air-Quality Index Monitoring across the Country. The Cities include:

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>City Stations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> State </th>
   <th style="text-align:left;"> City </th>
   <th style="text-align:right;"> Number of Stations </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Andhra_Pradesh </td>
   <td style="text-align:left;"> Visakhapatnam </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arunachal_Pradesh </td>
   <td style="text-align:left;"> Visakhapatnam </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bihar </td>
   <td style="text-align:left;"> Patna </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chandigarh </td>
   <td style="text-align:left;"> Chandigarh </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:left;"> Delhi </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kerala </td>
   <td style="text-align:left;"> Thiruvananthapuram </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kerala </td>
   <td style="text-align:left;"> Thrissur </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MadhyaPradesh </td>
   <td style="text-align:left;"> Bhopal </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maharashtra </td>
   <td style="text-align:left;"> Mumbai </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maharashtra </td>
   <td style="text-align:left;"> Nagpur </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maharashtra </td>
   <td style="text-align:left;"> Nashik </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Meghalaya </td>
   <td style="text-align:left;"> Shillong </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rajasthan </td>
   <td style="text-align:left;"> Jaipur </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tamil_Nadu </td>
   <td style="text-align:left;"> Chennai </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Telangana </td>
   <td style="text-align:left;"> Hyderabad </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Uttar_Pradesh </td>
   <td style="text-align:left;"> Lucknow </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Uttar_Pradesh </td>
   <td style="text-align:left;"> Muzaffarnagar </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West_Bengal </td>
   <td style="text-align:left;"> Kolkata </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
</tbody>
</table>

-   The parameters which we measure at the different Stations are given under the Specie Column and it includes -

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Specie Description</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Parameters </th>
   <th style="text-align:left;"> Description </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> pm25 </td>
   <td style="text-align:left;"> Particle pollution/particulate matter(particles less than or equal to 2.5 micrometers in diameter) </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pm10 </td>
   <td style="text-align:left;"> Particle pollution/particulate matter(particles less than or equal to 10 micrometers in diameter) </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> o3 </td>
   <td style="text-align:left;"> Ground-level ozone </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> so2 </td>
   <td style="text-align:left;"> Sulfur dioxide </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> no2 </td>
   <td style="text-align:left;"> Nitrogen dioxide </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> co </td>
   <td style="text-align:left;"> Carbon Monoxide </td>
   <td style="text-align:left;"> AQI </td>
  </tr>
  <tr>
   <td style="text-align:left;"> temperature </td>
   <td style="text-align:left;"> Temperature </td>
   <td style="text-align:left;"> Degree Celcius </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pressure </td>
   <td style="text-align:left;"> Air Pressure </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wind-gust </td>
   <td style="text-align:left;"> Wind Gust/Force </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> humidity </td>
   <td style="text-align:left;"> Relative Humidity </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wind-speed </td>
   <td style="text-align:left;"> Wind Speed </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dew </td>
   <td style="text-align:left;"> Dew Point </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> precipitation </td>
   <td style="text-align:left;"> Precipitation </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

The Air Quality Index (AQI) is an index for reporting air quality on a daily basis. It measures air pollution affects one's health within a short time period. The purpose of the AQI is to help people know how the local air quality impacts their health. The measurements in AQI are particularly helpful for our Data Analysis as it helps us compare the values at different stations and time points.

-   It also helps in identifying faulty standards and inadequate monitoring programmes.

-   AQI helps in analysing the change in air quality (improvement or degradation).

-   Comparing air quality conditions at different locations/cities.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Significance of the AQI Values</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> AQI Values </th>
   <th style="text-align:left;"> Level of Health Concern </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0-50 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51-100 </td>
   <td style="text-align:left;"> Moderate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 101-150 </td>
   <td style="text-align:left;"> Unhealthy for sensitive group </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 151-200 </td>
   <td style="text-align:left;"> Unhealthy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 201-300 </td>
   <td style="text-align:left;"> Very Unhealthy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 301-500 </td>
   <td style="text-align:left;"> Hazardous </td>
  </tr>
</tbody>
</table>

In further Analysis we will call pm25, pm10, o3, so2, no2 and co2 as pollutants and the remaining parameters as non-pollutants.

-   For Each parameters in Specie we measure it's minimum value, median value, maximum value and variance . The count variable is the number of times the measurement was taken for each of the parameters.

### TOP CITY STATIONS

Here we try to analyze which City Stations

-   Records More Observations compared to others, which will give us an idea of those centres being more frequently continuously recorded, which can be due to higher AQI Levels in the pollutants.

![](Data_Analysis_files/figure-html/obsv_per_station-1.png)<!-- -->

It is clearly visible that Delhi is definitely the most monitored centre, which is due to the presence the highest number of Real-Time Air Monitoring Stations in Delhi and more frequent montioring which is due to high AQI Levels which we will see in further analysis. The other centres have more or less the same number of observations.

-   Now Let's take a look at the Average Median AQI Levels of the Pollutants every year Station-wise. The Top 11 Stations will be considered in the further analysis where ever we need to do a Station-wise breakdown of Data or Analysis.

![](Data_Analysis_files/figure-html/avg_median_per_year_per_station-1.png)<!-- -->

# VISUAL OVERVIEW

-   Here we will look at several visualization to get an approximate idea about the spread, patterns, trends and key facts to notice about the Data, which will greatly impact our further Analysis and will prove helpful in finding out the parts of the Data which needs to be observed and tested carefully to provide valuable insight.

![](Data_Analysis_files/figure-html/Yearly_Station_Bar-1.png)<!-- -->

Here, from above bar plot, we see that the year 2020 was the one with the least levels of Pollutants compared to all the other years. 2021 has seen a hike in the AQI levels, which maybe due to opening of the factories, industries and also starting of Transportation facilities in many States. Though we cannot totally claim that, because we don't have data pertaining to entire of 2021.

![](Data_Analysis_files/figure-html/City_AQI_Box-1.png)<!-- -->

Taking look at the Top 11 Stations we can also see a drop in the AQI levels in the year 2020.

-   Here is an interesting relationship between month and average AQI levels per day of that Month, where we see that the AQI levels in each Measure drop almost linearly till July each year and then it starts rising. We will later explore this relationship in the Modelling Part of this Analysis.

![](Data_Analysis_files/figure-html/AQI_Yearly_Viz-1.png)<!-- -->

-   The Past 4 years Station-wise we can see the same pattern with Delhi being at the top of each month. This answers the question of why Delhi has so many more Observations recorded per year as compared to the other Stations. It's the concerning and high levels of AQI levels.

-   Now we see even more revealing pattern when we plot our Top 11 Station's Measure of AQI Levels in the last 4 years. The Maximum AQI Levels measured each day per month increases and then decreases till July where previously in the above plot which merged all the Stations didn't showed this much. This kind of behavior is ore pronounced in Delhi.

![](Data_Analysis_files/figure-html/Station_Monthly_Viz-1.png)<!-- -->

-   Let's turn to the Pollutant-wise breakdown of the AQI Levels, which may help us find out weather it was actually the Lockdowns which may have caused a dip in the AQI levels during the year 2020.

![](Data_Analysis_files/figure-html/Specie_Yearly_Viz-1.png)<!-- -->

Here we can clearly see that each Pollutant has decreased in the Year 2020 compared to all the other Years. But more interesting is the fact that particulate matter of both sizes have shown a big drop. Particulate Matters are mostly contributed by Vehicles, Transportation facilities and some Industries like Cement, etc. Since, there was lockdown we see it might have caused such a dip. Other Pollutants have also seen drop in the Year 2020. Further in the Analysis we will do Statistical Tests to find out if these drops were significant or not.

![](Data_Analysis_files/figure-html/Monthly_Pollutant_Viz_and_Daily_Data_wrangling_included-1.png)<!-- -->

### DISTRIBUTIONS

Distributions are key in deciding any changes been made to the system generating the data. They help us distinguish between two different groups. They help us look at the spread of the data and helps us take important decisions regarding what inferential techniques we can use and what sort of assumptions are valid. They are also fundamental part before doing any kind of Testing or Model Building.

-   Let's take a look at the Yearly Distribution of the Median AQI Levels

![](Data_Analysis_files/figure-html/Yearly_Pollutant_Hist-1.png)<!-- -->

They look quite familiar to Gamma Distribution. Later in the Inference Part we will try to find out the parameters and will do several Tests to verify this. We will also perform tests for whether the Year 2020 saw a dip in these AQI Levels.

We see more or less same distribution for Maximum AQI Levels and Minimum AQI Levels.

<img src="Data_Analysis_files/figure-html/Yearly_Pollutant_Hist_1-1.png" width=".49\linewidth" style="display: block; margin: auto;" /><img src="Data_Analysis_files/figure-html/Yearly_Pollutant_Hist_1-2.png" width=".49\linewidth" style="display: block; margin: auto;" />

-   A Pollutant-wise (Excluding pm10 and pm25) breakdown of the Median AQI Levels also reveal similar Gamma looking Distributions but with different parameters.

![](Data_Analysis_files/figure-html/Yearly_Indiv_Pollutant_Hist-1.png)<!-- -->

-   Now in all these above plots we have skipped pm10 and pm25 since they have much higher values of AQI compared to these gases. So, we won't be able to fit them in the above plots. Added with that pm25 doesn't seem to follow some known distribution. We will later see if any transformation or change can help us model that.

![](Data_Analysis_files/figure-html/Yearly_Indiv_Pollutant_Hist_2-1.png)<!-- -->

### RELATIONSHIPS - PATTERNS & TRENDS

Here we are going to explore the visual relationships between all the pollutants with all the other pollutants and not only that, it is important to realize that the non-pollutants also play a crucial role in deciding the level of pollutants measured. We will see more of it below in this section as well as a lot of it in the section where we build our Models. These relationships are best exposed with Scatter plots. Here we will take a look at these.

-   Here we look at the Scatter Plot matrix of the Pollutants where we have summarized the data in Yearly & Monthly basis so as to make the plots less overwhelming and more interpretable. We see strong correlation between all these marginals plots. Which can be attributed due to the conversions of one gas to the other that takes place naturally in the atmosphere. But we must also be careful that it may happen that these individual pollutants are related due to some other underlying common feature, which we will explore further in the Model Building section.

![](Data_Analysis_files/figure-html/Monthly_Specie_Scatterplot_Matrix-1.png)<!-- -->![](Data_Analysis_files/figure-html/Monthly_Specie_Scatterplot_Matrix-2.png)<!-- -->

Here we see that the non-pollutants also have strong correlations among themselves, which can be attributed for the fact that temperature, pressure, humidity, wind-speed, etc. are physically strongly related.

-   Let's take a look at the dependence of the Pollutants on Non-Pollutants

![](Data_Analysis_files/figure-html/Monthly_Pol_non_Pol_Scatterplot_Matrix-1.png)<!-- -->

There is a significant relationship between each of the non-pollutants on the AQI levels of the pollutants, which may be due to the fact that wind speeds, temperature, pressure, humidity, etc. changes the concentration of the pollutants in the Air. There are other physical processes in nature that also strongly intertwine them. We will see more of it in the later sections.

-   One really important thing to keep in mind is the measured values(Levels) of non-pollutants change with seasons, and months of the Year. So this effect should be kept in mind.

![](Data_Analysis_files/figure-html/Monthly_nonpol_viz-1.png)<!-- -->

-   There is also one more thing which can prove to be really helpful in understanding the relationship between the pollutants that is the variance of these parameters measured each day at the stations at real-time. Where we can see how these parameters variances are related. I haven't included the scatterplot matrix of the non-pollutants and of the pollutants and non-pollutants, as they visually don't convey much. But we will surely work with them in the later section.

![](Data_Analysis_files/figure-html/Monthly_Pol_non_Pol_Scatterplot_Matrix_Var-1.png)<!-- -->

So, with this visual picture of the Data in mind. We wrap the Visual Overview and most part of the EDA here. In the next section we will draw inferences from the data using several Inferential Statistical Technique.

# STATISTICAL INFERENCE

So, far what we have seen is a qualitative analysis, where we have looked at several visualizations which gave us a clear idea of what we see in the data. Now, it's time to ask some important questions regarding the data and trying to find quantitative answers to them. This will help us solidify our claims and believes which have in our mind after looking at the EDA. We will also strengthen our grip on the data by fitting several probability Models to the Histograms and Distributions we have seen in the previous section.

-   We will conduct all Tests, wherever needed, at a *0.01 level of significance*. Similarly, the Confidence Intervals wherever used will be *99% confidence intervals*.

### HYPOTHESIS TESTING

-   Let's get back to the first question with which we started the the Visual Overview of our data. It is clear that Delhi's number of observations recorder per year is much higher than the other. But the number of observations of other Cities look more or less same. Let's Test our Hypothesis of whether the proportion of the total observations recorded in a year are same for all the cities (Top 11) except Delhi.

![](Data_Analysis_files/figure-html/observation_prop_viz_and_chisq_test_for_prop-1.png)<!-- -->

We performed the **chi-square test for equality of proportion** on Year 2019, 2020 and 2021 respectively and clearly we fail to reject the null Hypothesis for the recent 2 years. Hence, all other City Stations except Delhi are equally monitored in the last 2 years.

-   Now, It is a natural question to ask - whether the AQI Levels of the pollutants similar across all the City Stations?

To answer this Question we can compare the mean AQI Levels of the City Stations. So, we can perform an **One-Factor Analysis of Variance** to test our Null Hypothesis of the mean AQI Levels of all the City Stations are equal.

![](Data_Analysis_files/figure-html/average_pollutant_one_factor_anova-1.png)<!-- -->

It is clear from the p-values that indeed the average AQI Levels of Pollutants in all the Top City Stations same in the past 3 Years.

-   Now the Question that rises is whether the Mean AQI Level of Pollutants in 2020 Less than that of 2019? Along with it we will also find out whether the Mean AQI Level of Pollutants in 2021 More than that of 2020 (Considering the Data of the First 6 months of 2020 and 2021)?

It is natural to think so, as India saw several Lockdowns at different places as well as a Nation-wide Lockdown in 2020, during which Major Factories and transportation facilities were closed, which usually are the top contributors of these Pollutants. Whereas in the Later months of 2020 and in 2021 Lockdown was mostly lifted and we may suspect that the Mean AQI Levels of Pollutants have increased.

So, to Test our Hypothesis of whether the Mean AQI Level of 2020 less than that of 2019 against the Alternative that the Mean AQI Level has Dropped in 2020, we will perform a **two sample t-test for equality of mean**. Here after looking at the boxplots, the variances for each year looks more or less the same, and I took variances of both the samples to be same.

![](Data_Analysis_files/figure-html/average_pollutant_yearly_t_test-1.png)<!-- -->

So, it is clear from the above two p-values that, Our guess that the Mean AQI of Pollutants have dropped in 2020 compared to 2019 doesn't seem to hold. But we see that the Average AQI of Pollutants for the data we have i.e. January to June of 2021 is definitely higher than that of 2020 by at least 7.5 with 99% confidence, which seems to be consistant with our thinking that the Lockdown might have caused a decrease in AQI Levels in 2020.

Let us try to explore the 2019 and 2020 comparison a bit more closely. We know that the Major Lockdown in India was from March to June of 2020 i.e. the first half of the Year. So, it makes more sense to compare the AQI Levels of 2019 during that phase with 2020 and as an extra check we will see if the values measured in 2021 similar to those of 2019 when the industries and transportation facilities were open as they are in 2021.

![](Data_Analysis_files/figure-html/average_pollutant_first6month_t_test-1.png)<!-- -->

Indeed Our guess was right here. The AQI values during the time when there was Lockdown that is the first half of 2020, there was a statistically significant drop in the mean AQI Levels of Pollutants, whereas the mean AQI Level of 2021 and 2019 are same.

A rather surprising fact which we noted in the Visual Overview Section through scatter plot is that the Average AQI Levels during the second half of 2020 was more compared to 2019.

![](Data_Analysis_files/figure-html/average_pollutant_last6month_t_test-1.png)<!-- -->

-   How different are the mean AQI Levels of each individual Pollutant?

Here we will try to look deep into each individual pollutant's mean and will be using similar **two-sample t-tests** and confidence intervals to decide how big a change was observed during the year 2020 compared to 2019 and 2021. For All the Pollutants I suspect that 2020 will be a drop in mean AQI Levels compared to 2019 and 2021 and 2019 & 2021 will have same mean AQI. Let's Test this now

![](Data_Analysis_files/figure-html/average_indiv_pollutant_first6month_t_test-1.png)<!-- -->

**The first row corresponds to t-test between 2019 & 2020, the second row for 2020 & 2021 and third row for 2019 & 2021.**

It is indeed the case that 2020 saw a dip in the mean AQI Levels compared to 2019 & 2021 in most of the pollutants in the first 6 months. It can be clearly seen from the p-values given under respective box plots.

Performing a similar series of tests on the last 6 months of 2020 & 2019. It turns out to be surprising that the mean AQI Levels of most of the pollutants have gone up, even though the first half of the year saw a major dip !

![](Data_Analysis_files/figure-html/average_indiv_pollutant_lastt6month_t_test-1.png)<!-- -->

We have seen several Tests in this section, and it seems that the first half of 2020 indeed saw a dip in Mean AQI Levels of Pollutants. It is also observed that it was the time when India went under a nation-wide lockdown which might have resulted in the drop of Mean AQI Levels of Pollutants.

But that surprising part which we eventually unfolded is that the Later half of 2020 saw an increase in the AQI Levels of Pollutants where I expected to see a drop or similar AQI Levels.

We will see more of Hypothesis Testing where ever needed, during our Analysis.

### PROBABILITY MODELS

We have already taken a look at the Distributions of all the Pollutants in the Distribution section of the Visual Overview. Now, it's time to fit Probability Models to the data we have observed. It is a really important part of parametric modelling. It will give us an idea about the noise we have in our data and in general what sort of AQI values of different pollutants we can expect every year. We will also carry our analysis on how different is the distribution of Year 2020 compared to 2019 & 2021.





-   One Important thing to keep in mind is that we have 3 types of Measure of AQI Levels for Each Day that is Maximum AQI detected on that day, Minimum AQI Detected on that Day and Median AQI Detected. So merging all these data results in bad fit for any model since there are lots of data points for high value and median value resulting in two different distributions. So we will take a look at them separately. And the Combined AQI for all pollutants which we will see will be without including particulate matter because they too take much higher value compared to the other pollutants.

-   We mostly observe that the histograms look quite close to Gamma, Lognormal, Weibull and Normal Density. So we tried fitting from these four models and if needed we will explore other models too. We will also take a look at how well the model fits our data best using **Chi-square Goodness of Fit Test**. In case of outliers we shift to some distance based model fitting approach i.e. **Hellinger-Distance**

-   Let's Start by comparing the Models for each Measure's AQI Level of 2019, 2020 & 2021.

![](Data_Analysis_files/figure-html/Model_Yearly_AQI-1.png)<!-- -->

-   Looking at the AQI Levels during the First Six Months Only.

![](Data_Analysis_files/figure-html/Model_AQI_First6Month-1.png)<!-- -->

-   Last 6 Months of 2019 & 2020

![](Data_Analysis_files/figure-html/Model_AQI_Last6Month-1.png)<!-- -->

-   Looking at the Pollutant wise AQI Levels During the First 6 Months

![](Data_Analysis_files/figure-html/Model_AQI_Indiv_First6Month-1.png)<!-- -->

-   Later Half of 2020 & 2019 Compared Pollutant-Wise

![](Data_Analysis_files/figure-html/Model_AQI_Indiv_Last6Month-1.png)<!-- -->
