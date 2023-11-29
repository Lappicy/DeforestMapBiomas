# LandScriptDeforestMap
Deforestation analysis on classified images.

The following code is aimed to help with deforestation analysis, using classified images - such as from MapBiomas.

## Download the package in R/Rstudio

If using windows as an OS (Operating System), it is needed to first download **Rtools** from the website (https://cran.r-project.org/bin/windows/Rtools/).

Then, to download **LandScriptDeforestMap** through **R**, you must download the *devtools* package beforehand (https://cran.r-project.org/web/packages/devtools/index.html).

To install and open the *devtools* package using the command line in R, run the following codes:
```r
install.packages("devtools")
library(devtools)
```
If the package (*devtools*) ir properly installed and opened, you must then install, and load, the *LandScriptDeforestMap* package through the github link with the code:
```r
devtools::install_github("Lappicy/LandScriptDeforestMap")
library(LandScriptDeforestMap)
```

## Usage
By defining a vector file (geopackage or shapefile), also called here as a geospatial file, and a folder with the needed images, this package may produce three main results for a proposed mesh (the refinement may be adjusted by an argument on the functions used):
1. A vector (geospatial) file with the frequency for each class inside each grid cell (from the proposed mesh);
2. Compare the deforestation time series with the growth of known classes (agriculture, pasture, urban and mining);
3. A map showing heatmaps for the acumulated, or for a given year, deforestation (or one of the other known classes).

## Practical example
As an example, within this github there is data for a study case of the brazilian Protected Area named "Caverna do Maroaga (Presidente Figueiredo)".

### Data used
To open the data used here, one must simply write at the console the function "example.files()":
```r
example.files()
```

The "CavernaMaroaga" is, inside the **R** environment, a sf object with the boundaries for "Caverda do Maroaga". The "MapBiomas_8_example" is a list, that contains the collection 8.0 from MapBiomas for the years 2013 to 2022 (10 elements on this list). These images were already cut to only have the extent of "Caverda do Maroaga". This was made to make the folder lighter, but using the original images should not affect the results or the processing time by any significant standards.

### Other way to acess data
Another way is to manually download everything from this github directory. There is an "Example application" folder, within it you can find a folder entitles "Data" which has 2 subdirectories. One, "GPKG", has a geopackage file with the boundaries of "Caverda do Maroaga", and the folder entitled "Mapbiomas8" has MapBiomas images (collection 8.0) from 2013 to 2022.

### The code itself
There are only three main functions that one must run for this practical example. One does the analysis and creates a sf object with all the necessary data for future analysis while the other two are graphical functions - one for a time series graph for deforestation and the possible driving vectors and the other is a map showing the locations of such deforestation or growth of these other vectors.

### Analysis with the *Growth.Analysis* function
To run the analysis (this may take a moment, possibly up to ~10 minutes depending on your computer specifications), one must simply define the geospatial data used (CavernaMaroaga) and the folder on which you downloaded the images or the MapBiomas object created (MapBiomas_8_example). This will output an object with a table like format, named "FinalAnalysis". The first and last two lines of it may be seen after the code.
```r
FinalAnalysis <- Growth.Analysis(geo.file = CavernaMaroaga,
                                 tif.folder = MapBiomas_8_example,
                                 mesh.size = 0.045,
                                 output.folder = "Results/",
                                 output.name = "Analysis_CavernaMaroaga",
                                 MAPBIOMAS = 8)
```

| ID_mesh |	Country | Category | Name | Year | Deforestation | Reforestation | Growth_Urban | Growth_Mining | Growth_Pasture | Growth_Agriculture | Forest | NonForest | Water | Others | Urban | Mining | Pasture | Agriculture | 0 | 3 | 4 | 6 | 11 | 12 | 15 | 24 | 30 | 33 | 41 |
| :----------: | :----------: | :--------------------: | :--------------------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: |
| 1	| Brazil | Environmental Protection Area | Caverna do Maroaga | 2013 | NA | NA | NA | NA | NA | NA | 9.09 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | NA | 9.09 | NA | NA | NA | NA | NA | NA | NA | NA | NA |
| 1 |	Brazil | Environmental Protection Area | Caverna do Maroaga | 2014 | 0 | 0 | 0 | 0 | 0 | 0 | 9.09 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | NA | NA | 9.09 | NA | NA | NA | NA | NA | NA | NA | NA |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |
| 221 | Brazil | Environmental Protection Area | Caverna do Maroaga |	2021 | 0 | 0 | 0 | 0 | 0 | 0 | 1.2141 | 0.0837 | 0.0252 | 0 | 0 | 0 | 0 | 0 | NA | 1.2141 | NA | NA | 0.0405 | 0.0432 | NA | NA | NA | 0.0252 | NA |
| 221 | Brazil |Environmental Protection Area |	Caverna do Maroaga | 2022 | 0.0144 | 0 | 0 | 0 | 0 | 0 | 1.1997 | 0.0396 | 0.0837 | 0 | 0 | 0 | 0 | 0 | NA | 1.1997 | NA | NA | NA | 0.0396 | NA | NA | NA | 0.0837 | NA |

### Correlation analysis with the *graphical.timeseries()* function
Below it is shown the plotted time series of deforestation with the known drivers evaluated (agriculture, mining, urban growth and pasture) for "Caverna do Maroaga". The correlation between the deforestation and these other classes were calculated, with the highest one (Pasture) being shown as a subtitle. To run this one must write:
```r
graphical.timeseries(proxy.table = FinalAnalysis,
                     comparison.names = c("Growth_Agriculture", "Growth_Mining",
                                          "Growth_Pasture", "Growth_Urban"),
                     comparison.color = c("darkorange", "grey50",  "#EA9999", "purple"),
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Caverna do Maroaga")
```
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Maroaga%20Deforestation%20vs%20Growth.png?raw=true)

### Output map from the *map.layout()* function
Lastely, we bring the result of the outputed map for the acumulated deforestation in "Caverna do Maroaga". To achieve this, you should run the following code:
```r
map.layout(mesh.data = FinalAnalysis,
           class = "Deforestation",
           year.used = "all",
           col.limits = c(0, 1, 2, 5),
           save.map.as = "Results/Map acumulated deforestation.png")
```
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Map%20acumulated%20deforestation.png?raw=true)

## Other practical examples
The code shown in this github directory has been widely tested throughout the Amazon region, using different spatial resolutions. In the next images we bring some examples for the Guiana Shield Region, which encompasses 6 countries (Brazil, Columbia, French Guiana, Guyana, Suriname and Venezuela). We may see through this map where the deforestation is more agressive, as well as observe the behaviour for other 3 known classes from MapBiomas (agriculture, mining and pasture).

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Guiana%20Shield%20Example.png?raw=true)

We can take a closer look to the correlations between deforestation and the different classes for the Brazilian part of the Guiana Shield Region with the *graphical.timeseries()* function. In the subtitle it is already shown which class has the highest correlation with deforestation (+0.70 for Pasture), but this can be visually seen as well. Not only that, we may observe that this relation between deforestation and pasture is very strong in every year except for 1988 and 1989 (which corresponds to the years where the construction of the Balbina hydropower plant took place). This is very important to better understand the deforestation patterns and its evolution throughout the years.

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Brazil%20example.png?raw=true)


