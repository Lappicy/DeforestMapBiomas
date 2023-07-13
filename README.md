# DeforestMapBiomas
Deforestation analysis on MapBiomas Data

The following code is aimed to help with deforestation analysis, using classified images - such as from MapBiomas.

## Usage
By defining a geospatial file (geopackage or shapefile) and a folder with the needed images, this software will produce three main results for a proposed mesh (the refinement may be adjusted by an argument on the functions used):
1. A geospatial file with the frequency for each class inside each grid cell (from the proposed mesh);
2. Compare the deforestation time series with the growth of known classes (agriculture, pasture, urban and mining);
3. A map showing heatmaps for the acumulated, or for a given year, deforestation (or one of the other known classes).

## Practical example
As an example, within this github there is data for a study case of the indigenous territory of Uaça 1 and 2, inside Brazil.
The data folder has 2 subdirectories. One, "GPKG", has a geopackage file with the boundaries of Uaça 1 and 2, and a folder entitles "Mapbiomas71" has MapBiomas images MapBiomas (collection 7.1) from 1985 to 2021. These images were already cut to only have the envolving rectangle of Uaça 1 and 2. This was made to make the folder lighter, but using the original images should not affect the results or the processing time by any significant standards.

### Correlation analysis with gg.deforestation.cor function
Below it is shown the correlation of different known classes in Uaça 1 and 2:
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Deforestation%20vs%20Growth.png?raw=true)

### Output map from the mesh.map function
Lastely, we bring the result of the outputed map for the acumulated deforestation in Uaça 1 and 2:
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Map%20acumulated%20deforestation.png?raw=true)

## Other practical examples
The code proposed in here was widely teste throughout the Amazon region, using many different spatial resolutions. Below we bring also some examples of possible outputs, using the Guiana Shield Region. The first image shows the gridded map, which encompasses 6 countries (Brazil, Columbia, French Guiana, Guyana, Suriname and Venezuela) that belongs to the Guiana Shield Region. We may see through this map where the deforestation is more agressive, as well as showing the behaviour for other 3 known classes from MapBiomas. In addition to the different countries, we also used (with a specific geospatial file) where the protected areas where - and, therefore, giving a more important information for the deforestation patterns.

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Guiana%20Shield%20Example.png?raw=true)

We could take a closer look to the correlations for Brazil. Where the function automatically gives us the highest correlation coefficient found between deforestation and the other classes (+0.70 for Pasture).

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Brazil%20example.png?raw=true)


