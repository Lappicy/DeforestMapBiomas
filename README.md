# DeforestMapBiomas
Deforestation analysis on MapBiomas Data

The following code is aimed to help with deforestation analysis, using classified images - such as from MapBiomas.

By defining a geospatial file (geopackage or shapefile) and a folder with the needed images, this software will produce three main results for a proposed mesh (the refinement may be adjusted by an argument on the functions used):
1. A geospatial file with the frequency for each class inside each grid cell (from the proposed mesh);
2. Compare the deforestation time series with the growth of known classes (agriculture, pasture, urban and mining);
3. A map showing heatmaps for the acumulated, or for a given year, deforestation (or one of the other known classes).

As an example, within this github there is data for a study case of the indigenous territory of Uaça 1 and 2, inside Brazil.
The data folder has 2 subdirectories. One, "GPKG", has a geopackage file with the boundaries of Uaça 1 and 2, and a folder entitles "Mapbiomas71" has MapBiomas images MapBiomas (collection 7.1) from 1985 to 2021. These images were already cut to only have the envolving rectangle of Uaça 1 and 2. This was made to make the folder lighter, but using the original images should not affect the results or the processing time by any significant standards.

Below it is shown the correlation of different known classes in Uaça 1 and 2:
