# Installation

## 1. System libraries for the `sf` dependency:
The sf package requires certain system libraries, particularly for handling spatial data. Depending on your operating system, you'll need to install these:

### Windows
No action needed

### Linux (Ubuntu)
```{bash}
sudo apt-get install libgdal-dev libgeos-dev libproj-dev
```
### macOS
```{bash}
brew install gdal geos proj
```

## 2. Install dependencies in R

```{r}
install.packages("devtools")
library(devtools)
install.packages(c("rnaturalearth", "sf", "units"))
library(rnaturalearth)
library(sf)
library(units)
```

## 3. Install `palat` :-D
```{r}
devtools::install_github("ClaudHGE/palat")
library(palat)
```



