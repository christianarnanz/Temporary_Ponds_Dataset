# TemPoPS  
**Temporary Ponds of Peninsular Spain**

This repository provides the supporting data and materials associated with the data paper  
**“TemPoPS (Temporary Ponds of Peninsular Spain): An Integrative Dataset Supporting Research and Conservation of an Endangered Habitat”** (DOI: *to be added*).  
The contents are intended to ensure transparency, reproducibility, and reuse of the dataset.

---

## Repository contents

The repository includes the following files and resources:

### Core dataset
- **`Temporary_Ponds_of_Peninsular_Spain_V0.csv`**  
  Main dataset in CSV format (comma-separated, `.` as decimal separator).

- **`Temporary_Ponds_of_Peninsular_Spain_V0.kml`**  
  Spatial representation of the main dataset in KML format for direct visualization in GIS.

---

### Metadata
- **`Temporary_Ponds_of_Peninsular_Spain_V0_eml.xml`**  
  Full metadata record in Ecological Metadata Language (EML) format.

- **`TemPoPS_Metadata_Summary.pdf`**  
  Human-readable summary of the metadata, derived from the EML file.

---

### Source documentation
- **`MS_References_temporary_ponds_database.csv`**  
  Detailed list of all information sources used to construct the database, including download links (when available) and assigned source categories, as described in the Methods section of the data paper.

---

### Spatial data
- **`Associated_polygons_Temporary_ponds_database_V0.gpkg`**  
  GeoPackage containing polygons derived from polygon-based sources. Individual ponds in the main dataset are linked to these polygons via identifier field "ID_POLYGON".

---

### Code and visualization
- **`Arnanz_et_al_Temporary_ponds_database.R`**  
  R script illustrating the workflow used to compile, harmonize, and document the dataset.

- **`index.html`**  
  Code supporting the online interactive viewer available at:  
  https://christianarnanz.github.io/Temporary_Ponds_Database/

---

## Notes on data use
- Coordinates are provided in decimal degrees (EPSG:4326), using a dot (`.`) as the decimal separator.
- The CSV file follows international formatting standards and should be imported explicitly when using spreadsheet software.
- The EML metadata file provides the authoritative description of variables, sources, and data structure.

---

## Citation
If you use this dataset, please cite the associated data paper:
> Arnanz et al. (2026). *Temporary Ponds of Peninsular Spain (TemPoPS): An integrative dataset supporting research and conservation of an endangered habitat*. Journal / Repository. DOI.





> Arnanz et al. (YEAR). *Temporary Ponds of Peninsular Spain (TemPoPS): An integrative dataset supporting research and conservation of an endangered habitat*. Journal / Repository. DOI.
