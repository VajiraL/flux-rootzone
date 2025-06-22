---
theme: gaia
_class: lead
paginate: true
backgroundColor: #fffc
marp: true

---
<style>
title {
  color: red;
}

h1 {
  color:rgb(48, 48, 109);
}
</style>

### **Data Science for Sustainable Development**

Markdown Presentation Ecosystem

---
<!-- _class: lead -->
# 1. Ecosystem-water interactions at Flux sites

---
## Flux towers

Measure exchange of $CO_2$, water vapour and energy between atmosphere and land using eddy covariance techniques.
![bg right:60% ](https://waterforfood.nebraska.edu/-/media/projects/dwfi/resources/2020-annual-report/small/fluxtowers.jpg)

---
## What I did:
- Obtain a compiled dataset of corrected flux data from multiple networks
- Check data coverage and quality
- 

---
## Data
[**FluxdataKit**](https://zenodo.org/records/14808331)
- Time series (half-hour and daily) data from 339 flux sites.
- Supplemented with remote sensing vegetation data (MODIS LAI and fAPAR)
- Varying temporal coverage
- Available as netCDF or CSV

---
## Data coverage

- [Locations of flux sites](flux_sites_map.html)
- Varying temporal coverage
![bg right:40% 45%](data_sequence.png)
![w:600 h:300](land_cover_distribution.png)

---
## Data Cleaning

Check for missing data in key variables.

![bg right:60% 60%](missing-heatmap.png)
![w:600 h:300](missingness_by_site.png)

---

![w:1100 h:650](missingness_by_variable.png)

---

---
## Mapping the sites in Budykyo space
Budyko Curve describes the theoretical energy
and water limits on the **catchment** water balance. It depicts the expected partitioning of P into ET and Q.

![bg right:50% 95%](budykyo_change.JPG)

---

### Calculating potential evapotraspiration (PET)

Mutiple empirical functions such as Penman-Monteith, Priestly-Taylor, Hargreves, Thornthwaite.

Wrapper function to calculate PET with different methods:
```R
calculate_pet <- function(method = "priestley_taylor", ...) {
  switch(method,
         "priestley_taylor" = calculate_pet_priestley_taylor(...),
         "penman_monteith" = calculate_pet_penman_monteith(...),
         "hargreaves" = calculate_pet_hargreaves(...),
         "thornthwaite" = calculate_pet_thornthwaite(...),
         stop("Unknown method. Choose from: priestley_taylor, penman_monteith, hargreaves, thornthwaite")
  )
}
```

---

![bg 65%](budyko_whole_period.png)

---

Is there a distiction between climates ?

![bg right:70% 95%](budyko_whole_period_koeppen.png)

---

How about land use ?

![bg right:70% 95%](budyko_whole_period_lu.png)

---

Root zone storage capacity

![bg right:70% 95%](budyko_whole_period_whc.png)

---
<!-- _class: lead -->
# work in progress !
![bg 65%](budyko_annual_trajectories.png)

---

# Main learnings

- Transparent quality assessment

```markdown

```

---
Example code

```markdown
# Slide 1

foobar

---

# Slide 2

foobar
```