# MISTR
Implementation of Ontario's Modelling and Inventory Support Tool (MIST). 
A set of models used for growth and yield analysis.


Development of the `MISTR` package was made possible thanks to the financial support of the [AWARE project  (NSERC CRDPJ 462973-14)](http://aware.forestry.ubc.ca/); grantee [Prof Nicholas Coops](http://profiles.forestry.ubc.ca/person/nicholas-coops/).

## Installation 
```
install.packages("devtools")
devtools::install_github("ptompalski/MISTR")
library(MISTR)
```

## Example usage

Selected stand attributes can be calculated using functions that return only one attribute (e.g. calc_BA(), or calc_GTV()), or using the function
StandAttributes() to calculate all available attributes at once. For example:

```
calc_BA(SI = 15, BHage = 40, SppId = "PJ")
calc_SI(H = 21, age = 67, SppId = "BW")

StandAttributes(SI = 20, age = 30, species = "AW")
StandAttributes(SI = 20, age = 10:50, species = "PR")

```
All calculations can be also performed for Standard Forest Units (SFU). For example:

```
StandAttributes(SI = 18, age = 85, species = "PJ", SFU = "PJ1")
StandAttributes(SI = 20, age = 50, SFU = "PJ1")

```

## References

Penner, M., Woods, M., Parton, J., Stinson, A., 2008. Validation of empirical yield curves for natural-origin stands in boreal Ontario. For. Chron. 84, 704–717. https://doi.org/10.5558/tfc84704-5

Sharma, M., Parton, J., Woods, M., Newton, P., Penner, M., Wang, J., Stinson, A., Bell, F.W., 2008. Ontario’s forest growth and yield modelling program: Advances resulting from the forestry research partnership. For. Chron. 84, 694–703. https://doi.org/10.5558/tfc84694-5

Penner, M., 2004. Developement of empirical yield curves for the PJ1, SP1, SB1, PO1 and SF1 Standard Forest Units. Forestry Research Parnership. https://www.forestresearch.ca/Projects/fibre/YC-PJ1-SP1-SB1-PO1-SF1.pdf
