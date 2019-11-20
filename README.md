# LostOpportunity
Data and code for calculating Lost Economic Opportunity for time-area fishery closures.
Code author: James A Smith (UCSC, NOAA)

Manuscript: Smith et al (in press) "Lost opportunity: quantifying the dynamic economic impact of time-area fishery closures" Journal of Applied Ecology

To run this example, download all data and code, and run the 'Lost_Opportunity_Sim' R script.
This is part of the simulation in Smith et al (in press), used to estimate lost economic opportunity due to the Loggerhead Conservation Area (LCA) in the California drift gillnet fishery (DGN). This is not the full simulation, becuase it supplies only 3 years of swordfish CPUE predictions, and does not simulate some of the additional scenarios reported in Smith et al.

Code: 
1) An R script to calculate Lost Economic Opportunity, for example data for the California drift gillnet swordfish fishery ('Lost_Opportunity_Sim.R')
2) An R script of two functions: the utility function, and a data summarising function ('Lost_Opportunity_Funs.R')

Data: 
1) A set of rasters representing the predicted swordfish CPUE for every day of three fishing seasons (1991-1993). These rasters are used by the utility fucntion to calcualte Lost Economic Opportunity for each season.
2) Pre-calculated areas and locations of raster cells, distances of raster cells to departure ports, and cell locations relative to all relevant time-area fishery closures ('ref_data.RDS').
3) Mean variable swordfish prices ('variable_prices.RDS').
4) Dates at which each fishery closure is active ('closure_dates.RDS').
