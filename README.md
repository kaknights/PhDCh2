# PhDCh2
Thesis chapter code: performance comparison of distance and plot methods
This READ ME file contains navigation information for the R code relating to analyses produced for:

Chapter 2: A comparison of the performance of distance sampling and plot-based methods for density estimation of high-density species

To ensure that referencing within the code works correctly, the folder structure should be as follows:

Ch2 (working directory)
	dataSim
	Fieldwork
		Evans St
	graphics
	results	
	scripts

All raw data is contained in the excel workbook "dataRaw.xlsx", which should be in Fieldwork/Evans St. All dataframes used in analysis are generated from data contained in this workbook.

All scripts containing functions, simulated data generation and analyses are in the 'scripts' folder.

This is an archived version of the code that was used to produce the analyses in my thesis, 'Cost-efficient abundance estimation methods for high-density species' (Kathryn Knights, University of Melbourne, 2024). It may be viewed and downloaded for examination purposes, but is not intended for sharing or modification by third parties.

To recreate the simulated data for fig. 2.3 use the script scripts/pt1Sims_Boab.R
Dataframes are saved in the dataSim/prelimMethods folder. The figure is created in scripts/biasPrecisionPlot.R