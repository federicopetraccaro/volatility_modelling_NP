# Volatility modelling with Neural Processes

This repository contains code used in the work "Volatility modelling with Neural Processes".
The following files can be found:

- **Econometric part (GARCH modelling).R**: In this R script:
1) financial data is downloaded for the SP500 index and 4 of its constituents
2) marketcap.csv file is read to obtain the market capitalization of each stock in the index
3) plots are produced
4) GARCH modelling is implemented
5) the Model Confidence Set procedure is implemented

- **Neural Processes implementation.ipynb**: contains NP implementation on SP500 data obtained in the R script above.
- **sp500_marketcap.csv**: contains market capitalization of each stock in the index.

