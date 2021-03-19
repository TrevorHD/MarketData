# Overview

An exploration of using machine learning for predictive modelling on three major U.S. stock market indices: the S&P 500, NASDAQ Composite, and the Dow Jones Industrial Average.

# Files

## Data

**DJDaily** *(.csv)* - Daily percent change for the Dow Jones Industrial Average, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**DJWeekly** *(.csv)* - Weekly percent change for the Dow Jones Industrial Average, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**NDDaily** *(.csv)* - Daily percent change for the NASDAQ Composite, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**NDWeekly** *(.csv)* - Weekly percent change for the NASDAQ Composite, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**SPDaily** *(.csv)* - Daily percent change for the S&P 500, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).

**SPWeekly** *(.csv)* - Weekly percent change for the S&P 500, calculated from the Yahoo Finance data ![here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).

## Scripts

**YFData** *(.py)* - Script that extracts index data from Yahoo Finance, adds lage variables, and saves it as a CSV file.
