import pandas as pd
import numpy as np
import yfinance as yf

# Define function to download data from Yahoo Finance
def getData(ticker, interval):
    if interval == "weekly":
        interval = "1wk"
    elif interval == "daily":
        interval = "1d"
    data = yf.download(ticker, start = "1985-01-01", end = "2021-01-01",
                       progress = False, interval = interval)
    return(data)

# Define function to calculate percent change on up to 5-week lag
def pctChange(data):
    data["PctChange"] = (data.Close - data.Open)/data.Open*100 
    data["BVolume"] = data.Volume/(1e9)
    for i in [1, 2, 3, 4, 5]:
       data["Lag" + str(i)] = data.PctChange.shift(i)
       data["VLag" + str(i)] = data.BVolume.shift(i)
    cons = [data.PctChange > 0, data.PctChange < 0, data.PctChange == 0]
    vals = ["Up", "Down", "Zero"]
    data["Direction"] = np.select(cons, vals)   
    data = data.drop(columns = ["Open", "High", "Low", "Close", "Adj Close", "Volume"], inplace = True)

# Collect data and write data to csv
SPDaily = getData("^GSPC", "daily"); pctChange(SPDaily)
SPWeekly = getData("^GSPC", "weekly"); pctChange(SPWeekly)
NDDaily = getData("^IXIC", "daily"); pctChange(NDDaily)
NDWeekly = getData("^IXIC", "weekly"); pctChange(NDWeekly)
DJDaily = getData("^DJI", "daily"); pctChange(DJDaily)
DJWeekly = getData("^DJI", "weekly"); pctChange(DJWeekly)
dflist = [SPDaily, SPWeekly, NDDaily, NDWeekly, DJDaily, DJWeekly]
filenames = ["SPDaily", "SPWeekly", "NDDaily", "NDWeekly", "DJDaily", "DJWeekly"]
for i in [0, 1, 2, 3, 4, 5]:
    dflist[i].to_csv(filenames[i]+".csv")
