# Forecasting-project
The project is implemented by Chaoyang Zheng,Yang Yu, Meng Peng and Katherine Gimon-Valencia for for master course:Forecasting Methods in HEC MONTREAL.
## Brief introduction 
This Proejct attempt to systematically explore various forecasting methods in order to accurately predict daily electricity demand for h=1 from data available on day t. This report specifically analyzes the electricity demand of the Rockland Electric Company
(RECO), a subsidiary of the Orange & Rockland Utilities Inc., which serves Northern New Jersey.

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/1.PNG "Optional title")

The detailed analysis can be found in the folder "report". In the  three-part series we illustrate how to proceed data cleaning and processing for time series data from anomaly detection to anomaly repairing, and implement a series of classical forecasting methods, including naive methods, exponential smoothing methods, regression and ARIMA. More detailed explanation can be found in reports.

The selected model's performance in the validation dataset can be seen from the following table:

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/3.PNG "Optional title")

We also noticed that slected models do not perform equally throughout the Year. To get more accurate prediction, we spit a typical year into the summer perod and the non-summer period as follows:

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/4.PNG "Optional title")


Then based on these two periods, we compare selected model's performance in the validation dataset:

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/5.PNG "Optional title")

We noticed that the TBATS performs best in the non-summer period while ARX works best in the summer, hence we combine this two models to get a hybrid model.

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/2.PNG "Optional title")

The  model selcted is the hybrid model, which performs the best at 3.595% MAPE in the validation dataset. This model implement the TBATS model in the summer perod and ARX in the non-summer period. The following graph show how the hubrid model can fit the actual data through time.

![Alt text](https://raw.githubusercontent.com/chaoyangzhengnash/forecasting-project/master/graph/6.PNG "Optional title")

