**YAHOO API IS DOWN OR MOVED TO ANOTHER ADRESS! THE FUNCTIONS AREN'T WORKING ANYMORE!**


StockScreener
=============
This small package give two objects, a `stocktable` which is nothing else that a `data.frame` filled with stock data of a specific share and `stocklist` which is just a list of `stocktable` objects. The main reason for the class `stocktable` is the (in my opinion pretty) plot function.

The source for the stock data is yahoo finance. Any stock can be easily accessed as csv. Therefore you need the shortcut of a share and the range and paste it into the following link (here for Amazon `AMZN` and a range of 3 days `3d`):

http://chartapi.finance.yahoo.com/instrument/1.0/AMZN/chartdata;type=quote;range=3d/csv

Example
-------
To pull the amazon data just type in R:
```{r}
Stock_AMZN <- readStock( 'AMZN', '3d' )
```
The `print` generic yields the following output:
```

 
   Stock:            AMZN 
   Company:          Amazon.com, Inc. 
   Currency:         USD 
   Observed Range:   3d 

          Date   close     high     low    open volume
1   2016-09-09 780.550 780.9499 779.550 780.240  35800
2   2016-09-09 779.930 780.4200 779.480 780.420  37900
3   2016-09-09 779.170 780.4200 779.000 779.900  33900
229 2016-09-13 761.705 762.4300 761.565 761.565  49300
230 2016-09-13 761.500 762.3600 761.010 761.690  61300
231 2016-09-13 761.710 761.9899 761.311 761.311   5300

 
   [[ 229 lines are not showed ]] 

```
The `plot` function, which is also a generic, gives for the Amazon data the following chart:
```{r}
plot( Stock_AMZN )
```

![AMZN stocktable chart](/images/AMZN.png)

Why a class stocklist?
----------------------

The problem after writing the `stocktable` class was, that I didn't know how to wrap anything around layout, which is used for the graphic, for multiple plots. Therefore I create a new class `stocklist` which can do multiple plots of `stocktable` objects.

**Example**

At first create a `stocklist` object of as many shares as we like with the command:
```{r}
Stocklist <- create_stocklist( 'AMZN', 'IBM', 'TSLA', 'GOOG', stockRanges = '3d' )
```
With the following plot command one can create a multiple plot of the stocks:

![stocklist chart](/images/stocklist.png)
