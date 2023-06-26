# Analysis and optimization of stock portfolios and their application in the Peruvian stock market, using operations research and simulation tools.

## Background

The stock market emerged as a means for companies and investors to buy and sell stocks in a fair, transparent, and efficient manner. The Peruvian stock market, represented by the Lima Stock Exchange (BVL), has experienced sustainable growth in recent years, especially in the last decade. The positive economic conditions in Peru, coupled with the growth of industrial metals such as copper and zinc, have driven investment in the stock market. This study emphasizes the importance of making informed investment decisions based on the appropriate selection of a diversified portfolio to manage risk. Given the significant sums of money at stake and the desire to maximize returns while minimizing risk, sophisticated tools and portfolio theory are employed to predict portfolio value and effectively manage invested capital in the face of market volatility.


## General objective:
To apply mathematical, operations research, and simulation tools to determine an appropriate stock portfolio for investment in the Lima Stock Exchange (BVL).


## Specific objectives:
- Apply Markowitz's portfolio theory and a simulation tool based on copulas to determine the optimal investment weights for each selected stock traded on the Lima Stock Exchange.
- Perform a statistical analysis of the selected stocks, comparing it with the assumptions of the Markowitz model and the stylized facts of financial asset returns.
- Analyze and compare results among different optimization tools in constructing a stock portfolio.
- Analyze the effect of diversification as a tool for risk reduction in an investment portfolio.


## Summary
The objective of this study was to determine efficient stock portfolios for investment in the Lima Stock Exchange (BVL), suggesting the recommended stocks and their composition. The Markowitz Mean-Variance model was applied, and a simulation model based on copulas was proposed and solved using a global non-linear optimization solver. A statistical analysis of the selected stocks was also conducted, comparing them with the assumptions of the Markowitz model and the stylized facts of financial asset returns.

The study used 30 of the most representative stocks traded in the BVL. The estimation and optimization period spanned from July 2007 to June 2017, with an evaluation period from July 2017 to June 2020. Bloomberg L.P. software was used for data extraction, and the statistical software R was employed for analysis and optimization.

Although the Markowitz model was computationally efficient, it confirmed its main criticisms, such as the assumption of normality in returns, which is not applicable to Peruvian stock market stocks, and the sensitivity of its results. On the other hand, the simulation model allowed for the adjustment of returns using statistical distributions and provided a dependency structure through copulas, capturing skewness and kurtosis moments that are not considered in the Markowitz model. However, the drawback was its higher computational demand.

Among the various optimized objective functions, the portfolios that maximized risk-adjusted profitability indicators such as the Sharpe Ratio and Sortino Ratio were deemed more plausible. In the backtesting performed, both models yielded similar results regarding the asset selection decision. Regarding weights, the simulation model consistently generated more diversified portfolios than the Markowitz model. Nevertheless, their performance was quite similar for the majority of efficient portfolios and even superior for the portfolio aiming to maximize the Sharpe Ratio. This is favorable because having a more diversified portfolio for the same level of profitability allows for more efficient risk management.

In the out-of-sample evaluation, portfolios that optimized the Sharpe Ratio performed the best in both models. The results showed significant improvements compared to benchmark indices, which were negatively affected by the COVID-19 crisis. The simulation portfolio, in particular, outperformed the S&P/BVL index and a naive diversification portfolio in terms of both returns and risk.


## Conclusions

- The study analyzed the behavior of stock returns in the Peruvian stock market using two different methods: the Markowitz model and a simulation model. The findings revealed that stock returns in Peru do not follow a normal distribution and exhibit leptokurtosis, indicating that losses may occur more frequently than expected. The returns showed low serial correlation, indicating independence, but volatility clustering was observed during the COVID-19 pandemic, making the market more volatile.

- The results emphasized the importance of considering the correlation among stocks when constructing portfolios. While attractive stocks based on risk-adjusted profitability indicators were included in optimal portfolios, stocks with low correlation also played a role in efficient risk management. The Markowitz model only considered mean, standard deviation, and correlation, while the simulation model incorporated additional parameters like skewness and kurtosis to better reflect the behavior of stock returns.

- Both methods yielded similar results in terms of stock selection and portfolio composition, indicating their consistency. However, the simulation model provided a more accurate representation of stock return behavior by adjusting historical data to statistical distributions and modeling the dependency structure through copulas.

- In summary, the study highlighted the non-normal distribution of stock returns in Peru, the importance of considering correlation, and the effectiveness of both the Markowitz model and simulation methods in constructing efficient portfolios.

