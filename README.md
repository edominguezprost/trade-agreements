# trade-agreements
R files to replicate Dadush &amp; Dominguez Prost (2023) (https://www.cambridge.org/core/journals/world-trade-review/article/preferential-trade-agreements-geopolitics-and-the-fragmentation-of-world-trade/788BA2CB330A716DC130B75CDB2016E8)

There are five R replication codes, mainly used to estimate results in Tables 2, 4, and 6.
  
The order is not random: 
a.	Firstly, we need to start running “1. Dataset Agreements – Code”
    This file takes the raw data from Kellogg Institute and updates it by considering the free trade agreements signed between 2017 and 2020
b.	Secondly, we run “2. Bilateral Trade – Code”, which downloads trade data and then merges it with the agreement dataset
c.	Thirdly, we can open “3. Main Estimations” to get general estimations but also Tables 2, 4, and 6
d.	Tables 1 and 5 are constructed using data from WTO (File “wto_rawdata”) for the agreements and R codes “4. Share of World Trade - Code” “5. Additionality Potential Agreements” to complete the columns in the table


