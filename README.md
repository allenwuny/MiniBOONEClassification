# This is the blueprint for out 9891 project 




## Project Tentaive Schedule 
- **November 10/11** Choose a dataset
- **November 11th - 20th** Complete Majority of project code 
- **November 20th - 27th** Start building out slides from our code output 
- **November 27th - 30th** Complete Our Project and The Recording 

## Dataset Explanations

### League of Legends/DOTA2
Both of these are MOBA (multiplayer online battle arena) games. There are 2 teams, where each team consists of 5 players. Each player chooses a unique hero/champion to play and the first team to destroy their opponent's base wins. The goal of this classification project is to see if we can use team composition to predict which team will win the game. There are tournaments held for these games that can have up to millions of dollars in prize money. 

#### Pro 
- Fairly easy to work with and I fitted the code from the HW to this dataset already.
- Balanced dataset so no need to worry about weights

#### Con
- With a preliminary regression, I get a test error of 46%. Not sure if this is a great dataset, but can be explained as part of the presentation. 

#### Source
- DOTA2 data source: https://archive.ics.uci.edu/ml/datasets/Dota2+Games+Results
- League of Legends dataset source: https://www.kaggle.com/datasnaek/league-of-legends

### Stocks
This dataset is separated into 3 groups, banks, insurance, and other companies financial statements. It will provide each line item of the respective financial statement as a feature for the observation (a company for a specific quarter for a specific year). The goal of this classification is to see if we can use a financial statement from a specific quarter to determine if a company is undervalued or overvalued. The valuation can be determined by looking at the next quarter to see if the market cap increased or decreased. A more logical approach is to use the quarter-to-quarter changes for each of these features to classify if the company is undervalued or overvalued. 

#### Pro
- Interesting problem to tackle, can potentially have real life usage to find undervalued companies

#### Con
- There are missing data for some of the features which makes it more difficult to create an accurate model
- Coding to solve the listed goal above will be somewhat challenging as we will need to calculate differences between quarters and apply the correct labels to the observations

#### Source
https://simfin.com/data/bulk

### Leaves
This dataset actaully comes from a paper looking at the problem of classifying a plant by the leaves it produces. There are a total of 100 species of plants and each plant has 16 samples. Each sample has 3 features (shape signature, an interior texture feature histogram, and a ﬁne-scale margin feature histogram) where each feature is broken into a 64 element vector. 

#### Pro
- Looks like a standard classification problem based on images of plant leaves
- Balanced dataset so no need to worry about weights

#### Con
- Since there's already a paper on it, specifically using the dataset for a classification problem, not sure if it is suitable. Prof may reject it, or if we may apply incorrect analysis to it. 

#### Source
- https://archive.ics.uci.edu/ml/datasets/One-hundred+plant+species+leaves+data+set
- https://www.researchgate.net/publication/266632357_Plant_Leaf_Classification_using_Probabilistic_Integration_of_Shape_Texture_and_Margin_Features

### MiniBooNE
This dataset is about particle identification between electron neutrinos and muon neutrinos. This dataset has over 130K observations with 50 features. 

#### Pro
- Fairly straight forward classification dataset between the 2 types of neutrinos. 

#### Con
- I do not know much about the MiniBooNE experiment
- Imbalanced dataset between electron vs muon (36499 vs 93565)

#### Source
- https://archive.ics.uci.edu/ml/datasets/MiniBooNE+particle+identification

### Bankruptcy
This dataset looks at bankrupct Polish companies and provides their financial statements. 

#### Pro
- Interesting problem to tackle, simliar to the stocks dataset to see if we can find companies that will file for bankruptcy (overvalued)

#### Con
- Companies are based in Poland, so not much relevance to the USA
- The dataset looks a little complicated to work with, may take some time during the data cleaning/wrangling portion

#### Source
- https://archive.ics.uci.edu/ml/datasets/Polish+companies+bankruptcy+data

### HERE WE POST POTENTIAL CODE 

- [A Link](http://google.com)
- [A Link](http://google.com)
- [A Link](http://google.com)
- [A Link](http://google.com)



### Or Add The API Snipet 

```python 
for i in hello: 
  print(hi) 


``` 
