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
MiniBooNE is an experiment at Fermilab designed to observe neutrino oscillations (BooNE is an acronym for the Booster Neutrino Experiment). A neutrino beam consisting primarily of muon neutrinos is directed at a detector filled with 800 tons of mineral oil (ultrarefined methylene compounds) and lined with 1,280 photomultiplier tubes.[1] An excess of electron neutrino events in the detector would support the neutrino oscillation interpretation of the LSND (Liquid Scintillator Neutrino Detector) result.

MiniBooNE started collecting data in 2002[2] and was still running in 2017.[3]


Experimental observation of solar neutrinos and atmospheric neutrinos provided evidence for neutrino oscillations, implying that neutrinos have masses. Data from the LSND experiment at Los Alamos National Laboratory are controversial since they are not compatible with the oscillation parameters measured by other neutrino experiments in the framework of the Standard Model. Either there must be an extension to the Standard Model, or one of the experimental results must have a different explanation. Moreover, the KARMEN experiment in Karlsruhe[4] examined a [low energy] region similar to the LSND experiment, but saw no indications of neutrino oscillations. This experiment was less sensitive than LSND, and both could be right.

Cosmological data can provide an indirect but rather model-dependent bound to the mass of sterile neutrinos, such as the ms < 0.26 eV (0.44 eV) at 95% (99.9%) confidence limit given by Dodelson et al..[5] However, cosmological data can be accommodated within models with different assumptions, such as that by Gelmini et al.[6]

MiniBooNE was designed to unambiguously verify or refute the LSND controversial result in a controlled environment.

2007
After the beam was turned on in 2002, the first results came in late March 2007, and showed no evidence for muon neutrino to electron neutrino oscillations in the LSND [low energy] region, refuting a simple 2-neutrino oscillation interpretation of the LSND results.[7] More advanced analyses of their data are currently being undertaken by the MiniBooNE collaboration; early indications are pointing towards the existence of the sterile neutrino,[8] an effect interpreted by some physicists to be hinting of the existence of the bulk[9] or Lorentz violation.[10]
2008
Some members of MiniBooNE have formed a new collaboration with outside scientists and proposed a new experiment (called MicroBooNE) designed to further investigate this.[11]
2018
With a study published on arXiv,[3] the Collaboration announced that the finding of neutrino oscillations at MiniBooNE are confirmed at a 4.8 sigma level and, when combined with data at LSND, at a 6.1 sigma level. This hints at the detection of sterile neutrinos and a significant deviation from known physics.[12] The implication of the paper is that some of the muon neutrinos are flipping to sterile neutrinos before switching identity again to electron neutrinos.[13]

[Publication](https://arxiv.org/abs/1702.02688)

[Wikipedia](https://en.wikipedia.org/wiki/MiniBooNE#:~:text=MiniBooNE%20is%20an%20experiment%20at,for%20the%20Booster%20Neutrino%20Experiment.&text=An%20excess%20of%20electron%20neutrino,Liquid%20Scintillator%20Neutrino%20Detector)%20result.)

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

### Ranked Choice

- Allen: MiniBooNE  LOL  DOTA2  Bankruptcy  Stocks  Leaves


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
