# Statistical Learning Projects Series 
## Worked Out 

### 23/05 Commit by Gaspare:
- Made minimal changes in the data preparation 
- Data Viz
- instead of removing all observations without BMI data, I used a Decision Tree to predict the value of their BMI based on all other data. 
- To simplify the interpretation of the data, I coded all categorical variables with OneHotEncoder. 
- The biggest problem with this data is that the classification of interest is a "rare event", less than 5% of our data in fact are positively classified. To overcome this problem, I used SMOTE (Synthetic Minority Over-sampling Technique) to balance the dataset with synthetic data. This drastically reduces the risk of bias.
- I reversed the Confusion Matrix because it was setting negatives as positives and vice versa making the Recall and Precision (and consequently F1) metrics completely wrong.
- I added another slightly algorithm, a bit more BlackBox (I'll get back to that) that performs equally well but with the opposite metrics. 



## TODO:
- Try to interpret the results to make inference.
- Make better and explanatory visualisations. 
- Try to implement some kind of feature selection.
- Implement other types of algorithms and compare performance. 

