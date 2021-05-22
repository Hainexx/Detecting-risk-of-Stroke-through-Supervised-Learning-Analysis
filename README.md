# Statistical Learning Projects Series 
## Worked Out 

### 23/05 Commit by Gaspare:
- Ho fatto minimi cambiamenti nella data preparation 
- Data Viz
- invece di rimuovere tutte le osservazione senza dati sul BMI, ho usato un Decision Tree per predirre il valore del loro BMI in base a tutti gli altri dati. 
- Per semplificare l'interpretazione dei dati ho codificato tutte le variabili categoriche con OneHotEncoder 
- Il più grande problema di questi dati è che la classificazione d'interesse è un "evento raro", meno del 5% dei nostri dati infatti sono classificati positivamente. Per ovviare a questo problema ho usato la SMOTE (Synthetic Minority Over-sampling Technique) per bilanciare il dataset con dati sintetici. Questo riduce drasticamente il rischio di bias.
- Ho invertito la Confusion Matrix perchè impostava i negativi come positivi e viceversa rendendo le metriche di Recall e Precision (e conseguentemente F1) completamente sbagliate.
- Ho aggiunto un altro algoritmo un po' più BlackBox (ci tornerò su) che performa bene uguale ma con le metriche opposte. 



## TODO:
- Cercare di interpretare i risultati per fare inferenza
- Fare Visualizations migliori e più esplicative. 
- Provare ad implementare una qualche forma di feature selection.
- implementare altri tipi algoritmi e compararne le performance. 

