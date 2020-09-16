from factor_analyzer import FactorAnalyzer

# Create factor analysis with loadings
def factor_analysis(df, factor_num):
    fa = FactorAnalyzer(factor_num, rotation='varimax')
    fa.fit(df)
    return fa.loadings_

# Create top 5 variables in each factor
def factor_dataframe (df, loads):
    factors = ['Factor ' + str(i) for i in range(1, (loads.shape[1] + 1))]
    fadf = pd.DataFrame(data=loads, columns=factors)
    fadf.insert(loc=0, column='VAR', value=df.columns)
    for i in range (1, (loads.shape[1] + 1)):
        factor_top5 = fadf.sort_values(by='Factor ' + str(i), ascending=False)[['VAR','Factor ' + str(i)]].iloc[:5]
        factor_top5.set_index('VAR', inplace=True) 
        print('\n')
        print(factor_top5)

# Insert prepared dataframe (must not contain single unique value in each column)
loads = factor_analysis(dummydf, 15)
factor_dataframe(dummydf, loads)