
df <- readr::read_csv("blood_transfusion.csv")
class(df)
sum(is.na(df))
dim(df)
head(df, 10)
tail(df, 10)
df[100, "Monetary"]
mean(df[['Monetary']])
above_avg <- df[['Monetary']] > mean(df[['Monetary']])
df[above_avg, 'Monetary']

df <- readr::read_csv('lab2_data/PDI__Police_Data_Initiative__Crime_Incidents.csv')
dim(df)
sort(colSums(is.na(df)))
sum(is.na(df))
range(df[['DATE_REPORTED']])
sort(table(df[['SUSPECT_AGE']]))
sort(table(df['ZIP']), decreasing = TRUE)
sort(table(df[['DAYOFWEEK']]))
sort(table(df[['DAYOFWEEK']])) / sum(table(df[['DAYOFWEEK']]))
