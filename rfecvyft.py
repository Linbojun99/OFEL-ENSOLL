from sklearn.feature_selection import RFECV
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from rich.progress import track
import time
import seaborn as sns

file_path = '/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/Data/pacific_tuna_model_data.csv'
# 
pacific_tuna_model_data = pd.read_csv(file_path)
# 
print(pacific_tuna_model_data.head())


X = pacific_tuna_model_data.iloc[:, 11:46]
y = pacific_tuna_model_data.iloc[:, 2]


min_features_to_select = 1
rfl = RandomForestRegressor(random_state=0)
cv = KFold(5)
rfecv = RFECV(
    estimator=rfl,
    step=1,
    cv=cv,
    scoring="neg_mean_squared_error",
    min_features_to_select=min_features_to_select,
    n_jobs=2,
)

rfecv.fit(X, y)

print(f"Optimal number of features: {rfecv.n_features_}")

import pickle

with open('rfecv_model_yft1.pkl', 'wb') as file:
    pickle.dump(rfecv, file)

pickle.dump(rfecv, open("rfecv_model_yft1.pickle.dat", "wb"))

n_scores = len(rfecv.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores + 1), rfecv.cv_results_['mean_test_score'])
plt.show()


selected_features_mask1 = rfecv.support_

X_selected1 = X.iloc[:, selected_features_mask1]

plt.figure(figsize=(10, 8))
sns.heatmap(X_selected1.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("所选特征的相关性矩阵")
plt.show()

rfecv1 = RFECV(
    estimator=RandomForestRegressor(random_state=42),
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,
    n_jobs=2
)

rfecv1.fit(X_selected1, y)

print(f"Optimal number of features in the new model: {rfecv1.n_features_}")

import pickle

with open('rfecv1_model_yft.pkl', 'wb') as file:
    pickle.dump(rfecv1, file)

selected_features_mask2 = rfecv1.support_
selected_features2 = X_selected1.columns[selected_features_mask2]

X_selected2 = X_selected1.loc[:, selected_features_mask2]

print("Selected features2:", selected_features2)

import seaborn as sns
import matplotlib.pyplot as plt

plt.figure(figsize=(10, 8))
sns.heatmap(X_selected2.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()


rfecv2 = RFECV(
    estimator=RandomForestRegressor(random_state=42),
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,
    n_jobs=2
)

rfecv2.fit(X_selected2, y)

print(f"Optimal number of features in the new model: {rfecv2.n_features_}")

import pickle

with open('rfecv2_model_yft.pkl', 'wb') as file:
    pickle.dump(rfecv2, file)

import pickle
with open('rfecv_model_yft3.pkl', 'wb') as file:
    pickle.dump(rfecv2, file)

n_scores2 = len(rfecv2.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores2 + 1), rfecv2.cv_results_['mean_test_score'])
plt.show()

selected_features3 = X_selected2.columns[rfecv2.support_]
X_selected3 = X_selected2.loc[:, rfecv2.support_]

print("Final refined selected features:", selected_features3)

selected_features_mask3 = rfecv2.support_
X_selected3 = X_selected2.loc[:, rfecv2.support_]
selected_features3 = X_selected2.columns[selected_features_mask3]

X_selected3 = X_selected2.loc[:, selected_features_mask3]

print("Selected features1:", selected_features3)





import seaborn as sns
import matplotlib.pyplot as plt

plt.figure(figsize=(10, 8))
sns.heatmap(X_selected3.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()
rfecv3 = RFECV(
    estimator=RandomForestRegressor(random_state=42),
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,
    n_jobs=2
)

rfecv3.fit(X_selected3, y)

selected_features4 = X_selected3.columns[rfecv3.support_]
X_selected4 = X_selected3.loc[:, rfecv3.support_]

print("Final refined selected features:", selected_features4)



n_scores3 = len(rfecv3.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores3 + 1), rfecv3.cv_results_['mean_test_score'])
plt.show()


selected_features = X.columns[rfecv.support_]

importances = rfecv.estimator_.feature_importances_
indices = np.argsort(importances)[::-1]

plt.figure(figsize=(10, 6))
plt.title('Feature Importances')
plt.bar(range(X[selected_features].shape[1]), importances[indices], color='b', align='center')
plt.xticks(range(X[selected_features].shape[1]), selected_features[indices], rotation=90)
plt.xlim([-1, X[selected_features].shape[1]])
plt.xlabel('Feature')
plt.ylabel('Importance')
plt.show()




import pandas as pd

feature_importances_df = pd.DataFrame({
    'Feature': selected_features,
    'Importance': importances
})

feature_importances_df = feature_importances_df.sort_values(by='Importance', ascending=False)

feature_importances_df.reset_index(drop=True, inplace=True)

feature_importances_df.to_csv('feature_importances_yft.csv', index=False)


mean_scores = rfecv.cv_results_['mean_test_score']
std_scores = rfecv.cv_results_['std_test_score']

scores_df = pd.DataFrame({
    'Step': range(1, len(mean_scores) + 1),
    'Mean Score': mean_scores,
    'Std Score': std_scores
})

scores_df.to_csv('rfecv_cv_results_yft.csv', index=False)



mean_scores2 = rfecv2.cv_results_['mean_test_score']
std_scores2 = rfecv2.cv_results_['std_test_score']

scores_df2 = pd.DataFrame({
    'Step': range(1, len(mean_scores2) + 1),
    'Mean Score': mean_scores2,
    'Std Score': std_scores2
})

scores_df2.to_csv('rfecv2_cv_results_yft.csv', index=False)
