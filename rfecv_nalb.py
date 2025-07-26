###
from sklearn.feature_selection import RFECV
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from rich.progress import track
import time
import seaborn as sns

###
file_path = '/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/Data/southern_alb_model_data.csv'
# 
southern_alb_model_data = pd.read_csv(file_path)
#
X = southern_alb_model_data.iloc[:, 11:46]
y = southern_alb_model_data.iloc[:, 0]

#
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

###
import pickle
with open('rfecv_model_alb.pkl', 'wb') as file:
    pickle.dump(rfecv, file)
#
n_scores = len(rfecv.cv_results_['mean_test_score'])

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores + 1), rfecv.cv_results_['mean_test_score'])
plt.show()

#############################Second circulate
selected_features_mask1 = rfecv.support_
X_selected1 = X.iloc[:, selected_features_mask1]

plt.figure(figsize=(10, 8))
sns.heatmap(X_selected1.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.show()

#
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

###
import pickle
with open('rfecv1_model_alb.pkl', 'wb') as file:
    pickle.dump(rfecv1, file)
selected_features_mask2 = rfecv1.support_
selected_features2 = X_selected1.columns[selected_features_mask2]
#
X_selected2 = X_selected1.loc[:, selected_features_mask2]
#
print("Selected features2:", selected_features2)
import seaborn as sns
import matplotlib.pyplot as plt
#
plt.figure(figsize=(10, 8))
sns.heatmap(X_selected2.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()



#############################Third circulate
rfecv2 = RFECV(
    estimator=RandomForestRegressor(random_state=42),  #
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,  # O
    n_jobs=2
)

#
rfecv2.fit(X_selected2, y)



#
selected_features3 = X_selected2.columns[rfecv2.support_]
X_selected3 = X_selected2.loc[:, rfecv2.support_]

print("Final refined selected features:", selected_features3)




#
selected_features_mask3 = rfecv2.support_
X_selected3 = X_selected2.loc[:, rfecv2.support_]
selected_features3 = X_selected2.columns[selected_features_mask3]

#
X_selected3 = X_selected2.loc[:, selected_features_mask3]

#
print("Selected features1:", selected_features3)

import seaborn as sns
import matplotlib.pyplot as plt

#
plt.figure(figsize=(10, 8))
sns.heatmap(X_selected3.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()


# Setting up another round of RFECV with possibly different parameters or estimator
rfecv3 = RFECV(
    estimator=RandomForestRegressor(random_state=42),
    parameters or even the model itself
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,
    n_jobs=2
)
rfecv3.fit(X_selected3, y)

#
selected_features4 = X_selected3.columns[rfecv3.support_]
X_selected4 = X_selected3.loc[:, rfecv3.support_]

print("Final refined selected features:", selected_features4)



#
n_scores3 = len(rfecv3.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores3 + 1), rfecv3.cv_results_['mean_test_score'])
plt.show()




#######
selected_features2 = X_selected2.columns[rfecv2.support_]

importances2 = rfecv2.estimator_.feature_importances_
indices2 = np.argsort(importances2)[::-1]

plt.figure(figsize=(10, 6))
plt.title('Feature Importances')
plt.bar(range(X[selected_features2].shape[1]), importances2[indices2], color='b', align='center')
plt.xticks(range(X[selected_features2].shape[1]), selected_features2[indices2], rotation=90)
plt.xlim([-1, X[selected_features2].shape[1]])
plt.xlabel('Feature')
plt.ylabel('Importance')
plt.show()


import pandas as pd

feature_importances_df = pd.DataFrame({
    'Feature': selected_features2,
    'Importance': importances2
})

feature_importances_df = feature_importances_df.sort_values(by='Importance', ascending=False)

feature_importances_df.reset_index(drop=True, inplace=True)

feature_importances_df.to_csv('feature_importances_alb.csv', index=False)




###
mean_scores = rfecv.cv_results_['mean_test_score']
std_scores = rfecv.cv_results_['std_test_score']

scores_df = pd.DataFrame({
    'Step': range(1, len(mean_scores) + 1),
    'Mean Score': mean_scores,
    'Std Score': std_scores
})

scores_df.to_csv('rfecv_cv_results_alb.csv', index=False)

