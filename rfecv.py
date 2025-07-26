#导入必要的库
from sklearn.feature_selection import RFECV
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from rich.progress import track
import time
import seaborn as sns

#加载数据
file_path = '/Users/linbojun/Library/CloudStorage/OneDrive-共用文件庫－onedrive/Oceanic Fisheries Ecosystem Laboratory/ENSO_LL/Data/pacific_tuna_model_data.csv'
# 
pacific_tuna_model_data = pd.read_csv(file_path)
# 
print(pacific_tuna_model_data.head())


# 划分特征和目标变量
X = pacific_tuna_model_data.iloc[:, 11:46] # 选取第11到46列为特征变量
y = pacific_tuna_model_data.iloc[:, 0] # 选取第1列为目标变量


#初始化并训练RFECV模型
min_features_to_select = 1  # 考虑的最小特征数
rfl = RandomForestRegressor(random_state=0)  # 创建随机森林回归器实例
cv = KFold(5)  # 创建KFold交叉验证实例，分为5份

rfecv = RFECV(
    estimator=rfl,
    step=1,
    cv=cv,
    scoring="neg_mean_squared_error",  # 使用负均方误差作为评分标准
    min_features_to_select=min_features_to_select,
    n_jobs=2,
)

rfecv.fit(X, y)

print(f"Optimal number of features: {rfecv.n_features_}")



# 保存训练好的模型
import pickle

with open('rfecv_model_alb.pkl', 'wb') as file:
    pickle.dump(rfecv, file)




# Assuming 'rfecv' is your RFECV fitted model object
n_scores = len(rfecv.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores + 1), rfecv.cv_results_['mean_test_score'])
plt.show()




# Get the mask of selected features 提取和分析选择的特征
selected_features_mask1 = rfecv.support_
# Apply this mask to the original feature dataset 'X' to get the subset of selected features
X_selected1 = X.iloc[:, selected_features_mask1]

# 用热力图可视化所选特征的相关性
plt.figure(figsize=(10, 8))
sns.heatmap(X_selected1.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("所选特征的相关性矩阵")
plt.show()


# Initialize a new RFECV model with the same or different estimator
rfecv1 = RFECV(
    estimator=RandomForestRegressor(random_state=42),  # You might choose to change the random state or other parameters
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,  # You can adjust this if needed
    n_jobs=2
)

# Fit the new RFECV model on the selected features
rfecv1.fit(X_selected1, y)

# Print the optimal number of features found in the new model
print(f"Optimal number of features in the new model: {rfecv1.n_features_}")




# 保存训练好的模型
import pickle

with open('rfecv1_model_alb.pkl', 'wb') as file:
    pickle.dump(rfecv1, file)


# Extracting the boolean mask of selected features and the feature names
selected_features_mask2 = rfecv1.support_
selected_features2 = X_selected1.columns[selected_features_mask2]

# Extract the subset of the dataset with the selected features
X_selected2 = X_selected1.loc[:, selected_features_mask2]

# Displaying the names of the selected features
print("Selected features2:", selected_features2)





import seaborn as sns
import matplotlib.pyplot as plt

# Plotting correlations among selected features
plt.figure(figsize=(10, 8))
sns.heatmap(X_selected2.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()




# Setting up another round of RFECV with possibly different parameters or estimator
rfecv2 = RFECV(
    estimator=RandomForestRegressor(random_state=42),  # You can alter model parameters or even the model itself
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,  # Or adjust this to enforce a smaller set of features
    n_jobs=2
)

# Fit RFECV on the already selected features
rfecv2.fit(X_selected2, y)



# Extracting the final set of refined features
selected_features3 = X_selected2.columns[rfecv2.support_]
X_selected3 = X_selected2.loc[:, rfecv2.support_]

print("Final refined selected features:", selected_features3)




# Extracting the boolean mask of selected features and the feature names
selected_features_mask3 = rfecv2.support_
X_selected3 = X_selected2.loc[:, rfecv2.support_]
selected_features3 = X_selected2.columns[selected_features_mask3]

# Extract the subset of the dataset with the selected features
X_selected3 = X_selected2.loc[:, selected_features_mask3]

# Displaying the names of the selected features
print("Selected features1:", selected_features3)





import seaborn as sns
import matplotlib.pyplot as plt

# Plotting correlations among selected features
plt.figure(figsize=(10, 8))
sns.heatmap(X_selected3.corr(), annot=True, cmap='coolwarm', fmt=".2f")
plt.title("Correlation Matrix of Selected Features")
plt.show()




# Setting up another round of RFECV with possibly different parameters or estimator
rfecv3 = RFECV(
    estimator=RandomForestRegressor(random_state=42),  # You can alter model parameters or even the model itself
    step=1,
    cv=KFold(5),
    scoring="neg_mean_squared_error",
    min_features_to_select=1,  # Or adjust this to enforce a smaller set of features
    n_jobs=2
)

# Fit RFECV on the already selected features
rfecv3.fit(X_selected3, y)

# Extracting the final set of refined features
selected_features4 = X_selected3.columns[rfecv3.support_]
X_selected4 = X_selected3.loc[:, rfecv3.support_]

print("Final refined selected features:", selected_features4)



# Assuming 'rfecv' is your RFECV fitted model object
n_scores3 = len(rfecv3.cv_results_['mean_test_score'])  # Updated attribute name

plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross-validation score (NB: negative mean squared error)")
plt.plot(range(1, n_scores3 + 1), rfecv3.cv_results_['mean_test_score'])
plt.show()




#######
# 获取选择的特征
selected_features2 = X_selected2.columns[rfecv2.support_]

# 获取特征重要性并进行排序
importances2 = rfecv2.estimator_.feature_importances_
indices2 = np.argsort(importances2)[::-1]

# 绘制特征重要性图
plt.figure(figsize=(10, 6))
plt.title('Feature Importances')
plt.bar(range(X[selected_features2].shape[1]), importances2[indices2], color='b', align='center')
plt.xticks(range(X[selected_features2].shape[1]), selected_features2[indices2], rotation=90)
plt.xlim([-1, X[selected_features2].shape[1]])
plt.xlabel('Feature')
plt.ylabel('Importance')
plt.show()


import pandas as pd

# Assuming 'selected_features' is the list of feature names
# and 'importances' is the array of importance scores from the previous steps

# Create a DataFrame with the feature importances
feature_importances_df = pd.DataFrame({
    'Feature': selected_features2,
    'Importance': importances2
})

# Sort the DataFrame based on importance scores
feature_importances_df = feature_importances_df.sort_values(by='Importance', ascending=False)

# Reset index to have a clean dataframe
feature_importances_df.reset_index(drop=True, inplace=True)

# Save the DataFrame to a CSV file
feature_importances_df.to_csv('feature_importances_alb.csv', index=False)





# 对于新版本的scikit-learn，请使用cv_results_获取得分
mean_scores = rfecv.cv_results_['mean_test_score']
std_scores = rfecv.cv_results_['std_test_score']

# 创建一个包含平均得分和标准差的 DataFrame
scores_df = pd.DataFrame({
    'Step': range(1, len(mean_scores) + 1),
    'Mean Score': mean_scores,
    'Std Score': std_scores
})

# 将 DataFrame 导出为 CSV 文件
scores_df.to_csv('rfecv_cv_results_alb.csv', index=False)

