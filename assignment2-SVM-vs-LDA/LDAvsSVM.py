import csv
import pandas as pd
import numpy as np
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn import svm
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt

#import CSV training data
try:
    with open("avila-tr.txt", newline='') as fobj:
        avila_tr = csv.reader(fobj)

        #Header is added to training_set.csv
        with open ('training_set.csv', 'w', newline='') as train_set:
            train_set_w = csv.writer(train_set)
            train_set_w.writerow(['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class'])
            for line in avila_tr:
                train_set_w.writerow(line)
except FileNotFoundError:
    avila_tr = None

try:
    with open("training_set.csv") as fobj:
        avila_tr_new = csv.reader(fobj)

except FileNotFoundError:
    avila_tr_new = None

#training data feature and target dataframe is created:
column_names = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class']
target = pd.read_csv("training_set.csv", usecols = ['Class'])
targetValues = target['Class'].values
features = pd.read_csv("training_set.csv", usecols = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing'])
X = np.array(features)
y = np.array(targetValues)

#import CSV test data
try:
    with open("avila-ts.txt", newline='') as fobj:
        avila_ts = csv.reader(fobj)

        #Header is added to testing_set.csv
        with open ('testing_set.csv', 'w', newline='') as test_set:
            test_set_w = csv.writer(test_set)
            test_set_w.writerow(['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class'])
            for line in avila_ts:
                test_set_w.writerow(line)
except FileNotFoundError:
    avila_ts = None

try:
    with open("testing_set.csv") as fobj:
        avila_ts_new = csv.reader(fobj)
except FileNotFoundError:
    avila_ts_new = None

#testing data feature and target dataframe is created:
column_names = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class']
target_ts = pd.read_csv("testing_set.csv", usecols = ['Class'])
targetValues_ts = target_ts['Class'].values
features_ts = pd.read_csv("testing_set.csv", usecols = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing'])
X_ts = np.array(features_ts)
y_ts = np.array(targetValues_ts)


###LDA####################################################################################
###TRAINING
X_new = X
clf_lda_svd = LinearDiscriminantAnalysis(solver='svd',n_components=2)
Xtrans = clf_lda_svd.fit(X_new, y).transform(X_new)
clf_lda_lsqr = LinearDiscriminantAnalysis(solver='lsqr',n_components=10)
clf_lda_lsqr.fit(X, y)

###TESTING
print('Different LDA Models and their accuracies')
print('LDA lsqr accuracy using model.score      :',clf_lda_lsqr.score(X_ts,y_ts),'\n')
print('LDA svd accuracy using model.score       :',clf_lda_svd.score(X_ts,y_ts))
y_pred=clf_lda_svd.predict(X_ts)
print('LDA accuracy using metrics.accuracy_score:',accuracy_score(y_ts, y_pred),'\n')


lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'turquoise', 'darkorange', 'blue' , 'dimgrey', 'pink', 'green', 'red', 'black', 'yellow', 'plum', 'gold']
for color, i in zip(colors, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'W', 'X', 'Y']):
    plt.scatter(X[y == i, 0], X[y == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('LDA projection of AVILA dataset (n_components = 2)')
lda_fig.savefig('1_LDA_on_Avila.png')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'turquoise', 'darkorange', 'blue' , 'dimgrey', 'pink', 'green', 'red', 'black', 'yellow', 'plum', 'gold']
for color, i in zip(colors, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'W', 'X', 'Y']):
    plt.scatter(Xtrans[y == i, 0], Xtrans[y == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('2D transformed LDA projection of AVILA dataset')
lda_fig.savefig('2_LDA_on_Avila_2D_reduction_using_transform.png')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'red', 'yellow', 'green']
for color, i in zip(colors, ['A', 'E', 'F', 'I']):
    plt.scatter(Xtrans[y == i, 0], Xtrans[y == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('2D transformed LDA projection of AVILA dataset_4_classes')
lda_fig.savefig('3_LDA_on_Avila_2D_reduction_4_classes.png')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'red', 'yellow', 'green']
for color, i in zip(colors, ['D', 'G', 'H', 'X']):
    plt.scatter(Xtrans[y == i, 0], Xtrans[y == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('2D transformed LDA projection of AVILA dataset_4_classes')
lda_fig.savefig('4_LDA_on_Avila_2D_reduction_4_classes.png')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'red', 'yellow', 'green']
for color, i in zip(colors, ['B', 'C', 'W', 'Y']):
    plt.scatter(Xtrans[y == i, 0], Xtrans[y == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('2D transformed LDA projection of AVILA dataset_4_classes')
lda_fig.savefig('5_LDA_on_Avila_2D_reduction_4_classes.png')

###SVM####################################################################################
###TRAINING
clf_svm_rbf = svm.SVC(kernel='rbf',gamma='auto', decision_function_shape='ovo')
clf_svm_rbf.fit(X, y)
clf_svm_sig = svm.SVC(kernel='sigmoid',gamma='auto', decision_function_shape='ovo')
clf_svm_sig.fit(X, y)
clf_svm_gama_scale = svm.SVC(kernel='rbf',gamma='scale', decision_function_shape='ovo')
clf_svm_gama_scale.fit(X, y)

###TESTING
print('Different SVM Models and their accuracies')
print('SVM accuracy using model.score rbf kernel      :',clf_svm_rbf.score(X_ts,y_ts))
y_pred=clf_svm_rbf.predict(X_ts)
print('SVM accuracy using metrics.accuracy_score      :',accuracy_score(y_ts, y_pred),'\n')
print('SVM accuracy using sigmoid kernel              :',clf_svm_sig.score(X_ts,y_ts))
print('SVM accuracy using rbf kernel and gamma = scale:',clf_svm_gama_scale.score(X_ts,y_ts),'\n')


###Calculating feature column variances to obtain 2 features having highest variance to generate 2D plots for LDM and SVM
print('Variances of the features')
print(features.var(axis=0),'\n')
print('"upper margin" and "interlinear spacing" have highest variance')
try:
    with open("training_set.csv") as fobj:
        avila_tr_new = csv.reader(fobj)

except FileNotFoundError:
    avila_tr_new = None

#creating 2D dataframe from training set
column_names = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class']
target_2d = pd.read_csv("training_set.csv", usecols = ['Class'])
targetValues_2d = target_2d['Class'].values
features_2d = pd.read_csv("training_set.csv", usecols = ['upper margin', 'interlinear spacing'])

try:
    with open("testing_set.csv") as fobj:
        avila_ts_new = csv.reader(fobj)

except FileNotFoundError:
    avila_ts_new = None

#creating 2D dataframe from testing set
column_names = ['intercolumnar distance', 'upper margin', 'lower margin', 'exploitation', 'row number', 'modular ratio', 'interlinear spacing', 'weight', 'peak number', 'modular ratio/ interlinear spacing', 'Class']
target_2d_ts = pd.read_csv("testing_set.csv", usecols = ['Class'])
targetValues_2d_ts = target_2d_ts['Class'].values
features_2d_ts = pd.read_csv("testing_set.csv", usecols = ['upper margin', 'interlinear spacing'])

#LDA projection of the 2D data
X_2d = np.array(features_2d)
y_2d = np.array(targetValues_2d)
X_2d_ts = np.array(features_2d_ts)
y_2d_ts = np.array(targetValues_2d_ts)
clf_lda_2d = LinearDiscriminantAnalysis(solver='svd',n_components=10)
X2dtrans = clf_lda_2d.fit(X_2d, y_2d).transform(X_2d)
print('LDA_2d accuracy:',clf_lda_2d.score(X_2d_ts,y_2d_ts),'\n')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'turquoise', 'darkorange', 'blue' , 'dimgrey', 'pink', 'green', 'red', 'black', 'yellow', 'plum', 'gold']
for color, i in zip(colors, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'W', 'X', 'Y']):
    plt.scatter(X_2d_ts[y_2d_ts == i, 0], X_2d_ts[y_2d_ts == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 4);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('2 features Plot')
lda_fig.savefig('6_Plot_of_2_features.png')

lda_fig = plt.figure(figsize=(18,12))
colors = ['navy', 'turquoise', 'darkorange', 'blue' , 'dimgrey', 'pink', 'green', 'red', 'black', 'yellow', 'plum', 'gold']
for color, i in zip(colors, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'W', 'X', 'Y']):
    plt.scatter(X2dtrans[y_2d == i, 0], X2dtrans[y_2d == i, 1], color=color, alpha=.8, label=i, s = 18)
plt.ylim(0, 1.5);
plt.xlim(-4, 4);
plt.legend(loc='best', shadow=False, scatterpoints=1)
plt.title('LDA considering 2 features')
lda_fig.savefig('7_LDA_on_2_features.png')

###SVM DB when 2 dimensions are considered
clf_svm_2d = svm.SVC(kernel='rbf',gamma='auto', decision_function_shape='ovr')
clf_svm_2d.fit(X_2d, y_2d)

###TESTING
Z = clf_svm_2d.predict(X_2d_ts)
print('SVM 2d accuracy:',accuracy_score(y_2d_ts, Z),'\n')
