from sklearn.base import BaseEstimator, TransformerMixin

class CorrelationFilter(BaseEstimator, TransformerMixin):
    def __init__(self, threshold=0.6, additional_drop_cols=None):
        self.threshold = threshold
        self.additional_drop_cols = additional_drop_cols

    def fit(self, X, y=None):
        correlation_matrix = X.corr()
        correlated_pairs = []
        for i in range(len(correlation_matrix.columns)):
            for j in range(i+1, len(correlation_matrix.columns)):
                if abs(correlation_matrix.iloc[i, j]) > self.threshold:
                    correlated_pairs.append((correlation_matrix.columns[i], correlation_matrix.columns[j]))

        variables_to_drop = set()
        for var1, var2 in correlated_pairs:
            if var1 < var2:
                variables_to_drop.add(var2)
            else:
                variables_to_drop.add(var1)

        if self.additional_drop_cols:
            variables_to_drop.update(self.additional_drop_cols)

        self.variables_to_drop_ = variables_to_drop

        return self

    def transform(self, X):
        X_transformed = X.drop(columns=self.variables_to_drop_)
        return X_transformed

class CleanMissingDataTransformer(BaseEstimator, TransformerMixin):
    def __init__(self):
        pass

    def fit(self, X, y=None):
        return self

    def transform(self, X):

        cat_imputer = SimpleImputer(missing_values = float('nan'), strategy='constant', fill_value='missing_value')
        cat_cols = X.select_dtypes(exclude=['int64','float64']).columns
        X[cat_cols] = cat_imputer.fit_transform(X[cat_cols])

        num_imputer = SimpleImputer(missing_values = float('nan'), strategy='median')
        num_cols = X.select_dtypes(include=['int64','float64']).columns
        X[num_cols] = num_imputer.fit_transform(X[num_cols])

        return X
