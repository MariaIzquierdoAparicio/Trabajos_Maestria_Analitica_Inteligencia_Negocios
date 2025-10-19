import pandas as pd
import numpy as np
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from datetime import datetime, timedelta
from Correlacion import CorrelationFilter

class DataCleaner:
    def __init__(self, excel_base_date):
        self.excel_base_date = excel_base_date

    def excel_to_date(self, excel_date):
        return self.excel_base_date + timedelta(days=excel_date - 2)

    def convert_to_category(self, df, columns):
        for col in columns:
            df[col] = df[col].astype('category')

    def one_hot_encode(self, df):
        return pd.get_dummies(df, drop_first=True)

    def clean(self, df):
        # Convertir columnas a categorías
        columns_to_convert = ['From.Grade', 'To.Grade', 'Is.Non.Annual.', 'Parent.Meeting.Flag',
                              'Days', 'CRM.Segment', 'MDR.High.Grade', 'School.Sponsor',
                              'NumberOfMeetingswithParents', 'SingleGradeTripFlag']
        self.convert_to_category(df, columns_to_convert)

        # Cambiar formato de fechas
        df['Departure.Date'] = df['Departure.Date'].apply(self.excel_to_date)
        df['Return.Date'] = df['Return.Date'].apply(self.excel_to_date)
        df['Deposit.Date'] = df['Deposit.Date'].apply(self.excel_to_date)

        # Correlación
        pipeline = Pipeline([
            ('correlation_filter', CorrelationFilter(threshold=0.6, additional_drop_cols=['Special.Pay', 'EZ.Pay.Take.Up.Rate', 'SchoolGradeType'])),
        ])
        df = pipeline.fit_transform(df)

        # Limpieza de valores faltantes
        cat_imputer = SimpleImputer(missing_values = float('nan'), strategy='constant')
        cat_cols = df.select_dtypes(exclude=['int64','float64']).columns
        df[cat_cols] = cat_imputer.fit_transform(df[cat_cols])

        num_imputer = SimpleImputer(missing_values = float('nan'), strategy='median')
        num_cols = df.select_dtypes(include=['int64','float64']).columns
        df[num_cols] = num_imputer.fit_transform(df[num_cols])

        # Codificación one-hot
        df = self.one_hot_encode(df)

        return df
