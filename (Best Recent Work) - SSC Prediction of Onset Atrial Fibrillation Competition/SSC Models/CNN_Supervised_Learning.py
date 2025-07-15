import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Conv1D, MaxPooling1D, Flatten, Dense
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, accuracy_score, recall_score

# CNN model creation
def create_model(input_shape, optimizer='adam', filters=8, kernel_size=5):
    model = Sequential([
        Conv1D(filters=filters, kernel_size=kernel_size, activation='relu', input_shape=input_shape),
        MaxPooling1D(pool_size=2),
        Flatten(),
        Dense(2, activation='relu'),
        Dense(1, activation='sigmoid')
    ])
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    return model

# Store all results
all_results = []

# Dataset indices
for i in [1, 2, 3]:
    # Load datasets
    train = pd.read_csv(f"reduced_over_data{i}.csv")
    test = pd.read_csv(f"reduced_test_data{i}.csv")
    valid = pd.read_csv(f"valid_data{i}.csv")

    # Split predictors and responses
    y_train = train['outcome_afib_aflutter_new_post'].values
    X_train = train.drop(columns=['outcome_afib_aflutter_new_post'])

    y_test = test['outcome_afib_aflutter_new_post'].values
    y_valid = valid['outcome_afib_aflutter_new_post'].values

    # Align test and valid sets to training columns
    train_cols = X_train.columns
    X_test = test.reindex(columns=train_cols, fill_value=0)
    X_valid = valid.reindex(columns=train_cols, fill_value=0)

    # Scale data
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)
    X_valid_scaled = scaler.transform(X_valid)

    # Reshape for CNN input
    X_train_scaled = X_train_scaled.reshape((X_train_scaled.shape[0], X_train_scaled.shape[1], 1))
    X_test_scaled = X_test_scaled.reshape((X_test_scaled.shape[0], X_test_scaled.shape[1], 1))
    X_valid_scaled = X_valid_scaled.reshape((X_valid_scaled.shape[0], X_valid_scaled.shape[1], 1))

    # Train CNN model
    model = create_model(input_shape=(X_train_scaled.shape[1], 1))
    model.fit(X_train_scaled, y_train, epochs=10, verbose=0)

    # Predict probabilities
    y_proba_test = model.predict(X_test_scaled)
    y_pred_test = (y_proba_test > 0.5).astype(int)

    y_proba_valid = model.predict(X_valid_scaled)
    y_pred_valid = (y_proba_valid > 0.5).astype(int)

    # Compute metrics
    results = {
        'auc_test': roc_auc_score(y_test, y_proba_test),
        'auc_valid': roc_auc_score(y_valid, y_proba_valid),
        'accuracy_test': accuracy_score(y_test, y_pred_test),
        'accuracy_valid': accuracy_score(y_valid, y_pred_valid),
        'recall_test': recall_score(y_test, y_pred_test),
        'recall_valid': recall_score(y_valid, y_pred_valid)
    }

    all_results.append(results)

# Compile results
results_df = pd.DataFrame(all_results)

# Show results
print("\nIndividual Results:")
print(results_df.round(4))

print("\nAggregated (Mean ± Std):")
summary = results_df.agg(['mean', 'std']).round(4)
print(summary)
