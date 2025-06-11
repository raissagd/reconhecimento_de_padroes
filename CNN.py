# ====================================================================
# CNN para MNIST – Exercício de Reconhecimento de Padrões
# Prof. Frederico Coelho, 23/11/2022
# ====================================================================

import tensorflow as tf
from tensorflow.keras import layers, models, callbacks
from tensorflow.keras.datasets import mnist
from tensorflow.keras.utils import to_categorical
import matplotlib.pyplot as plt

# 1) Carregar e pré‐processar MNIST
(x_train, y_train), (x_test, y_test) = mnist.load_data()
x_train = x_train.reshape(-1,28,28,1).astype('float32') / 255.0
x_test  = x_test.reshape(-1,28,28,1).astype('float32') / 255.0
y_train = to_categorical(y_train, 10)
y_test  = to_categorical(y_test, 10)

# 2) Hiperparâmetros
DROPOUT1 = 0.25
DROPOUT2 = 0.5
EPOCHS   = 12
BATCH    = 128

# 3) Construir o modelo
model = models.Sequential([
    layers.Conv2D(8, (5,5), activation='relu', padding='same', input_shape=(28,28,1)),
    layers.MaxPooling2D((2,2)),
    layers.Conv2D(16, (5,5), activation='relu', padding='same'),
    layers.MaxPooling2D((2,2)),
    layers.Conv2D(32, (5,5), activation='relu', padding='same'),
    layers.MaxPooling2D((2,2)),
    layers.Conv2D(64, (5,5), activation='relu', padding='same'),
    layers.MaxPooling2D((2,2)),
    layers.Dropout(DROPOUT1),
    layers.Flatten(),
    layers.Dense(4, activation='relu'),
    layers.Dropout(DROPOUT2),
    layers.Dense(10, activation='softmax')
])

# 4) Compilar
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# 5) Callback de parada antecipada
early_stop = callbacks.EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

# 6) Treinar e guardar histórico
history = model.fit(
    x_train, y_train,
    epochs=EPOCHS,
    batch_size=BATCH,
    validation_split=0.1,
    callbacks=[early_stop],
    verbose=2
)

# 7) Avaliar no teste
test_loss, test_acc = model.evaluate(x_test, y_test, verbose=0)
print(f'\n[Teste] Loss: {test_loss:.4f}  —  Accuracy: {test_acc:.4f}')

# --------------------------------------------------
# 8) Plot das acurácias de Treino vs Validação
# --------------------------------------------------
plt.figure(figsize=(6,4))
plt.plot(history.history['accuracy'],   label='Treino')
plt.plot(history.history['val_accuracy'], label='Validação')
plt.title('Acurácia de Treino e Validação')
plt.xlabel('Época')
plt.ylabel('Acurácia')
plt.legend()
plt.tight_layout()
plt.show()

# --------------------------------------------------
# 9) Heatmap único dos filtros da 1ª camada
# --------------------------------------------------
filters, biases = model.layers[0].get_weights()  # pesos da primeira Conv2D
n_filters = filters.shape[3]
n_cols    = 4
n_rows    = (n_filters + n_cols - 1) // n_cols

fig, axes = plt.subplots(n_rows, n_cols, figsize=(n_cols*2.5, n_rows*2.5))
axes = axes.flatten()
for i in range(n_filters):
    f = filters[:, :, 0, i]
    axes[i].imshow(f, interpolation='nearest')
    axes[i].set_title(f'Filtro {i}')
    axes[i].axis('off')
# desativa eixos vazios
for j in range(n_filters, len(axes)):
    axes[j].axis('off')
fig.suptitle('Heatmaps dos filtros da 1ª camada', y=1.02)
plt.tight_layout()
plt.show()
