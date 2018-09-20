import numpy as np
import matplotlib.pyplot as plt


def plot_faces(name_faces, name):
    fig, ax = plt.subplots(3, 5)
    for i, axi in enumerate(ax.flat):
        axi.imshow(name_faces[i], cmap='bone')
        axi.set(xticks=[], yticks=[])


def get_faces(faces, name):
    face_number = np.where(faces.target_names == name)[0][0]
    name_faces = faces.images[faces.target == face_number, :, :]

    return np.array([np.ravel(face) for face in name_faces])


def save_faces(faces, file):
    with open(file, 'a') as f:
        for face in faces:
            f.write(','.join((str(x) for x in face)) + '\n')
