<div align="center">

# Image Compressor

**Réduire les couleurs d'une image avec l'algorithme k-means, en Haskell.**

<img alt="Haskell" src="https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"/> <img alt="Stack" src="https://img.shields.io/badge/Stack-5D4F85?style=for-the-badge"/>

<sub>Projet à 2 · 2024</sub>

</div>

<br/>

<p align="center"><img src=".github/assets/image-compressor-before-after.png" alt="Image Compressor" width="820"/></p>

## Le projet en quelques mots

On donne au programme la liste des pixels d'une image, et il les regroupe en *N* couleurs avec **k-means**. Ci-dessus : l'image d'origine, puis la même image réduite à 8 couleurs par le programme.

## Ce que ça fait

- Lecture d'une liste de pixels `(x,y) (r,g,b)`
- **k-means** : centroïdes de départ, affectation par distance euclidienne, recalcul jusqu'à convergence
- Nombre de couleurs (`-n`) et seuil de convergence (`-l`) réglables
- Gestion d'erreurs stricte (arguments, format du fichier) avec code de sortie 84

## Lancer le projet

```bash
make
./imageCompressor -n 8 -l 0.5 -f examples/rose.txt
# sortie : chaque couleur finale suivie de la liste de ses pixels
```

## L'équipe

<a href="https://github.com/Wilson-Epitech/image-compressor/graphs/contributors"><img src="https://contrib.rocks/image?repo=Wilson-Epitech/image-compressor" alt="Contributeurs"/></a>

Wilson Bordichon, Bradley Malbrouck

**Ma partition :** J'ai écrit le **parsing des arguments** et des pixels, toute la **gestion d'erreurs**, le calcul de distance, une partie de l'algorithme et le point d'entrée du programme.
