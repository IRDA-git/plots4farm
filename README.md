# plots4farm

Ce package contient 3 fonctions distinctes:

- `create_aligned_grid` : Permet de créer une grille sur un polygone (`sf`).
- `plan.parcelle` : Permet de créer un plan de parcelles à partir des coins des blocs du dispositif expérimental. Cette fonction utilise `create_aligned_grid` de façon interne.
- `img_per_plots` : Classe les images (ex: drone) prises dans les parcelles expérimentales dans des dossiers séparés.

Installation: `remotes::install_github("IRDA/plots4farm")`
