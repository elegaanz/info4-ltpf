type couleur =
  | Rouge
  | Vert
  | Bleu
  | Jaune

type valeur =
  | Zero
  | Un
  | Deux
  | Trois
  | Quatre
  | Cinq
  | Six
  | Sept
  | Huit
  | Neuf
  | ChangementDeSens
  | Passer
  | PlusDeux

type carte =
  | Couleur of couleur * valeur
  | ChangementDeCouleur
  | PlusQuatre