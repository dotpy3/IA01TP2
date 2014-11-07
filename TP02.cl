(setq etats '((0 1) (1 1) (2 1) (3 1) (4 1) (5 1) (0 2) (1 2) (2 2) (3 2) (4 2) (5 2)))

; On a rajouté tous les états possibles dans une variable lexicale etats.

(setq operateurs '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3)))

; On a rajouté tous les opérateurs dans une variable lexicale operateurs.

; On veut à présent créer des fonctions qui puissent résoudre le problème de donner
; un parcours possible, que ce soit en profondeur ou en largeur d'abord.

(format t "Voici le résultat d'une recherche en profondeur d'abord.")

(defun exploreProfondeur (etatActuel etatFinal)
(let ((resultatFinal) (successeursPos) (resultat))
(if (equal etatActuel etatFinal) (setq resultatFinal etatFinal)
(progn (setq successeursPos (successeurs etatActuel))
(setq resultatFinal nil)
(if (not (eq successeursPos nil))
(progn (loop for i in successeursPos while (null resultat) do
(setq resultat (exploreProfondeur i etatFinal)))
(setq resultatFinal (cons etatActuel (list resultat)))
)
)
))
(print resultatFinal)
)
)

; MYMEMBER : FONCTION QUI PREND EN PARAMETRE UNE LISTE ET UNE LISTE DE LISTES
; SI LA LISTE SE TROUVE DANS LA LISTE DE LISTES gListe, LA FONCTION REND T
; LA FONCTION REND NIL SINON

(defun myMember (liste gListe)
(let ((trouve nil))
(loop for i in gListe while (null trouve) do
(if (equal i liste) (setq trouve T))
)
trouve
)
)

; SUCCESSEURS : VERIFIEE, MARCHE
; prend en paramètres une liste de 2 éléments qui correspond à un état actuel (cf la liste d'états "etats")
; rend la liste des états suivants possibles

(defun successeurs (etatActuel)
(let ((operateursPoss (opePossibles etatActuel)) (etatsPossibles))
(setq etatsPossibles (transfo operateursPoss etatActuel))
(loop for i in etatsPossibles do
(if (not (myMember i etats)) (setq etatsPossibles (remove i etatsPossibles)))
)
etatsPossibles)
)

; OPEPOSSIBLES : VERIFIEE, MARCHE
; prend en paramètres une liste de 2 éléments qui représente un état actuel (cf la liste d'états "etats").
; la valeur rendue dépend également de la valeur de la variable lexicale d'opérateurs possibles operateurs.

(defun opePossibles (etatActuel)
(let ((joueur (cadr etatActuel)) (opePossible operateurs))
(loop for i in opePossible do
(if (not (eq (car i) joueur)) (setq opePossible (remove i opePossible)))
)
(loop for i in opePossible do
(if (< (car etatActuel) (cadr i)) (setq opePossible (remove i opePossible)))
)
opePossible)
)

; TRANSFO : VERIFIEE, MARCHE
; prend en paramètres une liste de listes de 2 éléments dans opePossibles, et une liste de 2 éléments dans etatActuel

(defun transfo (opePossibles etatActuel)
; cette fonction, à partir des opérateurs possibles et de l'état actuel, renvoit les états suivants possibles
(loop for i in opePossibles
collect (list (- (car etatActuel) (cadr i)) (+ (mod (+ 1 (- (cadr etatActuel) 1)) 2) 1)))
)