const toutesLesLignes = [];

const colonnesScores = [
    "C8",
    "C7",
    "C6",
    "C5",
    "C4",
    "C3",
    "C2",
    "C1",
    "CS"
];


// --------------------------------------------------
// Lecture d'un CSV
// --------------------------------------------------

async function lireCSV(url) {

    const response = await fetch(url);

    if (!response.ok) {
        throw new Error(`Fichier introuvable : ${url}`);
    }

    const texte = await response.text();

    const lignes = texte
        .trim()
        .split(/\r?\n/);

    if (lignes.length < 2) {
        return [];
    }

    const entetes = lignes[0]
        .split(",")
        .map(x => x.trim().replace(/^"|"$/g, ""));

    return lignes.slice(1).map(ligne => {

        const valeurs = ligne
            .split(",")
            .map(x => x.trim().replace(/^"|"$/g, ""));

        const objet = {};

        entetes.forEach((entete, index) => {
            objet[entete] = valeurs[index];
        });

        return objet;
    });
}


// --------------------------------------------------
// Chargement des parties
// --------------------------------------------------

async function chargerParties() {

    let numero = 1;

    while (true) {

        const id = String(numero).padStart(4, "0");

        const fichier = `r/data/faraway_game_${id}.csv`;

        try {

            const lignes = await lireCSV(fichier);

            lignes.forEach(ligne => {
                ligne.partie = id;
                toutesLesLignes.push(ligne);
            });

            console.log(`Partie ${id} chargée`);

            numero++;

        } catch (erreur) {

            console.log(`Fin des fichiers à la partie ${id}`);
            break;
        }
    }
}


// --------------------------------------------------
// Calcul du score d'une joueuse
// --------------------------------------------------

function calculerScore(joueur) {

    let total = 0;

    colonnesScores.forEach(colonne => {

        const valeur = Number(joueur[colonne]);

        if (!Number.isNaN(valeur)) {
            total += valeur;
        }
    });

    return total;
}


// --------------------------------------------------
// Médiane
// --------------------------------------------------

function calculerMediane(valeurs) {

    const triees = [...valeurs].sort((a, b) => a - b);

    const milieu = Math.floor(triees.length / 2);

    if (triees.length % 2 === 0) {

        return (
            triees[milieu - 1] +
            triees[milieu]
        ) / 2;

    } else {

        return triees[milieu];
    }
}


// --------------------------------------------------
// Statistiques par joueuse
// --------------------------------------------------

function calculerStatistiquesJoueuses() {

    const joueuses = {};

    // Regroupement des scores par joueuse
    toutesLesLignes.forEach(joueur => {

        const nom = joueur.joueuse;
        const score = calculerScore(joueur);

        if (!joueuses[nom]) {
            joueuses[nom] = {
                nom: nom,
                scores: [],
                victoires: 0
            };
        }

        joueuses[nom].scores.push(score);
    });


    // --------------------------------------------------
    // Déterminer les victoires
    // --------------------------------------------------

    const parties = {};

    toutesLesLignes.forEach(joueur => {

        const score = calculerScore(joueur);

        if (!parties[joueur.partie]) {
            parties[joueur.partie] = [];
        }

        parties[joueur.partie].push({
            nom: joueur.joueuse,
            score: score
        });
    });


    Object.values(parties).forEach(partie => {

        const meilleurScore = Math.max(
            ...partie.map(joueur => joueur.score)
        );

        partie.forEach(joueur => {

            if (joueur.score === meilleurScore) {
                joueuses[joueur.nom].victoires++;
            }

        });
    });


    // --------------------------------------------------
    // Déterminer le Top 20 global
    // --------------------------------------------------

    const tousLesScores = toutesLesLignes
        .map(joueur => calculerScore(joueur))
        .sort((a, b) => b - a);

    const scoreLimiteTop20 =
        tousLesScores[Math.min(19, tousLesScores.length - 1)];


    // --------------------------------------------------
    // Statistiques de chaque joueuse
    // --------------------------------------------------

    const statistiques = Object.values(joueuses)
        .map(joueuse => {

            const scores = joueuse.scores;

            return {
                nom: joueuse.nom,

                parties: scores.length,

                pourcentageVictoire:
                    (joueuse.victoires / scores.length) * 100,

                scoresTop20:
                    scores.filter(
                        score => score >= scoreLimiteTop20
                    ).length,

                meilleurScore:
                    Math.max(...scores),

                scoreMin:
                    Math.min(...scores),

                scoreMedian:
                    calculerMediane(scores)
            };
        })

        // Uniquement les joueuses ayant plus de 3 parties
        .filter(joueuse => joueuse.parties > 3);


    // --------------------------------------------------
    // Tri par nombre de parties décroissant
    // --------------------------------------------------

    statistiques.sort((a, b) => {

        return b.parties - a.parties;

    });


    return statistiques;
}


// --------------------------------------------------
// Affichage du tableau des joueuses
// --------------------------------------------------

function afficherTableauJoueuses(statistiques) {

    const tableau = document.createElement("table");

    // --------------------------------------------------
    // En-têtes
    // --------------------------------------------------

    const thead = document.createElement("thead");
    const ligneEntete = document.createElement("tr");

    const entetes = [
        "<br> player",
        "nb <br> games",
        "% <br> win",
        "nb <br> top20",
        "max <br> score",
        "min <br> score",
        "median <br> score"
    ];

    entetes.forEach(texte => {

        const cellule = document.createElement("th");

        cellule.innerHTML = texte;

        ligneEntete.appendChild(cellule);
    });

    thead.appendChild(ligneEntete);
    tableau.appendChild(thead);


    // --------------------------------------------------
    // Lignes
    // --------------------------------------------------

    const tbody = document.createElement("tbody");

    statistiques.forEach(joueuse => {

        const ligne = document.createElement("tr");

        const valeurs = [
            joueuse.nom,
            joueuse.parties,
            Math.round(joueuse.pourcentageVictoire),
            joueuse.scoresTop20,
            joueuse.meilleurScore,
            joueuse.scoreMin,
            Math.round(joueuse.scoreMedian)
        ];

        valeurs.forEach(valeur => {

            const cellule = document.createElement("td");

            cellule.textContent = valeur;

            ligne.appendChild(cellule);
        });

        tbody.appendChild(ligne);
    });

    tableau.appendChild(tbody);


    // --------------------------------------------------
    // Affichage
    // --------------------------------------------------

    const conteneur =
        document.getElementById("tableau-joueuses");

    conteneur.innerHTML = "";

    conteneur.appendChild(tableau);
}


// --------------------------------------------------
// Affichage du classement des scores en double colonne
// --------------------------------------------------

function afficherTableauScores() {

    // --------------------------------------------------
    // Identifier la dernière partie
    // --------------------------------------------------

    const parties = [...new Set(
        toutesLesLignes.map(joueur => joueur.partie)
    )].sort();

    const dernierePartie = parties.at(-1);


    // --------------------------------------------------
    // Création de tous les scores
    // --------------------------------------------------

    const scores = toutesLesLignes.map((joueur, index) => {

        return {
            id: `${joueur.partie}_${index}`,
            joueuse: joueur.joueuse,
            score: calculerScore(joueur),
            partie: joueur.partie,
            ordre: index
        };

    });


    // --------------------------------------------------
    // Classement
    // --------------------------------------------------

    function classerScores(liste) {

        const classement = [...liste];

        classement.sort((a, b) => {

            // Score décroissant
            if (b.score !== a.score) {
                return b.score - a.score;
            }

            // En cas d'égalité :
            // le plus ancien apparaît en premier
            return a.ordre - b.ordre;

        });


        let rang = 1;

        classement.forEach((joueur, index) => {

            if (
                index === 0 ||
                joueur.score !== classement[index - 1].score
            ) {
                rang = index + 1;
            }

            joueur.rang = rang;

        });

        return classement;
    }


    // --------------------------------------------------
    // Ancien classement
    // --------------------------------------------------

    const anciensScores = scores.filter(
        joueur => joueur.partie !== dernierePartie
    );

    const ancienClassement =
        classerScores(anciensScores);


    // --------------------------------------------------
    // Nouveau classement
    // --------------------------------------------------

    const classement =
        classerScores(scores);


    // --------------------------------------------------
    // Correspondance ancien / nouveau classement
    // --------------------------------------------------

    const anciensParScore = {};

    ancienClassement.forEach(joueur => {

        if (!anciensParScore[joueur.score]) {
            anciensParScore[joueur.score] = [];
        }

        anciensParScore[joueur.score].push(joueur);

    });


    const correspondances = new Map();


    classement.forEach(joueur => {

        // Les scores de la dernière partie sont "new"
        if (joueur.partie === dernierePartie) {
            return;
        }

        const disponibles =
            anciensParScore[joueur.score];

        if (disponibles && disponibles.length > 0) {

            // On prend toujours le plus ancien
            const ancien = disponibles.shift();

            correspondances.set(
                joueur.id,
                ancien.rang
            );

        }

    });


    // --------------------------------------------------
    // Calcul de la tendance
    // --------------------------------------------------

    function obtenirTendance(joueur) {

        // Nouveau score
        if (joueur.partie === dernierePartie) {

            return {
                texte: "new",
                classe: "trend-new"
            };

        }


        const ancienRang =
            correspondances.get(joueur.id);


        // Aucun ancien score correspondant
        if (ancienRang === undefined) {

            return {
                texte: "new",
                classe: "trend-new"
            };

        }


        const evolution =
            ancienRang - joueur.rang;


        // Même rang
        if (evolution === 0) {

            return {
                texte: "",
                classe: ""
            };

        }


        // Monte dans le classement
        if (evolution > 0) {

            return {
                texte: `+${evolution}`,
                classe: "trend-up"
            };

        }


        // Descend dans le classement
        return {
            texte: `${evolution}`,
            classe: "trend-down"
        };

    }


    // --------------------------------------------------
    // Création du tableau
    // --------------------------------------------------

    const tableau = document.createElement("table");


    // --------------------------------------------------
    // En-têtes
    // --------------------------------------------------

    const thead = document.createElement("thead");

    const ligneEntete =
        document.createElement("tr");


    const entetes = [
        "rank",
        "score",
        "player",
        "trend",
        "rank",
        "score",
        "player",
        "trend"
    ];


    entetes.forEach(texte => {

        const cellule =
            document.createElement("th");

        cellule.textContent = texte;

        ligneEntete.appendChild(cellule);

    });


    thead.appendChild(ligneEntete);
    tableau.appendChild(thead);


    // --------------------------------------------------
    // Création des deux côtés
    // --------------------------------------------------

    const tbody =
        document.createElement("tbody");


    const nombreDeLignes =
        Math.ceil(classement.length / 2);


    for (let i = 0; i < nombreDeLignes; i++) {

        const ligne =
            document.createElement("tr");


        // --------------------------------------------------
        // Côté gauche :
        // meilleur score -> vers le milieu
        // --------------------------------------------------

        const joueurGauche =
            classement[i];


        // --------------------------------------------------
        // Côté droit :
        // pire score -> vers le milieu
        // --------------------------------------------------

        const indexDroit =
            classement.length - 1 - i;

        const joueurDroit =
            classement[indexDroit];


        // --------------------------------------------------
        // Fonction pour créer les cellules d'un joueur
        // --------------------------------------------------

        function ajouterJoueur(joueur) {

            if (!joueur) {

                for (let j = 0; j < 4; j++) {

                    const cellule =
                        document.createElement("td");

                    cellule.textContent = "";

                    ligne.appendChild(cellule);

                }

                return;
            }


            // Rank
            const celluleClassement =
                document.createElement("td");

            celluleClassement.textContent =
                joueur.rang;


            // Trend
            const celluleEvolution =
                document.createElement("td");

            const tendance =
                obtenirTendance(joueur);

            celluleEvolution.textContent =
                tendance.texte;

            if (tendance.classe) {
                celluleEvolution.classList.add(
                    tendance.classe
                );
            }


            // Score
            const celluleScore =
                document.createElement("td");

            celluleScore.textContent =
                joueur.score;


            // Player
            const celluleJoueuse =
                document.createElement("td");

            celluleJoueuse.textContent =
                joueur.joueuse;


            ligne.appendChild(celluleClassement);
            ligne.appendChild(celluleScore);
            ligne.appendChild(celluleJoueuse);
            ligne.appendChild(celluleEvolution);

        }


        // --------------------------------------------------
        // Ajouter gauche
        // --------------------------------------------------

        ajouterJoueur(joueurGauche);


        // --------------------------------------------------
        // Ajouter droite
        // --------------------------------------------------

        if (joueurDroit && joueurDroit.id !== joueurGauche.id) {
            ajouterJoueur(joueurDroit);
        }


        tbody.appendChild(ligne);

    }


    tableau.appendChild(tbody);


    // --------------------------------------------------
    // Affichage
    // --------------------------------------------------

    const conteneur =
        document.getElementById("tableau-scores");

    conteneur.innerHTML = "";

    conteneur.appendChild(tableau);

}


// --------------------------------------------------
// Statistiques générales
// --------------------------------------------------

async function afficherStatistiques() {

    try {

        await chargerParties();

        console.log("Toutes les lignes :", toutesLesLignes);


        // ------------------------------------------
        // Calcul des scores
        // ------------------------------------------

        const scores = toutesLesLignes.map(joueur => {

            const score = calculerScore(joueur);

            console.log(
                joueur.joueuse,
                joueur.partie,
                score
            );

            return score;
        });


        // ------------------------------------------
        // Statistiques globales
        // ------------------------------------------

        const nombreDeParties = new Set(
            toutesLesLignes.map(joueur => joueur.partie)
        ).size;


        const nombreDeScores = scores.length;


        const scoreMaximum = Math.max(...scores);


        const scoreMinimum = Math.min(...scores);


        const scoreMedian = calculerMediane(scores);


        // ------------------------------------------
        // Affichage des statistiques
        // ------------------------------------------

        document.getElementById("stats").textContent =
            `Nombre de parties : ${nombreDeParties}
             Nombre de scores  : ${nombreDeScores}
             Score maximum     : ${scoreMaximum}
             Score minimum     : ${scoreMinimum}
             Score médian      : ${scoreMedian}`;


        // ------------------------------------------
        // Statistiques par joueuse
        // ------------------------------------------

        const statistiquesJoueuses =
            calculerStatistiquesJoueuses();

        afficherTableauJoueuses(
            statistiquesJoueuses
        );

        afficherTableauScores();


    } catch (erreur) {

        console.error(erreur);

        document.getElementById("stats").textContent =
            "Erreur lors du chargement des données.";
    }
}


afficherStatistiques();