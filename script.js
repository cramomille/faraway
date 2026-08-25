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

    toutesLesLignes.forEach(joueur => {

        const nom = joueur.joueuse;
        const score = calculerScore(joueur);

        if (!joueuses[nom]) {

            joueuses[nom] = {
                nom: nom,
                parties: 0,
                meilleurScore: 0
            };
        }

        joueuses[nom].parties++;

        if (score > joueuses[nom].meilleurScore) {
            joueuses[nom].meilleurScore = score;
        }
    });

    // Transformation en tableau
    return Object.values(joueuses)

        // Tri par nombre de parties décroissant
        .sort((a, b) => {

            if (b.parties !== a.parties) {
                return b.parties - a.parties;
            }

            // En cas d'égalité :
            // meilleur score décroissant
            return b.meilleurScore - a.meilleurScore;
        });
}


// --------------------------------------------------
// Affichage du tableau des joueuses
// --------------------------------------------------

function afficherTableauJoueuses(statistiques) {

    const tableau = document.createElement("table");

    const thead = document.createElement("thead");
    const ligneEntete = document.createElement("tr");

    const entetes = [
        "Joueuse",
        "Parties",
        "Meilleur score"
    ];

    entetes.forEach(texte => {

        const cellule = document.createElement("th");

        cellule.textContent = texte;

        ligneEntete.appendChild(cellule);
    });

    thead.appendChild(ligneEntete);
    tableau.appendChild(thead);


    const tbody = document.createElement("tbody");

    statistiques.forEach(joueuse => {

        const ligne = document.createElement("tr");


        const celluleNom = document.createElement("td");
        celluleNom.textContent = joueuse.nom;


        const celluleParties = document.createElement("td");
        celluleParties.textContent = joueuse.parties;


        const celluleMeilleurScore = document.createElement("td");
        celluleMeilleurScore.textContent = joueuse.meilleurScore;


        ligne.appendChild(celluleNom);
        ligne.appendChild(celluleParties);
        ligne.appendChild(celluleMeilleurScore);

        tbody.appendChild(ligne);
    });

    tableau.appendChild(tbody);

    document.getElementById("tableau-joueuses").appendChild(tableau);
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
Nombre de scores : ${nombreDeScores}
Score maximum : ${scoreMaximum}
Score minimum : ${scoreMinimum}
Score médian : ${scoreMedian}`;


        // ------------------------------------------
        // Statistiques par joueuse
        // ------------------------------------------

        const statistiquesJoueuses =
            calculerStatistiquesJoueuses();


        afficherTableauJoueuses(
            statistiquesJoueuses
        );


    } catch (erreur) {

        console.error(erreur);

        document.getElementById("stats").textContent =
            "Erreur lors du chargement des données.";
    }
}


afficherStatistiques();