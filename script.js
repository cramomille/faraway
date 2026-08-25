const nombreDeParties = 100;

// Tableau qui contiendra tous les joueurs de toutes les parties
const toutesLesLignes = [];

// Colonnes correspondant aux scores
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
// Lecture d'un fichier CSV
// --------------------------------------------------

async function lireCSV(url) {
    const response = await fetch(url);

    if (!response.ok) {
        throw new Error(`Impossible de lire ${url}`);
    }

    const texte = await response.text();

    const lignes = texte
        .trim()
        .split("\n");

    if (lignes.length < 2) {
        return [];
    }

    const entetes = lignes[0]
        .split(",")
        .map(colonne => colonne.trim());

    return lignes.slice(1).map(ligne => {

        const valeurs = ligne.split(",");

        const objet = {};

        entetes.forEach((entete, index) => {
            objet[entete] = valeurs[index];
        });

        return objet;
    });
}


// --------------------------------------------------
// Chargement de toutes les parties
// --------------------------------------------------

async function chargerParties() {

    for (let i = 1; i <= nombreDeParties; i++) {

        const id = String(i).padStart(4, "0");

        const fichier = `r/data/faraway_game_${id}.csv`;

        try {

            const lignes = await lireCSV(fichier);

            lignes.forEach(ligne => {
                ligne.partie = id;
                toutesLesLignes.push(ligne);
            });

        } catch (erreur) {

            // Le fichier n'existe probablement pas.
            // On arrete simplement la recherche.
            break;
        }
    }
}


// --------------------------------------------------
// Calcul des scores
// --------------------------------------------------

function calculerScore(joueur) {

    return colonnesScores.reduce((total, colonne) => {

        return total + Number(joueur[colonne] || 0);

    }, 0);
}


// --------------------------------------------------
// Mediane
// --------------------------------------------------

function calculerMediane(valeurs) {

    const triees = [...valeurs].sort((a, b) => a - b);

    const milieu = Math.floor(triees.length / 2);

    if (triees.length % 2 === 0) {

        return (triees[milieu - 1] + triees[milieu]) / 2;

    }

    return triees[milieu];
}


// --------------------------------------------------
// Affichage
// --------------------------------------------------

async function afficherStatistiques() {

    try {

        await chargerParties();

        const scores = toutesLesLignes.map(calculerScore);

        const nombreDeScores = scores.length;

        // Chaque fichier correspond a une partie.
        const nombreDeParties = new Set(
            toutesLesLignes.map(ligne => ligne.partie)
        ).size;

        const meilleurScore = Math.max(...scores);

        const pireScore = Math.min(...scores);

        const scoreMedian = calculerMediane(scores);

        document.getElementById("stats").textContent =
`Nombre de parties : ${nombreDeParties}
Nombre de scores : ${nombreDeScores}
Score maximum : ${meilleurScore}
Score minimum : ${pireScore}
Score médian : ${scoreMedian}`;

    } catch (erreur) {

        console.error(erreur);

        document.getElementById("stats").textContent =
            "Erreur lors du chargement des donnees.";
    }
}


afficherStatistiques();