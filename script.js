const toutesLesLignes = [];

const colonnesScores = ["C8", "C7", "C6", "C5", "C4", "C3", "C2", "C1", "CS"];


// ==================================================
// CSV
// ==================================================

async function lireCSV(url) {
    const response = await fetch(url);

    if (!response.ok) {
        throw new Error(`Fichier introuvable : ${url}`);
    }

    const lignes = (await response.text()).trim().split(/\r?\n/);

    if (lignes.length < 2) return [];

    const entetes = lignes[0]
        .split(",")
        .map(x => x.trim().replace(/^"|"$/g, ""));

    return lignes.slice(1).map(ligne => {
        const valeurs = ligne
            .split(",")
            .map(x => x.trim().replace(/^"|"$/g, ""));

        return Object.fromEntries(
            entetes.map((entete, i) => [entete, valeurs[i]])
        );
    });
}


// ==================================================
// CHARGEMENT DES PARTIES
// ==================================================

async function chargerParties() {
    for (let numero = 1; ; numero++) {
        const partie = String(numero).padStart(4, "0");

        try {
            const lignes =
                await lireCSV(`r/data/faraway_game_${partie}.csv`);

            lignes.forEach(ligne => {
                ligne.partie = partie;
                ligne.score = calculerScore(ligne);
                toutesLesLignes.push(ligne);
            });

        } catch {
            break;
        }
    }
}


// ==================================================
// SCORE
// ==================================================

function calculerScore(joueur) {
    return colonnesScores.reduce((total, colonne) => {
        const valeur = Number(joueur[colonne]);

        return Number.isNaN(valeur)
            ? total
            : total + valeur;
    }, 0);
}


// ==================================================
// MÉDIANE
// ==================================================

function calculerMediane(valeurs) {
    const triees = [...valeurs].sort((a, b) => a - b);
    const milieu = Math.floor(triees.length / 2);

    return triees.length % 2
        ? triees[milieu]
        : (triees[milieu - 1] + triees[milieu]) / 2;
}


// ==================================================
// STATISTIQUES PAR JOUEUSE
// ==================================================

function calculerStatistiquesJoueuses() {
    const joueuses = {};
    const parties = {};

    toutesLesLignes.forEach(joueur => {
        const nom = joueur.joueuse;

        joueuses[nom] ??= {
            nom,
            scores: [],
            victoires: 0
        };

        joueuses[nom].scores.push(joueur.score);

        parties[joueur.partie] ??= [];
        parties[joueur.partie].push(joueur);
    });


    // Victoires
    Object.values(parties).forEach(partie => {
        const meilleurScore =
            Math.max(...partie.map(j => j.score));

        partie.forEach(joueur => {
            if (joueur.score === meilleurScore) {
                joueuses[joueur.joueuse].victoires++;
            }
        });
    });


    // Top 20
    const tousLesScores = toutesLesLignes
        .map(j => j.score)
        .sort((a, b) => b - a);

    const limiteTop20 =
        tousLesScores[Math.min(19, tousLesScores.length - 1)];


    return Object.values(joueuses)
        .map(joueuse => {
            const scores = joueuse.scores;

            return {
                nom: joueuse.nom,
                parties: scores.length,

                pourcentageVictoire:
                    joueuse.victoires / scores.length * 100,

                scoresTop20:
                    scores.filter(s => s >= limiteTop20).length,

                meilleurScore:
                    Math.max(...scores),

                scoreMin:
                    Math.min(...scores),

                scoreMedian:
                    calculerMediane(scores)
            };
        })
        .filter(joueuse => joueuse.parties > 3)
        .sort((a, b) => b.parties - a.parties);
}


// ==================================================
// TABLEAU DES JOUEUSES
// ==================================================

function afficherTableauJoueuses(statistiques) {

    const colonnes = [
        ["<br>player", "col-left"],
        ["nb <br>games", "col-right"],
        ["% <br>win", "col-right"],
        ["nb <br>top20", "col-right"],
        ["max <br>score", "col-right"],
        ["min <br>score", "col-right"],
        ["median <br>score", "col-right"]
    ];

    const tableau = document.createElement("table");

    tableau.innerHTML = `
        <thead>
            <tr>
                ${colonnes.map(([texte, classe]) =>
                    `<th class="${classe}">${texte}</th>`
                ).join("")}
            </tr>
        </thead>

        <tbody>
            ${statistiques.map(joueuse => {

                const valeurs = [
                    joueuse.nom,
                    joueuse.parties,
                    Math.round(joueuse.pourcentageVictoire),
                    joueuse.scoresTop20,
                    joueuse.meilleurScore,
                    joueuse.scoreMin,
                    Math.round(joueuse.scoreMedian)
                ];

                return `
                    <tr>
                        ${valeurs.map((valeur, i) =>
                            `<td class="${colonnes[i][1]}">${valeur}</td>`
                        ).join("")}
                    </tr>
                `;

            }).join("")}
        </tbody>
    `;

    document
        .getElementById("tableau-joueuses")
        .replaceChildren(tableau);
}


// ==================================================
// CLASSEMENT
// ==================================================

function classerScores(liste) {
    let rang = 0;
    let scorePrecedent;

    return [...liste]
        .sort((a, b) =>
            b.score - a.score ||
            a.ordre - b.ordre
        )
        .map((joueur, index) => {

            if (joueur.score !== scorePrecedent) {
                rang = index + 1;
                scorePrecedent = joueur.score;
            }

            return {
                ...joueur,
                rang
            };
        });
}


// ==================================================
// TABLEAU DES SCORES
// ==================================================

function afficherTableauScores() {

    const parties = [
        ...new Set(
            toutesLesLignes.map(j => j.partie)
        )
    ].sort();

    const dernierePartie1 = parties.at(-1);
    const dernierePartie2 = parties.at(-2);
    const dernierePartie3 = parties.at(-3);

    const scores = toutesLesLignes.map((joueur, ordre) => ({
        id: `${joueur.partie}_${ordre}`,
        joueuse: joueur.joueuse,
        score: joueur.score,
        partie: joueur.partie,
        ordre
    }));


    const classement = classerScores(scores);


    // --------------------------------------------------
    // Colonnes
    // --------------------------------------------------

    const colonnes = [
        ["rank", "col-right"],
        ["player", "col-left"],
        ["score", "col-right"],
        ["new", "col-left"],

        ["rank", "col-right"],
        ["player", "col-left"],
        ["score", "col-right"],
        ["new", "col-left"]
    ];


    // --------------------------------------------------
    // Création du tableau
    // --------------------------------------------------

    const tableau = document.createElement("table");

    tableau.innerHTML = `
        <thead>
            <tr>
                ${colonnes.map(([texte, classe]) =>
                    `<th class="${classe}">${texte}</th>`
                ).join("")}
            </tr>
        </thead>

        <tbody></tbody>
    `;

    const tbody = tableau.querySelector("tbody");


    // --------------------------------------------------
    // Création des lignes
    // --------------------------------------------------

    for (let i = 0; i < Math.ceil(classement.length / 2); i++) {

        const gauche = classement[i];

        const droite =
            classement[classement.length - 1 - i];

        const joueurs = [gauche];

        if (droite && droite.id !== gauche.id) {
            joueurs.push(droite);
        }

        const ligne = document.createElement("tr");

        joueurs.forEach(joueur => {

            const nouveau = 
                joueur.partie === dernierePartie1
                    ? "x1"
                    : joueur.partie === dernierePartie2
                        ? "x2"
                        : joueur.partie === dernierePartie3
                            ? "x3"
                            : "";

            const valeurs = [
                joueur.rang,
                joueur.joueuse,
                joueur.score,
                nouveau
            ];

            valeurs.forEach((valeur, index) => {

                const cellule = document.createElement("td");

                cellule.textContent = valeur;
                cellule.classList.add(colonnes[index][1]);

                if (["x1", "x2", "x3"].includes(valeur)) {
                    cellule.classList.add("new");
                }

                ligne.appendChild(cellule);
            });
        });


        // Si la ligne n'a qu'un joueur,
        // ajouter les 4 cellules manquantes.
        while (ligne.children.length < 8) {
            const cellule = document.createElement("td");
            ligne.appendChild(cellule);
        }


        tbody.appendChild(ligne);
    }

    document
        .getElementById("tableau-scores")
        .replaceChildren(tableau);
}


// ==================================================
// STATISTIQUES GÉNÉRALES
// ==================================================

async function afficherStatistiques() {

    try {

        await chargerParties();

        const scores =
            toutesLesLignes.map(j => j.score);


        const nombreDeParties =
            new Set(
                toutesLesLignes.map(j => j.partie)
            ).size;


        document.getElementById("stats").textContent =
            `Nombre de parties : ${nombreDeParties}
             Nombre de scores  : ${scores.length}
             Score maximum     : ${Math.max(...scores)}
             Score minimum     : ${Math.min(...scores)}
             Score médian      : ${calculerMediane(scores)}`;


        afficherTableauJoueuses(
            calculerStatistiquesJoueuses()
        );

        afficherTableauScores();

    } catch (erreur) {

        console.error(erreur);

        document.getElementById("stats").textContent =
            "Erreur lors du chargement des données.";
    }
}


afficherStatistiques();