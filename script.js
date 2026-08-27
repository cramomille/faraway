const toutesLesLignes = [];
const colonnesScores = ["C8", "C7", "C6", "C5", "C4", "C3", "C2", "C1", "CS"];

async function lireCSV(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Fichier introuvable : ${url}`);

    const lignes = (await response.text()).trim().split(/\r?\n/);
    if (lignes.length < 2) return [];

    const entetes = lignes[0].split(",").map(x => x.trim().replace(/^"|"$/g, ""));

    return lignes.slice(1).map(ligne => {
        const valeurs = ligne.split(",").map(x => x.trim().replace(/^"|"$/g, ""));
        return Object.fromEntries(entetes.map((e, i) => [e, valeurs[i]]));
    });
}

async function chargerParties() {
    for (let numero = 1; ; numero++) {
        const partie = String(numero).padStart(4, "0");

        try {
            const lignes = await lireCSV(`r/data/faraway_game_${partie}.csv`);
            lignes.forEach(ligne => {
                ligne.partie = partie;
                toutesLesLignes.push(ligne);
            });
        } catch {
            break;
        }
    }
}

function calculerScore(joueur) {
    return colonnesScores.reduce((total, colonne) => {
        const valeur = Number(joueur[colonne]);
        return Number.isNaN(valeur) ? total : total + valeur;
    }, 0);
}

function calculerMediane(valeurs) {
    const t = [...valeurs].sort((a, b) => a - b);
    const m = Math.floor(t.length / 2);
    return t.length % 2 ? t[m] : (t[m - 1] + t[m]) / 2;
}


// ==================================================
// STATISTIQUES JOUEUSES
// ==================================================

function calculerStatistiquesJoueuses() {
    const joueuses = {};
    const parties = {};

    toutesLesLignes.forEach(joueur => {
        const nom = joueur.joueuse;
        const score = calculerScore(joueur);

        joueuses[nom] ??= { nom, scores: [], victoires: 0 };
        joueuses[nom].scores.push(score);

        parties[joueur.partie] ??= [];
        parties[joueur.partie].push({ nom, score });
    });

    Object.values(parties).forEach(partie => {
        const max = Math.max(...partie.map(j => j.score));

        partie.forEach(joueur => {
            if (joueur.score === max) {
                joueuses[joueur.nom].victoires++;
            }
        });
    });

    const scores = toutesLesLignes
        .map(calculerScore)
        .sort((a, b) => b - a);

    const limiteTop20 = scores[Math.min(19, scores.length - 1)];

    return Object.values(joueuses)
        .map(({ nom, scores, victoires }) => ({
            nom,
            parties: scores.length,
            pourcentageVictoire: victoires / scores.length * 100,
            scoresTop20: scores.filter(s => s >= limiteTop20).length,
            meilleurScore: Math.max(...scores),
            scoreMin: Math.min(...scores),
            scoreMedian: calculerMediane(scores)
        }))
        .filter(j => j.parties > 3)
        .sort((a, b) => b.parties - a.parties);
}


// ==================================================
// TABLEAU JOUEUSES
// ==================================================

function afficherTableauJoueuses(statistiques) {
    const tableau = document.createElement("table");

    tableau.innerHTML = `
        <thead>
            <tr>
                <th><br>player</th>
                <th>nb <br>games</th>
                <th>% <br>win</th>
                <th>nb <br>top20</th>
                <th>max <br>score</th>
                <th>min <br>score</th>
                <th>median <br>score</th>
            </tr>
        </thead>
        <tbody>
            ${statistiques.map(j => `
                <tr>
                    <td>${j.nom}</td>
                    <td>${j.parties}</td>
                    <td>${Math.round(j.pourcentageVictoire)}</td>
                    <td>${j.scoresTop20}</td>
                    <td>${j.meilleurScore}</td>
                    <td>${j.scoreMin}</td>
                    <td>${Math.round(j.scoreMedian)}</td>
                </tr>
            `).join("")}
        </tbody>
    `;

    document.getElementById("tableau-joueuses").replaceChildren(tableau);
}


// ==================================================
// CLASSEMENT DES SCORES
// ==================================================

function afficherTableauScores() {
    const parties = [...new Set(toutesLesLignes.map(j => j.partie))].sort();
    const dernierePartie = parties.at(-1);

    const scores = toutesLesLignes.map((joueur, ordre) => ({
        id: `${joueur.partie}_${ordre}`,
        joueuse: joueur.joueuse,
        score: calculerScore(joueur),
        partie: joueur.partie,
        ordre
    }));

    function classer(liste) {
        let rang = 0;
        let precedent;

        return [...liste]
            .sort((a, b) => b.score - a.score || a.ordre - b.ordre)
            .map((joueur, i, tableau) => {
                if (joueur.score !== precedent) {
                    rang = i + 1;
                    precedent = joueur.score;
                }
                return { ...joueur, rang };
            });
    }

    const classement = classer(scores);

    function tendance(joueur) {
        return joueur.partie === dernierePartie
            ? ["new", "trend-new"]
            : ["", ""];
    }

    const lignes = [];

    for (let i = 0; i < Math.ceil(classement.length / 2); i++) {
        const gauche = classement[i];
        const droite = classement[classement.length - 1 - i];

        const joueurs = [gauche, droite]
            .filter((j, index) => j && (index === 0 || j.id !== gauche.id));

        lignes.push(`
            <tr>
                ${joueurs.map(j => {
                    const [texte, classe] = tendance(j);

                    return `
                        <td>${j.rang}</td>
                        <td>${j.score}</td>
                        <td>${j.joueuse}</td>
                        <td class="${classe}">${texte}</td>
                    `;
                }).join("")}
            </tr>
        `);
    }

    const tableau = document.createElement("table");

    tableau.innerHTML = `
        <thead>
            <tr>
                <th>rank</th>
                <th>score</th>
                <th>player</th>
                <th>trend</th>
                <th>rank</th>
                <th>score</th>
                <th>player</th>
                <th>trend</th>
            </tr>
        </thead>
        <tbody>
            ${lignes.join("")}
        </tbody>
    `;

    document.getElementById("tableau-scores").replaceChildren(tableau);
}


// ==================================================
// INITIALISATION
// ==================================================

async function afficherStatistiques() {
    try {
        await chargerParties();

        const scores = toutesLesLignes.map(calculerScore);

        document.getElementById("stats").textContent =
            `Nombre de parties : ${new Set(toutesLesLignes.map(j => j.partie)).size}
             Nombre de scores  : ${scores.length}
             Score maximum     : ${Math.max(...scores)}
             Score minimum     : ${Math.min(...scores)}
             Score médian      : ${calculerMediane(scores)}`;

        afficherTableauJoueuses(calculerStatistiquesJoueuses());
        afficherTableauScores();

    } catch (erreur) {
        console.error(erreur);
        document.getElementById("stats").textContent =
            "Erreur lors du chargement des données.";
    }
}

afficherStatistiques();